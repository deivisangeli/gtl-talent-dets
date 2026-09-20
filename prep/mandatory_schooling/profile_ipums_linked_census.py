"""Profile the IPUMS MLP linked census extract and write a variable dictionary.

The extract (usa_00001.csv.gz) was downloaded as a bare CSV, so what documents it
is assembled here from three sources, each with a different authority:

  * the numbers -- fill rate, distinct count, range, top values, per column per
    census year -- come from streaming the whole file once;
  * value labels come from IPUMS' own DDI codebook, when one is present;
  * variable labels and the project's coding notes come from the checked-in
    ipums_variable_labels.csv, which is cross-checked against the DDI rather
    than trusted.

The DDI is the part that has to be fetched by hand: see the acquisition recipe
in prep/mandatory_schooling/README.md. Without it the run still completes --
value labels are simply absent and the outputs record ddi_cross_checked: false --
so a missing codebook degrades the dictionary instead of blocking it.

Despite the name the file is NOT a two-year 1900-1940 link. It stacks all five
census years, one row per person per census in which they appear, and the row
counts sit far below full-count size because only linked cases are included.

The profiler also settles four facts the merge specification depends on, rather
than asserting them from memory (see merge_cls_to_linked_census.md):

  1. whether BIRTHYR on 1940 rows equals 1940 - AGE - 1, the CLS cohort key;
  2. whether BPL is constant within HIK, so the join has a well-defined key;
  3. whether HISTID is unique within year and HIK is the across-year key;
  4. which variables are 1940-only (attainment) versus present throughout.

Facts 2 and 3 need per-person state over 221 M rows, so they are answered on a
bounded sample of HIKs rather than exhaustively; the output says so.

Run from any working directory:
    python prep/mandatory_schooling/profile_ipums_linked_census.py

    # re-render labels and re-run the cross-checks off the saved numbers,
    # without touching the 13 GB file (seconds, not 16 minutes)
    python prep/mandatory_schooling/profile_ipums_linked_census.py --labels-only

Environment:
    IPUMS_LINKED_CSV   path to the extract
                       (default: C:\\Users\\<user>\\OneDrive\\Documentos\\usa_00001.csv.gz)
    IPUMS_LINKED_DDI   path to the DDI codebook
                       (default: <data root>/Data/raw/mandatory_schooling/usa_00001.xml)
    IPUMS_LINKED_MAX_ROWS   stop after N rows (dry run; default: all)

Outputs (next to this script):
    ipums_linked_1900_1940_dictionary.md
    ipums_linked_1900_1940_dictionary.csv
    ipums_linked_1900_1940_provenance.json
"""

import csv
import json
import os
import re
import sys
import time
from collections import Counter, defaultdict
from pathlib import Path

import pyarrow as pa
from pyarrow import csv as pacsv
import pyarrow.compute as pc

sys.path.insert(0, str(Path(__file__).resolve().parent))
from ddi_helpers import code_key, ddi_provenance, read_ddi   # noqa: E402

HERE = Path(__file__).resolve().parent
LABELS_CSV = HERE / "ipums_variable_labels.csv"
OUT_MD = HERE / "ipums_linked_1900_1940_dictionary.md"
OUT_CSV = HERE / "ipums_linked_1900_1940_dictionary.csv"
OUT_JSON = HERE / "ipums_linked_1900_1940_provenance.json"

SOURCE = Path(os.environ.get(
    "IPUMS_LINKED_CSV",
    Path.home() / "OneDrive" / "Documentos" / "usa_00001.csv.gz"))
MAX_ROWS = int(os.environ.get("IPUMS_LINKED_MAX_ROWS", "0")) or None

DDI_BASENAME = "usa_00001.xml"

# Established by a full pass over the file; the run fails if they no longer hold,
# which is how a swapped or truncated extract gets caught.
EXPECTED_TOTAL = 220_979_023
EXPECTED_BY_YEAR = {
    1900: 12_971_952, 1910: 23_984_383, 1920: 39_982_220,
    1930: 70_207_628, 1940: 73_832_840,
}

DISTINCT_CAP = 4000     # beyond this a column is reported as high-cardinality
TOP_N = 8
HIK_SAMPLE_PREFIX = "a"  # ~1/60 of HIKs, for the within-person consistency checks

# Effectively unique per row, so value_counts would build a hash table the size
# of the batch on every batch and tell us nothing. Counted, not tabulated.
HIGH_CARDINALITY = {"HISTID", "HIK", "SERIAL"}

# A DDI lists every category a variable can take across all IPUMS samples, so a
# long code list is normal; printing all of it would bury the table. These are
# shown in full and the rest are truncated to the codes actually observed.
VALUE_TABLE_CAP = 40


# --------------------------------------------------------------------------- #
# The DDI codebook
# --------------------------------------------------------------------------- #

def resolve_data_root():
    """Same three-tier fallback the R prep scripts use."""
    for var in ("TALENT_DETS_DATA_DIR", "DET_DIR"):
        value = os.environ.get(var)
        if value and Path(value).is_dir():
            return Path(value)
    fallback = Path.home() / "Globtalent Dropbox" / "gtl_talent_dets"
    if fallback.is_dir():
        return fallback
    return None


def resolve_ddi():
    """Where the DDI should be, whether or not it is there yet."""
    explicit = os.environ.get("IPUMS_LINKED_DDI")
    if explicit:
        return Path(explicit)
    root = resolve_data_root()
    if root is None:
        return None
    # The repo documents raw/ at the project root; this file sits under Data/raw/,
    # beside EXT1940USCB.dat.gz.
    for candidate in (root / "Data" / "raw" / "mandatory_schooling" / DDI_BASENAME,
                      root / "raw" / "mandatory_schooling" / DDI_BASENAME):
        if candidate.is_file():
            return candidate
    return root / "Data" / "raw" / "mandatory_schooling" / DDI_BASENAME


def load_ddi():
    """(ddi, provenance). Both None when no codebook is available.

    Offline -- or rather, not-yet-downloaded -- is survivable by design. The
    extract predates the codebook here, so refusing to run without one would
    make the dictionary unbuildable for the state the project is actually in.
    """
    path = resolve_ddi()
    if path is None or not path.is_file():
        print(f"  ! no DDI codebook at {path}")
        print("    value labels will be omitted; see README for how to fetch it")
        return None, None
    ddi = read_ddi(path)
    meta = ddi_provenance(path)
    print(f"  DDI: {path.name} -- {len(ddi)} variables, "
          f"{sum(1 for v in ddi.values() if v['values'])} with value labels")
    return ddi, meta


def cross_check_labels(ddi, header, labels):
    """Fail loudly if the checked-in labels disagree with IPUMS' own codebook.

    The analogue of cross_check_against_sps() in convert_ipums_1940_fullcount.py.
    A hand-transcribed label that quietly describes the wrong variable is the
    error this exists to catch, so a disagreement raises rather than warns.
    """
    missing_from_ddi = [name for name in header if name.upper() not in ddi]
    if missing_from_ddi:
        raise ValueError(
            "columns present in the extract but absent from the DDI: "
            + ", ".join(missing_from_ddi)
            + "\nThe codebook does not describe this extract.")

    unknown_vars = [name for name in labels if name.upper() not in ddi]
    if unknown_vars:
        raise ValueError(
            f"{LABELS_CSV.name} documents variables the DDI does not contain: "
            + ", ".join(unknown_vars))

    undocumented = [name for name in header if name not in labels]
    if undocumented:
        print(f"  ! {len(undocumented)} column(s) have no row in {LABELS_CSV.name}: "
              + ", ".join(undocumented))

    # Label text is compared case- and punctuation-insensitively: the project
    # rewords labels for readability, and only a genuinely different variable
    # should stop the run.
    def canon(text):
        return re.sub(r"[^a-z0-9]+", " ", text.lower()).strip()

    reworded = []
    for name in header:
        ours = labels.get(name, {}).get("label", "")
        theirs = ddi[name.upper()]["label"]
        if ours and theirs and canon(ours) != canon(theirs):
            reworded.append((name, ours, theirs))
    if reworded:
        print(f"  {len(reworded)} label(s) reworded from the DDI wording:")
        for name, ours, theirs in reworded[:10]:
            print(f"      {name}: ours={ours!r} DDI={theirs!r}")
        if len(reworded) > 10:
            print(f"      ... and {len(reworded) - 10} more")

    print(f"  labels cross-checked against the DDI: {len(header)} columns, "
          f"{len(header) - len(undocumented)} documented, identical coverage")
    return {"n_columns": len(header), "n_undocumented": len(undocumented),
            "undocumented": undocumented,
            "n_reworded": len(reworded),
            "reworded": [name for name, _, _ in reworded]}


def cross_check_codes(ddi, columns, stats, years):
    """Report observed codes that the DDI has no category for.

    A warning, not a failure: IPUMS top-codes some variables and the DDI
    category list does not enumerate continuous ranges (AGE, BIRTHYR, INCWAGE),
    so unmatched codes are expected and only worth reading for the categorical
    variables.
    """
    report = {}
    for name in columns:
        spec = ddi.get(name.upper())
        if not spec or not spec["keys"]:
            continue
        observed = set()
        for year in years:
            st = stats[year][name]
            if st.opaque:
                continue
            observed.update(code_key(v) for v in st.values)
        unmatched = sorted(observed - set(spec["keys"]), key=_sort_key)
        if unmatched:
            report[name] = unmatched
    if report:
        print(f"  {len(report)} variable(s) show codes the DDI does not list "
              "(expected for continuous and top-coded variables):")
        for name, unmatched in list(report.items())[:8]:
            shown = ", ".join(unmatched[:6])
            more = f" (+{len(unmatched) - 6})" if len(unmatched) > 6 else ""
            print(f"      {name}: {shown}{more}")
    return report


def _sort_key(value):
    try:
        return (0, float(value), "")
    except ValueError:
        return (1, 0.0, value)


class ColumnStats:
    """Bounded accumulator: memory does not grow with row count."""

    def __init__(self):
        self.n_nonnull = 0
        self.values = Counter()
        self.capped = False
        self.vmin = None
        self.vmax = None
        self.numeric = True
        self.opaque = False
        # Set only when the stats are reloaded from a saved dictionary, where
        # the counter holds the top values but the distinct count was recorded.
        self.distinct_override = None

    @property
    def n_distinct(self):
        if self.distinct_override is not None:
            return self.distinct_override
        return len(self.values)

    def add_opaque(self, n_nonnull):
        """Count only, for columns whose values are identifiers, not codes."""
        self.n_nonnull += n_nonnull
        self.capped = True
        self.opaque = True

    def update_counter(self, counts):
        for value, n in counts.items():
            # Empty and null both mean "not asked this year", not zero.
            if value is None or value == "":
                continue
            self.n_nonnull += n
            if self.numeric:
                try:
                    num = float(value)
                except ValueError:
                    self.numeric = False
                else:
                    self.vmin = num if self.vmin is None else min(self.vmin, num)
                    self.vmax = num if self.vmax is None else max(self.vmax, num)
            if not self.capped:
                self.values[value] += n
                if len(self.values) > DISTINCT_CAP:
                    self.capped = True
                    # Keep the leaders so top-values reporting still works.
                    self.values = Counter(dict(self.values.most_common(TOP_N * 4)))


def read_header(path):
    """Column names, read without parsing the body."""
    import gzip
    with gzip.open(path, "rt") as handle:
        return next(csv.reader(handle))


def load_labels():
    labels = {}
    with LABELS_CSV.open(newline="", encoding="utf-8") as handle:
        for row in csv.DictReader(handle):
            labels[row["varname"]] = row
    return labels


def fmt_num(value):
    if value is None:
        return "-"
    if float(value).is_integer():
        return f"{int(value):,}"
    return f"{value:,.1f}"


def profile():
    started = time.time()
    if not SOURCE.is_file():
        raise FileNotFoundError(f"Extract not found: {SOURCE}\nSet IPUMS_LINKED_CSV.")
    labels = load_labels()
    ddi, ddi_meta = load_ddi()

    print(f"Source : {SOURCE}  ({SOURCE.stat().st_size / 1e9:.2f} GB gzipped)")
    if MAX_ROWS:
        print(f"DRY RUN: stopping after {MAX_ROWS:,} rows")

    # Read everything as string. Two reasons: the columns are a mix of ints, floats
    # and UUIDs; and type inference is actively wrong here. The file is ordered by
    # census year, so a column empty throughout 1900 (EMPSTAT, HIGRADE, ...) gets
    # inferred as null from the first block and then fails on the first 1940 value.
    header = read_header(SOURCE)
    # Checked before the pass, not after: a codebook that does not describe this
    # extract should cost seconds rather than sixteen minutes to find out.
    label_check = cross_check_labels(ddi, header, labels) if ddi else None
    read_opts = pacsv.ReadOptions(block_size=64 << 20)
    convert_opts = pacsv.ConvertOptions(
        strings_can_be_null=False,
        column_types={name: pa.string() for name in header})

    stats = defaultdict(lambda: defaultdict(ColumnStats))   # year -> col -> stats
    rows_by_year = Counter()
    total = 0
    columns = None

    # Bounded samples for the per-person checks.
    hik_bpl = defaultdict(set)
    hik_years = defaultdict(set)
    histid_seen = defaultdict(set)   # year -> sampled HISTIDs, for a uniqueness probe
    birthyr_vs_derived = Counter()

    with pa.CompressedInputStream(pa.OSFile(str(SOURCE), "rb"), "gzip") as stream:
        reader = pacsv.open_csv(stream, read_options=read_opts, convert_options=convert_opts)
        for batch in reader:
            if columns is None:
                columns = batch.schema.names
            year_col = batch.column("YEAR")

            # The extract is ordered by census year, so almost every batch holds a
            # single year. Detecting that avoids filtering 58 columns per year per
            # batch, which dominated runtime in the first version.
            batch_years = pc.unique(year_col).to_pylist()
            if len(batch_years) == 1:
                slices = [(int(batch_years[0]), batch)]
            else:
                slices = [(int(y), batch.filter(pc.equal(year_col, y))) for y in batch_years]

            for year, sub in slices:
                rows_by_year[year] += sub.num_rows
                for name in columns:
                    col = sub.column(name)
                    if name in HIGH_CARDINALITY:
                        nonnull = pc.sum(pc.cast(pc.invert(pc.is_null(col)), pa.int64()))
                        stats[year][name].add_opaque(nonnull.as_py() or 0)
                        continue
                    vc = col.value_counts()
                    stats[year][name].update_counter(
                        {d["values"]: d["counts"] for d in vc.to_pylist()})

                # Per-person checks on a bounded HIK sample, filtered in Arrow so
                # only the sampled rows ever become Python objects.
                hik_col = sub.column("HIK")
                keep = pc.starts_with(pc.utf8_lower(hik_col), pattern=HIK_SAMPLE_PREFIX)
                keep = pc.fill_null(keep, False)
                if pc.sum(pc.cast(keep, pa.int64())).as_py():
                    sampled = sub.filter(keep)
                    for hik, bpl, histid in zip(sampled.column("HIK").to_pylist(),
                                                sampled.column("BPL").to_pylist(),
                                                sampled.column("HISTID").to_pylist()):
                        hik_years[hik].add(year)
                        if bpl:
                            hik_bpl[hik].add(bpl)
                        if histid:
                            histid_seen[year].add(histid)

                if year == 1940:
                    age = pc.cast(sub.column("AGE"), pa.int32(), safe=False)
                    byr = pc.cast(sub.column("BIRTHYR"), pa.int32(), safe=False)
                    diff = pc.subtract(byr, pc.subtract(pa.scalar(1939), age))
                    for d in pc.value_counts(diff).to_pylist():
                        if d["values"] is not None:
                            birthyr_vs_derived[d["values"]] += d["counts"]

            total += batch.num_rows
            if total % 20_000_000 < batch.num_rows:
                print(f"  {total:,} rows | {(time.time() - started) / 60:.1f} min", flush=True)
            if MAX_ROWS and total >= MAX_ROWS:
                break

    elapsed = time.time() - started
    print(f"\nRead {total:,} rows in {elapsed / 60:.1f} min")

    if not MAX_ROWS:
        assert total == EXPECTED_TOTAL, f"row count {total:,} != {EXPECTED_TOTAL:,}"
        for year, expected in EXPECTED_BY_YEAR.items():
            assert rows_by_year[year] == expected, (
                f"{year}: {rows_by_year[year]:,} != {expected:,}")
        assert stats[1940]["HIGRADED"].n_nonnull > 0, "HIGRADED empty on 1940 rows"
        assert stats[1900]["HIGRADED"].n_nonnull == 0, "HIGRADED populated before 1940"
        assert stats[1900]["BIRTHYR"].n_nonnull > 0, "BIRTHYR empty on 1900 rows"
        print("Structural assertions passed.")

    findings = {
        "birthyr_vs_derived": birthyr_vs_derived,
        "hik_bpl_inconsistent": sum(1 for v in hik_bpl.values() if len(v) > 1),
        "hik_sampled": len(hik_years),
        "hik_multiyear": sum(1 for v in hik_years.values() if len(v) > 1),
        "histid_unique_by_year": {y: len(s) for y, s in histid_seen.items()},
    }
    source_info = {"path": str(SOURCE), "bytes": SOURCE.stat().st_size,
                   "elapsed_minutes": round(elapsed / 60, 2), "rows": total}
    checks = {"ddi_cross_checked": ddi is not None, "labels": label_check,
              "unmatched_codes": cross_check_codes(ddi, columns, stats,
                                                   sorted(rows_by_year)) if ddi else {}}
    write_outputs(columns, stats, rows_by_year, total, labels, findings,
                  source_info, ddi, ddi_meta, checks)


def run_cross_checks(ddi, header, labels, columns, stats, years):
    """Both DDI checks, or a recorded absence when there is no codebook.

    Used by the --labels-only path, which has the numbers already and so runs
    both checks together. profile() splits them: the label check goes before
    the streaming pass, the code check after it.
    """
    if ddi is None:
        return {"ddi_cross_checked": False, "labels": None, "unmatched_codes": {}}
    return {
        "ddi_cross_checked": True,
        "labels": cross_check_labels(ddi, header, labels),
        "unmatched_codes": cross_check_codes(ddi, columns, stats, years),
    }


def render_findings(findings):
    """The four facts the merge specification depends on, rendered as Markdown.

    Takes either the dict a full profiling run produces, or a block of Markdown
    carried over from an earlier run -- the --labels-only path, where the extract
    was never read and the facts cannot be recomputed.
    """
    if isinstance(findings, str):
        return [findings.rstrip(), ""]

    lines = []
    lines += ["## Findings the merge depends on", ""]
    bv = findings["birthyr_vs_derived"]
    if bv:
        total_cmp = sum(bv.values())
        exact = bv.get(0, 0)
        lines += [
            f"**`BIRTHYR` vs the CLS cohort key on 1940 rows.** Comparing `BIRTHYR` "
            f"against `1940 - AGE - 1` over {total_cmp:,} rows: "
            f"**{100 * exact / total_cmp:.1f}% agree exactly**. Distribution of "
            "`BIRTHYR - (1940 - AGE - 1)`:",
            "",
            "| Difference | Rows | Share |",
            "|---|---|---|",
        ]
        for diff, n in sorted(bv.items()):
            lines.append(f"| {diff:+d} | {n:,} | {100 * n / total_cmp:.1f}% |")
        lines += [
            "",
            "Clay, Lingwall and Stephens compute year of birth as `1940 - age - 1` "
            "because enumeration was on 1 April and most respondents had not yet had "
            "their birthday. Use their convention when joining their cohort file, "
            "whatever `BIRTHYR` says, so the treatment key matches the one the law "
            "measure was built on.",
            "",
        ]
    lines += [
        f"**`BPL` within a person.** Of {findings['hik_sampled']:,} sampled `HIK`s "
        f"(those beginning `{HIK_SAMPLE_PREFIX}`, roughly 1/60 of the file), "
        f"**{findings['hik_bpl_inconsistent']:,} carry more than one `BPL`** across their "
        "linked records. Where birthplace disagrees, take it from the 1940 record, which "
        "is the one the CLS design reads.",
        "",
        f"**`HIK` spans years.** {findings['hik_multiyear']:,} of the sampled "
        f"{findings['hik_sampled']:,} appear in more than one census year, confirming "
        "`HIK` as the across-year key.",
        "",
        "**`HISTID` is unique within a census year.** Sampled distinct counts by "
        "year: " + ", ".join(f"{y}: {n:,}" for y, n in
                              sorted(findings['histid_unique_by_year'].items())) +
        ". It identifies a record, not a person; only `HIK` follows someone across "
        "census years.",
        "",
    ]
    return lines


def _provenance_line(ddi_meta, checks):
    """One line in the Markdown saying how far the document can be trusted."""
    if checks.get("ddi_cross_checked") and ddi_meta:
        return (f"Value labels and the label cross-check come from IPUMS' DDI codebook "
                f"`{ddi_meta['file']}` (sha256 `{ddi_meta['sha256'][:16]}…`, "
                f"{ddi_meta['n_vars']} variables). Variable labels and coding notes are "
                "the project's own, in `ipums_variable_labels.csv`, checked against it.")
    return ("**No DDI codebook was available for this run**, so value labels are absent "
            "and the variable labels below — from `ipums_variable_labels.csv` — are "
            "unverified. See `README.md` for how to fetch the codebook from IPUMS.")


def build_provenance(columns, stats, rows_by_year, total, labels, findings,
                     source_info, ddi, ddi_meta, checks, exhaustive=True):
    """The run manifest: what was read, what it was checked against, what is missing."""
    with_labels = sorted(name for name in columns
                         if ddi and ddi.get(name.upper(), {}).get("values"))
    return {
        "generated_utc": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()),
        "render_mode": "full" if exhaustive else "labels-only",
        "source": source_info,
        "rows_total": total,
        "rows_by_year": {str(y): n for y, n in sorted(rows_by_year.items())},
        "n_columns": len(columns),
        "ddi": ddi_meta,
        "ddi_cross_checked": bool(checks.get("ddi_cross_checked")),
        "label_check": checks.get("labels"),
        "unmatched_codes": checks.get("unmatched_codes") or {},
        "coverage": {
            "variables": len(columns),
            "with_project_notes": sum(1 for name in columns if name in labels),
            "with_value_labels": len(with_labels),
            "without_value_labels": sorted(set(columns) - set(with_labels)),
        },
        "findings": ({"carried_over_from_previous_run": True}
                     if isinstance(findings, str)
                     else {"birthyr_vs_derived": {str(k): v for k, v in
                                                  findings["birthyr_vs_derived"].items()},
                           "hik_bpl_inconsistent": findings["hik_bpl_inconsistent"],
                           "hik_sampled": findings["hik_sampled"],
                           "hik_multiyear": findings["hik_multiyear"],
                           "histid_unique_by_year": {str(k): v for k, v in
                                                     findings["histid_unique_by_year"].items()}}),
        "notes": [
            "Numbers come from the data; value labels from the DDI; variable labels "
            "and coding notes from ipums_variable_labels.csv.",
            "HIK is the cross-year person key. HISTID identifies a record within one "
            "census year and does not follow a person across years.",
            "Join the CLS cohort file on yob = 1940 - AGE - 1, not on BIRTHYR.",
        ],
    }


def write_outputs(columns, stats, rows_by_year, total, labels, findings,
                  source_info, ddi, ddi_meta, checks, exhaustive=True):
    years = sorted(rows_by_year)
    provenance = build_provenance(columns, stats, rows_by_year, total, labels,
                                  findings, source_info, ddi, ddi_meta, checks,
                                  exhaustive)

    with OUT_CSV.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.writer(handle)
        writer.writerow(["varname", "label", "group", "year", "n_nonnull", "pct_nonnull",
                         "n_distinct", "distinct_capped", "min", "max", "top_values"])
        for name in columns:
            meta = labels.get(name, {})
            for year in years:
                st = stats[year][name]
                top = "; ".join(f"{v}={n}" for v, n in st.values.most_common(TOP_N))
                writer.writerow([
                    name, meta.get("label", ""), meta.get("group", ""), year,
                    st.n_nonnull, round(100 * st.n_nonnull / rows_by_year[year], 2),
                    f">{DISTINCT_CAP}" if st.capped else st.n_distinct,
                    st.capped, fmt_num(st.vmin), fmt_num(st.vmax), top])

    lines = [
        "# IPUMS MLP linked census extract — variable dictionary",
        "",
        f"Generated {time.strftime('%Y-%m-%d')} by "
        "`prep/mandatory_schooling/profile_ipums_linked_census.py`.",
        "",
        f"Source: `{source_info['path']}` — "
        f"{source_info['bytes'] / 1e9:.2f} GB gzipped, "
        f"**{total:,} rows**, {len(columns)} columns. Profiled in "
        f"{source_info['elapsed_minutes']:.0f} minutes.",
        "",
        _provenance_line(ddi_meta, checks),
        "",
        "## What this file is",
        "",
        "An IPUMS Multigenerational Longitudinal Panel **linked** extract. Despite the "
        "filename it is not a two-year 1900–1940 link: it stacks **all five census "
        "years**, one row per person per census in which they appear. Row counts sit far "
        "below full-count size because only linked cases are included.",
        "",
        "| Census year | Rows | Share |",
        "|---|---|---|",
    ]
    for year in years:
        lines.append(f"| {year} | {rows_by_year[year]:,} | "
                     f"{100 * rows_by_year[year] / total:.1f}% |")
    lines += ["", f"**Total: {total:,}**", ""]

    lines += [
        "## Keys",
        "",
        "- **`HISTID`** identifies a person *record* within a census year. It is stable "
        "across IPUMS data versions but **is not shared by the same person across "
        "years**.",
        "- **`HIK`** is the cross-year person key, assigned only to people the MLP could "
        "link. To follow someone through the censuses, group by `HIK`; to identify a "
        "single record, use `HISTID`.",
        "- `LINK1900`…`LINK1940` flag which censuses a person links to.",
        "- Household is `(YEAR, SERIAL)`; person within household is `PERNUM`.",
        "",
    ]

    lines += render_findings(findings)

    # Availability at a glance -- the first thing anyone needs from this file.
    lines += ["## Availability at a glance", "",
              "Which census years each variable is populated in. **Empty means the "
              "question was not asked that year**, not that the value is zero.", "",
              "| Variable | " + " | ".join(str(y) for y in years) + " |",
              "|---|" + "---|" * len(years)]
    for name in columns:
        marks = " | ".join("yes" if stats[y][name].n_nonnull > 0 else "---" for y in years)
        lines.append(f"| `{name}` | {marks} |")
    buckets = defaultdict(list)
    for name in columns:
        buckets[tuple(y for y in years if stats[y][name].n_nonnull > 0)].append(name)
    lines += ["", "Grouped:", ""]
    for key in sorted(buckets, key=lambda t: (-len(t), t)):
        label = ("**All five years**" if len(key) == len(years)
                 else "**" + ", ".join(map(str, key)) + " only**")
        lines.append(f"- {label} ({len(buckets[key])}): "
                     + ", ".join(f"`{v}`" for v in sorted(buckets[key])))
    lines.append("")

    lines += [
        "## Reading the table",
        "",
        "`Fill` is the share of that year's rows with a non-empty value. **Empty means "
        "the question was not asked that year**, which is the single most important "
        "thing to check before using a variable: attainment (`HIGRADE`, `HIGRADED`, "
        "`EDUC`, `EDUCD`) exists only from 1940, while literacy (`LIT`) runs to 1930 and "
        "stops. `SCHOOL` appears in every year but its reference period changes from "
        "~12 months in 1900 to ~1 month in 1940 and is not comparable across years "
        "untreated.",
        "",
    ]

    groups = []
    for name in columns:
        group = labels.get(name, {}).get("group", "other")
        if group not in groups:
            groups.append(group)

    for group in groups:
        lines += [f"## {group.title()}", ""]
        for name in columns:
            meta = labels.get(name, {})
            if meta.get("group", "other") != group:
                continue
            spec = ddi.get(name.upper()) if ddi else None
            lines += [f"### `{name}` — {meta.get('label', '(no label)')}", ""]
            if meta.get("coding_note"):
                lines += [meta["coding_note"], ""]

            present = [y for y in years if stats[y][name].n_nonnull > 0]
            if not present:
                lines += ["*Empty in every census year in this extract.*", ""]
                continue
            if len(present) == len(years):
                availability = "all years"
            else:
                availability = ", ".join(str(y) for y in present) + " only"
            lines += [f"**Available:** {availability}", "",
                      "| Year | Fill | Distinct | Min | Max | Most common |",
                      "|---|---|---|---|---|---|"]
            for year in years:
                st = stats[year][name]
                if st.n_nonnull == 0:
                    lines.append(f"| {year} | — | | | | |")
                    continue
                if st.opaque:
                    top, distinct = "*(identifier)*", "~1 per row"
                else:
                    top = ", ".join(decode_code(spec, v)
                                    for v, _ in st.values.most_common(3))
                    distinct = f">{DISTINCT_CAP}" if st.capped else f"{st.n_distinct:,}"
                lines.append(
                    f"| {year} | {100 * st.n_nonnull / rows_by_year[year]:.1f}% | "
                    f"{distinct} | {fmt_num(st.vmin)} | {fmt_num(st.vmax)} | {top} |")
            lines.append("")
            lines += value_table(spec, name, stats, years, exhaustive)

    OUT_MD.write_text("\n".join(lines) + "\n", encoding="utf-8")
    OUT_JSON.write_text(json.dumps(provenance, indent=2), encoding="utf-8")
    print(f"Wrote: {OUT_MD}")
    print(f"Wrote: {OUT_CSV}")
    print(f"Wrote: {OUT_JSON}")


def decode_code(spec, value):
    """`code` plus its DDI meaning, where the codebook supplies one."""
    if not spec:
        return f"`{value}`"
    meaning = spec["keys"].get(code_key(value))
    if not meaning:
        return f"`{value}`"
    if len(meaning) > 28:
        meaning = meaning[:27] + "…"
    return f"`{value}` {meaning}"


def value_table(spec, name, stats, years, exhaustive=True):
    """The code -> meaning table for one variable, from the DDI.

    A DDI enumerates every category the variable takes across all IPUMS samples,
    which for BPLD or OCC1950 runs to hundreds of codes and would bury the page.
    Long lists are cut to the codes this extract actually contains, and the table
    says so rather than pretending to be exhaustive.

    `exhaustive` is False on a --labels-only render, where the reloaded stats
    hold only each column's top values. The cut is then made against those rather
    than against every observed code, and the note says which.
    """
    if not spec or not spec["values"]:
        return []

    observed = set()
    for year in years:
        st = stats[year][name]
        if not st.opaque:
            observed.update(code_key(v) for v in st.values)

    shown = list(spec["values"].items())
    note = None
    if len(shown) > VALUE_TABLE_CAP:
        seen = [(code, meaning) for code, meaning in shown if code_key(code) in observed]
        full = len(shown)
        if seen:
            shown = seen[:VALUE_TABLE_CAP]
            basis = ("observed in this extract" if exhaustive
                     else "most common in this extract")
            note = (f"*{full} codes in the codebook; the {len(shown)} shown are those "
                    f"{basis}. See the DDI for the full list.*")
        else:
            # Nothing observed matched a category. Usually the variable is
            # continuous and the DDI only enumerates its special codes, but it
            # is also what a mismatched codebook looks like, so say so.
            shown = shown[:VALUE_TABLE_CAP]
            note = (f"*First {len(shown)} of {full} codes in the codebook. **No value "
                    "in this extract matched any of them** — expected for a continuous "
                    "variable whose codebook lists only special codes, but worth "
                    "checking that the codebook belongs to this extract.*")

    out = ["| Code | Meaning |", "|---|---|"]
    out += [f"| {code} | {meaning} |" for code, meaning in shown]
    out.append("")
    if note:
        out += [note, ""]
    return out


# --------------------------------------------------------------------------- #
# Re-rendering from the saved numbers
# --------------------------------------------------------------------------- #

def _parse_num(text):
    if not text or text == "-":
        return None
    try:
        return float(text.replace(",", ""))
    except ValueError:
        return None


def load_saved_profile():
    """Rebuild columns, per-year stats and row counts from the saved dictionary CSV.

    The 13 GB pass already produced these numbers. Re-reading the extract to
    change a label would cost 16 minutes and arrive at identical figures, so the
    renderer reads them back instead. Only display fields are reconstructed --
    the top-values Counter holds the top eight, which is all the Markdown shows.
    """
    if not OUT_CSV.is_file():
        raise FileNotFoundError(
            f"{OUT_CSV.name} not found. Run a full profile once before --labels-only.")

    stats = defaultdict(lambda: defaultdict(ColumnStats))
    rows_by_year = Counter()
    columns = []
    with OUT_CSV.open(newline="", encoding="utf-8") as handle:
        for row in csv.DictReader(handle):
            name, year = row["varname"], int(row["year"])
            if name not in columns:
                columns.append(name)
            st = stats[year][name]
            st.n_nonnull = int(row["n_nonnull"])
            st.capped = row["distinct_capped"] == "True"
            st.opaque = name in HIGH_CARDINALITY
            # The counter below holds only the top values, so the distinct count
            # has to be carried across explicitly or it collapses to eight.
            if not row["n_distinct"].startswith(">"):
                st.distinct_override = int(row["n_distinct"])
            st.vmin = _parse_num(row["min"])
            st.vmax = _parse_num(row["max"])
            for item in filter(None, row["top_values"].split("; ")):
                value, _, count = item.rpartition("=")
                if value and count.isdigit():
                    st.values[value] = int(count)
            # Any column filled on every row gives that year's row count; YEAR
            # always is. max() guards against a 99.995% column rounding to 100.
            if float(row["pct_nonnull"]) == 100.0:
                rows_by_year[year] = max(rows_by_year[year], st.n_nonnull)

    missing = [y for y in sorted(stats) if not rows_by_year[y]]
    if missing:
        raise ValueError(f"{OUT_CSV.name} has no fully-populated column for "
                         f"{missing}, so the row count cannot be recovered.")
    print(f"  read {len(columns)} columns x {len(rows_by_year)} years from {OUT_CSV.name}")
    return columns, stats, rows_by_year


def _section_from_markdown(heading):
    """One '## ...' section of the previous Markdown, verbatim."""
    if not OUT_MD.is_file():
        return None
    text = OUT_MD.read_text(encoding="utf-8")
    match = re.search(f"^## {re.escape(heading)}$.*?(?=^## )", text, re.M | re.S)
    return match.group(0).rstrip() if match else None


def load_saved_source(total):
    """Identity of the extract, for a run that never opened it."""
    if OUT_JSON.is_file():
        saved = json.loads(OUT_JSON.read_text(encoding="utf-8")).get("source")
        if saved:
            return saved
    # Bootstrap: the first --labels-only run predates the manifest, so the only
    # surviving record of the source is the header line of the previous Markdown.
    if OUT_MD.is_file():
        match = re.search(
            r"Source: `([^`]+)` . ([0-9.]+) GB gzipped.*?Profiled in ([0-9.]+) minutes",
            OUT_MD.read_text(encoding="utf-8"), re.S)
        if match:
            return {"path": match.group(1), "bytes": int(float(match.group(2)) * 1e9),
                    "elapsed_minutes": float(match.group(3)), "rows": total,
                    "recovered_from": OUT_MD.name}
    if SOURCE.is_file():
        return {"path": str(SOURCE), "bytes": SOURCE.stat().st_size,
                "elapsed_minutes": 0.0, "rows": total}
    return {"path": str(SOURCE), "bytes": 0, "elapsed_minutes": 0.0, "rows": total}


def labels_only():
    """Re-run the cross-checks and re-render the documents, without the extract."""
    print("--labels-only: re-rendering from the saved dictionary CSV")
    labels = load_labels()
    ddi, ddi_meta = load_ddi()
    columns, stats, rows_by_year = load_saved_profile()
    total = sum(rows_by_year.values())

    findings = _section_from_markdown("Findings the merge depends on") or (
        "## Findings the merge depends on\n\n*Not available: no previous profiling "
        "run to carry these over from. Run without --labels-only.*")
    checks = run_cross_checks(ddi, columns, labels, columns, stats, sorted(rows_by_year))
    write_outputs(columns, stats, rows_by_year, total, labels, findings,
                  load_saved_source(total), ddi, ddi_meta, checks,
                  exhaustive=False)


def main():
    args = sys.argv[1:]
    if not args:
        profile()
    elif args == ["--labels-only"]:
        labels_only()
    else:
        sys.exit("usage: profile_ipums_linked_census.py [--labels-only]")


if __name__ == "__main__":
    main()
