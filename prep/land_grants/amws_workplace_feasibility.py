"""Frozen 80-entry, explicitly manual workplace-location feasibility audit."""
import argparse
import collections
import csv
from datetime import datetime, timezone
import itertools
import json
from pathlib import Path
import random
import shutil
import subprocess
import tempfile

import amws_name_cleaning as base

RELEASE = "amws_workplace_feasibility_20261023"
EDITIONS = ("1906", "1938", "1955", "1986")
TEXT_FIELDS = ("raw_name", "address_text", "field_text", "birthplace_text", "biography_text")
STATUSES = {"explicit", "lookup_needed", "ambiguous", "unavailable", "not_applicable"}
SAMPLE_FIELDS = ["sample_id", "source_key", "edition", "v06_source_row", "v06_name", "v06_birth_year", "v06_record_sha256",
                 "source_file", "source_columns", "encoding_note", *TEXT_FIELDS]


def root_path(root=None):
    return Path(root) if root else base.data_root()


def destination(root=None):
    return root_path(root) / "output/land_grants" / RELEASE


def read_json(path):
    return json.loads(Path(path).read_text(encoding="utf-8"))


def select(root=None):
    source = base.locations(root)[0]
    groups = collections.defaultdict(list)
    with source.open(encoding="utf-8-sig", newline="") as f:
        for index, row in enumerate(csv.DictReader(f),1):
            ed = row["source_key"].split(":")[0]
            name = row["name_raw"] if ed == "1986" else ", ".join(x for x in (row["last"],row["first"]) if x)
            groups[ed].append(dict(source_key=row["source_key"], edition=ed, v06_source_row=str(index),
                                   v06_name=name, v06_birth_year=row["birth_year"], v06_record_sha256=base.record_digest(row)))
    if set(groups) != set(EDITIONS) or sum(map(len,groups.values())) != 134311:
        raise ValueError("v06 sampling population changed")
    return draw_sample(groups)


def draw_sample(groups):
    rng = random.Random(47)
    sample = []
    for edition in EDITIONS:
        pool = sorted(groups[edition],key=lambda r:r["source_key"])
        if len({r["source_key"] for r in pool}) != len(pool):
            raise ValueError("Duplicate source IDs")
        offset=len(sample)
        sample.extend(dict(sample_id=str(offset+i+1),**row) for i,row in enumerate(rng.sample(pool,20)))
    return sample


def raw_paths(root=None):
    root = root_path(root)
    return {"1906": root/"input/amws/amws_1906.xlsx", "1938":root/"input/amws/amws_1938.dta",
            "1955":root/"input/amws/amws_1955.dta", "1986":root/"Data/processed/amws/amws_ed86_final.csv"}


def original_entries(sample, root=None):
    files = raw_paths(root)
    ids = {r["source_key"] for r in sample}
    entries = {}
    from openpyxl import load_workbook
    wb = load_workbook(files["1906"],read_only=True,data_only=True)
    try:
        for row in wb.worksheets[0].iter_rows(values_only=True):
            if row[0] is None:
                continue
            key = f"1906:{int(row[0])}"
            if key in ids:
                if key in entries:
                    raise ValueError("Duplicate workbook ID")
                entries[key] = dict(raw_name=str(row[1] or ""), address_text=str(row[2] or ""), field_text=str(row[3] or ""),
                                    birthplace_text=str(row[4] or ""), biography_text=str(row[5] or ""),
                                    source_columns="workbook columns 2, 3, 4, 5, 6", encoding_note="Exact workbook Unicode cell values")
    finally:
        wb.close()
    rscript = shutil.which("Rscript")
    if not rscript:
        raise ValueError("Rscript with haven/readr is required for original Stata fields")
    with tempfile.TemporaryDirectory(prefix="amws_workplace_") as temp:
        ids_path, out_path = Path(temp)/"ids.csv", Path(temp)/"raw.csv"
        base.write_csv(ids_path,[dict(source_key=key) for key in sorted(ids)], ["source_key"])
        result = subprocess.run([rscript,str(Path(__file__).with_name("read_amws_workplace_sources.R")),str(root_path(root)),str(ids_path),str(out_path)],
                                capture_output=True,text=True,encoding="utf-8",errors="replace")
        if result.returncode:
            raise ValueError(f"Original Stata ingestion failed: {result.stderr}")
        for row in base.read_csv(out_path):
            key = row.pop("source_key")
            if key in entries:
                raise ValueError("Duplicate raw ID")
            entries[key] = row
    with files["1986"].open(encoding="utf-8-sig",newline="") as f:
        for row in csv.DictReader(f):
            key = f"1986:{int(row['entry_id'])}"
            if key in ids:
                if key in entries:
                    raise ValueError("Duplicate original 1986 ID")
                entries[key] = dict(raw_name=row["name_raw"], address_text="", field_text="", birthplace_text=row["birth_place"],
                                    biography_text=row["raw_text_adjusted"], source_columns="name_raw; birth_place; raw_text_adjusted",
                                    encoding_note="Original UTF-8 CSV text; mailing address remains within full biography")
    if set(entries) != ids:
        raise ValueError(f"Original source join incomplete: {sorted(ids-set(entries))}")
    return [dict(**row,source_file=str(files[row["edition"]]),**entries[row["source_key"]]) for row in sample]


def protected_checksums(root=None):
    source, source_manifest, _ = base.locations(root)
    expected = read_json(source_manifest)
    if base.digest(source) != expected["sha256"]:
        raise ValueError("v06 checksum mismatch")
    checks = {str(p):base.digest(p) for p in [source,source_manifest,*raw_paths(root).values()]}
    for name in ("amws_name_cleaning_20261022", "amws_name_regex_calibration_20261023", "amws_parent_offspring_candidates_20261023"):
        directory = root_path(root)/"output/land_grants"/name
        complete = directory/"completion_manifest.json"
        data = read_json(complete)
        checks[str(complete)] = base.digest(complete)
        checks.update({str(directory/name):value for name,value in data["output_checksums"].items()})
    verify_checksums(checks)
    return checks


def verify_checksums(checks):
    for name, checksum in checks.items():
        if base.digest(Path(name)) != checksum:
            raise ValueError(f"Protected file changed: {name}")


def prepare(root=None):
    dest = destination(root)
    if dest.exists():
        raise ValueError("Release already exists; do not redraw")
    checks = protected_checksums(root)
    selected = select(root)
    sample = original_entries(selected,root)
    dest.mkdir(parents=True)
    base.write_csv(dest/"sample_80.csv",sample,SAMPLE_FIELDS)
    base.write_json(dest/"sample_manifest.json",dict(seed=47, sample_size=80, records_per_edition=20,
                    algorithm="One random.Random(47); ascending editions; sample(source_key-sorted v06 records,20) per edition; no filters or replacements",
                    sample_ids=[r["source_key"] for r in sample],sample_sha256=base.digest(dest/"sample_80.csv"),
                    input_checksums=checks,prepared_at_utc=datetime.now(timezone.utc).isoformat()))
    (dest/"manual_reviews.jsonl").touch()
    return dest


def show(start,end,root=None):
    for row in base.read_csv(destination(root)/"sample_80.csv"):
        if start <= int(row["sample_id"]) <= end:
            print(f"\n[{row['sample_id']}] {row['source_key']} {row['v06_name']}")
            for field in TEXT_FIELDS:
                print(f"{field}: {row[field]}")


def reviews(root=None):
    return [json.loads(line) for line in (destination(root)/"manual_reviews.jsonl").read_text(encoding="utf-8").splitlines() if line]


def evidence(row, field, quote):
    if field not in {"address_text","biography_text"} or not quote or quote not in row[field]:
        raise ValueError("Workplace evidence must be literal original address/biography text")
    start = row[field].index(quote)
    return dict(field=field,start=start,end=start+len(quote),quote=quote)


def check_review(review,row):
    if review["source_key"] != row["source_key"] or review["edition"] != row["edition"] or review["full_entry_reviewed"] is not True:
        raise ValueError("Review identity or coverage mismatch")
    if review["employment_status"] not in {"active","emeritus","retired","mixed","ambiguous","unavailable"}:
        raise ValueError("Invalid employment status")
    if not review["notes"]:
        raise ValueError("Every review requires an explanation")
    for workplace in review["workplaces"]:
        if workplace["employer_type"] not in {"university","other","unclear"} or workplace["country_scope"] not in {"US","foreign","unclear"}:
            raise ValueError("Invalid workplace classification")
        if not workplace["explanation"] or not workplace["evidence"]:
            raise ValueError("Workplace requires evidence and explanation")
        for ev in workplace["evidence"]:
            if ev != evidence(row,ev["field"],ev["quote"]):
                raise ValueError("Evidence offsets changed")
        quotes=" ".join(ev["quote"] for ev in workplace["evidence"])
        for field in ("employer","university","state","county"):
            status=workplace[field+"_status"]
            value=workplace.get(field+"_as_written","")
            if status not in STATUSES:
                raise ValueError("Invalid feasibility status")
            if status == "explicit" and (not value or value not in quotes):
                raise ValueError("Explicit field lacks literal supporting value")
            if status != "explicit" and value:
                raise ValueError("Do not fill inferred locations or unresolved names")
        if workplace["role_as_written"] and workplace["role_as_written"] not in quotes:
            raise ValueError("Role is not in workplace evidence")
        if workplace.get("city_as_written","") and workplace["city_as_written"] not in quotes:
            raise ValueError("City is not in workplace evidence")
        if workplace["country_scope"] == "foreign" and any(workplace[k+"_status"] != "not_applicable" for k in ("state","county")):
            raise ValueError("US state/county must be not applicable for foreign workplaces")
        if workplace["employer_type"] == "other" and workplace["university_status"] != "not_applicable":
            raise ValueError("Non-university employer is not a missing university")
    if review["history_status"] not in {"available","partial","ambiguous","unavailable"}:
        raise ValueError("Invalid history status")
    if review["history_date_clarity"] not in {"explicit_years","two_digit_or_open_ended","mixed","undated","ambiguous","no_history"}:
        raise ValueError("Invalid date assessment")
    for ev in review["history_evidence"]:
        if ev != evidence(row,ev["field"],ev["quote"]):
            raise ValueError("History evidence mismatch")
    if review["history_status"] != "unavailable" and not review["history_evidence"]:
        raise ValueError("History assessment requires literal evidence")


def append_reviews(items,root=None):
    dest=destination(root)
    if (dest/"completion_manifest.json").exists():
        raise ValueError("Completed release is immutable")
    manifest=read_json(dest/"sample_manifest.json")
    if base.digest(dest/"sample_80.csv") != manifest["sample_sha256"]:
        raise ValueError("Frozen sample changed")
    sample={r["sample_id"]:r for r in base.read_csv(dest/"sample_80.csv")}
    seen={r["sample_id"] for r in reviews(root)}
    new=[]
    for item in items:
        review=dict(item);key=str(review["sample_id"])
        if key in seen or key not in sample:
            raise ValueError("Unknown or repeated review")
        row=sample[key]
        review.update(sample_id=key,source_key=row["source_key"],edition=row["edition"],full_entry_reviewed=True)
        check_review(review,row)
        new.append(review);seen.add(key)
    with (dest/"manual_reviews.jsonl").open("a",encoding="utf-8",newline="\n") as f:
        for review in new:f.write(json.dumps(review,ensure_ascii=False)+"\n")
    return len(seen)


class ManualBatch:
    """Construct explicitly supplied human assessments; never classify absent rows."""
    def __init__(self,root=None):
        self.root=root
        self.rows={int(r["sample_id"]):r for r in base.read_csv(destination(root)/"sample_80.csv")}

    def ev(self,i,quote,field="biography_text"):
        return evidence(self.rows[i],field,quote)

    def work(self,i,employer,role,quote,kind="university",ss="lookup_needed",state="",city="",es="explicit",scope="unclear",address=False,explanation="Current employment is supported by the career text; identifying the actual institution/site and its geography requires a lookup."):
        ev=[self.ev(i,quote)]
        if address:ev.append(self.ev(i,self.rows[i]["address_text"],"address_text"))
        return dict(employer_as_written=employer,role_as_written=role,employer_type=kind,country_scope=scope,employer_status=es,
                    university_status=("explicit" if es=="explicit" else es) if kind=="university" else "not_applicable",
                    university_as_written=employer if kind=="university" and es=="explicit" else "",state_status=ss,state_as_written=state,
                    city_as_written=city,county_status="not_applicable" if ss=="not_applicable" else "ambiguous" if ss=="ambiguous" else "lookup_needed",
                    county_as_written="",evidence=ev,explanation=explanation)

    def entry(self,i,work,history="",hs="available",hc="two_digit_or_open_ended",status="active",notes="Career evidence identifies a current employer. A mailing/residential address is not substituted for a workplace; county requires lookup."):
        return dict(sample_id=i,employment_status=status,workplaces=work,history_status=hs,history_date_clarity=hc,
                    history_evidence=[self.ev(i,history)] if history else [],notes=notes)


def record_status(review,field):
    """Entry-level feasibility is for at least one workplace, not complete history."""
    values={work[field+"_status"] for work in review["workplaces"]}
    if not values:
        return "unavailable"
    # A recoverable affiliation is retained even if another concurrent one is unclear.
    return next(status for status in ("explicit","ambiguous","lookup_needed","unavailable","not_applicable") if status in values)


def validate(root=None,complete=True):
    dest=destination(root)
    manifest=read_json(dest/"sample_manifest.json")
    verify_checksums(manifest["input_checksums"])
    sample=base.read_csv(dest/"sample_80.csv")
    if base.digest(dest/"sample_80.csv") != manifest["sample_sha256"]:
        raise ValueError("Frozen sample checksum changed")
    if len(sample)!=80 or len({r["source_key"] for r in sample})!=80 or [r["sample_id"] for r in sample]!=[str(i) for i in range(1,81)]:
        raise ValueError("Sample IDs/size changed")
    if collections.Counter(r["edition"] for r in sample)!={edition:20 for edition in EDITIONS}:
        raise ValueError("Edition quotas changed")
    reconstructed=original_entries(select(root),root)
    if sample!=reconstructed or manifest["sample_ids"]!=[r["source_key"] for r in sample]:
        raise ValueError("Sample, source join or original text does not reproduce")
    decisions=reviews(root)
    if len(decisions)!=80 or len({r["sample_id"] for r in decisions})!=80:
        raise ValueError("Need 80 unique explicit manual reviews")
    byid={r["sample_id"]:r for r in sample}
    for review in decisions:
        check_review(review,byid[review["sample_id"]])
    if complete:
        completion=read_json(dest/"completion_manifest.json")
        verify_checksums({str(dest/name):value for name,value in completion["output_checksums"].items()})
        verify_checksums(completion["code_checksums"])
    return dict(sample_records=80,records_per_edition=20,completed_manual_reviews=80,
                sampling_reproduced=True,original_sources_and_text_reproduced=True,
                literal_evidence_offsets_valid=True,protected_sources_and_previous_releases_unchanged=True,
                no_external_location_research=True)


def publish(root=None):
    dest=destination(root)
    if (dest/"completion_manifest.json").exists():
        raise ValueError("Completed release is immutable")
    validation=validate(root,complete=False)
    sample={r["sample_id"]:r for r in base.read_csv(dest/"sample_80.csv")}
    decisions=sorted(reviews(root),key=lambda r:int(r["sample_id"]))
    flat=[];mentions=[]
    for review in decisions:
        row=sample[review["sample_id"]]
        flat.append(dict(sample_id=review["sample_id"],source_key=review["source_key"],edition=review["edition"],
                         name_as_written=row["raw_name"],employment_status=review["employment_status"],
                         **{field+"_status":record_status(review,field) for field in ("employer","university","state","county")},
                         workplace_mentions=str(len(review["workplaces"])),
                         has_non_university_workplace=str(any(w["employer_type"]=="other" for w in review["workplaces"])).lower(),
                         has_foreign_workplace=str(any(w["country_scope"]=="foreign" for w in review["workplaces"])).lower(),
                         history_status=review["history_status"],history_date_clarity=review["history_date_clarity"],
                         history_evidence=json.dumps(review["history_evidence"],ensure_ascii=False),
                         workplaces=json.dumps(review["workplaces"],ensure_ascii=False),notes=review["notes"],full_entry_reviewed="true"))
        for index,work in enumerate(review["workplaces"],1):
            mention={**{k:review[k] for k in ("sample_id","source_key","edition")},"workplace_index":str(index),
                     **{k:(json.dumps(v,ensure_ascii=False) if isinstance(v,list) else v) for k,v in work.items()}}
            mentions.append(mention)
    base.write_csv(dest/"review_decisions.csv",flat,list(flat[0]))
    base.write_csv(dest/"workplace_mentions.csv",mentions,list(mentions[0]))
    counts=[]
    for edition in EDITIONS:
        entries=[r for r in flat if r["edition"]==edition]
        for field in ("employer","university","state","county"):
            for status in sorted(STATUSES):
                counts.append(dict(edition=edition,field=field,status=status,records=sum(r[field+"_status"]==status for r in entries),denominator=20))
    base.write_csv(dest/"feasibility_by_edition.csv",counts,list(counts[0]))
    history=[]
    for edition in EDITIONS:
        entries=[r for r in flat if r["edition"]==edition]
        for status in ("available","partial","ambiguous","unavailable"):
            history.append(dict(edition=edition,history_status=status,records=sum(r["history_status"]==status for r in entries),denominator=20))
    base.write_csv(dest/"history_by_edition.csv",history,list(history[0]))
    write_report(dest,flat)
    code=[Path(__file__),Path(__file__).with_name("read_amws_workplace_sources.R"),
          Path(__file__).with_name("test_amws_workplace_feasibility.py"),Path(base.__file__)]
    base.write_json(dest/"completion_manifest.json",dict(completed_at_utc=datetime.now(timezone.utc).isoformat(),validation=validation,
                    code_checksums={str(p.resolve()):base.digest(p) for p in code},
                    output_checksums={p.name:base.digest(p) for p in sorted(dest.iterdir()) if p.is_file()}))
    return dict(validation=validation,explicit_counts={field:sum(r[field+"_status"]=="explicit" for r in flat) for field in ("employer","university","state","county")})


def write_report(dest,rows):
    lines=["# AMWS workplace-location feasibility review", "", "Release: 2026-10-23.", "",
           "All 80 sampled entries were read in full: 20 each from 1906, 1938, 1955 and 1986. "
           "This is a manual evidence/feasibility audit, with no external institution research, county lookup or population-scale employment extraction.", "",
           "## Main results", "",
           "Counts below mean **at least one current-as-reported workplace/affiliation** has the indicated evidence. "
           "They do not mean every concurrent workplace is resolved. Institution names are retained as written, including abbreviations; "
           "explicit does not mean a canonical institution ID, modern institutional name or historical campus has been verified.", "",
           "| Edition | Employer explicit | University affiliation explicit | Work state/territory explicit | Work county explicit |",
           "|---|---:|---:|---:|---:|"]
    for ed in EDITIONS:
        part=[r for r in rows if r["edition"]==ed]
        lines.append("| "+ed+" | "+" | ".join(f"{sum(r[f+'_status']=='explicit' for r in part)}/20" for f in ("employer","university","state","county"))+" |")
    totals={f:sum(r[f+"_status"]=="explicit" for r in rows) for f in ("employer","university","state","county")}
    lines += ["", f"Across the balanced sample, employers are explicit in **{totals['employer']}/80**, "
              f"university affiliations in **{totals['university']}/80**, work states/territories in **{totals['state']}/80**, "
              f"and work counties in **{totals['county']}/80**. "
              "These are sample feasibility counts, not population estimates or automated-extraction accuracy.", "",
              "A named current employer is often recoverable before its specific site or county. "
              "The 1955 full entry headers and 1986 employer-named departmental mailing addresses are especially useful. "
              "County resolution generally requires a subsequent historical site/address lookup and an explicit boundary convention; it has not been performed here.", "",
              "## All feasibility categories", "",
              "The categories are explicit, lookup_needed, ambiguous, unavailable and not_applicable. "
              "Lookup_needed records a task still to do; it is not a successful geocode. "
              "For multiple affiliations, an explicit result takes priority, then ambiguous, lookup_needed, unavailable and not_applicable. "
              "workplace_mentions.csv preserves every individual assessment, including unresolved concurrent affiliations.", "",
              "| Edition | Field | Explicit | Lookup needed | Ambiguous | Unavailable | Not applicable |",
              "|---|---|---:|---:|---:|---:|---:|"]
    for ed in EDITIONS:
        part=[r for r in rows if r["edition"]==ed]
        for field in ("employer","university","state","county"):
            lines.append("| "+ed+" | "+field+" | "+" | ".join(str(sum(r[field+"_status"]==s for r in part)) for s in ("explicit","lookup_needed","ambiguous","unavailable","not_applicable"))+" |")
    lines += ["", "## Employment history and special cases", "",
              "Available means a dated earlier employment role has usable employer context. Partial includes missing employer names, "
              "military service without an earlier civilian worksite, or damaged date tokens. Dates are assessed as printed; "
              "two-digit years and open-ended spells are not silently expanded into a complete timeline. Only representative history evidence was recorded.", "",
              "| Edition | History available | Partial | Ambiguous | Unavailable | Entries with non-university work | Foreign workplace context |",
              "|---|---:|---:|---:|---:|---:|---:|"]
    for ed in EDITIONS:
        part=[r for r in rows if r["edition"]==ed]
        lines.append("| "+ed+" | "+" | ".join(str(sum(r["history_status"]==s for r in part)) for s in ("available","partial","ambiguous","unavailable"))+
                     f" | {sum(r['has_non_university_workplace']=='true' for r in part)} | {sum(r['has_foreign_workplace']=='true' for r in part)} |")
    employment=collections.Counter(r["employment_status"] for r in rows)
    lines += ["", "Employment status counts: "+", ".join(f"{key}: {value}" for key,value in sorted(employment.items()))+". "
              "Emeritus is an affiliation, not evidence of an active salaried appointment. Retired entries remain in the denominator. "
              "Non-university employers are not coded as missing universities; clearly foreign workplaces are not assigned US states/counties. "
              "The foreign-context flag denotes geography outside the US state/county scope, including overseas P. I.; it makes no historical sovereignty claim. "
              "Hawaii, T. H. is retained as a historical territory designation, not retroactively labeled a state in 1955.", "",
              "## Evidence examples", "",
              "| Record | Finding | Literal evidence |", "|---|---|---|"]
    examples=[("1906:338","Institutional address supplies state, but no county","address_text","Princeton University, Princeton, N. J."),
              ("1938:10377","Industrial employer includes work city/state","biography_text","OLDBURY ELECTRO- CHEM. CO, NIAGARA FALLS, N. Y"),
              ("1955:20017","Full original header is more informative than truncated address fields","biography_text","Utah State Agricultural College, Logan, Utah."),
              ("1986:59946","Departmental mailing address corroborates current university site","biography_text","Dept Appl Mcch & Eng Sci Univ Calif San Diego Box 109 La Jolla CA 92093"),
              ("1938:8548","Explicit county belongs to a previous job, not the current workplace","biography_text","county entomologist and hort. commissioner, Santa Clara County, 92-04"),
              ("1955:42450","Do not substitute Akron home address for named Barberton workplace","biography_text","DIRECTOR CERAMIC RESEARCH, OHIO BRASS CO BARBERTON, 54-"),
              ("1955:31796","Retirement and club address do not establish current employment","biography_text","geologist, 10-42; RETIRED."),
              ("1986:49016","Damaged state token needs lookup rather than silent repair","biography_text","Dept of Nutrit Sci Univ of Wis 1300 Linden Dr Madison Wl 53706")]
    sample={r["source_key"]:r for r in base.read_csv(dest/"sample_80.csv")}
    for key,finding,field,quote in examples:
        if quote not in sample[key][field]:raise ValueError("Report quote is not literal")
        lines.append(f"| {key} | {finding} | {quote} |")
    lines += ["", "The nameless entry 1986:26493 includes incompatible Wisconsin and North Carolina career/mailing-address tails. "
              "Both are preserved as ambiguous evidence, not confirmed concurrent employment. Original-page inspection would be needed. "
              "The 1955 Stanford Research Institute entry identifies the institute/site but does not by itself establish its historical university relationship.", "",
              "## Recommended next stage", "",
              "1. Extract employer/role/date mentions with original offsets, separating current, previous, emeritus, visiting, consulting and uncertain roles. "
              "Keep multiple affiliations and validate on a fresh reviewed sample; this audit does not calibrate an automatic parser.",
              "2. Resolve named institutions and employers to historically appropriate entities and campuses, using institutional addresses as site evidence. "
              "Do not default a university system or medical school to the main campus, or a corporation to its headquarters.",
              "3. Resolve states and counties from verified historical work sites, keeping explicit text and lookup-derived geography separate. "
              "Specify contemporaneous versus harmonized county boundaries before assigning county FIPS; check independent cities, territories and foreign sites.",
              "4. Use a dated parental workplace near the offspring's birth as an additional candidate feature only when the relevant time interval is supported. "
              "An employer reported decades later does not establish where a potential parent worked when the offspring was born.", "",
              "## Sampling, provenance and validation", "",
              "The frame is all 134,311 v06 records, without name-quality, matching-eligibility or employment-evidence filters. "
              "One random.Random(47) generator draws 20 per edition from source_key-sorted pools, in ascending edition order. "
              "No entries were replaced. The frozen sample has sequential display IDs; an indexing correction before any reviews is logged in the sample manifest, "
              "with the selected source IDs and original text unchanged.", "",
              "1906 uses exact workbook name/address/field/birthplace/biography cells. 1938 uses AMSname, address_legacy, field_legacy and a. "
              "1955 uses original entry text plus separate source fields; 1986 uses original name_raw, birth_place and raw_text_adjusted. "
              "Valid Unicode strings are retained; any invalid Stata UTF-8 is decoded as Latin-1 with no spelling fixes. "
              "Address, birthplace and biography are separately labeled; full original text remains in sample_80.csv.", "",
              "manual_reviews.jsonl contains all 80 explicit human decisions and literal evidence offsets. review_decisions.csv provides one row per entry; "
              "workplace_mentions.csv provides one row per current/ambiguous affiliation. feasibility_by_edition.csv and history_by_edition.csv retain denominators of 20. "
              "The validator reconstructs the draw and original-source joins, checks all evidence/value substrings, verifies complete review coverage, "
              "and checks v06, raw sources, both name-cleaning releases and the matching release against protected checksums. "
              "No source, previous release or candidate pair is modified.", ""]
    (dest/"feasibility_report.md").write_text("\n".join(lines),encoding="utf-8")


def main():
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument("command",choices=["prepare","show","publish","validate"])
    parser.add_argument("--root",type=Path)
    parser.add_argument("--start",type=int,default=1)
    parser.add_argument("--end",type=int,default=10)
    args=parser.parse_args()
    if args.command=="prepare":print(prepare(args.root))
    elif args.command=="show":show(args.start,args.end,args.root)
    else:print(json.dumps((publish if args.command=="publish" else validate)(args.root),indent=2))


if __name__=="__main__":
    main()
