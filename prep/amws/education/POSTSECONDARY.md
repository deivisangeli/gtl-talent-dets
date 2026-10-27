# All earned postsecondary qualifications

This additive extension publishes `amws_education_postsecondary_20261019` and
refreshes the college-allocation outcomes in
`amws_birth_county_training_postsecondary_20261019`. The existing canonical
education file, frozen parser and earlier releases remain unchanged.

## Qualification scope

The original `ba_bs`, `ma`, `ms`, and `phd` families retain their awards,
positions, literal evidence, and institution assignments. Additional earned
qualifications are exported in four separate families:

| Family | Included evidence |
|---|---|
| `bachelors_other` | Other recorded bachelor qualifications, including B.Ch.E., BE and medical bachelor awards |
| `masters_other` | Professional master's qualifications and academic master's labels outside the existing MA/MS lists |
| `doctorate_other` | Other doctoral qualifications, including MD, DVM, DDS, DSc, D.E. and DEng |
| `postsecondary_credential` | Engineering qualifications such as C.E./E.E., Kand, and postsecondary diplomas and certificates |

The full existing award ledger supplies already-recognized qualifications that
were previously omitted from the primary lists. A separate parser namespace
adds missing labels (Kand, D.E., DEng, Diploma, Certificate and Cert) using the
same edition-specific education boundaries as the frozen parser. It does not
repair damaged education headings or change existing award evidence.

Honorary awards, bare titles, licenses, memberships, employment and recognized
school-level credentials are excluded. Generic diplomas/certificates require
explicit postsecondary institution evidence. Uncertain levels are retained as
`training_eligibility=unresolved_level`, receive no location assignment, and
prevent a never-local conclusion unless other evidence establishes ever-local
training. Missing graduation years alone do not disqualify an award.

Old award identifiers remain stable. Newly recognized labels receive identifiers
after the old maximum within the source entry. New-family positions follow
biography order. `evidence_key` identifies source key, literal label offsets and
literal label, so old institution decisions are never transferred by shifted
row positions.

## Institution matching and export

Existing primary awards retain every exported assignment and coordinate.
Additional qualifications reuse the published crosswalk by exact institution
text, including its provisional qualifications. Conditional mappings are
evaluated separately for the new award's label, family, date, evidence and
bounded education context. Missing/conflicting dates fail date predicates.
Conflicting components and incomplete co-institution mappings remain incomplete.

Cached reference coordinates are attached only to accepted IDs. Missing
coordinates remain unresolved. This run performs no new web research, fuzzy
institution matching, historical-campus reconstruction or person deduplication.

The release is under `output/amws/amws_education_postsecondary_20261019/`:

- `education_awards.csv`: all source awards, including excluded and honorary
  evidence, with eligibility and stable identifiers.
- `education_awards_openalex.csv`: included training awards and location evidence.
- `education_records.csv`: full-biography coverage and additional-award counts.
- `amws_open_alex.csv`: the original 136,489-entry US person view with appended
  qualification families; original row order and non-derived cells are preserved.
- `schema.json`: families, maximum award counts and institution slots; consumers
  discover actual numbered columns rather than assuming four families/two slots.
- Coverage, unmatched-institution research inventory, unresolved-level inventory,
  reference coordinates, source/code checksums, snapshots and completion manifest.

The person-level `oa_*` aggregates now describe all included training awards;
existing four-family columns and legacy `education_status` stay unchanged.
Use the expanded aggregate counts or long ledger to assess new-scope coverage.
The new file is consumed explicitly by the new outcomes pipeline rather than
replacing `Data/processed/amws/amws_open_alex.csv`.

## Reproduction

From the repository root:

```powershell
$env:TALENT_DETS_DATA_DIR = 'C:/Users/megaj/Globtalent Dropbox/gtl_talent_dets'
python -m unittest discover -s prep/amws/education -p test_postsecondary_qualifications.py
python prep/amws/education/build_postsecondary_release.py --data-root "$env:TALENT_DETS_DATA_DIR"
python prep/amws/education/validate_postsecondary_release.py --data-root "$env:TALENT_DETS_DATA_DIR"
Rscript --vanilla prep/land_grants/10_build_amws_birth_county_training_postsecondary.R
Rscript --vanilla analysis/land_grants/training_50km/run_birth_county_training_postsecondary.R
Rscript --vanilla analysis/land_grants/training_50km/validate_birth_county_training_postsecondary.R
```

Set the data root to the local Dropbox location on another machine. Existing
Python/R dependencies and the installed condition-evaluation helper are reused.
Checkpoint files use `AMWS_CHECKPOINT_ROOT` or `~/.codex/amws_checkpoints/`.
Completed releases are checksum-verified rather than overwritten.

## College-allocation refresh

The separate prep release is
`output/land_grants/amws_birth_county_training_postsecondary_20261019/`.
Results are in
`results/land_grants/event_studies/amws_birth_county_training_postsecondary_20261019/`.
The [birth-county training definitions](../../../analysis/land_grants/training_50km/BIRTH_COUNTY_TRAINING.md)
apply with the expanded qualification scope: ever/never/unknown inside the birth
county and within 50 km of its centroid, always restricted to scientists born
in that county and normalized by its estimated cohort births.

All 12,360 eligible entries, denominators, county geometry and four frozen
estimation samples are retained. There are 24 category models and four total
benchmarks, with the original CSDID settings and e=0 displayed reference.
Classification transitions, transition summaries and old-versus-expanded dynamic
and overall ATT comparisons accompany the usual panels, models, diagnostics and
four six-panel PNG/PDF figures. Total counts and effects stay unchanged; existing
ever-local classifications cannot be lost.

Independent validators check original ledger evidence and primary assignments,
conditional mappings, every long/wide join, full-source coverage, source
eligibility, spatial predicates/distances, counts/rates, frozen samples and
actual estimator inputs. Reports and validator snapshots are saved in separate
`*_validation` sibling folders without changing the completed release manifests.

Heading-related extraction failures remain unresolved in this release. Some
additional qualifications introduce unmatched institutions or uncertain levels,
so expanded scope can move a previously never-local entry into unknown as well
as reveal local training.

## Published coverage

The full 156,139-biography pool retains all 337,692 original primary-family
awards and assignments and adds 42,474 earned qualifications: 7,946 other
bachelor awards, 5,770 other master awards, 19,872 other doctorates and 8,886
postsecondary credentials. The label extension adds 1,939 ledger detections
before honorary/eligibility exclusions. The US person view still has 136,489
entries in the same order.

Within the fixed college-allocation preparation sample, 12,360 entries supply
30,475 included training awards and 734 distinct located institutions:

| Geography | Ever: previous → expanded | Never: previous → expanded | Unknown: previous → expanded |
|---|---:|---:|---:|
| Exact birth county | 2,562 → 2,630 | 5,940 → 5,667 | 3,858 → 4,063 |
| Within 50 km | 3,413 → 3,530 | 5,275 → 5,003 | 3,672 → 3,827 |

Every original ever-local entry remains ever local. In the exact-county version,
372 previous never entries become unknown, 25 become ever, and 167 previous
unknown entries become ever or never. For 50 km, the corresponding counts are
331, 55 and 176. Increased unknown counts reflect the requirement to locate all
reported training before declaring never local.

All eight sampled entries previously excluded solely for their degree types
now have extracted qualifications. The damaged-heading cases in that sample
remain unchanged; newly extracted awards without applicable institution mappings
still have unresolved locations.

All 40 targeted tests and both independent release validators pass. The
education audit verifies 384,309 ledger awards and 59,645,693 unchanged original
person-file cells. The outcome audit independently verifies all 24,720
classifications, all four frozen samples and all 28 models' actual inputs and
influence-function intervals. Original release checksums remain valid.

All 28 refreshed models completed. Category effects sum to the shared total
within 1.56e-15 for dynamics and 3.34e-16 for overall ATT; the total benchmark
differs from the old estimates by at most 5.56e-16. Small-cohort warnings remain.
Joint pre-trend Wald tests are unavailable in four relative-time models and all
14 calendar models because of singular covariance. The saved covariance/rank
diagnostics distinguish unavailable tests from observed pre-trend statistics.

## Latest zero-extraction audit

After the expansion, 288 eligible college-allocation entries have zero extracted
qualifications, compared with 598 in the original four-family release. This is
an extraction status, not a verified absence of training information.

A random sample of 20, drawn without replacement from the source-key-sorted
288-entry pool using seed `20261020`, was manually inspected:

| Finding | Entries |
|---|---:|
| Visible degree information missed by extraction | 13 |
| Institutions or attendance listed without an earned qualification | 5 |
| Honorary qualification only, with no earned qualification stated | 1 |
| Blank source biography | 1 |

The sample exposes a remaining degree-label gap: `1986:20640`, Robert Michael
Cooper, explicitly reports UCSF `PharmD`, which the published extractor still
misses. Other missed records have damaged or fused education headings and, in
some cases, merged biography text. The sample does not establish that every
remaining extraction failure is caused by a damaged heading. Attendance-only
records remain outside the current earned-qualification definition.

Full unchanged biography text, exact sample keys, manual inspection notes,
sampling metadata and input checksums are saved as HTML and JSON under
`results/land_grants/event_studies/amws_birth_county_training_postsecondary_20261019_validation/samples/`,
with stem `no_extracted_training_sample20_seed20261020`.

This audit documents remaining coverage limitations. It does not change the
published extraction, classifications or model estimates; repairing `PharmD`
recognition and damaged headings requires a subsequent versioned refresh.

## Unknown-training audit and multi_v04

This audit covers all 3,827 eligible entries whose 50 km training class is
`unknown`, not only the zero-extraction ones. It asks how much of `unknown` the
extractor causes. `education_regex_v04.py` (`multi_v04`) is the result. It is
a new file. The frozen `education_regex.py` and its `EXPECTED_HASH` are
unchanged, and **nothing is republished**: the education release, the outcomes
and the event-study estimates all stand as published.

### Sample and reading

`sample_unknown_training_audit.py` sorts the pool by `source_key` and draws 150
keys with `random.Random(20261019).sample`. The first 100 are the development
sample and the next 50 the holdout. The holdout was read once, after the rules
were written. Every biography was read in full. Verdicts, literal evidence and
the root cause of each `unknown` are saved in `review_dev.jsonl` and
`review_holdout.jsonl`.

| Root cause of `unknown` (development, n = 100) | Entries | Wilson 95% |
|---|---:|---|
| Institution text correct, crosswalk assigns no location | 72 | 62.5–79.9% |
| Extractor: institution missed (OCR `Univ` tokens, damaged first label) | 6 | 2.8–12.5% |
| Source text destroyed or merged | 6 | 2.8–12.5% |
| Extractor: awards missed (damaged or fused `Educ` heading) | 4 | 1.6–9.8% |
| Extractor: false award (`Am`, `Md`, 1955 name header) | 4 | 1.6–9.8% |
| Extractor: board diploma counted as an unresolved credential | 4 | 1.6–9.8% |
| Extractor: wrong institution | 3 | 1.0–8.5% |
| No earned qualification reported | 1 | 0.2–5.4% |

The extractor causes **21 of 100** (95% CI 14–30%). **Matching causes most of
`unknown`.** Its leading failures:

- **Bare state names in 1938/1955:** Illinois, California, Iowa, Kansas.
- **Conditional system labels in 1986:** `Univ Calif`, `Univ Tex` and `Univ Wis`. The crosswalk assigns `Univ Wis` only before 1956, for example.
- **OCR forms of Illinois:** `Univ 111`, `Univ III`, `Southern 111 Univ`, 474 awards in all.

`matching_inventory.csv` lists the 852 incomplete institution strings in the
pool. The top 20 cover 63% of the 4,867 incomplete awards. **1,726 unknown
entries are blocked only by those 20 strings.** This work leaves that inventory
unresolved.

### Rules added in multi_v04

All rules are grammar or OCR patterns, never person-specific.

- **Labels:** `PharmD` and `E.Met`.
- **Headings and section bounds:**
  - Damaged 1986 headings are recognised (`E due:`, `EdIC:`, `F.duc:`, and `Educ111 Col` fused to the school).
  - An `Educ` token inside career or membership text (`MED EDUC, 60`) is ignored when an earlier damaged heading or an unheaded clause exists and the late heading's own section has no degrees.
  - Without any heading, the clause after the birth line and specialty is read as education when a label precedes the first career section. The record is flagged `education_heading_inferred` and the clause ends at the first career date range.
  - `Prof Eap:`-style damaged career headings end the section.
- **False awards removed:**
  - The 1955 name/title/address header, before the `@SPECIALTY@` marker, no longer yields labels. Examples: `DR. E(VERETTE)`, `L(EE)`, `W(ILLI)AM`, `Darlington, Md.`. A merged earlier entry's `label, School, yy` tail before the marker is kept.
  - Undotted mixed-case `Am`/`Md` mean American/Maryland unless one award year follows (`AB, 48, Am, 50`).
  - A marriage field fused to the next label (`m. B.S`) no longer hides the real label.
- **Institutions:**
  - OCR `Univ` tokens are accepted: `Unis`, `Uniy`, `Unix`, `l nix`, `Lmv`, `lnxt Tcehnol`.
  - The first readable label takes the school of an unreadable label (`B«S, California, 34, M.S, 36`) or of a fellowship (`fellow, Ohio State, 41, Ph.D, 41`) just before it. These awards are flagged `institution_from_preceding_clause`.
  - A school is shared across a parenthesised subject (`B.S.(chem. eng) & B.S.(chem), School, 49`).
  - One- and two-letter bare schools (`Mi.35`) are rejected.
  - `feliow` counts as a fellowship.
- **Board certification:** `dipl`/`cert` after `Am Bd …`, `Am Acad …` and similar is flagged `board_certification`. It is a licence and should be excluded like a title. Under the current rules such credentials stay as `unresolved_level` and alone prevent a never-local conclusion for **116** unknown entries.

### Validation

`compare_education_parsers.py` runs both parsers, with the postsecondary label
extension, over all 156,139 biographies.

**Award-level changes:**

- **Changed in place:** 5,947 awards.
  - 3,291 re-classed as board certifications.
  - 1,522 institutions filled in.
  - 614 institutions changed, for example `Loyola Col` → `Loyola Col, Md`.
  - 354 junk one- or two-letter institutions cleared.
- **Removed:** 4,434 awards. Almost all are false `Md` (1,585) and `Am` (1,559), or 1955 header tokens.
- **New:** 5,589 awards, 5,262 of them in 1986.
- **Entries:** 2,178 gain their first training award. 197 lose all training awards; every one of those was inspected and each lost only false labels.

**Checks:**

- **Spot check:** of 30 seeded new training awards (seed `20261020`, final rules), all 30 are earned qualifications. Their institution text is clean in 20, noisy in 3 and missing in 7. Earlier draws during development found heading-residue junk (`MS o/.`, a specialty read as `PhD`). That kind of error remains possible in the unheaded fallback.
- **Holdout:** 128 earned awards in 50 entries.

| Holdout (n = 50 entries) | multi_v03 | multi_v04 |
|---|---:|---:|
| Award recall | 114/128 = 89.1% (82.5–93.4) | 123/128 = 96.1% (91.2–98.3) |
| Award precision | 114/125 = 91.2% (84.9–95.0) | 123/123 = 100% (97.0–100) |
| Award with correct institution | 107/128 = 83.6% | 116/128 = 90.6% |

All 11 v03 false positives were board diplomas/certificates, `Am`, or a 1955
name header.

### Estimated effect on the outcome

**This is an estimate, not a rebuild.** It reuses the published assignments of
awards with the same institution text, family and resolved year (then text and
year) and the release's county–institution distances. Re-deriving the published
classes from the v03 awards this way matches 99.91% of the 12,360 entries.

- Unknown falls from 3,818 to 3,554 (−264, −6.9%): 40 entries become ever-nearby and 228 never-nearby.
- 4 never-nearby entries become unknown. In each the corrected institution text (`(Md)`, `Iowa` for `S, Iowa`) has no exact-text assignment.
- Of the holdout, 5 of 50 entries leave `unknown`.

The extractor can remove roughly a tenth of `unknown`. Most of the rest needs
institution matching work, starting with `matching_inventory.csv`.

### Reproduction

```powershell
$env:TALENT_DETS_DATA_DIR = 'C:/Users/megaj/Globtalent Dropbox/gtl_talent_dets'
python -m unittest discover -s prep/amws/education -p "test_*.py"
python prep/amws/education/sample_unknown_training_audit.py
python prep/amws/education/compare_education_parsers.py
```

Outputs are in `output/amws/education_audit_unknown_training_20261019/`:

- review packets and manual verdicts;
- `award_diff.csv` and its summary;
- `spotcheck_review.csv`, `holdout_scores.json`;
- `classification_estimate*.csv`, `matching_inventory.csv`;
- `sample_manifest.json`, which holds the keys and the code and input checksums.

Rerunning the sampler reproduces the same keys but rewrites the manifest without
the parser checksums.

The versioned refresh below has since been run.

## multi_v04 release and college-allocation refresh

`build_postsecondary_release_v04.py` publishes
`output/amws/amws_education_postsecondary_v04_20261019/`. Its award scope comes
from `postsecondary_qualifications_v04.py`: the postsecondary_v1 rules applied
to multi_v04 awards, with `board_certification` excluded like titles and
honorary awards.

- **Reuse:** an award keeps its 20261019 `award_index` and institution assignment when its evidence key, institution text, resolved year, family and eligibility are all unchanged.
- **Reassignment:** every other included award is assigned from the published crosswalk by exact institution text, through `build_postsecondary_release.assign`. Conditions are evaluated against the multi_v04 education section.
- **Unchanged:** every previous release, the frozen parser and `Data/processed/amws/amws_open_alex.csv`.
- **Person view:** all eight family blocks, the education lists and status, and the `oa_*` summaries are rewritten. Every other cell, the row order and all 136,489 rows are unchanged.

| Release ledger | Awards |
|---|---:|
| Assignment reused from 20261019 | 369,850 |
| Reassigned by exact text (new or changed awards) | 8,137 |
| Removed (`removed_awards.csv`) | 7,686 |
| … no longer reported (`Md`, `Am`, 1955 header tokens) | 4,394 |
| … now excluded as board certifications | 3,291 |
| Changed text, year or family (`changed_awards.csv`) | 2,630 |

2,225 biographies have a recovered `Educ` heading (`education_heading_ocr` or
`education_heading_inferred`). 305 still have none.

`validate_postsecondary_release_v04.py` independently re-parses every biography
and re-derives each reuse or reassignment decision. It also checks the
coordinates and reconciles every wide award cell with the ledger. Its report is
in the `_validation` sibling folder.

The outcomes are rebuilt by
`prep/land_grants/11_build_amws_birth_county_training_postsecondary_v04.R` into
`output/land_grants/amws_birth_county_training_postsecondary_v04_20261019/`.

- **Same as before:** the rules, the 12,360 eligible entries, the denominators and the frozen samples. The comparison baseline is the 20261019 postsecondary outcomes.
- **Ever-local losses:** an ever-local entry may now be lost only through a removed or changed award. None was lost in either geography.

| Class | Same county: 20261019 → v04 | 50 km: 20261019 → v04 |
|---|---:|---:|
| Ever | 2,630 → 2,661 | 3,530 → 3,571 |
| Never | 5,667 → 5,936 | 5,003 → 5,236 |
| Unknown | 4,063 → 3,763 | 3,827 → 3,553 |

At 50 km, 278 unknown entries were classified: 237 as never and 41 as ever. 4
never entries became unknown, because their corrected institution text has no
exact-text assignment. Entries with no qualifying award fell from 288 to 117.
The rest of `unknown` is matching: 2,174 entries are `unmatched_award` and
1,164 `no_assigned_location`.

`analysis/land_grants/training_50km/run_birth_county_training_postsecondary_v04.R`
reran the 28 models with the original settings and seeds into
`results/land_grants/event_studies/amws_birth_county_training_postsecondary_v04_20261019/`.

- **Model checks:** all 28 succeeded. Category effects sum to the total within 1.4e-15, and the county-born totals equal the 20261019 estimates.
- **Change in overall ATTs:** at most 0.007 scientists per 1,000 births (`v03_vs_v04_post_treatment_att.csv`). Every change is well inside one standard error. Unknown-training effects shrink slightly and ever/never effects grow slightly. Dynamic coefficients move by at most 0.02 (`v03_vs_v04_dynamic_att.csv`).
- **Validation:** `validate_birth_county_training_postsecondary_v04.R` passes.

```powershell
$env:TALENT_DETS_DATA_DIR = 'C:/Users/megaj/Globtalent Dropbox/gtl_talent_dets'
python -m unittest discover -s prep/amws/education -p "test_*.py"
python prep/amws/education/build_postsecondary_release_v04.py --data-root "$env:TALENT_DETS_DATA_DIR"
python prep/amws/education/validate_postsecondary_release_v04.py --data-root "$env:TALENT_DETS_DATA_DIR"
Rscript --vanilla prep/land_grants/11_build_amws_birth_county_training_postsecondary_v04.R
Rscript --vanilla analysis/land_grants/training_50km/run_birth_county_training_postsecondary_v04.R
Rscript --vanilla analysis/land_grants/training_50km/validate_birth_county_training_postsecondary_v04.R
```

Completed releases are checksum-verified, never overwritten. The remaining lever
on `unknown` is `matching_inventory.csv` in the audit folder.

## v05 release: manual institution matches and the 1986 field classification

`build_postsecondary_release_v05.py` publishes
`output/amws/amws_education_postsecondary_v05_20261020/` on top of the v04
release. It does not re-parse biographies: families, award indices, labels,
years and eligibility are v04's, and nothing is removed.

**Which awards change.** An award qualifies when it is included, `eligible` and
not fully matched in v04. It then gets one of:

- the override ID from `medical_campus_overrides.csv` in
  `output/amws/manual_institution_matches_top500_20261019/`, for an MD, dental
  or pharmacy degree granted by a separate medical campus;
- otherwise, the ID its institution text has in `top500_manual_matches.csv`.

An award that already held some IDs keeps them and gains the manual one
(`manual_union_with_existing_ids`).

**What is never changed.**

- Awards v04 fully matched, including the 201 whose ID differs from the
  crosswalk. Those are listed in `manual_conflicts_review.csv`.
- Non-eligible awards.
- Strings whose crosswalk ID is blank.

**Provenance on a manual award.**

- `assignment_class`: `manual_top500_crosswalk` or `manual_medical_campus_override`.
- `raw_mapping_status`: the manual confidence.
- `record_id`: `top500:<rank>` or the override rule.
- New ledger columns `manual_assignment` and `manual_confidence`.

| | v04 | v05 |
|---|---:|---:|
| Included awards | 377,987 | 377,987 |
| Awards with an institution | 302,507 (80.0%) | 355,903 (94.2%) |
| Assigned by the crosswalk | | 52,752 |
| Assigned by a medical-campus override | | 653 |
| Institutions used | 1,621 | 1,677 |

Of the 53,405 manual awards, 2,836 are `clear`, 47,343 `likely` and 3,226
`ambiguous`. The 56 institutions new to v04 take their coordinates from the
GTAllocation snapshot (`education_geolocated/reference_institutions.csv`), with
full provenance in `institution_reference_coordinates.csv`.

**Person view.** `amws_open_alex.csv` keeps v04's rows, order and every
non-award cell, rebuilds the award blocks and `oa_*` summaries from the v05
ledger, and adds four columns:

- `manual_assigned_awards`;
- `physical_science`, `physical_science_unresolved` and `physical_science_mixed`,
  from `output/amws/physical_science_dummy_20261020/`. These are the 1986 edition
  only and blank for 1906, 1938 and 1955. `physical_science = 0` means no
  Physical match was detected, not a verified non-Physical scientist; use
  `physical_science_unresolved` to tell the two apart.

The builder asserts that every person without a manual award reproduces v04's
award cells and summaries exactly.

**Unmatched store.** The 22,204 included awards still without a full match are
listed one per row in `unmatched_awards.csv`. `unmatched_strings.csv` groups
them by text and is the worklist for the next matching round.

| `unmatched_category` | Awards |
|---|---:|
| `outside_top500` | 12,939 |
| `no_institution_text` (a parsing problem) | 7,678 |
| `unresolved_level` | 652 |
| `not_in_openalex` | 397 |
| `unresolved_split` | 281 |
| `no_institution` | 137 |
| `partially_matched` | 120 |

`normalization_candidate_id` records, for 1,853 `outside_top500` awards, the
institution that an OCR-normalised version of the text matches with at least
90% agreement among fully matched awards, for example "Uniy Pittsburgh" →
University of Pittsburgh. It is recorded for review and **not applied**.

`validate_postsecondary_release_v05.py` re-derives every manual assignment from
the crosswalk, the overrides and the snapshot, independently of the builder, and
checks the person view cell by cell against v04 and the field release. Its report
is `amws_education_postsecondary_v05_20261020_validation/independent_validation.json`.

```powershell
$env:TALENT_DETS_DATA_DIR = 'C:/Users/megaj/Globtalent Dropbox/gtl_talent_dets'
python -m unittest discover -s prep/amws/education -p "test_*.py"
python prep/amws/education/build_postsecondary_release_v05.py --data-root "$env:TALENT_DETS_DATA_DIR"
python prep/amws/education/validate_postsecondary_release_v05.py --data-root "$env:TALENT_DETS_DATA_DIR"
```

The land-grants training outcomes on v05 are built by
`prep/land_grants/12_build_amws_birth_county_training_postsecondary_v05.R`. They are
estimated by `analysis/land_grants/training_50km/run_birth_county_training_postsecondary_v05.R`
and `..._v05_unbalanced.R`, and validated by `validate_..._v05.R`.

- **Unknown entries:** 50 km unknown falls from 3,553 to 1,057.
- **Ever trained locally:** the overall effect (relative-time-balanced sample, standard decades) rises from 0.143 to 0.208 per 1,000 births inside the county, and from 0.144 to 0.206 within 50 km.
- **Total:** the county-born total is unchanged.

## v06 release: duplicate entries removed

`build_postsecondary_release_v06.py` publishes
`output/amws/amws_education_postsecondary_v06_20261021/`: v05 minus entries the
source captured twice. Every remaining row of every file is byte-identical to v05.

**How duplicates are found.**
- Biography text from `education_enriched_20261011_updated_final/source_biographies.csv` is normalised: lowercased, keeping letters and digits only. The person view has no 1955 text, so it can't be used for this.
- Entries are blocked on edition × the first 60 normalised characters. For 1986 that is the name, birthplace and birth date; for earlier editions, the birthplace, birth date and first degree.
- Two entries in a block are linked when one of these holds:
  - their texts are identical;
  - one is a prefix of the other;
  - their first 400 characters are at least 85% similar;
  - the pair is marked `duplicate` in `output/amws/amws_dedup_review_20261021/borderline_pairs_reviewed.csv`. All 35 borderline pairs there were read and confirmed to be the same person.

**Which copy is kept:**
1. the `kept=TRUE` member (1906–1955 only);
2. then the longest text;
3. then the lowest line id.

In 8 groups (all 1938) the `kept` flag picks a shorter copy over a longer one. `duplicates_removed.csv` lists every removed entry with its keeper, link kind, similarity, flags and differing columns.

| Edition | Removed | Pattern |
|---|---:|---|
| 1955 | 1,472 | One block of `amws_1955.dta` (lines 2–2353) repeated as 2403–4754, plus 8 small pairs |
| 1986 | 585 | The same entry read twice 1–5 lines apart, around page and column breaks: 326 truncated copies, 179 exact, 50 OCR variants, 30 reviewed |
| 1938 | 119 | Re-entered pages, about 17–93 lines apart |
| 1906 | 2 | |

That is 2,178 entries in 2,159 groups, taking the person view from 136,489 to 134,311 rows. Same-name entries with different text (e.g. William A. vs William B. Allen, 1955) are not linked. `validate_postsecondary_release_v06.py` recomputes the groups independently and checks every file row by row against v05.

**Effect on the land-grants training outcomes.**
- **Scripts:** built by `prep/land_grants/13_build_amws_birth_county_training_postsecondary_v06.R`; models `run_..._v06.R` (28) and `..._v06_unbalanced.R` (14); validator `validate_..._v06.R`.
- **Sample:** 40 eligible duplicates drop out (12,360 → 12,320), and every remaining entry keeps its class.
- **Effects:** no overall effect moves by more than 0.002 and no dynamic coefficient by more than 0.021. For example, the relative-sample total goes from 0.336 to 0.334 and ever trained in the county stays at 0.208.
- **Companion:** the Oct 21 companion PDF (Figures 13–16) was rebuilt from these results (`build_birth_county_training_v05_companion.py --release v06`) and replaced on Drive under the same file id.
