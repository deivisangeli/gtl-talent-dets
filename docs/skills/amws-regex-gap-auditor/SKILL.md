---
name: amws-regex-gap-auditor
description: Workflow for auditing American Men and Women of Science parsed-entry tables where birthplace, birth date, or field extraction is missing. Use when Codex needs to inspect AMWS lineid/raw_text rows, distinguish true missing information from regex/OCR/segmentation failures, and produce non-destructive R regex rule proposals to recover extractable birthplace, birth_date, and field values.
---

# AMWS Regex Gap Auditor

## Overview

Audit AMWS entries that were already segmented into participant-level `raw_text` rows and parsed with regex. Focus only on rows where `birthplace`, `birth_date`, or `field` is empty, and produce an explicit pattern inventory plus R regex proposals without modifying source documents, raw XLSX files, or the main parser.

## Core Rule

Never edit, overwrite, or move source `.doc`, `.docx`, `.pdf`, raw segmentation XLSX, or existing parsed CSV files. Write all audit outputs under a new subdirectory next to the parsed files:

```text
<run_dir>/regex_gap_audit/
```

This skill proposes improvements. Do not apply changes to the production parser unless the user asks in a separate implementation task.

## Required Inputs

Use the parsed AMWS table as the primary input. It must include:

```text
lineid or global_lineid
raw_text
birthplace
birth_date
field
```

If available, also read `parse_flag`, `batch_id`, and the current parser script that generated the table. Treat `raw_text` as the only evidence for whether information is present in the transcribed Word text.

## Required Outputs

Create:

```text
regex_gap_audit/
├── regex_gap_audit_cases.csv
├── regex_pattern_inventory.csv
├── regex_test_results.csv
├── proposed_regex_rules.R
└── audit_summary.md
```

`regex_gap_audit_cases.csv` must have one row per audited gap case:

```text
global_lineid
lineid
batch_id
missing_birthplace
missing_birth_date
missing_field
current_birthplace
current_birth_date
current_field
case_class
evidence_text
recommended_action
raw_text
```

`regex_pattern_inventory.csv` must have one row per proposed regex pattern:

```text
pattern_id
target_field
priority
case_class
regex
description
covered_lineids
false_positive_risk
do_not_match_examples
```

`regex_test_results.csv` must show before/after effects:

```text
global_lineid
lineid
pattern_id
target_field
old_value
proposed_value
changed
needs_manual_review
raw_text
```

`proposed_regex_rules.R` must contain reusable R code only. It should define helper functions or candidate extraction blocks that can be copied into the parser later, but it must not read/write project data or overwrite the parser.

`audit_summary.md` must report:

- total rows audited;
- counts by missing field;
- counts by `case_class`;
- number of recoverable `birthplace`, `birth_date`, and `field` values;
- examples of true missing cases;
- examples of recoverable regex cases;
- remaining manual-review cases.

## Workflow

1. Read the parsed table and identify gap rows where any of `birthplace`, `birth_date`, or `field` is empty.
2. Preserve the full original `raw_text`; create normalized helper text only for matching.
3. Review the gap rows manually enough to understand recurrent patterns before writing regex. Do not start from a single example.
4. Classify each gap row into exactly one `case_class`.
5. Build regex candidates from recurring textual evidence in `raw_text`.
6. Test each regex only against gap rows first, then spot-check against non-gap rows to estimate false positives.
7. Write all five required outputs under `regex_gap_audit/`.
8. Report counts and examples to the user; keep proposed parser changes separate from applied parser changes.

## Case Classes

Use exactly these `case_class` values:

```text
true_missing
recoverable_regex
segmentation_problem
ocr_corruption
manual_review
```

Classify as `true_missing` only when `raw_text` lacks textual evidence for the missing value. Common examples:

- `see previous edition`;
- `deceased`;
- a valid entry that starts with name and field/education but has no `b <birth block>`;
- `b <date>` with no place before the date, for missing birthplace only.

Classify as `recoverable_regex` when the information is visible in `raw_text` and a bounded regex can extract it without broad inference. Examples:

```text
b San Francisco, Calif, Apr 14, 36
h Campbell, Calif, Jan 19, 16
NORMAN'b Grand Coulee Dam, Wash, Oct 19, 38
b Monmouth, 111, Aug, 19,49
; m 68;AB 7 CHEMISTRY Educ
```

Classify as `segmentation_problem` when `raw_text` is a continuation fragment, page debris, or a row missing the participant start.

Classify as `ocr_corruption` when information may be present but OCR damage is too severe for a reliable regex.

Use `manual_review` only when none of the above is defensible.

## Regex Mapping Rules

Prefer incremental, auditable regexes over one large permissive expression. Each proposed regex must have a narrow trigger, a target field, covered examples, and known exclusions.

Birth marker variants to consider:

```text
[,.] b <place/date>
<name>'b <place/date>
<name> b <place/date>
[,.] h <place/date>
[,._-]b_<place/date>
```

Date variants to consider:

```text
Apr 14, 36
Apr 12. 31
Aug, 19,49
Jan 15,22
Nov 18, 41
No* JM7
ran 14, zt>
June 5, 24
```

Field variants to consider:

```text
<date>; m 68;AB 7 CHEMISTRY Educ
<date>. VASCULAR ...
<date>; US citizen; m 62; c 1. PLANT PHYSIOLOGY. Educ
<name>, FIELD, see previous edition
<name>, FIELD. Educ:
```

For fields, strip demographic prefixes before extraction:

```text
US citizen
nat US
m <year or OCR-noisy age/year>
c <number>
wid
div
sep
```

Stop field extraction before:

```text
Educ
Prof Exp
Concurrent Pos
Honors & Awards
Mem
Res
Mailing Add
```

## R Proposal Requirements

Write `proposed_regex_rules.R` using project-style R and common tidyverse/stringr helpers. The file should be self-contained functions or candidate blocks such as:

```r
extract_birth_marker_variant <- function(raw_text) {
  # return tibble(birthplace, birth_date, rule_id)
}

extract_field_after_noisy_demographics <- function(raw_text, birth_date_end) {
  # return tibble(field, rule_id)
}
```

Do not hard-code final values by `lineid` except in a clearly marked test fixture or example vector. The proposed rules must generalize from textual patterns.

## Validation

Before finishing, verify:

- every gap row appears in `regex_gap_audit_cases.csv`;
- every proposed regex has at least one covered `lineid`;
- no proposed regex overwrites a non-empty current value unless flagged as a candidate comparison;
- `proposed_regex_rules.R` is syntactically readable R;
- `audit_summary.md` totals match the CSV outputs.
