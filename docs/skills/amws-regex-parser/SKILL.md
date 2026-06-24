---
name: amws-regex-parser
description: Workflow for parsing American Men and Women of Science raw segmented entries with conservative regex extraction. Use when Codex needs to analyze AMWS raw_text verbete structure, then turn lineid/raw_text rows into an initial parsed table with birth_place, birth_date, field, and parse_flag while preserving raw_text and applying only low-risk OCR/punctuation normalizations.
---

# AMWS Regex Parser

## Overview

Parse AMWS entries that were already segmented into participant-level `raw_text`
rows. This skill is for the first regex pass after raw segmentation and before
gap auditing.

Do not start by writing regexes. First establish the structure of the verbetes
from a reproducible random sample of `raw_text`, then implement regexes around
that structure.

## Core Rule

Never edit, overwrite, or move source `.doc`, `.docx`, `.pdf`, or raw
segmentation XLSX files. The parser may write derived CSV outputs next to the
raw segmentation workbook under the current run directory.

Preserve `raw_text` exactly as normalized text for auditability. Do not rewrite
or infer the participant biography. Apply conservative normalizations only to
extracted fields.

## Required Input

Use the raw combined segmentation workbook:

```text
<run_dir>/amws_entries_raw_combined.xlsx
```

Required columns:

```text
batch_id
lineid
raw_text
```

`lineid` in this input may be batch-local. The parser must create a final
sequential `lineid` from `global_lineid`.

## Structural Reconnaissance

Before creating or changing the parser, inspect 100 random verbetes from the raw
segmentation workbook. Use `raw_text` as the evidence, not the parsed output.
Use a fixed seed so later agents can reproduce the same reconnaissance:

```r
set.seed(1986)
sample <- raw_entries |>
  mutate(global_lineid = row_number(),
         raw_text = stringr::str_squish(raw_text)) |>
  dplyr::slice_sample(n = min(100, nrow(raw_entries)))
```

On that sample, record counts and examples for:

- birth markers: spaced `b`, OCR `h`, glued `'b` / `.b`, and `b_`;
- date-like strings after the birth marker;
- `Educ` and other section markers;
- `see previous`, `deceased`, and rows with no birth block;
- OCR-heavy or segmentation-damaged rows where regex recovery is unsafe.

The first 100-entry pilot reconnaissance with seed `1986` found:

```text
rows: 100
spaced b marker: 81
OCR h marker: 2
glued b marker: 3
b_ marker: 0
date-like string: 86
Educ marker: 88
see previous: 4
```

Use these counts as orientation, not as hard-coded assumptions. Re-run the
sample when the raw segmentation input changes materially.

## Common AMWS Verbete Structure

Most entries follow this structure:

```text
NAME, b <birth_place>, <birth_date>; <demographic fragments>; <field>. Educ: ...
```

Examples from the pilot sample:

```text
b Calcutta, India, Mar 24, 43; US citizen; m 68; AB 7 CHEMISTRY Educ
b Cairo, Egypt, Apr 3, 37; m 60; c 2 MOLECULAR BIOLOGY. Educ
b Philadelphia. Pa. June 23. 26; m 56 c 4ECOLOGY Educ
b Iowa City, Iowa. Apr 12. 31; m 74- c CARDIOVASCULAR PHYSIOLOGY Educ
b New York, NY, Jan 5, 33; m 58; cMATERIALS SCIENCE. Educ
b Hagerstown, MD, Dec 3,50. QUANTUM OPTICS, LASER PHYSICS. Educ
```

### Birth Place

`birth_place` is usually the text between the birth marker and the first
date-like string. It may use commas or periods as separators:

```text
b Calcutta, India, Mar 24, 43
b Philadelphia. Pa. June 23. 26
b Brooklyn, NY, Mar 3,26
```

Common marker variants to support in the parser:

```text
, b <place> <date>
. b <place> <date>
 h <place> <date>        # OCR for b in some rows
NORMAN'b <place> <date>  # b glued to a corrupted/name token
b_<place> <date>
```

Do not infer a birthplace when the text before the date is missing or too
corrupt. Leave it blank for the gap auditor.

### Birth Date

`birth_date` usually follows the birth marker and birthplace. Typical date
forms include:

```text
Sept 30, 40
Sept 30. 40
Jan 15,22
June 23. 26
Mar 3,26
Dec 3,50
Apr 12. 31
```

Accept month abbreviations and full month names. Treat commas, periods, and
missing spaces between day and year as mechanical punctuation variants. Normalize
only clear punctuation variants in the extracted `birth_date`; keep `raw_text`
unchanged.

### Field

`field` usually appears after `birth_date`, after short demographic fragments.
Common prefixes before the field include:

```text
US citizen
nat US
m 68
m 74-
c 1
c 2.
c 4
cMATERIALS
div
wid
sep
```

The field is the discipline phrase after these fragments and before the next
section marker. Examples:

```text
; US citizen; m 68; AB 7 CHEMISTRY Educ
; m 60; c 2 MOLECULAR BIOLOGY. Educ
; m 56 c 4ECOLOGY Educ
; m 58; cMATERIALS SCIENCE. Educ
. CHEMICAL PHARMACOLOGY, DRUG METABOLISM. Educ
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

Do not abandon `field` extraction only because `birth_date` failed. When the
birth block is OCR-damaged, still attempt a bounded field extraction from the
text before `Educ` if the candidate looks like a discipline phrase and not an
institution, degree, job title, address, or OCR debris.

### Non-Birth Entries

Some valid entries have no birth block. Common cases:

```text
NAME, field, see previous edition
NAME, field, deceased
```

For these, leave `birth_place` and `birth_date` blank and recover `field` only
when the field text before `see previous` or `deceased` is readable.

Rows that are continuation fragments, page debris, or heavily corrupted OCR
should stay partially blank and be routed to the gap auditor rather than fixed
by broad regex.

## Required Output

Write the initial parsed table:

```text
<run_dir>/amws_entries_regex_parsed.csv
```

The public parsed columns used by downstream stages are:

```text
lineid
raw_text
birth_place
birth_date
field
parse_flag
```

The script may retain helper columns such as `batch_id`, `batch_lineid`,
`name_raw`, or `global_lineid`, but downstream final export must use the public
schema above.

Also write an audit-flag CSV containing rows where `parse_flag != "ok"`:

```text
<run_dir>/amws_entries_regex_audit_flags.csv
```

## Conservative Normalizations

Apply these during parsing, not during final XLSX consolidation:

- `111 -> Ill` in `birth_place`, only when it appears as a state-like token.
- Mechanical date punctuation, such as `June 22. 33 -> June 22, 33`,
  `Jan 15,22 -> Jan 15, 22`, and `Aug, 19,49 -> Aug 19, 49`.
- Simple place separators, such as `Washington DC -> Washington, DC`.
- Obvious field cleanup, such as `4ECOLOGY -> ECOLOGY`.

Do not apply broad geocoding, name repair, manual inference, or corrections that
require external knowledge.

## Parse Flags

Use `parse_flag = "ok"` only when `birth_place`, `birth_date`, and `field` are
all non-empty after conservative normalization.

Otherwise concatenate missing flags in this order:

```text
no_birth
no_birth_date
no_birth_place
no_field
```

## Validation

Before finishing, verify:

- output row count equals input row count;
- final `lineid` is unique and sequential;
- `raw_text` is non-empty for all rows;
- output has `birth_place`, not only legacy `birthplace`;
- no `birth_place` value contains bare `111`;
- audit flags contain exactly rows where `parse_flag != "ok"`.
