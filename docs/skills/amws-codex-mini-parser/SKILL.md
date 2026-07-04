---
name: amws-codex-mini-parser
description: Workflow for parsing already segmented American Men and Women of Science lineid/raw_text entries with Codex gpt-5.4-mini subagents using 40-word prefixes. Use when Codex needs to extract birth_place, birth_date, and field without regex or direct OpenAI API calls, consolidate strict JSON outputs, and validate against corrected AMWS parser outputs.
---

# AMWS Codex Mini Parser

## Overview

Parse AMWS entries that were already segmented into participant-level
`lineid`/`raw_text` rows. This is an alternative for step 2 only: extracting
`birth_place`, `birth_date`, and `field` from existing raw entries with Codex
subagents.

Do not use this skill to segment DOCX, PDF, OCR, or Word-derived text. Do not
replace step 1. Do not write scripts that call the OpenAI API directly.

## Core Rule

Never edit, overwrite, or move source `.doc`, `.docx`, `.pdf`, raw OCR, or raw
segmentation files. Write derived outputs under:

```text
<run_dir>/gpt54_mini_codex_parse/
```

Preserve `raw_text` for auditability. Send only the first 40 words of each
entry to the parsing subagents by default.

## Required Input

Use the raw combined segmentation workbook:

```text
<run_dir>/amws_entries_raw_combined.xlsx
```

Required column:

```text
raw_text
```

Use an existing `lineid` column when present. If `lineid` is absent or
batch-local, create a global sequential `lineid` from row order and preserve any
original identifier in helper columns.

## Prefix Construction

For each row:

1. Normalize whitespace with a simple squish operation.
2. Split on whitespace.
3. Keep the first 40 words as `prefix_text`.
4. Do not send the full `raw_text` to subagents unless the user explicitly asks
   for a diagnostic rerun.

The 40-word default comes from the first-10-pages AMWS pilot: the needed birth
and field information appeared within the first section of the entry, with the
first section marker reached by at most 39 words.

## Subagent Workflow

Create batch input JSON files with approximately 30-31 entries each. For each
batch, run a Codex subagent with:

```text
model = "gpt-5.4-mini"
reasoning_effort = "low"
```

Instruct each subagent to use only the provided `lineid` and `prefix_text`.
Forbid external knowledge and inference beyond the visible text. Require JSON
only, with no markdown or commentary.

Each subagent response must be a JSON array of objects with this exact schema:

```json
{
  "lineid": 1,
  "birth_place": "",
  "birth_date": "",
  "field": "",
  "confidence": "high",
  "notes": ""
}
```

Use `confidence` values only from:

```text
high
medium
low
```

Keep `notes` short and focused on OCR ambiguity or missing evidence.

## Extraction Rules

Extract `birth_place` from text after a birth marker such as `b` or likely OCR
`h`, ending before the visible date. Leave blank if no readable place is
present.

Extract `birth_date` as the visible date string. Normalize only obvious
mechanical punctuation and spacing, such as missing spaces after commas. Do not
convert to ISO format and do not infer missing components.

Extract `field` as the discipline phrase after the birth/date and demographic
fragments, stopping before section markers such as:

```text
Educ
Prof Exp
Concurrent Pos
Honors & Awards
Mem
Res
Mailing Add
see previous
deceased
```

For `see previous` or `deceased` entries, leave birth fields blank and recover
`field` only if it is visibly present before the marker.

Preserve OCR text when it is the only evidence. Do not repair names,
institutions, addresses, or fields using outside knowledge.

## Required Outputs

Write outputs under:

```text
<run_dir>/gpt54_mini_codex_parse/
```

Required artifacts:

```text
gpt54_mini_codex_batch_inputs.json
batch_XX_input.json
batch_XX_output.json
amws_entries_gpt54_mini_codex_parsed.json
amws_entries_gpt54_mini_codex_parsed.csv
amws_entries_gpt54_mini_codex_parsed.xlsx
gpt54_mini_codex_run_summary.csv
```

Also write this validation file when a corrected comparison workbook is present:

```text
gpt54_mini_codex_validation_against_final.csv
```

The consolidated parsed table must include:

```text
lineid
birth_place
birth_date
field
confidence
notes
```

It may include helper columns such as `prefix_text`, `raw_text`, batch id, or
comparison fields.

## Validation

Before finishing, verify:

- one and only one response per `lineid`;
- no missing or duplicate `lineid`;
- JSON output parses cleanly;
- all required columns are present;
- `confidence` uses only `high`, `medium`, or `low`.

If this file exists, compare against it:

```text
<run_dir>/regex_gap_audit/amws_1986_ed16_pilot_nonrisky_corrected.xlsx
```

Report:

- row count processed;
- nonempty counts for `birth_place`, `birth_date`, and `field`;
- complete rows with all three fields nonempty;
- exact matches by field against the corrected workbook;
- exact matches across all three fields;
- confidence counts;
- representative mismatch examples.

## Pilot Defaults And Results

Use these defaults unless the user gives a different run design:

```text
prefix words: 40
batch size: 30-31 rows
model: gpt-5.4-mini
reasoning_effort: low
```

The first-10-pages pilot produced:

```text
rows processed: 183
birth_place nonempty: 165
birth_date nonempty: 165
field nonempty: 182
complete rows: 164
all-fields exact matches against final corrected: 112
confidence high/medium/low: 144/12/27
```
