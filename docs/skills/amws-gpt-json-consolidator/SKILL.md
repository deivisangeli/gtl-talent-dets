---
name: amws-gpt-json-consolidator
description: Consolidate American Men and Women of Science GPT/Codex batch JSON outputs into validated JSON, CSV, and XLSX files using the reusable R script. Use when Codex needs to run the final AMWS step 2 consolidation from batch_XX_output.json files, join parsed fields with raw_text/prefix_text, and report validation metrics.
---

# AMWS GPT JSON Consolidator

## Overview

Use this skill only for the final consolidation step after AMWS entries have
already been segmented and parsed by GPT/Codex agents. This skill does not
segment DOCX/OCR text, does not replace step 1, and does not parse entries with
regex or subagents.

The canonical implementation is:

```text
prep/amws/consolidate_amws_gpt_json_parse.R
```

## Required Inputs

Run the skill on a directory containing:

```text
source_rows_for_consolidation.json
batch_01_output.json
batch_02_output.json
...
batch_XX_output.json
```

`source_rows_for_consolidation.json` must include:

```text
lineid
batch_id
prefix_text
raw_text
```

Each `batch_XX_output.json` must be a JSON array with objects containing:

```text
lineid
birth_place
birth_date
field
confidence
notes
```

Valid `confidence` values are `high`, `medium`, and `low`.

## Run Command

From the repo root, run:

```powershell
Rscript prep/amws/consolidate_amws_gpt_json_parse.R
```

The default run directory is:

```text
<Dropbox root>/output/amws/transcription_runs/amws16_A_0_200_regex_only_pages1_191/gpt54_mini_codex_parse
```

For another run, set environment variables before calling `Rscript`.

Common overrides:

```powershell
$env:AMWS_GPT_PARSE_RUN_DIR="C:/path/to/gpt54_mini_codex_parse"
Rscript prep/amws/consolidate_amws_gpt_json_parse.R
```

```powershell
$env:AMWS_GPT_PARSE_RUN_ID="amws16_A_0_200_regex_only_pages1_191"
$env:AMWS_GPT_PARSE_SUBDIR="gpt54_mini_codex_parse"
Rscript prep/amws/consolidate_amws_gpt_json_parse.R
```

Other supported overrides:

```text
AMWS_GPT_PARSE_SOURCE_FILE
AMWS_GPT_PARSE_OUTPUT_PREFIX
AMWS_GPT_PARSE_BATCH_COUNT
AMWS_GPT_PARSE_BATCH_SIZE
AMWS_GPT_PARSE_STRICT
```

Keep `AMWS_GPT_PARSE_STRICT=TRUE` unless the user explicitly asks for a
best-effort output despite validation issues.

## Outputs

The script writes these files in the run directory:

```text
amws_entries_gpt54_mini_codex_parsed.json
amws_entries_gpt54_mini_codex_parsed.csv
amws_entries_gpt54_mini_codex_parsed.xlsx
gpt54_mini_codex_run_summary.csv
gpt54_mini_codex_validation_issues.json
```

The consolidated table columns are:

```text
batch_file
batch_id
lineid
raw_text
prefix_text
birth_place
birth_date
field
confidence
notes
```

The XLSX must contain:

```text
parsed_entries
run_summary
```

## Validation

After running the script, verify:

- row count equals `input_rows` in `gpt54_mini_codex_run_summary.csv`;
- `lineid` is unique and spans the expected range;
- `validation_issues` is `0` unless strict mode was intentionally disabled;
- the XLSX opens and contains `parsed_entries` and `run_summary`;
- `raw_text` and `prefix_text` are present in the consolidated table.

Report these metrics:

```text
parsed_rows
unique_lineids
detected_batch_files
birth_place_nonempty
birth_date_nonempty
field_nonempty
all_three_nonempty
confidence_high
confidence_medium
confidence_low
validation_issues
```

When giving the final answer, link the final XLSX and mention the CSV/JSON paths
only if the user asks or they are central to the request.
