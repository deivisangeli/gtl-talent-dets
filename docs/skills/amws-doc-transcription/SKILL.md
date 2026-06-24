---
name: amws-doc-transcription
description: Workflow for non-destructive segmentation of American Men and Women of Science OCR Word documents (.doc, .docx) into participant-level raw entries. Use when Codex needs to split AMWS document pages among agents and ask each agent to separate each participant verbete into an XLSX file with lineid and raw_text only.
---

# AMWS DOC Entry Segmentation

## Core Rule

Never edit, overwrite, or move source `.doc`, `.docx`, or `.pdf` files. All batch files, manifests, validation files, and combined outputs must be written under a new run directory:

```text
<Dropbox root>/output/amws/transcription_runs/<run_id>/
```

Use the canonical Dropbox root from `paths.R` / `TALENT_DETS_DATA_DIR` when available. On this project, the default local root is:

```text
C:/Users/<username>/Globtalent Dropbox/gtl_talent_dets
```

This skill is for raw segmentation only. Do not clean OCR, correct names, extract fields, generate corrected DOCX files, create per-agent R scripts, or ask agents to resolve uncertain text.

## Required Run Layout

Create one run directory per segmentation pass:

```text
output/amws/transcription_runs/<run_id>/
├── batch_manifest.csv
├── run_summary.csv
├── amws_entries_raw_combined.xlsx
├── validation_report.csv
├── batch_001/
│   └── batch_001_entries.xlsx
├── batch_002/
│   └── batch_002_entries.xlsx
```

Each subagent owns exactly one `batch_<id>/` directory. The main agent owns the run root, manifest, validation, and combined output.

## Main Agent Workflow

1. Identify the source `.docx` / `.doc` file, the page or chunk range, and the requested number of subagents.
2. Extract the assigned text from the source document into plain text or CSV batch inputs when useful. If the user says to use only the transcribed DOCX text, do not use images, PDFs, rendered page images, or new OCR.
3. Create `<run_id>` and `batch_manifest.csv` before spawning agents.
4. Split the work into balanced page batches. Record `page_basis = "word_detected_chunk"` when boundaries come from Word XML page/chunk breaks, or `page_basis = "pdf_physical_page"` only when the user explicitly wants PDF page boundaries.
5. Spawn exactly the requested number of subagents. Give each subagent only its batch id, input text/pages, page range, output directory, XLSX schema, and non-destruction rule.
6. Do not ask subagents to consolidate files. Subagents only produce their own `batch_<id>_entries.xlsx`.
7. After all subagents return, validate each XLSX and combine all batch rows into `amws_entries_raw_combined.xlsx`.
8. Write `run_summary.csv` with batch counts, total raw entries, validation status, and any missing outputs.

## Batch Manifest Schema

`batch_manifest.csv` must include:

```text
run_id
batch_id
agent_label
source_file
paired_pdf
page_start
page_end
page_basis
output_dir
status
notes
```

## Subagent Output

Each subagent must create exactly one required output in its own batch directory:

```text
batch_<id>_entries.xlsx
```

The workbook must contain a single worksheet with exactly these columns, in this order:

```text
lineid
raw_text
```

Rules:

- Use one row per participant verbete.
- Set `lineid` to a sequential integer within the batch: `1, 2, 3, ...`.
- Put the full raw verbete text in `raw_text`.
- Preserve the transcribed text as raw text. Do not correct OCR, spelling, names, dates, locations, fields, addresses, or punctuation.
- Do not add columns such as name, birth date, field, page, note, issue, or status.
- Do not create `issues.csv`, corrected DOCX files, R scripts, or cleaned CSV files by default.

## Definition Of An AMWS Entry

Use the structure observed in the first pages of AMWS edition 16 as the baseline.

A participant verbete usually starts with a printed participant name in uppercase:

```text
SURNAME, GIVEN NAMES...
```

Examples:

```text
AABOE, ASGER (HARTVIG), b Copenhagen, Denmark, Apr 26, 22; ...
ABBOTT, ROBERT FRED, JR, b Klamath Falls, Ore, May 27, 47; ...
```

After the name, a verbete often has one of these patterns:

- `, b <place>, <date>; ...`
- `. b <place>, <date>; ...`
- `<field>, see previous edition`
- `deceased`
- a clear biographical block without birth information.

The verbete includes all text for that participant up to immediately before the next participant starts.

## Entry Boundary Rules

Treat a new participant as likely when a line or embedded span begins with:

- an uppercase surname followed by a comma and given names;
- optional suffixes such as `JR`, `SR`, `II`, `III`, or parenthetical names;
- then `b`, `see previous edition`, `deceased`, or a plausible AMWS biographical block.

Treat the following as continuation of the current verbete, not as a new entry:

- lines beginning with `Educ:`;
- lines beginning with `Prof Exp:`;
- lines beginning with `Concurrent Pos:`;
- lines beginning with `Honors & Awards:`;
- lines beginning with `Mem:`;
- lines beginning with `Res:`;
- lines beginning with `Mailing Add:`;
- broken lines that continue an address, institutional title, research description, education record, or professional experience.

Join entries split across lines or pages into one `raw_text` cell.

Treat these as non-entry noise unless they are clearly part of the previous verbete:

- section letters such as `A`;
- page headers or page/name markers such as `2/AARONOFF` or `ABNEY/9`;
- isolated OCR debris such as `cs; et^cl`;
- fragments that do not contain a participant name or a clear continuation of a prior participant.

If OCR corrupts the beginning of a name but the rest of the text is clearly a participant biography, keep it as one verbete and preserve the raw text. Do not invent or repair the name.

If a fragment could be either a new participant or continuation, choose the option that avoids creating a false participant from obvious field, address, or career text. When still uncertain, keep the fragment with the surrounding verbete rather than creating a standalone row from debris.

## Validation

Before finishing a run, the main agent must verify:

- no source `.doc`, `.docx`, or `.pdf` file changed;
- every manifest row has `batch_<id>_entries.xlsx` or a failure note;
- each batch XLSX has exactly one worksheet with exactly `lineid` and `raw_text`;
- `lineid` is sequential and non-empty within each batch;
- every `raw_text` value is non-empty;
- combined row count equals the sum of batch row counts.

Report missing outputs, schema errors, non-sequential line IDs, or empty raw text in `validation_report.csv` and `run_summary.csv`.
