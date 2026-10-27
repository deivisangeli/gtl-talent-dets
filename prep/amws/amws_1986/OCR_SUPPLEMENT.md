# Edition-16 OCR supplement integration

The pipeline reads the immutable reviewed 14-document OCR delivery, not the
original unrepaired OCR. It runs separately from the DOCX cleaning workflow.
Set `TALENT_DETS_DATA_DIR` explicitly to the Dropbox project root; R loads
`paths.R`, and Python accepts the same environment variable or `--data-root`.
All raw inputs, earlier-edition person data, analysis panels, gender outputs,
and education outputs are protected from modification.

## Workflow

Run from the repository root with Python, Rscript, and bundled openpyxl
available. The run directory defaults to
`output/amws/ocr_supplement_20261011`; Python also accepts `--stage` and the
parser receives the absolute run directory as its second argument.

1. Run `ocr_supplement.py prepare` to inventory every page/block and hash sources.
2. Run `11_parse_ocr_supplement.R <repo-root> <stage> patterns` to export the
   established segmentation expressions without executing legacy workflows.
3. Load reviewed `block_overrides.json` decisions, then run
   `ocr_supplement.py segment`. The original block text is retained separately.
4. Run `11_parse_ocr_supplement.R <repo-root> <stage>` to parse and geocode only
   the supplement. Existing parser functions and local geodata are reused.
   Four local R workers perform parsing; `AMWS_OCR_PARSE_WORKERS` controls this.
   Parsed rows are cached by source key and exact adjusted text.
   Run `13_normalize_ocr_dates.R <repo-root> <stage>` afterwards. It applies
   existing OCR digit/day/year helpers only to complete, valid date tokens;
   missing components are never inferred.
5. Review `review_candidates.csv`, apply explicit source-keyed decisions in
   `manual_overrides.json`, and rerun parsing. Inspect the source scan; record
   evidence, action, confidence, and changed fields. Correct only supported values.
6. Run `ocr_supplement.py audit`, inspect all 200 stratified scans, and record
   `audit_reviews.json`. Correct errors and inspect other affected records.
7. Run `12_publish_ocr_supplement.py build`. Review `duplicate_candidates.csv`
   and supply `duplicate_decisions.json`; then rebuild. Names alone do not
   authorize a duplicate decision. Same-source or exact biography evidence is needed.
8. Export each candidate CSV to its paired XLSX with
   `support/export_ocr_workbooks_streaming.py <csv> <xlsx> <sheet-name>`.
   Pass `--reference <baseline-xlsx>` for the four existing distributions.
   The artifact-tool exporter was verified on the baseline final workbook, but
   failed its string-size limit on the larger full distribution. The streaming
   fallback preserves column widths, filters and final-workbook header styling;
   `xlsx_export_fallback.json` records the failure. Every exported cell is checked.
9. Run `12_publish_ocr_supplement.py validate`, then `publish`. Validation checks
   every CSV/XLSX cell, old input checksums, page/block accounting, source keys,
   final eligibility, and both scan-review ledgers. Publication backs up all
   replaced files, checks staged checksums, and rolls back on installation failure.

## Outputs and identifiers

The four edition-16 distributions retain their existing columns. Old CSV rows,
ordering, corrections and final IDs are preserved. New final IDs start above
the old maximum. The new full and processed copies retain notices, incomplete
entries and unresolved cases. Candidate values remain in the disposition ledger;
an unresolved ownership/validity decision withholds its birth year from the
official copies until supported by a scan decision.

Source keys encode PDF stem, one-based PDF page, corrected block index and
split-piece number. `source_registry.csv` links them to final entry IDs.
`effective_blocks.csv`, `block_assignments.csv`, `entry_dispositions.csv`,
`manual_overrides.json`, and `continuation_links.json` retain provenance.
Cross-file continuations join only consecutive ranges in the same volume.
Continuations of existing DOCX entries are linked without rewriting their rows.

The final dataset requires nonempty city/year and accepted country, with US
births successfully geocoded. Non-US records are retained without US geocodes.
Notices have no invented birth information and do not enter final counts.

`output/amws/amws_all_editions_persons.csv` and `.xlsx` stack the existing
1906–1955 `kept` rows and the expanded 1986 final dataset. They use common birth
and geographic columns plus `edition` and edition-qualified `source_record_id`.
Early biography/date fields absent from the combined source remain blank.
Existing geographic candidate rows are retained under the original convention.
1986 is not deduplicated against earlier editions; totals count source records,
not verified unique people. The early combined file is unchanged.

`summary.json`, `validation.json`, `baseline_manifest.json`, and
`publication.json` provide counts, checksums, backups and release status.
Rebuilding from frozen baselines gives identical rows and identifiers; calling
`publish` again verifies installed hashes and appends nothing.

The legacy mirror exporter defaults to preserving IDs when a final file exists.
`AMWS_ED86_PRESERVE_ENTRY_IDS=TRUE` explicitly selects that behavior to
retain published IDs instead of assigning IDs from the current row positions.
Removing or changing previously published records then fails explicitly.

The default driver resolves input, staging and publication paths through
`paths.R`:

```text
python prep/amws/amws_1986/run_ocr_supplement.py prepare --rscript <Rscript>
python prep/amws/amws_1986/run_ocr_supplement.py stage --rscript <Rscript>
python prep/amws/amws_1986/run_ocr_supplement.py publish --rscript <Rscript> --xlsx-python <Python-with-openpyxl>
```

Stage artifacts include explicit scan decisions, duplicate decisions, page and
block accounting, source hashes, a frozen baseline, a 200-record fixed audit
selection, and a separate queue of unclassified headings. PDF page numbers are
authoritative; the `printed_page` estimate is nominal and can differ where the
source PDF repeats a printed page (D800 PDF pages 75–76).

## 2026-10-11 release accounting

The published conservative append contains 31,332 candidates from 14 documents and
1,792 pages: 19,731 additions, 41 duplicates, 7,954 exclusions, and 3,606 unresolved
candidates. The expanded 1986 final contains 99,308 rows; the four-edition person
stack contains 152,354 rows, including the unchanged 53,046 early-edition kept
rows. Full and processed distributions each contain 126,100 rows; filtered
contains 104,182. Check `publication.json` for actual installation status.

All 200 accepted audit biographies were inspected against scans; 32 records
needed explicit name, birth, county or segmentation corrections. The 33 known
recognition issues, 219 single-heading suffix guards, 13 interleaved headers,
two unresolved margin blocks, and source-based duplicates were reviewed.

**Exception review is not complete.** The 3,606 unresolved candidates include
remaining suspect names, dates, segmentation and geographic matches awaiting
individual scan decisions. They are not included in final counts. Their original
OCR and candidate parses remain in the sidecars; uncertain names/years/cities,
invalid dates and uncertain geocodes are withheld from the public full/processed
copies as appropriate. Incomplete biographies and notices remain in those copies.
No script treats these pending cases as manually approved.

Analysis panels, gender datasets and education enrichments remain awaiting
refresh. Their protected checksums and the 1906–1955 combined input are unchanged.

## Verification

```text
Rscript prep/amws/amws_1986/support/test_ocr_supplement.R <repo-root>
python -m unittest discover -s prep/amws/amws_1986/support -p test_ocr_supplement.py
```

Coverage includes existing-ID preservation, missing US geocodes, non-US
eligibility, century parsing, page/column continuations, different-volume
boundaries, orphan fragments, notices and deterministic segmentation.
Raw sources and review scans are never modified.

Analysis panels and their gender/education derivatives require a separate refresh
after publication. This pipeline does not switch any analysis input paths.
