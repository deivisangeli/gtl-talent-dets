# Faculty roster research protocol

## Unit and period

- Research all instructional faculty at the assigned Andrews college from its
  opening or relevant institutional transition through academic year 1950-51.
- Use the academic-year starting year as `academic_year_start`.
- Never carry a person into an unobserved year solely because the person occurs
  in adjacent rosters.
- Every target college-year must receive a coverage status, including years for
  which no usable roster can be found.
- `found_roster_count` is the number of unique included normalized people in
  that college-year, not the number of appointment lines. A person holding two
  listed roles counts once.
- Use `source_located_inaccessible` when a specific roster-bearing source is
  identified but cannot be accessed or digitized. Reserve `not_found` for years
  with no located roster source and `source_found_not_processed` for accessible
  material still awaiting extraction.

## Source priority

Use the same source families that produced the first faculty collection, in
this order:

1. Contemporary official catalogs, registers, yearbooks, annual reports, and
   trustee or regents reports.
2. Official university archives, institutional histories, library exhibits,
   and historical catalog collections.
3. Historical books, state or local histories, and government education
   reports.
4. Contemporary professional journals and newspapers when official rosters
   cannot be recovered.

Prefer enumerated rosters to retrospective counts. Record a second source when
it materially verifies a name, instructional role, discipline, or total.

Before submission, every source inventory row must have one final
`source_status`: `manually_transcribed`, `source_located_inaccessible`,
`not_roster_bearing`, or `duplicate_source`. An accessible OCR/downloaded source
cannot be discarded when its old rows are removed; it remains unprocessed until
its full roster section is read. Every `manually_transcribed` source must have at
least one corresponding `manual_page_log.csv` row. Explain inaccessible,
non-roster, and duplicate classifications in source notes.

## Manual visual transcription

- OCR may be used to locate a faculty section, but OCR text is not authoritative
  roster evidence.
- Open every accessible roster page and read it visually at sufficient
  resolution. Transcribe each included name and role from the page image.
- Follow each faculty/officers roster from its heading through the start of the
  next section. Never treat the first roster page as the complete annual roster;
  multi-page sections must be rendered, logged, and transcribed page by page.
- Set `transcription_method=manual_visual` and
  `page_visually_verified=TRUE` only after inspecting the cited page. Record the
  printed or PDF page in `source_page` and describe corrections or ambiguity in
  `verification_notes`.
- Preserve verbatim OCR in `raw_roster_line` when available, but never copy an
  unverified OCR row into normalized fields.
- In `coverage.csv`, record the number of roster pages inspected and set
  `manual_verification_status` to `complete`, `partial`, or `not_applicable`.
  Missing and inaccessible sources remain missing; they are never zero faculty.
- Record every inspected roster page in `manual_page_log.csv`, including pages
  with no included rows. Use the exact same `source_id` and `source_page` locator
  as the roster entries, report included/excluded line counts, and document what
  was visually checked. This page log is the sampling frame for primary audit.

## Inclusion

Include professors, instructors, lecturers, tutors, demonstrators, teaching
assistants, and administrators with documented instructional responsibilities.
Preparatory, model-school, extension, medical, and branch-unit instructors may
be included when the unit belonged to the allocated institution at the selected
site; record the division explicitly.

Exclude trustees, inactive or honorary emeriti, research-only staff, and purely
administrative or operational employees unless teaching is explicitly
documented. Mark ambiguous entries `review` rather than silently including or
excluding them.

## Staging deliverables

Each batch directory contains five CSVs:

- `sources.csv`: one row per source document or digital object.
- `roster_entries.csv`: one row per printed appointment line.
- `coverage.csv`: one row per target college-year.
- `identity_proposals.csv`: within-college links and proposed cross-college
  matches. Researchers must not finalize cross-college identities.
- `batch_summary.csv`: batch status and unresolved issues.

Preserve raw text alongside normalized fields. Every included roster entry must
have teaching evidence, a source ID, and a page or section locator when the
source provides one. Agents write only inside their assigned staging directory.
When a coverage year uses more than one source, separate `source_ids` with a
semicolon; every listed ID must occur in `sources.csv`.

`person_name_normalized` and `role_raw` must be clean enough for analysis. Put
verbatim OCR, including damaged glyphs, in `raw_roster_line`; do not carry OCR
garbage into the normalized name. Status lines, headers, degree fragments, and
vacancy notices are not people and must be excluded. Before submission, inspect
every row emitted by `05_review_submitted_batch.R` in
`content_quality_flags.csv`, correct it against the page image, and rerun the
review until the flag file is empty. Also scan the remaining normalized names
for plausible-looking OCR errors that a character rule cannot detect.

Set `review_status` in `batch_summary.csv` to `submitted` when research is
ready for primary review. Only the primary reviewer may change it to
`accepted`.

An accepted batch may not contain `source_found_not_processed` years or
`scope_decision=review` roster rows. Access-blocked sources may remain only when
documented as `source_located_inaccessible` with the source and obstacle stated.
Acceptance additionally requires all automated checks, source/legacy
reconciliation, and the primary review of all uncertain pages plus a
chronologically stratified 10 percent page sample for each institution (minimum
five pages, or every page when fewer than five exist).

If an audited page contains a substantive transcription error, expand review to
every roster page in that source volume and the adjacent five academic years.
If more than 2 percent of sampled rows contain substantive errors, or the error
reflects a systematic layout/parsing problem, re-transcribe the entire
institution before acceptance. Record every sampled page as `pass` or `fail`
with reviewer notes in `primary_page_audit.csv`.
