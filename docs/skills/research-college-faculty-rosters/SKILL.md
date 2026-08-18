---
name: research-college-faculty-rosters
description: Manual historical research workflow for building annual instructional-faculty rosters for the Andrews college-allocation institutions from each opening or transition through 1950-51. Use when Codex or worker agents must locate authoritative catalogs, registers, yearbooks, reports, or archival rosters; visually transcribe faculty names and roles; document college-year coverage and gaps; or audit faculty-roster batch CSVs for the GTL Talent Determinants project.
---

# Research College Faculty Rosters

## Core rules

Research every academic year in the assigned `targets.csv`, using
`academic_year_start` as the year key. Include all instructional faculty, not
only founding faculty. Never carry a person into an unobserved year because the
person appears in adjacent rosters.

One adequate roster-bearing source is sufficient for a college-year. Do not
search for a second source merely to corroborate information already stated
clearly in a reliable source. Seek another source only when the first is
illegible, internally ambiguous, demonstrably incomplete, or does not establish
an instructional role.

Treat the batch schema as frozen. Do not edit pipeline code, column definitions,
validation rules, assignments, or files outside the assigned staging directory.

## Source priority

Use the same source families as the first faculty search, in this order:

1. Contemporary official catalogs, registers, yearbooks, annual reports, and
   trustee or regents reports.
2. Official university archives, institutional histories, library exhibits,
   and historical catalog collections.
3. Historical books, state or local histories, and government education
   reports.
4. Contemporary professional journals and newspapers when no official roster
   can be recovered.

Prefer enumerated rosters to retrospective summaries or faculty counts. Record
the direct URL or stable digital-object URL, repository, title, covered academic
years, and complete numeric roster-page range.

## Research workflow

1. Read `targets.csv` and, when present, `legacy_seed.csv` and
   `legacy_coverage_seed.csv`. Use legacy records only as search leads until the
   cited page is inspected.
2. Search the prioritized source families for each target year. A catalog or
   yearbook may cover several target years, but map its stated academic year
   carefully rather than inferring from upload or publication metadata alone.
3. Download or open the source and locate the complete faculty/officers section.
   OCR may help locate pages but is not authoritative evidence.
4. Inspect every roster page visually at readable resolution, continuing from
   the roster heading through the next section. Do not stop after the first
   roster page.
5. Transcribe every appointment line, including excluded lines needed to make
   page counts auditable. Normalize names and roles only after checking the page
   image; preserve uncertain or damaged text in `raw_roster_line` and notes.
6. Log every inspected page, including pages with no included rows. Use the same
   `source_id` and numeric `source_page` locator in the source inventory, page
   log, and roster entries.
7. Add one coverage row for every target college-year. Missing or inaccessible
   rosters are missing data, never zero faculty.
8. Propose within-college identity links when independently observed annual
   records clearly refer to the same person. Do not finalize cross-college
   identities.
9. Reconcile counts, resolve all `scope_decision=review` rows, finalize source
   statuses, and set the batch to `submitted`. Only the primary reviewer may set
   `accepted`.

## Inclusion decisions

Include professors, associate or assistant professors, instructors, lecturers,
tutors, demonstrators, teaching assistants, and administrators whose teaching
is explicitly documented. Include preparatory, model-school, extension,
medical, or branch-unit instructors only when the unit belonged to the allocated
institution at the selected site; record the division.

Exclude trustees, honorary or inactive emeriti, research-only staff, students,
and purely administrative or operational employees unless the roster documents
instructional duties. Record uncertain cases as `review` while researching, but
resolve them to `include` or `exclude` before submission.

`found_roster_count` is the number of unique included normalized people in the
college-year, not the number of appointment lines. Count a person with multiple
listed roles once.

## Frozen output schema

Do not add, remove, or rename columns. Preserve this order.

```text
sources.csv
batch_id,research_agent,college_id,event_id,college,source_id,source_title,source_type,repository,source_url,local_path,file_sha256,academic_year_start,academic_year_end,roster_pages,access_date,extraction_method,source_status,notes

roster_entries.csv
batch_id,research_agent,roster_entry_id,college_id,event_id,college,academic_year_start,academic_year_label,person_name_raw,person_name_normalized,role_raw,discipline_raw,rank_normalized,discipline_normalized,division,appointment_status,scope_decision,teaching_evidence,source_id,source_page,confidence,raw_roster_line,transcription_method,page_visually_verified,verification_notes,notes

coverage.csv
batch_id,research_agent,college_id,event_id,college,academic_year_start,academic_year_label,source_ids,expected_roster_count,found_roster_count,roster_pages_reviewed,manual_verification_status,coverage_status,gap_reason,next_source_candidate,review_notes

manual_page_log.csv
batch_id,research_agent,page_review_id,college_id,event_id,college,academic_year_start,academic_year_label,source_id,source_page,page_title,visual_review_status,n_included_rows,n_excluded_rows,reviewer_notes,reviewed_at

identity_proposals.csv
batch_id,research_agent,proposal_id,college_id,person_name_normalized,local_person_key,candidate_faculty_id,link_scope,evidence,confidence,researcher_recommendation,primary_decision,decision_notes

batch_summary.csv
batch_id,research_agent,institutions_expected,institutions_completed,target_college_years,college_years_complete,college_years_likely_complete,college_years_partial,college_years_not_found,roster_entries,pages_visually_reviewed,rows_visually_verified,unresolved_questions,review_status,submitted_at,reviewed_at,review_notes
```

Use only these controlled values:

```text
source_status: manually_transcribed | source_located_inaccessible | not_roster_bearing | duplicate_source
coverage_status: complete | likely_complete | partial | source_found_not_processed | source_located_inaccessible | not_found
manual_verification_status: complete | partial | not_applicable
scope_decision: include | exclude | review
confidence: high | medium | low
transcription_method: manual_visual
page_visually_verified: TRUE
visual_review_status: complete
review_status: not_started | in_progress | submitted | needs_revision | accepted
```

Use `source_located_inaccessible` only when a specific source is identified but
cannot be opened or digitized. Use `not_found` only when no roster-bearing source
was located. An accessible roster awaiting transcription remains
`source_found_not_processed` and prevents submission.

## Submission checks

Before submission, verify that:

- every target institution-year has exactly one coverage row;
- every covered year cites an existing source and at least one visually reviewed
  roster page;
- every decided roster row has `transcription_method=manual_visual`,
  `page_visually_verified=TRUE`, a numeric page locator, teaching evidence when
  included, and nonempty verification notes;
- every manually transcribed source has page-log rows, and page-log included and
  excluded counts match the roster rows;
- normalized names and roles contain no OCR fragments, headers, vacancy notices,
  status lines, or degree fragments masquerading as people;
- coverage counts equal unique included normalized people; and
- all accessible sources are processed and all inclusion decisions are resolved.

The primary reviewer audits uncertain pages and a chronologically distributed
10 percent page sample per institution against the same cited source. This audit
checks transcription accuracy and does not require a second source.
