# Andrews Faculty Longitudinal Panel

This directory contains the reproducible scaffolding for extending the Andrews
college-allocation faculty data from one opening/transition roster per college
to an annual instructional-faculty panel.

The target universe is the 57 selected colleges in
`andrews_founding_faculty_coverage.csv`, from each college's
`opening_or_transition_year` through academic year 1950-51. Institutions are
assigned to 19 fixed batches of exactly three colleges. Three research batches
can be active in parallel, but each writes only to its own staging directory.

The authoritative extraction method is manual visual transcription. OCR may
locate a roster section, but every retained line must be checked against its
rendered page. Researchers log every page in `manual_page_log.csv`; the primary
reviewer then audits all uncertain pages and a stratified 10 percent page sample
per institution before a batch can be accepted.

Run from the repository root with `TALENT_DETS_DATA_DIR` set:

```powershell
Rscript prep/land_grants/faculty/01_initialize_faculty_panel.R
Rscript prep/land_grants/faculty/03_prepare_legacy_seed.R
Rscript prep/land_grants/faculty/08_migrate_manual_verification_schema.R
Rscript prep/land_grants/faculty/validate_faculty_scaffold.R
Rscript prep/land_grants/faculty/02_validate_and_merge_faculty_batches.R
Rscript prep/land_grants/faculty/04_build_faculty_person_year.R

# After a researcher submits a batch
Rscript prep/land_grants/faculty/05_review_submitted_batch.R --batch=batch_01
Rscript prep/land_grants/faculty/07_prepare_primary_audit_sample.R --batch=batch_01
Rscript prep/land_grants/faculty/09_record_primary_page_audit.R --batch=batch_01 --audit-page-id=batch_01_page_0001 --result=pass --notes="Compared every listed row with the rendered source page."
Rscript prep/land_grants/faculty/06_set_batch_review_decision.R --batch=batch_01 --decision=needs_revision --notes="Describe required corrections"
```

The initializer writes to:

```text
<DET_DIR>/output/land_grants/faculty_longitudinal/
```

It is non-destructive: existing nonempty staging files are not overwritten.
The validator accepts only batches whose `batch_summary.csv` has
`review_status=accepted`. Canonical merged files are rebuilt solely from those
accepted staging files.

See `research_protocol.md` for inclusion, sourcing, and handoff rules.
