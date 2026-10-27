# Faculty counts, main college-allocation colleges, opening to 1950

This directory builds one number per college and benchmark year: the institution's
instructional staff. It covers the 20 colleges in the main college-allocation event
studies, which are the 16 standard-decade Main events plus the 4 that only the
alternative-timing sample adds (Alabama A&M, UNH, NC A&T, Northern Illinois).

The benchmark years are each college's `opening_or_transition_year` plus every
year ending in 0 or 5 up to 1950, for 307 college-years in all.

This dataset is separate from the name-level roster panel in `../faculty/`. That
panel transcribes every person every year and is still mostly unbuilt; this one
records counts only, compiled by web research.

## Sources

Counts come from these sources, in order of preference within a year:

1. *Report of the Commissioner of Education*, annual, 1870–1916. It has
   institution tables for universities and colleges, schools of science,
   normal schools, medical schools and institutions for the colored race.
2. *Biennial Survey of Education*, the chapters on universities, colleges and
   professional schools and on higher education, from 1918 on.
3. The land-grant statistics series: the Office of Experiment Stations series to
   about 1916, then the Bureau / Office of Education series.
4. ACE, *American Universities and Colleges*, editions from 1928 on.
5. The college's own catalogs and registers, where we counted the faculty list,
   and the complete or likely-complete legacy opening rosters in
   `faculty_longitudinal/legacy_opening_coverage.csv`.

## Rules

- **What counts.** Instructional staff at the selected site: professors of every
  rank, instructors, lecturers, tutors and teaching assistants. Where a source
  splits them, the preparatory and collegiate counts are kept as well.
- **Campuses.** UT Austin excludes Galveston (UTMB is event 25 in its own right),
  Cornell is Ithaca only, and Illinois is Urbana-Champaign only. A figure that
  combines campuses is kept at `confidence = low`.
- **Year.** `observed_year` is the start of the academic year, so 1889 for
  1889-90. A cell may use a source up to 2 years away; `status` and
  `year_offset` record when it does.
- **Choosing among sources.** The order is:
  1. a low-confidence row fills a cell only when nothing better exists;
  2. otherwise the nearest year wins;
  3. then the source family, in the order above;
  4. then confidence.

  Partial legacy rosters are known undercounts. They stay in the observations
  file but never fill a cell.
- **Missing is missing.** A cell with no source is `not_found` and has no count,
  never zero.

## Run

The research files go in `<DET_DIR>/output/land_grants/faculty_counts_main20_20261022/staging/`.
Then run these from the repository root, with `TALENT_DETS_DATA_DIR` set:

```powershell
Rscript prep/land_grants/faculty_counts/01_build_faculty_counts.R
Rscript prep/land_grants/faculty_counts/validate_faculty_counts.R
```

## Outputs

These are written to the same release directory:

- `faculty_count_observations.csv`: every sourced figure, with its URL, page,
  verbatim OCR line, definition and confidence.
- `faculty_count_panel.csv`: one row per college × benchmark year, giving the
  chosen count, `status` (`observed` / `substitute_within_2y` / `not_found`),
  its source, and the number and same-year range of competing figures.
- `coverage_summary.csv`: coverage per college.
- `validation.csv`: the hard checks. The validator stops if any fails.
- `review_flags.csv`: plausibility flags, which are not failures. They cover
  jumps over 2.5× or drops over 50% between benchmarks, same-year sources more
  than 1.5× apart, opening counts that disagree with the legacy roster, and
  low-confidence picks.
