# Linked 1950 sample-line respondents

`build_linked_1950_sample_line.R` creates a separate, birthplace-partitioned
Parquet dataset for respondents on the **1950 census sample line**. It enriches
the sample-line extract with the full census fields and derived fields from
`D:/ipums_linked_1900_1950/`. It does not change the existing linked extract or
any mandatory-schooling estimate.

Run from the repository root:

```powershell
Rscript prep/mandatory_schooling/test_build_linked_1950_sample_line.R
Rscript prep/mandatory_schooling/build_linked_1950_sample_line.R
```

The defaults are the supplied
`Data/raw/mandatory_schooling/sample_line_linked_1940_1950_census.gz`,
`D:/ipums_linked_1900_1950/`, and the new output
`D:/ipums_linked_1950_sample_line/`. Override them with
`IPUMS_1950_SAMPLE_LINE_SOURCE`, `IPUMS_LINKED_1950_PARQUET_DIR`, and
`IPUMS_1950_SAMPLE_LINE_PARQUET_DIR`. The output directory must not exist before
the build. `IPUMS_1950_SAMPLE_LINE_EXPECTED_COUNT` changes the expected 1950
source count for synthetic fixtures; the production default is **15,337,444**.

## Selection and linkage

The source has 15,337,444 sample-line rows from 1950 and 3,690,810 from 1940.
The dataset selects **all 1950 source rows**, matching each to the broader
Parquet by `YEAR = 1950` and exact `HISTID`. It then looks up a 1940 row by
`HIK` when the 1950 row has `LINK1940 = 1` and the 1940 row has
`LINK1950 = 1`. Each person has at most one output row per year. A 1950
respondent without a valid reciprocal 1940 row remains in the output.

The 1940 sample-line source does **not** define cohort membership. It supplies
four row-specific fields when a selected person's linked 1940 row also appears
there. Otherwise those fields are null on the 1940 row. The 1950 `SLWT` is not
copied to the 1940 row; analysts joining the two years should take that weight
from the 1950 record explicitly.

All 62 broader-Parquet fields are preserved, including `birth_bpl` and
`scientist_occ1950`. The four added fields are `SLWT` (numeric sample-line
weight), `SLREC` (sample-line code), `VETWWI`, and `VETWWII` (coded veteran
responses). See `field_dictionary.csv` in the output for field descriptions.
The broader extract's DDI supplies descriptions of inherited fields; the
sample-line gzip has no companion DDI in the supplied directory, so the four
added fields retain their raw codes without new value-label claims.

## Output and checks

The output uses ZSTD Parquet at `birth_bpl=<code>/*.parquet` and includes
`_manifest.json`, `field_dictionary.csv`, `source_year_counts.csv`, and
`validation_year_bpl.csv`. `birth_bpl` is inherited from the broader dataset:
1950 rows use their own birthplace and linked 1940 rows use the 1950 anchor.
The original row-level `BPL` remains available.

The converter validates source counts and unique IDs, equality of shared
fields on sample-line/base matches, one-to-one reciprocal 1940 links, unique
output IDs, and output birthplace/scientist counts against the selected base
rows. It writes to a staging directory and promotes that directory only after
validation. The manifest records matched and missing 1940 links, year counts,
and source provenance.

The 2026-09-29 build passed these checks. It contains **15,337,444 1950 rows**
and **14,633,823 linked 1940 rows**. All 14,633,823 respondents flagged for a
1940 link had one reciprocal row; the other 703,621 respondents remain as
1950-only records. Of the linked 1940 rows, 732,803 also have their own 1940
sample-line fields. The 140 Parquet files occupy about 1.63 GB.

Read the dataset with DuckDB, for example:

```sql
SELECT YEAR, COUNT(*) AS rows, COUNT(*) FILTER (WHERE SLREC = 2) AS sample_line_rows
FROM read_parquet('D:/ipums_linked_1950_sample_line/birth_bpl=*/*.parquet',
                  hive_partitioning = true)
GROUP BY YEAR ORDER BY YEAR;
```
