# IPUMS linked 1900–1950 census dataset

This guide describes the project’s birthplace-partitioned Parquet conversion of
`D:/usa_00003.csv.gz`. The extract covers the 1900, 1910, 1920, 1930, 1940, and
1950 U.S. censuses and includes the IPUMS/MLP linkage fields. The companion
`D:/usa_00003_ddi.xml` is the authority for source-variable types, descriptions,
and category labels. The reproducible conversion is implemented in
[`convert_ipums_linked_1950.R`](convert_ipums_linked_1950.R).

The default converted dataset is `D:/ipums_linked_1900_1950/`. Set
`IPUMS_LINKED_1950_PARQUET_DIR` when using another location. The Parquet data
remain outside the git repository; this Markdown file documents the existing
build rather than containing the data.

## Contents and layout

The conversion preserves all **60 source variables** with DDI-based classes
(54 integers, three one-decimal numeric fields, and three character fields) and
adds `birth_bpl` and `scientist_occ1950`. It writes ZSTD-compressed Parquet
under `birth_bpl=<code>/*.parquet`. The 2026-09-25 build has **146 Parquet files**,
occupying **19.69 GiB**. Counts below are census *rows*, not distinct
people: a linked person can appear in multiple census years.

| Census year | Rows | Rows with `scientist_occ1950 = TRUE` |
|---:|---:|---:|
| 1900 | 31,354,291 | 2,696 |
| 1910 | 49,849,396 | 8,227 |
| 1920 | 68,380,374 | 21,388 |
| 1930 | 89,184,374 | 36,076 |
| 1940 | 106,120,702 | 48,826 |
| 1950 | 118,881,111 | 86,622 |
| **All years** | **463,770,248** | **203,835** |

The year counts agree with `_manifest.json`. Scientist-row counts were queried
from the converted Parquet and reconciled to the converter’s total. These are
unrestricted extract counts, not population rates or counts of unique scientists.

## Birthplace partition and linkage

`birth_bpl` is normally a **three-character, 1950-anchored birthplace code**,
with `unknown` as a special value. It is not the
census row’s place of residence. For a 1950 row, it is that row’s `BPL`, even
when `HIK` is blank. For a pre-1950 row, it is the `BPL` of its unique 1950
`HIK` match with a recorded birthplace. A pre-1950 row without such a match is placed in
`birth_bpl=unknown`. Its original row-level `BPL` remains available. The
partitions include U.S. states, territories, foreign birthplaces, and
`unknown`. In this build, all rows in `unknown` are pre-1950 rows without a
unique 1950 anchor; the value does not mean that the original row’s birthplace
is unknown.

The build has **77,229,397 unique 1950 `HIK` anchors**. Of pre-1950 rows,
**171,024,163** (49.6%) lack a unique 1950 match and are in the `unknown`
partition. Another **41,651,714 1950 rows** have blank `HIK`; those rows are
still retained and partitioned by their own `BPL`. Linkage availability must
therefore be considered when comparing linked trajectories. Do not substitute
`birth_bpl` for `STATEFIP` (current residence) or for an earlier row’s
original `BPL` without intending the 1950-anchored definition.

## Scientist occupation flag

`scientist_occ1950` is a nullable Boolean derived from the **same row’s**
`OCC1950`, which expresses occupations on the 1950 classification basis across
the extract’s census years. The suffix `1950` refers to the occupation coding
scheme; the flag is not restricted to census year 1950.

| `OCC1950` code | DDI label |
|:---|:---|
| `007` | Chemists |
| `061` | Agricultural scientists |
| `062` | Biological scientists |
| `063` | Geologists and geophysicists |
| `067` | Mathematicians |
| `068` | Physicists |
| `069` | Misc. natural scientists |

The flag is `TRUE` for exactly these seven codes, `FALSE` for any other
recorded `OCC1950` code, and `NULL` if `OCC1950` is blank. Some other recorded
codes denote non-occupational or unknown responses, so `FALSE` does not by
itself establish employment in a different occupation. A `TRUE` value records
a scientist-coded *census-row occupation*, not an “ever a scientist” status for
the linked individual. The code labels above match the supplied DDI; `069` is
abbreviated “Misc.” there.

The separate [1950 birth-state scientist analysis](../../analysis/mandatory_schooling/describe_linked_1950_scientists_by_state.R)
restricts to white men aged 18 or older with `BPL = STATEFIP` and finds 35,399
scientist rows. That is a filtered subset of the 86,622 scientist rows in the
full 1950 extract. Residence in one’s birth state in 1950 does **not** prove
that the person never migrated and returned; `MIGRATE5` is unpopulated in
1950.

## Reading and provenance

Read Hive partitions with R and DuckDB. For example, this returns scientist
rows by original birthplace in the 1950 census:

```r
library(DBI)
library(duckdb)

root <- Sys.getenv("IPUMS_LINKED_1950_PARQUET_DIR",
                   unset = "D:/ipums_linked_1900_1950")
glob <- gsub("\\\\", "/", file.path(root, "birth_bpl=*", "*.parquet"))
con <- dbConnect(duckdb())
dbGetQuery(con, "
  SELECT BPL, count(*) AS scientist_rows
  FROM read_parquet(?, hive_partitioning = true)
  WHERE YEAR = 1950 AND scientist_occ1950 = TRUE
  GROUP BY BPL ORDER BY BPL
", params = list(glob))
dbDisconnect(con, shutdown = TRUE)
```

The converted directory also contains:

- `dictionary.md`: generated long-form descriptions of all source variables;
- `variable_dictionary.csv`: DDI labels, classes, and year-specific coverage;
- `value_labels.csv`: complete DDI code-to-label mapping;
- `validation_year_bpl.csv`: counts by census year and original `BPL`;
- `_manifest.json`: source/DDI provenance, build settings, row and partition
  counts, linkage counts, and validation results.

Metadata caveat: the generated `YEAR` nonblank and fill-percentage fields in
`variable_dictionary.csv` and `dictionary.md` are incorrect because `YEAR`
was used as both a grouping field and a coverage-column alias. Use
`_manifest.json` or the table above for year-level row counts. The Parquet
`YEAR` values themselves are unaffected.

The converter used DuckDB with **12 threads** and a **20 GB** memory limit.
Its validation matched source and Parquet counts, matched scientist-flag
counts, found zero partition mismatches, and confirmed the DDI columns and
seven scientist labels. Use the dictionary and label files for the full schema
instead of treating this guide as a replacement for the DDI.
