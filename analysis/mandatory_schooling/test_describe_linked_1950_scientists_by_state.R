# Small end-to-end DuckDB/Parquet fixture for the state descriptives.
# Run: Rscript analysis/mandatory_schooling/test_describe_linked_1950_scientists_by_state.R

file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
stopifnot(length(file_arg) == 1L)
script_dir <- dirname(normalizePath(sub("^--file=", "", file_arg[[1L]]),
                                    winslash = "/"))
source(file.path(script_dir, "describe_linked_1950_scientists_by_state.R"))

tmp <- tempfile("linked_1950_scientists_test_")
dir.create(tmp)
partition <- file.path(tmp, "birth_bpl=001")
dir.create(partition)
fixture <- data.frame(
  YEAR = c(1950, 1950, 1950, 1950, 1950, 1950, 1950, 1950, 1940),
  SEX = c(1, 1, 1, 1, 1, 2, 1, 1, 1),
  RACE = c(1, 1, 1, 1, 1, 1, 2, 1, 1),
  AGE = c(32, 44, 21, 35, 17, 40, 40, 40, 40),
  BPL = c(1, 1, 1, 2, 1, 1, 1, 1, 1),
  STATEFIP = c(1, 1, 1, 2, 1, 1, 1, 2, 1),
  OCC1950 = c(7, 68, 999, 999, 61, 61, 61, 61, 61),
  scientist_occ1950 = c(TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE)
)
con <- DBI::dbConnect(duckdb::duckdb())
DBI::dbWriteTable(con, "fixture", fixture)
invisible(DBI::dbExecute(con, sprintf("COPY fixture TO %s (FORMAT PARQUET)",
                                     as.character(DBI::dbQuoteString(
                                       con, sql_path(file.path(partition, "part.parquet")))))))
DBI::dbDisconnect(con, shutdown = TRUE)

states <- data.frame(state_bpl_code = c("001", "002"),
                     state_name = c("Alabama", "Alaska"), state_bpl = c(1L, 2L))
occupations <- data.frame(occ1950_code = sprintf("%03d", SCI_CODES),
                          occupation = paste("Occupation", SCI_CODES))
counts <- summarize_parquet(tmp, states, threads = 2L, memory_limit = "1GB")
tables <- build_tables(counts, states, occupations)
validation <- validate_tables(tables, states,
                              c(eligible_men = 4, scientists = 2))
stopifnot(
  nrow(tables$state) == 2L,
  nrow(tables$occupation) == 14L,
  identical(as.numeric(tables$state$eligible_men), c(3, 1)),
  identical(as.numeric(tables$state$scientists), c(2, 0)),
  tables$state$n_occ1950_007[[1L]] == 1,
  tables$state$n_occ1950_068[[1L]] == 1,
  tables$state$scientists_per_100k_men[[2L]] == 0,
  all(is.na(tables$occupation$occupation_share_state_pct[
    tables$occupation$state_bpl_code == "002"])),
  sum(tables$occupation$occupation_share_state_pct[
    tables$occupation$state_bpl_code == "001"]) == 100,
  validation$value[validation$metric == "scientists"] == 2
)
unlink(tmp, recursive = TRUE)
cat("State-scientist DuckDB fixture passed.\n")
