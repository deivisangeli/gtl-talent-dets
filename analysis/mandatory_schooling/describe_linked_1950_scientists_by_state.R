# State distribution of 1950 scientist occupations among adult white men
# residing in their birth state. Run from any working directory:
#   Rscript analysis/mandatory_schooling/describe_linked_1950_scientists_by_state.R

suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
})

SCI_CODES <- c(7L, 61L, 62L, 63L, 67L, 68L, 69L)
EXPECTED_TOTALS <- c(eligible_men = 23523317, scientists = 35399)

sql_path <- function(path) gsub("\\\\", "/", path)
fail_if <- function(condition, message) if (isTRUE(condition)) stop(message, call. = FALSE)

repo_root <- function() {
  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  candidate <- if (length(file_arg)) {
    dirname(normalizePath(sub("^--file=", "", file_arg[[1L]]), winslash = "/"))
  } else {
    Sys.getenv("GTL_REPO", unset = getwd())
  }
  while (!file.exists(file.path(candidate, "paths.R"))) {
    parent <- dirname(candidate)
    if (identical(parent, candidate)) stop("Could not locate paths.R", call. = FALSE)
    candidate <- parent
  }
  normalizePath(candidate, winslash = "/")
}

data_root <- function(root = repo_root()) {
  env <- new.env(parent = baseenv())
  sys.source(file.path(root, "paths.R"), envir = env)
  candidates <- c(Sys.getenv("TALENT_DETS_DATA_DIR"),
                  get("TALENT_DETS_DATA_DIR", envir = env),
                  Sys.getenv("DET_DIR"),
                  file.path("C:/Users", Sys.info()[["user"]],
                            "Globtalent Dropbox/gtl_talent_dets"))
  hit <- candidates[nzchar(candidates) & dir.exists(candidates)]
  fail_if(!length(hit), "Could not locate Dropbox data root; set TALENT_DETS_DATA_DIR")
  normalizePath(hit[[1L]], winslash = "/")
}

read_state_labels <- function(path) {
  fail_if(!file.exists(path), paste("Missing DDI-derived value labels:", path))
  labels <- utils::read.csv(path, colClasses = "character", check.names = FALSE)
  bpl <- labels[labels$name == "BPL" & grepl("^[0-9]{3}$", labels$code),
                c("code", "value_label")]
  bpl_numeric <- as.integer(bpl$code)
  states <- bpl[bpl_numeric >= 1L & bpl_numeric <= 56L, , drop = FALSE]
  fail_if(anyDuplicated(states$code) > 0L || anyDuplicated(states$value_label) > 0L,
          "Duplicate state codes or labels in DDI-derived dictionary")
  states$state_bpl <- as.integer(states$code)
  names(states) <- c("state_bpl_code", "state_name", "state_bpl")
  states <- states[order(states$state_bpl), , drop = FALSE]
  rownames(states) <- NULL
  states
}

read_occupation_labels <- function(path) {
  labels <- utils::read.csv(path, colClasses = "character", check.names = FALSE)
  selected <- labels[labels$name == "OCC1950" &
                       labels$code %in% sprintf("%03d", SCI_CODES),
                     c("code", "value_label")]
  selected <- selected[match(sprintf("%03d", SCI_CODES), selected$code), , drop = FALSE]
  fail_if(nrow(selected) != length(SCI_CODES) || anyNA(selected$code) ||
            anyNA(selected$value_label), "Missing scientist occupation labels")
  names(selected) <- c("occ1950_code", "occupation")
  rownames(selected) <- NULL
  selected
}

summarize_parquet <- function(parquet_dir, states, threads = 12L, memory_limit = "20GB") {
  fail_if(!dir.exists(parquet_dir), paste("Missing Parquet directory:", parquet_dir))
  files <- Sys.glob(file.path(parquet_dir, "birth_bpl=*", "*.parquet"))
  fail_if(!length(files), paste("No birthplace-partitioned Parquet files in:", parquet_dir))
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, sprintf("SET threads = %d", as.integer(threads)))
  DBI::dbExecute(con, sprintf("SET memory_limit = %s",
                           as.character(DBI::dbQuoteString(con, memory_limit))))
  parquet_glob <- sql_path(file.path(parquet_dir, "birth_bpl=*", "*.parquet"))
  codes <- paste(states$state_bpl, collapse = ", ")
  occ_counts <- paste(sprintf(
    "count(*) FILTER (WHERE OCC1950 = %d) AS n_occ1950_%03d", SCI_CODES, SCI_CODES),
    collapse = ",\n    ")
  query <- sprintf("SELECT BPL AS state_bpl, count(*) AS eligible_men,
    count(*) FILTER (WHERE scientist_occ1950 = TRUE) AS scientists,
    %s
    FROM read_parquet(%s, hive_partitioning = true)
    WHERE YEAR = 1950 AND SEX = 1 AND RACE = 1 AND AGE >= 18
      AND BPL IN (%s) AND STATEFIP = BPL
    GROUP BY BPL ORDER BY BPL", occ_counts,
    as.character(DBI::dbQuoteString(con, parquet_glob)), codes)
  DBI::dbGetQuery(con, query)
}

build_tables <- function(counts, states, occupations) {
  fail_if(anyDuplicated(counts$state_bpl) > 0L, "Duplicate states in DuckDB result")
  fail_if(!setequal(counts$state_bpl, states$state_bpl),
          "DuckDB state coverage differs from the DDI state/DC codes")
  counts <- counts[match(states$state_bpl, counts$state_bpl), , drop = FALSE]
  count_cols <- sprintf("n_occ1950_%03d", SCI_CODES)
  numeric_cols <- c("eligible_men", "scientists", count_cols)
  counts[numeric_cols] <- lapply(counts[numeric_cols], as.numeric)
  national_scientists <- sum(counts$scientists)
  fail_if(national_scientists <= 0, "No scientists in eligible sample")

  state <- data.frame(state_bpl_code = states$state_bpl_code,
                      state_name = states$state_name,
                      eligible_men = counts$eligible_men,
                      scientists = counts$scientists,
                      counts[count_cols],
                      scientists_per_100k_men = 1e5 * counts$scientists / counts$eligible_men,
                      share_us_scientists_pct = 100 * counts$scientists / national_scientists,
                      check.names = FALSE)
  occupation <- do.call(rbind, lapply(seq_along(SCI_CODES), function(i) {
    n <- counts[[count_cols[[i]]]]
    data.frame(state_bpl_code = states$state_bpl_code,
               state_name = states$state_name,
               occ1950_code = occupations$occ1950_code[[i]],
               occupation = occupations$occupation[[i]],
               occupation_count = n,
               state_scientists = counts$scientists,
               occupation_share_state_pct = ifelse(counts$scientists > 0,
                                                   100 * n / counts$scientists,
                                                   NA_real_),
               stringsAsFactors = FALSE)
  }))
  occupation <- occupation[order(occupation$state_bpl_code,
                                 match(occupation$occ1950_code,
                                       occupations$occ1950_code)), , drop = FALSE]
  rownames(occupation) <- NULL
  list(state = state, occupation = occupation)
}

validate_tables <- function(tables, states, expected_totals = NULL) {
  state <- tables$state
  occupation <- tables$occupation
  count_cols <- sprintf("n_occ1950_%03d", SCI_CODES)
  fail_if(nrow(state) != nrow(states) || nrow(occupation) != nrow(states) * length(SCI_CODES),
          "Unexpected state or state-occupation row count")
  fail_if(anyNA(state[, c("eligible_men", "scientists", count_cols)]) ||
            any(state$eligible_men <= 0) || any(state$scientists < 0),
          "Missing or invalid state counts")
  fail_if(any(rowSums(state[count_cols]) != state$scientists),
          "Scientist flag disagrees with the seven OCC1950 counts")
  fail_if(any(occupation$occupation_count < 0) ||
            any(occupation$occupation_count > occupation$state_scientists),
          "Invalid occupation counts")
  fail_if(abs(sum(state$share_us_scientists_pct) - 100) > 1e-8,
          "National scientist shares do not sum to 100%")
  fail_if(any(!is.na(occupation$occupation_share_state_pct[
    occupation$state_scientists == 0])),
    "Zero-scientist states must have undefined within-state occupation shares")
  if (!is.null(expected_totals)) {
    fail_if(sum(state$eligible_men) != expected_totals[["eligible_men"]] ||
              sum(state$scientists) != expected_totals[["scientists"]],
            "Totals differ from the verified 2026-09-25 extract snapshot")
  }
  data.frame(metric = c("state_dc_rows", "state_occupation_rows", "eligible_men",
                        "scientists", "national_share_pct", "zero_scientist_states"),
             value = c(nrow(state), nrow(occupation), sum(state$eligible_men),
                       sum(state$scientists), sum(state$share_us_scientists_pct),
                       sum(state$scientists == 0)),
             stringsAsFactors = FALSE)
}

main <- function() {
  parquet_dir <- Sys.getenv("IPUMS_LINKED_1950_PARQUET_DIR",
                            unset = "D:/ipums_linked_1900_1950")
  label_path <- file.path(parquet_dir, "value_labels.csv")
  states <- read_state_labels(label_path)
  fail_if(nrow(states) != 51L, "Expected 51 DDI state/DC birthplace codes")
  occupations <- read_occupation_labels(label_path)
  message("Scanning 1950 Parquet rows with DuckDB (12 threads, 20 GB limit)...")
  counts <- summarize_parquet(parquet_dir, states)
  tables <- build_tables(counts, states, occupations)
  validation <- validate_tables(tables, states, EXPECTED_TOTALS)

  out_dir <- file.path(data_root(), "results/mandatory_schooling/linked_1950_scientists")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(tables$state, file.path(out_dir, "state_summary.csv"),
                   row.names = FALSE, na = "")
  utils::write.csv(tables$occupation, file.path(out_dir, "state_occupation.csv"),
                   row.names = FALSE, na = "")
  notes <- c(
    "1950 scientist occupations by birth state: white men aged 18 or older",
    paste("Built:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    paste("Parquet source:", normalizePath(parquet_dir, winslash = "/")),
    paste("State and occupation labels:", normalizePath(label_path, winslash = "/")),
    "Population: YEAR=1950, SEX=1 (male), RACE=1 (white), AGE>=18,",
    "BPL in DDI U.S. state/DC codes 001-056, and STATEFIP=BPL.",
    "Residence in one's birth state in 1950 is not proof of never having migrated",
    "or never having crossed a state line. MIGRATE5 is unpopulated in 1950.",
    "Scientist = scientist_occ1950 TRUE, i.e. OCC1950 007,061,062,063,067,068,069.",
    "The indicator describes the 1950 occupation, not any lifetime occupation.",
    "Unweighted record counts. The rate denominator is all eligible adult men,",
    "including those with missing or non-scientist occupations.",
    "share_us_scientists_pct = 100 * state scientists / all-state scientists.",
    "occupation_share_state_pct = 100 * occupation count / state scientists;",
    "it is blank when the state has zero scientists. CSV code fields are identifiers.",
    "Alaska and Hawaii follow the extract's DDI geography codes; DC is included.",
    "Validation:",
    paste(sprintf("  %s: %s", validation$metric, format(validation$value,
                                                       scientific = FALSE, trim = TRUE)),
          collapse = "\n"))
  writeLines(notes, file.path(out_dir, "notes.txt"))
  message("Wrote ", out_dir)
  print(validation, row.names = FALSE)
}

if (sys.nframe() == 0L) main()
