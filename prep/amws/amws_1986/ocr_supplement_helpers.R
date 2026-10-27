# Load reusable definitions, never the legacy scripts' executable workflows.
load_amws_ocr_helpers <- function(repo_root) {
  suppressPackageStartupMessages({
    library(dplyr); library(readr); library(stringr); library(tibble)
    library(stringdist); library(sf); library(data.table)
  })
  env <- new.env(parent = globalenv())
  sys.source(file.path(repo_root, "prep/amws/state_alias.R"), env)
  constants <- c("MONTH_RX", "DATE_RX", "OCR_NOV_RX", "state_or_region_tokens",
    "broad_place_tokens", "bad_place_markers", "month_regex", "section_regex",
    "field_regex", "COUNTRY_ALIAS", "CANADA_PROVINCE_ALIAS", "state_ocr_alias",
    "state_to_fp", "NAME_TOKEN", "GIVEN_TOKEN", "SUFFIX_RX", "ENTRY_START_BIRTH",
    "ENTRY_START_SEE_PREVIOUS", "ENTRY_START_NO_BIRTH_FIELD", "ENTRY_START_NO_COMMA_BIRTH",
    "DAMAGED_WITH_MONTH_AND_EDUC", "DAMAGED_WITH_MONTH", "DAMAGED_GLUE_BIRTH_MONTH",
    "DAMAGED_WITH_FIELD_AND_EDUC", "DAMAGED_POUND_EDUC", "DAMAGED_SEE_PREVIOUS",
    "ISOLATED_DAMAGED_NAME", "DASH_PROF_DAMAGED_START", "CONTINUATION_START", "NOISE_RX",
    "SAFE_EMBEDDED_PATTERNS")
  for (path in c("support/build_amws_1986_ed16_first10_regex_raw_xlsx.R",
                 "01_run_regex_all_docx.R", "02_build_birth_year_city.R",
                 "10_apply_piloted_regex_corrections_all_countries.R")) {
    for (expr in parse(file.path(repo_root, "prep/amws/amws_1986", path))) {
      if (!is.call(expr) || !identical(expr[[1]], as.name("<-")) || !is.symbol(expr[[2]])) next
      rhs <- expr[[3]]
      if ((is.call(rhs) && identical(rhs[[1]], as.name("function"))) ||
          as.character(expr[[2]]) %in% constants) eval(expr, env)
    }
  }
  env
}

amws_final_eligible <- function(d) {
  present <- function(x) !is.na(x) & nzchar(trimws(x)) & toupper(trimws(x)) != "NA"
  present(d$birth_city) & present(d$birth_year) & present(d$birth_country) &
    tolower(trimws(d$birth_country)) != "zo" &
    !(toupper(d$is_us_birth) == "TRUE" & toupper(d$is_us_geocoded) != "TRUE")
}

# Old IDs come from the already published final dataset, not current row positions.
amws_preserve_final_ids <- function(projected, previous) {
  stopifnot(!anyDuplicated(previous$entry_id))
  signature <- function(d) do.call(paste, c(d[setdiff(names(previous), "entry_id")], sep = "\r"))
  old <- signature(previous); new <- signature(projected)
  # Occurrence counters support distinct old entries whose exported values coincide.
  occurrence_key <- function(s) paste(s, ave(seq_along(s), s, FUN = seq_along), sep = "\r")
  idx <- match(occurrence_key(new), occurrence_key(old))
  stopifnot(all(seq_len(nrow(previous)) %in% idx))
  ids <- previous$entry_id[idx]
  fresh <- is.na(idx)
  ids[fresh] <- as.character(max(as.integer(previous$entry_id), 0L) + seq_len(sum(fresh)))
  projected$entry_id <- ids
  projected <- projected[c("entry_id", setdiff(names(projected), "entry_id"))]
  projected[order(as.integer(projected$entry_id)), , drop = FALSE]
}
