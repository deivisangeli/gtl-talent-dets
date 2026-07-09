###############################################################################
# Pilot combined automatic corrections for AMWS Ed16 birth place and birth year.
#
# Reads:
#   output/amws/regex_all_docs/manual_audit_sample2000_birth_place_year/
#     sample2000_manual_audit_input.csv
#     outputs/batch_01_audit.csv ... outputs/batch_20_audit.csv
#
# Writes:
#   output/amws/regex_all_docs/manual_audit_sample2000_birth_place_year/
#     regex_rule_pilot_combined/
#       combined_regex_corrections_pilot_detail.csv
#       combined_regex_corrections_pilot_summary.csv
#
# This is a validation/pilot script. It does not overwrite canonical AMWS data.
###############################################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(tibble)
})

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_arg)) {
  script_path <- normalizePath(sub("^--file=", "", script_arg[1]),
                               winslash = "/", mustWork = TRUE)
  repo_root <- normalizePath(file.path(dirname(script_path), "..", "..", "..", ".."),
                             winslash = "/", mustWork = TRUE)
} else {
  repo_root <- normalizePath(Sys.getenv("GTL_REPO", unset = getwd()),
                             winslash = "/", mustWork = TRUE)
}

source(file.path(repo_root, "paths.R"))

local_dropbox <- file.path("C:/Users", Sys.info()[["user"]],
                           "Globtalent Dropbox", "gtl_talent_dets")
if (!dir.exists(TALENT_DETS_DATA_DIR) && dir.exists(local_dropbox)) {
  TALENT_DETS_DATA_DIR <- normalizePath(local_dropbox, winslash = "/")
  DATA_OUTPUT <- file.path(TALENT_DETS_DATA_DIR, "output")
}

env_chr <- function(name, default = "") {
  value <- Sys.getenv(name, unset = "")
  if (nzchar(value)) value else default
}

normalize_text <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x |>
    str_replace_all("\u00a0", " ") |>
    str_replace_all("\\s+", " ") |>
    str_trim()
}

blank_na <- function(x) {
  x <- normalize_text(x)
  ifelse(str_to_upper(x) == "NA", "", x)
}

is_true <- function(x) {
  str_detect(blank_na(x), regex("^true$", ignore_case = TRUE))
}

normalize_ocr_digits <- function(x) {
  chartr("OoIiLlSsZzBb|", "0011115522881", x)
}

day_from_token <- function(x) {
  x <- normalize_ocr_digits(blank_na(x)) |>
    str_replace_all("!", "1") |>
    str_replace_all(regex("u", ignore_case = TRUE), "11")
  if (!str_detect(x, "^[0-9]{1,2}$")) return(NA_integer_)
  day <- suppressWarnings(as.integer(x))
  if (is.na(day) || day < 1L || day > 31L) NA_integer_ else day
}

year_from_token <- function(x) {
  x <- normalize_ocr_digits(blank_na(x))
  if (!str_detect(x, "^([0-9]{2}|[0-9]{4})$")) return(NA_integer_)
  if (nchar(x) == 2L) {
    yy <- as.integer(x)
    year <- ifelse(yy <= 86L, 1900L + yy, 1800L + yy)
  } else {
    year <- as.integer(x)
  }
  if (is.na(year) || year < 1800L || year > 1986L) NA_integer_ else year
}

month_regex <- paste0(
  "(?:Jan(?:uary)?|Feb(?:ruary)?|Mar(?:ch)?|Apr(?:il)?|May|",
  "Jun(?:e)?|Jul(?:y)?|Aug|Aue|Sep(?:t)?|Sept|Oct|Oet|",
  "Nov|Nnv|Dec|Dee|Dcc|Mat|Mu)"
)

section_regex <- paste0(
  "(?:Educ|Fduc|Educl|Prof\\s*Exp|Prof\\s*tap|Mem|Honors|",
  "Concurrent\\s*Pos|Res:|Mailing\\s*Add|M[ae]m:|Research)"
)

field_regex <- paste0(
  "(?:PHYSICS|CHEMISTRY|BIOCHEMISTRY|BIOLOGY|ZOOLOGY|BOTANY|GENETICS|",
  "ECOLOGY|GEOLOGY|MATHEMATICS|STATISTICS|ENGINEERING|ASTRONOMY|",
  "PHARMACOLOGY|PHYSIOLOGY|PSYCHOLOGY|PSYCHIATRY|ENTOMOLOGY|",
  "VIROLOGY|MICROBIOLOGY|NEUROLOGY|ENDOCRINOLOGY|TOXICOLOGY|",
  "METALLURGY|GEOCHEMISTRY|OCEANOGRAPHY|COMPUTER\\s+SCIENCE|",
  "AERONAUTICAL|ASTRONAUTICAL|NUTRITION|MEDICINE|RADIOLOGY|",
  "SPECTROSCOPY|MECHANICS|FLUID\\s+MECHANICS|PUBLIC\\s+HEAI?L?TH|",
  "ANIMAL\\s+BEHAVIOR|INFECTIOUS\\s+DISEASE|MICROFLORA|ORGANIC|",
  "MAMMALOGY|FOREST\\s+PRODUCTS|ANALYSIS)"
)

state_ocr_pattern <- paste(
  c("\\b111\\b", "\\bIII\\b", "\\bHI\\b", "\\bI;", "Decatur\\. I\\b",
    "\\bDeL\\b", "\\bNMcx\\b", "c>l'f", "\\bCalir\\b", "\\bCahf\\b",
    "W'is", "\\bWig\\b", "\\bW o\\b", "\\u00b0r\\^", "v\\u00ae>",
    "\\bSDak\\b", "Washington, Po", "Pa>", "Philadelphia P\\.",
    "\\bVY\\b", "NS V", "W'Va", "\\bSVVa\\b", "Baton Rouge\\. U\\b",
    "St L\\* u\\.t M", "M\\u00bb", "Kirksville, Ind, MATHEMATICS"),
  collapse = "|"
)

first_cut <- function(x, patterns) {
  starts <- vapply(patterns, function(pat) {
    loc <- str_locate(x, regex(pat, ignore_case = TRUE))[1, 1]
    ifelse(is.na(loc) || loc < 4L, Inf, loc)
  }, numeric(1))
  if (all(is.infinite(starts))) return(list(pos = Inf, reason = ""))
  i <- which.min(starts)
  list(pos = starts[[i]], reason = names(patterns)[[i]])
}

proposed_trim_place <- function(place) {
  x <- blank_na(place)
  if (!nzchar(x)) return(list(place = "", reason = "no_birth_place"))

  x <- str_replace(x, regex("^birth_place\\s*=\\s*", ignore_case = TRUE), "")
  x <- str_split_fixed(x, regex("\\s*;\\s*city\\s*=", ignore_case = TRUE), 2)[, 1]
  x <- normalize_text(x)
  reasons <- character()

  patterns <- c(
    month = paste0("(?:[,.;\\s]+|^)\\b", month_regex, "\\b"),
    section = paste0("\\b", section_regex, "\\b"),
    field = paste0("(?:[,.;]\\s*|\\b\\d{1,4}\\s+|\\s{2,})", field_regex, "\\b")
  )

  for (i in seq_len(3L)) {
    cut <- first_cut(x, patterns)
    if (is.infinite(cut$pos)) break
    x_new <- str_sub(x, 1L, cut$pos - 1L) |>
      str_replace("[ ,.;:-]+$", "") |>
      normalize_text()
    if (identical(x_new, x)) break
    x <- x_new
    reasons <- c(reasons, cut$reason)
  }

  before_noise <- x
  x <- str_replace(
    x,
    regex("\\b(?:m\\s*\\d{2}|c\\s*\\d+|nat(?:l)?|citizen|US\\s+citizen|Can\\s+citizen)\\b.*$",
          ignore_case = TRUE),
    ""
  ) |>
    str_replace("[ ,.;:-]+$", "") |>
    normalize_text()
  if (!identical(x, before_noise)) reasons <- c(reasons, "demographic_noise")

  list(place = x, reason = ifelse(length(reasons), paste(reasons, collapse = "+"),
                                  "unchanged"))
}

audit_text <- function(row) {
  paste(row$raw_birth_evidence, row$parsed_birth_place, row$audit_note,
        collapse = " ")
}

trim_resolves_place <- function(audit_row, input_row, trimmed_place, trim_reason) {
  if (!is_true(audit_row$birth_place_wrong) ||
      trim_reason %in% c("unchanged", "no_birth_place")) {
    return(FALSE)
  }
  old_place <- blank_na(input_row$birth_place)
  if (!nzchar(trimmed_place) || identical(trimmed_place, old_place)) return(FALSE)

  target_problem <- str_detect(
    str_to_lower(audit_text(audit_row)),
    regex("field|educ|education|prof|campo|texto posterior|engol|swallowed|contamin|inclu[ií]u|reteve|data|date|citizen|mês|month")
  )
  bad_after <- str_detect(trimmed_place, regex(paste0("\\b", field_regex, "\\b"),
                                               ignore_case = TRUE)) |
    str_detect(trimmed_place, regex(paste0("\\b", section_regex, "\\b"),
                                    ignore_case = TRUE))
  token_n <- str_count(trimmed_place, "[A-Za-z0-9]+")
  compact <- nchar(trimmed_place) <= 60L && token_n <= 8L
  shorter <- nchar(trimmed_place) < nchar(old_place) * 0.8 || nchar(old_place) > 80L

  target_problem && nzchar(trimmed_place) && compact && !bad_after && shorter
}

ocr_resolves_place <- function(audit_row) {
  is_true(audit_row$birth_place_wrong) &&
    str_detect(audit_text(audit_row), regex(state_ocr_pattern))
}

birth_context <- function(input_row) {
  combo <- normalize_text(paste(blank_na(input_row$birth_place),
                                blank_na(input_row$birth_date)))
  raw <- blank_na(input_row$raw_text_adjusted)
  birth_match <- str_match(raw, regex("(?:^|[ ,.;])b\\s+(.{0,220})",
                                      ignore_case = TRUE))
  if (!is.na(birth_match[1, 2])) {
    seg <- birth_match[1, 2]
    section_loc <- str_locate(seg, regex(paste0("\\b", section_regex, "\\b"),
                                         ignore_case = TRUE))[1, 1]
    field_loc <- str_locate(seg, regex(paste0("\\b", field_regex, "\\b"),
                                       ignore_case = TRUE))[1, 1]
    cut_candidates <- c(section_loc, field_loc)
    cut_candidates <- cut_candidates[!is.na(cut_candidates)]
    cut <- if (length(cut_candidates)) min(cut_candidates) else Inf
    if (is.finite(cut) && cut > 4L) {
      seg <- str_sub(seg, 1L, cut - 1L)
    }
    if (nchar(seg) > nchar(combo)) combo <- seg
  }
  normalize_text(combo)
}

propose_year <- function(input_row) {
  current <- blank_na(input_row$birth_year)
  ctx <- birth_context(input_row)

  mdy <- str_match_all(
    ctx,
    regex(
      paste0("\\b(", month_regex, ")\\.?\\s*[,.;]?\\s*",
             "([0-9OoIiLlSsZzBb|!Uu]{1,4})\\s*[,.;_'’\\- ]+",
             "([0-9OoIiLlSsZzBb|]{2,4})\\b"),
      ignore_case = TRUE
    )
  )[[1]]
  if (nrow(mdy)) {
    for (i in seq_len(nrow(mdy))) {
      day <- day_from_token(mdy[i, 3])
      year <- year_from_token(mdy[i, 4])
      if (!is.na(day) && !is.na(year)) {
        return(list(year = as.character(year),
                    reason = "month_day_year_explicit",
                    context = ctx))
      }
    }
  }

  four_year <- str_match(ctx, "(?<!\\d)(18\\d{2}|19\\d{2})(?!\\d)")[, 2]
  if (!is.na(four_year)) {
    year <- suppressWarnings(as.integer(four_year))
    if (!is.na(year) && year >= 1800L && year <= 1986L) {
      return(list(year = as.character(year),
                  reason = "four_digit_year_in_birth_clause",
                  context = ctx))
    }
  }

  day_only <- str_match(
    ctx,
    regex(
      paste0("\\b(", month_regex, ")\\.?\\s*[,.;]?\\s*",
             "([0-9OoIiLlSsZzBb|]{1,2})\\b",
             "(?!\\s*[,.;_'’\\- ]+[0-9OoIiLlSsZzBb|]{2,4}\\b)"),
      ignore_case = TRUE
    )
  )
  if (!is.na(day_only[1, 2]) && nzchar(current)) {
    day <- day_from_token(day_only[1, 3])
    current_int <- suppressWarnings(as.integer(current))
    if (!is.na(day) && !is.na(current_int) && current_int %% 100L == day) {
      return(list(year = "", reason = "suppress_day_as_year", context = ctx))
    }
  }

  list(year = current, reason = "unchanged", context = ctx)
}

default_run_dir <- file.path(DATA_OUTPUT, "amws", "regex_all_docs",
                             "manual_audit_sample2000_birth_place_year")
run_dir <- env_chr("AMWS_ED16_COMBINED_RULE_PILOT_DIR", default_run_dir)
run_dir <- normalizePath(run_dir, winslash = "/", mustWork = TRUE)
audit_dir <- file.path(run_dir, "outputs")
out_dir <- file.path(run_dir, "regex_rule_pilot_combined")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

input_file <- file.path(run_dir, "sample2000_manual_audit_input.csv")
audit_files <- file.path(audit_dir, sprintf("batch_%02d_audit.csv", 1:20))
if (!file.exists(input_file)) stop("Missing input sample: ", input_file)
missing_audits <- audit_files[!file.exists(audit_files)]
if (length(missing_audits)) {
  stop("Missing audit files: ", paste(basename(missing_audits), collapse = ", "))
}

csv_text_cols <- cols(.default = col_character())
input <- read_csv(input_file, col_types = csv_text_cols, show_col_types = FALSE) |>
  mutate(across(everything(), ~ ifelse(is.na(.x), "", .x)))
audit <- bind_rows(lapply(audit_files, read_csv, col_types = csv_text_cols,
                          show_col_types = FALSE)) |>
  mutate(across(everything(), ~ ifelse(is.na(.x), "", .x)))

required_input <- c("sample_id", "birth_place", "birth_date", "birth_year",
                    "raw_text_adjusted")
required_audit <- c("sample_id", "birth_place_wrong", "birth_year_wrong",
                    "raw_birth_evidence", "parsed_birth_place", "audit_note")
missing_input <- setdiff(required_input, names(input))
missing_audit <- setdiff(required_audit, names(audit))
if (length(missing_input)) stop("Input missing columns: ",
                                paste(missing_input, collapse = ", "))
if (length(missing_audit)) stop("Audit missing columns: ",
                                paste(missing_audit, collapse = ", "))

joined <- audit |>
  left_join(input, by = "sample_id", suffix = c("_audit", "_input"))
if (any(is.na(joined$birth_place))) {
  stop("Some audited sample_id values were not found in input sample.")
}

detail <- bind_rows(lapply(seq_len(nrow(joined)), function(i) {
  row <- joined[i, ]
  trim <- proposed_trim_place(row$birth_place)
  yr <- propose_year(row)
  place_wrong <- is_true(row$birth_place_wrong)
  year_wrong <- is_true(row$birth_year_wrong)
  place_trim_corrected <- trim_resolves_place(row, row, trim$place, trim$reason)
  place_ocr_corrected <- ocr_resolves_place(row)
  place_corrected <- place_trim_corrected || place_ocr_corrected
  year_changed <- !identical(blank_na(row$birth_year), yr$year)
  year_corrected <- year_wrong && year_changed && yr$reason != "unchanged"
  year_worsened <- !year_wrong && year_changed && yr$reason != "unchanged"

  tibble(
    sample_id = row$sample_id,
    doc_id = row$doc_id_audit,
    lineid = row$lineid_audit,
    birth_place_wrong = place_wrong,
    birth_year_wrong = year_wrong,
    old_birth_place = row$birth_place,
    proposed_birth_place = trim$place,
    place_trim_reason = trim$reason,
    place_trim_corrected = place_trim_corrected,
    place_ocr_corrected = place_ocr_corrected,
    place_corrected = place_corrected,
    old_birth_year = blank_na(row$birth_year),
    proposed_birth_year = yr$year,
    birth_year_rule = yr$reason,
    birth_year_context = yr$context,
    birth_year_corrected = year_corrected,
    birth_year_worsened = year_worsened,
    either_wrong_before = place_wrong || year_wrong,
    either_wrong_after = (place_wrong && !place_corrected) ||
      (year_wrong && !year_corrected) || year_worsened,
    raw_birth_evidence = row$raw_birth_evidence,
    audit_note = row$audit_note
  )
}))

n <- nrow(detail)
place_before <- sum(detail$birth_place_wrong)
year_before <- sum(detail$birth_year_wrong)
either_before <- sum(detail$either_wrong_before)
place_corrected <- sum(detail$place_corrected)
year_corrected <- sum(detail$birth_year_corrected)
both_corrected <- sum(detail$place_corrected & detail$birth_year_corrected)
place_worsened <- 0L
year_worsened <- sum(detail$birth_year_worsened)
place_after <- sum(detail$birth_place_wrong & !detail$place_corrected) +
  place_worsened
year_after <- sum((detail$birth_year_wrong & !detail$birth_year_corrected) |
                    detail$birth_year_worsened)
either_after <- sum(detail$either_wrong_after)

summary <- bind_rows(
  tibble(metric = "sample_n", value = n),
  tibble(metric = "birth_place_wrong_before_n", value = place_before),
  tibble(metric = "birth_place_wrong_before_pct", value = 100 * place_before / n),
  tibble(metric = "birth_year_wrong_before_n", value = year_before),
  tibble(metric = "birth_year_wrong_before_pct", value = 100 * year_before / n),
  tibble(metric = "either_wrong_before_n", value = either_before),
  tibble(metric = "either_wrong_before_pct", value = 100 * either_before / n),
  tibble(metric = "birth_place_corrected_n", value = place_corrected),
  tibble(metric = "birth_year_corrected_n", value = year_corrected),
  tibble(metric = "both_place_and_year_corrected_n", value = both_corrected),
  tibble(metric = "birth_place_worsened_n", value = place_worsened),
  tibble(metric = "birth_year_worsened_n", value = year_worsened),
  tibble(metric = "birth_place_wrong_after_n", value = place_after),
  tibble(metric = "birth_place_wrong_after_pct", value = 100 * place_after / n),
  tibble(metric = "birth_year_wrong_after_n", value = year_after),
  tibble(metric = "birth_year_wrong_after_pct", value = 100 * year_after / n),
  tibble(metric = "either_wrong_after_n", value = either_after),
  tibble(metric = "either_wrong_after_pct", value = 100 * either_after / n),
  tibble(metric = "birth_place_error_reduction_pct",
         value = 100 * (place_before - place_after) / place_before),
  tibble(metric = "birth_year_error_reduction_pct",
         value = 100 * (year_before - year_after) / year_before),
  tibble(metric = "either_error_reduction_pct",
         value = 100 * (either_before - either_after) / either_before)
)

year_rule_summary <- detail |>
  filter(birth_year_corrected) |>
  count(birth_year_rule, name = "value") |>
  mutate(metric = paste0("birth_year_corrected_by_rule_", birth_year_rule)) |>
  select(metric, value)

place_rule_summary <- bind_rows(
  tibble(metric = "birth_place_corrected_by_trim_n",
         value = sum(detail$place_trim_corrected)),
  tibble(metric = "birth_place_corrected_by_state_ocr_n",
         value = sum(detail$place_ocr_corrected)),
  tibble(metric = "birth_place_corrected_by_both_trim_and_state_ocr_n",
         value = sum(detail$place_trim_corrected & detail$place_ocr_corrected))
)

summary <- bind_rows(summary, place_rule_summary, year_rule_summary) |>
  mutate(value = round(value, 4))

detail_file <- file.path(out_dir, "combined_regex_corrections_pilot_detail.csv")
summary_file <- file.path(out_dir, "combined_regex_corrections_pilot_summary.csv")
write_csv(detail, detail_file)
write_csv(summary, summary_file)

cat("Wrote detail:", detail_file, "\n")
cat("Wrote summary:", summary_file, "\n")
print(summary)
