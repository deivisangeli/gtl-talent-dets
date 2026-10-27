# Read-only AMWS sources; outputs a seed-42 sample and reviewed inference pilot.
# First run with --prepare, review every row in amws_gender_name_review_100.csv,
# then rerun without --prepare. Never changes the existing AMWS panels.
suppressPackageStartupMessages(library(data.table))
script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
repo_root <- if (length(script_arg)) {
  normalizePath(file.path(dirname(sub("^--file=", "", script_arg[1])), "..", ".."),
                winslash = "/", mustWork = TRUE)
} else normalizePath(Sys.getenv("GTL_REPO", unset = getwd()), winslash = "/")
source(file.path(repo_root, "paths.R"))
stopifnot(dir.exists(AMWS_OUTPUT))
out_dir <- file.path(AMWS_OUTPUT, "gender_pilot")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
early_file <- file.path(AMWS_OUTPUT, "amws_combined_us_geocoded.csv")
late_file <- file.path(TALENT_DETS_DATA_DIR, "Data", "processed", "amws",
                       "amws_ed86_final.csv")
panel_file <- file.path(DATA_OUTPUT, "us_panel_county_amws_combined_year.csv")
inputs <- c(early_file, late_file, panel_file,
            file.path(AMWS_INPUT, c("amws_1906.xlsx", "amws_1938.dta", "amws_1955.dta")))
input_md5_before <- tools::md5sum(inputs)
normalize_geoid <- function(x) sprintf("%05d", suppressWarnings(as.integer(x)))
has_value <- function(x) !is.na(x) & trimws(as.character(x)) != "" &
  !toupper(trimws(as.character(x))) %in% c("NA", "N/A")
early <- fread(early_file)
early[, source_row := .I]
early <- early[as.character(kept) %in% c("TRUE", "True", "true", "1") &
                 has_value(birth_year) & has_value(geoid)]
early[, `:=`(year = suppressWarnings(as.integer(birth_year)),
             GEOID = normalize_geoid(geoid))]
early <- early[!is.na(year) & year >= 1800L & year <= 1960L & !is.na(GEOID)]
early <- early[, .(record_id = paste(edition, lineid, source_row, sep = ":"), edition,
                  source_row,
                  source_record_id = as.character(lineid), birth_year = year,
                  GEOID, original_name = paste(last, first, sep = ", "),
                  given_source = first)]
late <- fread(late_file)
late[, source_row := .I]
late[, `:=`(year = suppressWarnings(as.integer(birth_year)),
            GEOID = normalize_geoid(geo_geoid))]
text <- late$raw_text_adjusted
text[is.na(text)] <- ""
see_pos <- regexpr("\\bsee\\s+prev(?:ious)?(?:\\s+edition)?\\b", text,
                   ignore.case = TRUE, perl = TRUE)
birth_pos <- regexpr("(^|[[:space:],.;])b[[:space:]]+", text,
                     ignore.case = TRUE, perl = TRUE)
late[, contaminated := see_pos > 0L & (birth_pos < 0L | birth_pos > see_pos)]
late <- late[birth_country == "USA" & has_value(birth_year) & has_value(geo_geoid) &
               !is.na(year) & year >= 1800L & year <= 1960L & !is.na(GEOID) &
               !contaminated]
late <- late[, .(record_id = paste("1986", entry_id, sep = ":"), edition = 1986L,
                source_row,
                source_record_id = as.character(entry_id), birth_year = year,
                GEOID, original_name = name_raw,
                given_source = ifelse(grepl(",", name_raw),
                                      sub("^[^,]*,\\s*", "", name_raw), NA_character_))]
pool <- rbindlist(list(early, late))
panel <- fread(panel_file, select = c("GEOID", "year", "n_amws",
                                      "n_amws_1906_1955_dedup", "n_amws_1986"))
panel[, GEOID := normalize_geoid(GEOID)]
stopifnot(!anyDuplicated(panel[, .(GEOID, year)]), !anyDuplicated(pool$record_id))
pool <- pool[panel[, .(GEOID, birth_year = year)], on = .(GEOID, birth_year), nomatch = 0]
# Independently reconcile the individual sampling population to every panel cell.
counts <- pool[, .(older = sum(edition != 1986L), newer = sum(edition == 1986L)),
               by = .(GEOID, year = birth_year)]
check <- merge(panel, counts, by = c("GEOID", "year"), all.x = TRUE)
check[is.na(older), older := 0L]
check[is.na(newer), newer := 0L]
stopifnot(all(check$older == check$n_amws_1906_1955_dedup),
          all(check$newer == check$n_amws_1986),
          all(check$older + check$newer == check$n_amws),
          nrow(pool) == sum(panel$n_amws))
setorder(pool, edition, source_row)
RNGkind("Mersenne-Twister", "Inversion", "Rejection")
set.seed(42)
sample <- copy(pool[sample.int(nrow(pool), 100L, replace = FALSE)])
sample[, sample_id := .I]
setcolorder(sample, c("sample_id", setdiff(names(sample), "sample_id")))

# Fetch original biographies only for sampled older records.
sample[, source_text := NA_character_]
for (ed in c(1906L, 1938L, 1955L)) {
  ids <- sample[edition == ed, source_record_id]
  if (!length(ids)) next
  if (ed == 1906L) {
    raw <- as.data.table(readxl::read_excel(file.path(AMWS_INPUT, "amws_1906.xlsx"),
                                           col_names = FALSE))
    raw <- raw[, .(source_record_id = as.character(.SD[[1]]),
                   source_text = paste(.SD[[2]], .SD[[5]], .SD[[6]], sep = " | "))]
  } else {
    raw <- as.data.table(haven::read_dta(file.path(AMWS_INPUT,
                                                  paste0("amws_", ed, ".dta"))))
    raw <- if (ed == 1938L) raw[, .(source_record_id = as.character(lineid),
                                    source_text = paste(AMSname, a, sep = " | "))] else
      raw[, .(source_record_id = as.character(lineid), source_text = original)]
  }
  # Legacy raw 1955 line IDs can repeat; retain all candidate source texts.
  raw <- raw[, .(source_text = paste(unique(source_text), collapse = " || ")),
             by = source_record_id]
  idx <- which(sample$edition == ed)
  sample$source_text[idx] <- raw$source_text[match(sample$source_record_id[idx],
                                                 raw$source_record_id)]
}
late_raw <- fread(late_file, select = c("entry_id", "raw_text_adjusted"))
idx <- which(sample$edition == 1986L)
sample$source_text[idx] <- late_raw$raw_text_adjusted[
  match(sample$source_record_id[idx], as.character(late_raw$entry_id))]

extract_given <- function(x) {
  if (is.na(x) || !nzchar(trimws(x))) return(NA_character_)
  # Parenthesized letter expansions concatenate: W(illiam) -> William.
  x <- gsub("[()]", "", x)
  x <- trimws(gsub("^(?:(?:Dr|Prof|Rev|Gen|Mr|Mrs|Miss)\\.?\\s+)+", "", x,
                    ignore.case = TRUE, perl = TRUE))
  token <- strsplit(x, "\\s+")[[1]][1]
  token <- gsub("^[^[:alpha:]]+|[^[:alpha:]]+$", "", token)
  # Initials require source review, rather than substituting a middle name.
  if (nchar(gsub("[^[:alpha:]]", "", token)) < 2L) return(NA_character_)
  tolower(token)
}
sample[, first_name_auto := vapply(given_source, extract_given, character(1))]
# Some legacy Stata biography strings contain unmarked Latin-1 bytes.
# Normalize only invalid UTF-8 strings so the separate pilot CSV is readable.
char_cols <- names(sample)[vapply(sample, is.character, logical(1))]
sample[, (char_cols) := lapply(.SD, function(x) {
  bad <- !is.na(x) & !validUTF8(x)
  x[bad] <- iconv(x[bad], from = "latin1", to = "UTF-8", sub = "byte")
  x
}), .SDcols = char_cols]
sample_file <- file.path(out_dir, "amws_gender_sample_100.csv")
review_file <- file.path(out_dir, "amws_gender_name_review_100.csv")
fwrite(sample, sample_file)
if (!file.exists(review_file)) {
  review <- sample[, .(sample_id, record_id, original_name, first_name_auto,
                      first_name_reviewed = first_name_auto,
                      review_status = "pending", review_notes = "")]
  fwrite(review, review_file)
}
cat("SAMPLING POPULATION:", nrow(pool), "\n")
print(pool[, .N, by = edition][order(edition)])
cat("SAMPLED EDITIONS:\n")
print(sample[, .N, by = edition][order(edition)])
if ("--prepare" %in% commandArgs(TRUE)) {
  cat("Review every sampled name in:", review_file, "\n")
} else {
  stopifnot(requireNamespace("gender", quietly = TRUE),
            requireNamespace("genderdata", quietly = TRUE))
  review <- fread(review_file, na.strings = "")
  stopifnot(nrow(review) == 100L, !anyDuplicated(review$record_id),
            identical(review$record_id, sample$record_id),
            all(review$review_status %in% c("confirmed", "corrected", "unreadable")))
  result <- merge(sample, review[, .(record_id, first_name_reviewed, review_status,
                                     review_notes)], by = "record_id", sort = FALSE)
  setorder(result, sample_id)
  result[, `:=`(method = ifelse(birth_year < 1930L, "ipums", "ssa"),
                year_min = 10L * (birth_year %/% 10L),
                year_max = 10L * (birth_year %/% 10L) + 9L,
                proportion_male = NA_real_, proportion_female = NA_real_,
                package_prediction = NA_character_)]
  result[method == "ipums", `:=`(year_min = pmax(year_min, 1789L),
                                 year_max = pmin(year_max, 1930L))]
  result[method == "ssa", `:=`(year_min = pmax(year_min, 1880L),
                               year_max = pmin(year_max, 2012L))]
  for (i in seq_len(nrow(result))) {
    if (result$review_status[i] == "unreadable" ||
        !has_value(result$first_name_reviewed[i])) next
    pred <- gender::gender(result$first_name_reviewed[i],
                           years = c(result$year_min[i], result$year_max[i]),
                           method = result$method[i])
    if (!nrow(pred)) next
    stopifnot(nrow(pred) == 1L)
    result[i, `:=`(proportion_male = pred$proportion_male,
                   proportion_female = pred$proportion_female,
                   package_prediction = pred$gender)]
  }
  result[, reference_majority_proportion := pmax(proportion_male, proportion_female)]
  result[, classification := fifelse(
    review_status == "unreadable" | !has_value(first_name_reviewed), "unreadable",
    fifelse(is.na(reference_majority_proportion), "unmatched",
            fifelse(reference_majority_proportion < 0.95, "ambiguous",
                    fifelse(proportion_male > proportion_female, "male", "female"))))]
  result[, `:=`(seed = 42L, threshold = 0.95,
                gender_version = as.character(packageVersion("gender")),
                genderdata_version = as.character(packageVersion("genderdata")))]
  # Optional independent spot-checks are kept separate from package labels.
  result[, `:=`(validation_status = "not_checked", validated_gender = NA_character_,
                validation_source_url = NA_character_, validation_notes = NA_character_)]
  spotcheck_file <- file.path(out_dir, "amws_gender_pilot_spotcheck.csv")
  if (file.exists(spotcheck_file)) {
    audit <- fread(spotcheck_file)
    stopifnot(!anyDuplicated(audit$record_id),
              all(audit$record_id %in% result$record_id),
              all(audit$validated_gender %in% c("male", "female")))
    idx <- match(audit$record_id, result$record_id)
    result[idx, `:=`(validated_gender = audit$validated_gender,
                     validation_source_url = audit$validation_source_url,
                     validation_notes = audit$validation_notes)]
    result[idx, validation_status := fifelse(classification == validated_gender,
                                             "confirmed", "disagrees_with_prediction")]
  }
  classes <- c("male", "female", "ambiguous", "unmatched", "unreadable")
  summary <- data.table(classification = classes,
                        n = as.integer(table(factor(result$classification, levels = classes))))
  stopifnot(nrow(result) == 100L, uniqueN(result$record_id) == 100L,
            sum(summary$n) == 100L,
            all(is.na(result$proportion_male) |
                  abs(result$proportion_male + result$proportion_female - 1) < 1e-8),
            all(result[classification %in% c("male", "female"),
                       reference_majority_proportion] >= 0.95))
  fwrite(result, file.path(out_dir, "amws_gender_pilot_100.csv"))
  fwrite(summary, file.path(out_dir, "amws_gender_pilot_summary.csv"))
  input_md5_after <- tools::md5sum(inputs)
  stopifnot(identical(input_md5_before, input_md5_after))
  metadata <- data.table(path = inputs, md5 = unname(input_md5_after))
  fwrite(metadata, file.path(out_dir, "amws_gender_pilot_source_checksums.csv"))
  print(summary)
  cat("Wrote:", file.path(out_dir, "amws_gender_pilot_100.csv"), "\n")
}
