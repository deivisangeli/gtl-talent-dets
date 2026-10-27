# Gender inference for the records counted by the current combined AMWS panel.
# Run from the repo root: Rscript prep/amws/build_amws_gender_inference.R
# Uses installed gender/genderdata reference data; no remote name lookups.
suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
})
stopifnot(requireNamespace("gender", quietly = TRUE),
          requireNamespace("genderdata", quietly = TRUE))
script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
repo_root <- if (length(script_arg)) {
  normalizePath(file.path(dirname(sub("^--file=", "", script_arg[1])), "..", ".."),
                winslash = "/", mustWork = TRUE)
} else normalizePath(Sys.getenv("GTL_REPO", unset = getwd()), winslash = "/")
source(file.path(repo_root, "paths.R"))
if (!dir.exists(TALENT_DETS_DATA_DIR) && !nzchar(Sys.getenv("TALENT_DETS_DATA_DIR"))) {
  TALENT_DETS_DATA_DIR <- file.path("C:/Users", Sys.info()[["user"]],
                                   "Globtalent Dropbox", "gtl_talent_dets")
  DATA_INPUT <- file.path(TALENT_DETS_DATA_DIR, "input")
  DATA_OUTPUT <- file.path(TALENT_DETS_DATA_DIR, "output")
  AMWS_INPUT <- file.path(DATA_INPUT, "amws")
  AMWS_OUTPUT <- file.path(DATA_OUTPUT, "amws")
}
stopifnot(dir.exists(AMWS_OUTPUT))

threshold <- 0.95
normalize_geoid <- function(x) sprintf("%05d", suppressWarnings(as.integer(x)))
has_value <- function(x) !is.na(x) & trimws(as.character(x)) != "" &
  !toupper(trimws(as.character(x))) %in% c("NA", "N/A")
utf8 <- function(x) {
  bad <- !is.na(x) & !validUTF8(x)
  x[bad] <- iconv(x[bad], from = "latin1", to = "UTF-8", sub = "byte")
  x
}

# For 1986, only a visible surname separator supplies an automatic boundary.
# An unpunctuated header is not assumed to contain a single-token surname.
header_given <- function(x) {
  if (!has_value(x)) return(list(given_source = NA_character_, boundary = "missing_name"))
  if (grepl(",", x, fixed = TRUE)) {
    return(list(given_source = sub("^[^,]*,\\s*", "", x), boundary = "surname_comma"))
  }
  period_pattern <- "^[[:alpha:]][[:alpha:] '-]+\\.\\s+(.+)$"
  if (grepl(period_pattern, x, perl = TRUE)) {
    return(list(given_source = sub(period_pattern, "\\1", x, perl = TRUE),
                boundary = "surname_period"))
  }
  list(given_source = NA_character_, boundary = "uncertain_name_boundary")
}

extract_given <- function(x) {
  fail <- function(reason) list(first_name_auto = NA_character_, extraction_status = reason)
  if (!has_value(x)) return(fail("missing_given_name"))
  x <- trimws(gsub("\\s+", " ", x))
  x <- trimws(gsub("^(?:(?:Dr|Prof|Professor|Rev|Reverend|Gen|Mr|Mrs|Miss|Capt|Col|Lt)\\.?\\s+)+",
                    "", x, ignore.case = TRUE, perl = TRUE))
  # Dean may be a given name or a title: require review to distinguish it.
  if (grepl("^Dean\\b", x, ignore.case = TRUE)) return(fail("ambiguous_title"))
  if (!nzchar(x)) return(fail("missing_given_name"))
  token <- strsplit(x, "\\s+")[[1L]][1L]
  # Permit a leading expanded initial such as S(amuel), not damaged brackets.
  if (grepl("[()]", token) &&
      !grepl("^[[:alpha:]]+\\([[:alpha:]]+\\)[,.;]?$", token)) {
    return(fail("corrupted_given_name"))
  }
  token <- gsub("[()]", "", token)
  token <- gsub("^[,.;]+|[,.;]+$", "", token)
  if (grepl("^[[:alpha:]]$", token)) return(fail("initial_only"))
  if (tolower(token) %in% c("jr", "sr", "ii", "iii", "iv", "v", "prof", "dr")) {
    return(fail("uncertain_given_name"))
  }
  if (!grepl("^[[:alpha:]]{2,}(?:[-'][[:alpha:]]{2,})*$", token, perl = TRUE)) {
    return(fail("corrupted_given_name"))
  }
  list(first_name_auto = tolower(token), extraction_status = "automatic")
}

classify <- function(first_name, male, female) {
  majority <- pmax(male, female)
  fcase(!has_value(first_name), "unreadable",
        is.na(majority), "unmatched",
        majority < threshold, "ambiguous",
        male > female, "male", default = "female")
}

# Focused checks for extraction and classification boundaries.
stopifnot(extract_given("Prof. S(amuel) F(rank)")$first_name_auto == "samuel",
          extract_given("DR. FRANK E(ARL)")$first_name_auto == "frank",
          extract_given("G(ORONWY) O(WEN)")$first_name_auto == "goronwy",
          header_given("TAYLOR. HAROLD NATHANIEL")$given_source == "HAROLD NATHANIEL",
          header_given("VAN SMITH. ROBERT")$given_source == "ROBERT",
          is.na(header_given("CLARK LINCOLN DUFTON")$given_source),
          is.na(extract_given("R E KEITH")$first_name_auto),
          is.na(extract_given("DEAN STANLEY EDWIN")$first_name_auto),
          is.na(extract_given("FR4NK")$first_name_auto),
          is.na(extract_given("S(AMUEL")$first_name_auto),
          identical(classify(c("a", "b", "c", "d", NA_character_),
                            c(.95, .9499, .05, NA, .99),
                            c(.05, .0501, .95, NA, .01)),
                    c("male", "ambiguous", "female", "unmatched", "unreadable")))

early_file <- file.path(AMWS_OUTPUT, "amws_combined_us_geocoded.csv")
late_file <- file.path(TALENT_DETS_DATA_DIR, "Data", "processed", "amws", "amws_ed86_final.csv")
panel_file <- file.path(DATA_OUTPUT, "us_panel_county_amws_combined_year.csv")
pilot_dir <- file.path(AMWS_OUTPUT, "gender_pilot")
review_file <- file.path(pilot_dir, "amws_gender_name_review_100.csv")
pilot_file <- file.path(pilot_dir, "amws_gender_pilot_100.csv")
audit_file <- file.path(pilot_dir, "amws_gender_pilot_spotcheck.csv")
inputs <- unique(c(early_file, late_file, panel_file,
                   file.path(AMWS_INPUT, c("amws_1906.xlsx", "amws_1938.dta", "amws_1955.dta")),
                   list.files(pilot_dir, full.names = TRUE)))
stopifnot(all(file.exists(inputs)), file.exists(review_file), file.exists(pilot_file),
          file.exists(audit_file))
md5_before <- tools::md5sum(inputs)

# Reproduce the current panel's record eligibility, without further deduplication.
early <- fread(early_file, encoding = "UTF-8")
early[, source_row := .I]
early <- early[as.character(kept) %in% c("TRUE", "True", "true", "1") &
                 has_value(birth_year) & has_value(geoid)]
early[, `:=`(year = suppressWarnings(as.integer(birth_year)), GEOID = normalize_geoid(geoid))]
early <- early[!is.na(year) & year >= 1800L & year <= 1960L & !is.na(GEOID)]
early <- early[, .(record_id = paste(edition, lineid, source_row, sep = ":"), edition,
                  source_row, source_record_id = as.character(lineid), birth_year = year,
                  GEOID, original_name = paste(last, first, sep = ", "),
                  given_source = first, name_boundary = "legacy_given_name_field")]
late <- fread(late_file, encoding = "UTF-8")
late[, source_row := .I]
late[, `:=`(year = suppressWarnings(as.integer(birth_year)), GEOID = normalize_geoid(geo_geoid))]
raw <- late$raw_text_adjusted
raw[is.na(raw)] <- ""
see_pos <- regexpr("\\bsee\\s+prev(?:ious)?(?:\\s+edition)?\\b", raw,
                   ignore.case = TRUE, perl = TRUE)
birth_pos <- regexpr("(^|[[:space:],.;])b[[:space:]]+", raw,
                     ignore.case = TRUE, perl = TRUE)
late[, contaminated := see_pos > 0L & (birth_pos < 0L | birth_pos > see_pos)]
late <- late[birth_country == "USA" & has_value(birth_year) & has_value(geo_geoid) &
               !is.na(year) & year >= 1800L & year <= 1960L & !is.na(GEOID) & !contaminated]
late <- late[, .(record_id = paste("1986", entry_id, sep = ":"), edition = 1986L,
                source_row, source_record_id = as.character(entry_id), birth_year = year,
                GEOID, original_name = name_raw)]
given <- rbindlist(lapply(late$original_name, header_given))
late[, `:=`(given_source = given$given_source, name_boundary = given$boundary)]
pool <- rbindlist(list(early, late), use.names = TRUE)
panel <- fread(panel_file, select = c("GEOID", "year", "n_amws",
                                      "n_amws_1906_1955_dedup", "n_amws_1986"))
panel[, GEOID := normalize_geoid(GEOID)]
stopifnot(!anyDuplicated(panel[, .(GEOID, year)]), !anyDuplicated(pool$record_id))
pool <- pool[panel[, .(GEOID, birth_year = year)], on = .(GEOID, birth_year), nomatch = 0L]
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
char_cols <- names(pool)[vapply(pool, is.character, logical(1))]
pool[, (char_cols) := lapply(.SD, utf8), .SDcols = char_cols]
extracted <- rbindlist(lapply(pool$given_source, extract_given))
pool[, `:=`(first_name_auto = extracted$first_name_auto,
             first_name = extracted$first_name_auto,
             extraction_status = extracted$extraction_status,
             extraction_notes = NA_character_)]
pool[name_boundary %in% c("uncertain_name_boundary", "missing_name"),
     extraction_status := name_boundary]

review <- fread(review_file, na.strings = "", encoding = "UTF-8")
stopifnot(nrow(review) == 100L, !anyDuplicated(review$record_id),
          all(review$review_status %in% c("confirmed", "corrected", "unreadable")))
idx <- match(review$record_id, pool$record_id)
stopifnot(!anyNA(idx), all(pool$original_name[idx] == review$original_name))
pool[idx, `:=`(first_name = review$first_name_reviewed,
               extraction_status = paste0("pilot_", review$review_status),
               extraction_notes = review$review_notes)]
pool[idx[review$review_status == "unreadable"], first_name := NA_character_]
pool[, `:=`(method = fifelse(birth_year < 1930L, "ipums", "ssa"),
             year_min = 10L * (birth_year %/% 10L),
             year_max = 10L * (birth_year %/% 10L) + 9L)]
pool[method == "ipums", `:=`(year_min = pmax(year_min, 1789L), year_max = pmin(year_max, 1930L))]
pool[method == "ssa", `:=`(year_min = pmax(year_min, 1880L), year_max = pmin(year_max, 2012L))]

queries <- unique(pool[has_value(first_name), .(first_name, method, year_min, year_max)])
setorder(queries, method, year_min, first_name)
windows <- unique(queries[, .(method, year_min, year_max)])
predictions <- vector("list", nrow(windows))
for (i in seq_len(nrow(windows))) {
  w <- windows[i]
  names <- queries[method == w$method & year_min == w$year_min & year_max == w$year_max,
                   first_name]
  cat(sprintf("Reference batch %d/%d: %s %d-%d, %d names\n", i, nrow(windows),
              w$method, w$year_min, w$year_max, length(names)))
  p <- as.data.table(gender::gender(names, years = c(w$year_min, w$year_max), method = w$method))
  if (!nrow(p)) next
  setnames(p, c("name", "gender"), c("first_name", "package_prediction"))
  p[, method := w$method]
  predictions[[i]] <- p
}
predicted <- rbindlist(predictions, use.names = TRUE)
key <- c("first_name", "method", "year_min", "year_max")
stopifnot(!anyDuplicated(predicted[, ..key]))
lookup <- merge(queries, predicted, by = key, all.x = TRUE, sort = FALSE)
pool <- merge(pool, lookup, by = key, all.x = TRUE, sort = FALSE)
setorder(pool, edition, source_row)
pool[, `:=`(reference_majority_proportion = pmax(proportion_male, proportion_female),
             classification_raw = classify(first_name, proportion_male, proportion_female))]

# The full batched run must reproduce the manually reviewed pilot predictions.
pilot <- fread(pilot_file, encoding = "UTF-8")
idx <- match(pilot$record_id, pool$record_id)
same_number <- function(a, b) all((is.na(a) & is.na(b)) |
                                   (!is.na(a) & !is.na(b) & abs(a - b) < 1e-12))
stopifnot(length(idx) == 100L, !anyNA(idx),
          all(pool$classification_raw[idx] == pilot$classification),
          same_number(pool$proportion_male[idx], pilot$proportion_male),
          same_number(pool$proportion_female[idx], pilot$proportion_female))

pool[, `:=`(classification_final = classification_raw,
             classification_source = fifelse(classification_raw %in% c("male", "female"),
                                               "name_reference", "unresolved"),
             validated_gender = NA_character_, validation_source_url = NA_character_,
             validation_notes = NA_character_)]
audit <- fread(audit_file, encoding = "UTF-8")
# Only the already documented identity-specific correction is authorized here.
stopifnot(nrow(audit) == 1L, audit$record_id == "1906:19:17", audit$validated_gender == "male")
idx <- match(audit$record_id, pool$record_id)
stopifnot(!anyNA(idx), pool$birth_year[idx] == 1868L,
          pool$original_name[idx] == "Adams, Prof. C(omfort) A(very)",
          pool$classification_raw[idx] == "female")
pool[idx, `:=`(classification_final = audit$validated_gender,
               classification_source = "verified_override",
               validated_gender = audit$validated_gender,
               validation_source_url = audit$validation_source_url,
               validation_notes = audit$validation_notes)]
stopifnot(sum(pool$classification_raw != pool$classification_final) == 1L)
pool[, `:=`(threshold = threshold, gender_version = as.character(packageVersion("gender")),
             genderdata_version = as.character(packageVersion("genderdata")))]

summarize_counts <- function(x, grouping = NULL) {
  out <- x[, .(men = sum(classification_final == "male"),
                women = sum(classification_final == "female"),
                ambiguous = sum(classification_final == "ambiguous"),
                unmatched = sum(classification_final == "unmatched"),
                unreadable = sum(classification_final == "unreadable"),
                total_records = .N), by = grouping]
  out[, `:=`(classified = men + women, unresolved = ambiguous + unmatched + unreadable)]
  out[, coverage := fifelse(total_records > 0L, classified / total_records, NA_real_)]
  stopifnot(all(out$classified + out$unresolved == out$total_records))
  out
}
overall <- summarize_counts(pool)
annual <- merge(data.table(birth_year = 1800L:1960L), summarize_counts(pool, "birth_year"),
                by = "birth_year", all.x = TRUE, sort = TRUE)
count_cols <- c("men", "women", "ambiguous", "unmatched", "unreadable", "total_records",
                "classified", "unresolved")
for (nm in count_cols) set(annual, which(is.na(annual[[nm]])), nm, 0L)
annual[, coverage := fifelse(total_records > 0L, classified / total_records, NA_real_)]
by_edition <- summarize_counts(pool, "edition")
setorder(by_edition, edition)
stopifnot(nrow(pool) == sum(panel$n_amws), uniqueN(pool$record_id) == nrow(pool),
          nrow(annual) == 161L, identical(annual$birth_year, 1800L:1960L),
          all(annual$classified + annual$unresolved == annual$total_records),
          all(colSums(annual[, ..count_cols]) == unlist(overall[, ..count_cols])),
          all(colSums(by_edition[, ..count_cols]) == unlist(overall[, ..count_cols])),
          all(is.na(pool$proportion_male) |
                abs(pool$proportion_male + pool$proportion_female - 1) < 1e-8),
          all(pool[classification_raw %in% c("male", "female"),
                   reference_majority_proportion] >= threshold))

# A release refresh must reproduce saved decisions for unchanged biographies.
# An explicit frozen baseline avoids comparing against a partially refreshed file.
baseline_file <- Sys.getenv("AMWS_GENDER_BASELINE", unset = "")
if (nzchar(baseline_file)) {
  baseline <- as.data.table(read.csv(baseline_file, colClasses = "character",
    na.strings = "", check.names = FALSE, encoding = "UTF-8"))
  idx <- match(baseline$record_id, pool$record_id)
  stopifnot(!anyNA(idx), !anyDuplicated(baseline$record_id))
  preserved <- c("source_record_id", "source_row", "GEOID", "birth_year",
    "original_name", "first_name", "classification_raw", "classification_final",
    "classification_source", "validated_gender", "validation_source_url", "validation_notes")
  for (col in preserved) {
    old <- baseline[[col]]; new <- as.character(pool[[col]][idx])
    # CSV empty fields read back as NA; compare their serialized meaning.
    new[!is.na(new) & new == ""] <- NA_character_
    same <- (is.na(old) & is.na(new)) | (!is.na(old) & !is.na(new) & old == new)
    if (any(!same)) {
      print(data.table(record_id = baseline$record_id[!same], column = col,
                       old = old[!same], new = new[!same])[1:min(10L, sum(!same))])
      stop("Baseline gender preservation failed: ", col)
    }
  }
  for (col in c("proportion_male", "proportion_female")) {
    stopifnot(same_number(as.numeric(baseline[[col]]), pool[[col]][idx]))
  }
  cat("Preserved", nrow(baseline), "baseline decisions; added", nrow(pool) - nrow(baseline), "records.\n")
}

out_dir <- file.path(AMWS_OUTPUT, "gender_inference")
figure_dir <- file.path(out_dir, "figures")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
setcolorder(pool, c("record_id", "edition", "source_record_id", "source_row", "GEOID",
                   "birth_year", "original_name", "given_source", "name_boundary",
                   "first_name_auto", "first_name", "extraction_status", "extraction_notes",
                   "method", "year_min", "year_max", "proportion_male", "proportion_female",
                   "reference_majority_proportion", "package_prediction", "classification_raw",
                   "classification_final", "classification_source", "validated_gender",
                   "validation_source_url", "validation_notes", "threshold", "gender_version",
                   "genderdata_version"))
fwrite(pool, file.path(out_dir, "amws_gender_records.csv"))
fwrite(lookup, file.path(out_dir, "amws_gender_name_reference.csv"))
fwrite(overall, file.path(out_dir, "amws_gender_overall_counts.csv"))
fwrite(annual, file.path(out_dir, "amws_gender_birth_year_counts.csv"))
fwrite(by_edition, file.path(out_dir, "amws_gender_edition_coverage.csv"))

colors <- c("Men" = "#2E5266", "Women" = "#A74482", "Unresolved" = "#898F99")
count_labels <- function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
base_theme <- theme_minimal(base_size = 11, base_family = "Arial") +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 11, margin = margin(b = 15)),
        plot.caption = element_text(size = 9, hjust = 0, lineheight = 1.2,
                                    margin = margin(t = 15)),
        plot.margin = margin(18, 22, 18, 18), legend.position = "none")
total_plot_data <- data.table(group = factor(c("Men", "Women", "Unresolved"),
                                             levels = c("Men", "Women", "Unresolved")),
                              n = c(overall$men, overall$women, overall$unresolved))
total_plot <- ggplot(total_plot_data, aes(group, n, fill = group)) +
  geom_col(width = .6) + geom_text(aes(label = count_labels(n)), vjust = -.65, size = 4) +
  scale_fill_manual(values = colors) +
  scale_y_continuous(labels = count_labels, expand = expansion(mult = c(0, .13))) +
  labs(title = "AMWS records by inferred gender",
       subtitle = "US-born panel records, birth years 1800-1960",
       x = NULL, y = "Number of records",
       caption = paste("Name-based inference at a 95% reference-proportion cutoff; includes the documented Adams correction.",
                       "Unresolved = ambiguous, unmatched, or unreadable. Counts are records, not verified unique people.",
                       sep = "\n")) + base_theme
annual_plot_data <- melt(annual, id.vars = "birth_year", measure.vars = c("men", "women"),
                         variable.name = "group", value.name = "n")
annual_plot_data[, group := factor(group, levels = c("men", "women"), labels = c("Men", "Women"))]
annual_plot <- ggplot(annual_plot_data, aes(birth_year, n, color = group, fill = group)) +
  geom_area(alpha = .12, linewidth = 0) + geom_line(linewidth = .7) +
  facet_wrap(~group, ncol = 1, scales = "free_y") +
  scale_color_manual(values = colors) + scale_fill_manual(values = colors) +
  scale_x_continuous(breaks = seq(1800, 1960, 20), limits = c(1800, 1960),
                     expand = expansion(mult = c(.005, .005))) +
  scale_y_continuous(labels = count_labels, limits = c(0, NA),
                     expand = expansion(mult = c(0, .08))) +
  labs(title = "AMWS inferred men and women by birth year",
       subtitle = "Name-based classifications; panel axes use different count scales",
       x = "Birth year", y = "Number of records",
       caption = paste("95% reference-proportion cutoff with documented correction; unresolved records appear in the tables.",
                       "Counts reflect directory coverage. Later birth cohorts have incomplete career coverage.",
                       "Existing counting rules retained: no deduplication between 1986 and earlier editions.", sep = "\n")) +
  base_theme + theme(strip.text = element_text(hjust = 0, face = "bold", size = 12),
                      panel.spacing = grid::unit(1.2, "lines"))
for (format in c("png", "pdf")) {
  device <- if (format == "pdf") grDevices::cairo_pdf else "png"
  ggsave(file.path(figure_dir, paste0("amws_gender_overall_counts.", format)), total_plot,
         width = 11, height = 6.5, units = "in", dpi = 300, device = device, bg = "white")
  ggsave(file.path(figure_dir, paste0("amws_gender_birth_year_counts.", format)), annual_plot,
         width = 11, height = 8, units = "in", dpi = 300, device = device, bg = "white")
}

md5_after <- tools::md5sum(inputs)
stopifnot(identical(md5_before, md5_after))
fwrite(data.table(path = inputs, md5_before = unname(md5_before), md5_after = unname(md5_after),
                  unchanged = unname(md5_before == md5_after)),
       file.path(out_dir, "amws_gender_source_checksums.csv"))
metadata <- data.table(
  metric = c("run_time", "R_version", "gender_version", "genderdata_version", "threshold",
             "population", "birth_year_min", "birth_year_max", "reference_batches",
             "distinct_name_window_queries", "pilot_rows_reproduced", "verified_overrides",
             "counting_rule", "reference_rule", "precision"),
  value = as.character(c(format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), R.version.string,
                         as.character(packageVersion("gender")), as.character(packageVersion("genderdata")),
                         threshold, nrow(pool), 1800L, 1960L, nrow(windows), nrow(queries),
                         nrow(pilot), nrow(audit),
                         "Current combined panel records; no extra deduplication",
                         "IPUMS before 1930; SSA from 1930; birth-decade windows",
                         "Not estimated; reference proportions are not verified accuracy")))
fwrite(metadata, file.path(out_dir, "amws_gender_run_metadata.csv"))
cat("\nOverall counts:\n")
print(overall)
cat("\nCoverage by edition:\n")
print(by_edition)
cat("\nAll validations passed; source and pilot checksums unchanged.\nSaved to:", out_dir, "\n")
