# Descriptive AMWS birth-cohort comparison; no causal GI Bill effect is estimated.
# Run: Rscript analysis/gi_bill/analysis_amws_gi_bill_cohorts.R
suppressPackageStartupMessages({ library(data.table); library(ggplot2) })
stopifnot(requireNamespace("gender", quietly = TRUE),
          requireNamespace("genderdata", quietly = TRUE))
script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
repo_root <- if (length(script_arg)) {
  normalizePath(file.path(dirname(sub("^--file=", "", script_arg[1])), "..", ".."),
                winslash = "/", mustWork = TRUE)
} else normalizePath(Sys.getenv("GTL_REPO", unset = getwd()), winslash = "/")
source(file.path(repo_root, "paths.R"))
stopifnot(dir.exists(TALENT_DETS_DATA_DIR))
gender_dir <- file.path(AMWS_OUTPUT, "gender_inference")
record_file <- file.path(gender_dir, "amws_gender_records.csv")
annual_file <- file.path(gender_dir, "amws_gender_birth_year_counts.csv")
out_dir <- file.path(TALENT_DETS_DATA_DIR, "results", "gi_bill", "cohort_comparison")
figure_dir <- file.path(out_dir, "figures")
threshold <- 0.95
years <- 1910L:1950L
count_cols <- c("men", "women", "ambiguous", "unmatched", "unreadable", "total_records",
                "classified", "unresolved")
categories <- c("male", "female", "ambiguous", "unmatched", "unreadable")

# Protect the original classification outputs, their source files, and AMWS panels.
source_manifest <- fread(file.path(gender_dir, "amws_gender_source_checksums.csv"))
inputs <- unique(c(source_manifest$path,
                  list.files(gender_dir, recursive = TRUE, full.names = TRUE),
                  list.files(file.path(AMWS_OUTPUT, "gender_pilot"), full.names = TRUE),
                  list.files(DATA_OUTPUT, pattern = "^us_panel_county_amws.*\\.csv$",
                             full.names = TRUE)))
stopifnot(all(file.exists(inputs)))
md5_before <- tools::md5sum(inputs)
# The base CSV reader correctly decodes repeated literal quotes in OCR headers.
# fread can retain their CSV escape characters and double them on audit export.
records <- as.data.table(read.csv(record_file, stringsAsFactors = FALSE,
                                  na.strings = "", check.names = FALSE,
                                  encoding = "UTF-8"))
original_annual <- fread(annual_file)
stopifnot(nrow(records) == 112291L, !anyDuplicated(records$record_id),
          all(records$classification_final %in% categories),
          all(records$threshold == threshold))
x <- copy(records[birth_year %in% years])
stopifnot(all(x$method == fifelse(x$birth_year < 1930L, "ipums", "ssa")),
          all(x$year_min == 10L * (x$birth_year %/% 10L)),
          all(x$year_max == x$year_min + 9L))

safe_divide <- function(a, b) {
  z <- rep(NA_real_, length(a))
  ok <- !is.na(a) & !is.na(b) & b != 0
  z[ok] <- a[ok] / b[ok]
  z
}
classify <- function(name, male, female) {
  majority <- pmax(male, female)
  fcase(is.na(name) | !nzchar(name), "unreadable",
        is.na(majority), "unmatched", majority < threshold, "ambiguous",
        male > female, "male", default = "female")
}
same_numbers <- function(a, b) {
  all((is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & abs(a - b) < 1e-12))
}
stopifnot(is.na(safe_divide(0, 0)), safe_divide(0, 2) == 0,
          identical(classify(c("a", "b", "c", "d", NA_character_),
                            c(.95, .9499, .05, NA, .99),
                            c(.05, .0501, .95, NA, .01)),
                    c("male", "ambiguous", "female", "unmatched", "unreadable")))

# Use the saved extracted/reviewed first names; do not repeat or broaden cleaning.
queries <- unique(x[!is.na(first_name) & nzchar(first_name),
                    .(first_name, year_min, year_max)])
setorder(queries, year_min, first_name)
windows <- unique(queries[, .(year_min, year_max)])
predictions <- vector("list", nrow(windows))
for (i in seq_len(nrow(windows))) {
  w <- windows[i]
  query_names <- queries[year_min == w$year_min & year_max == w$year_max, first_name]
  cat(sprintf("SSA batch %d/%d: %d-%d, %d names\n", i, nrow(windows),
              w$year_min, w$year_max, length(query_names)))
  p <- as.data.table(gender::gender(query_names, years = c(w$year_min, w$year_max),
                                  method = "ssa"))
  if (!nrow(p)) next
  setnames(p, c("name", "gender", "proportion_male", "proportion_female"),
           c("first_name", "ssa_package_prediction", "ssa_proportion_male",
             "ssa_proportion_female"))
  predictions[[i]] <- p
}
key <- c("first_name", "year_min", "year_max")
predicted <- rbindlist(predictions, use.names = TRUE)
stopifnot(!anyDuplicated(predicted[, ..key]))
lookup <- merge(queries, predicted, by = key, all.x = TRUE, sort = FALSE)
x <- merge(x, lookup, by = key, all.x = TRUE, sort = FALSE)
setorder(x, birth_year, edition, source_row)
x[, ssa_classification_raw := classify(first_name, ssa_proportion_male, ssa_proportion_female)]
x[, ssa_classification_final := ssa_classification_raw]
# Carry forward any documented identity-specific correction if it enters this window.
x[!is.na(validated_gender) & validated_gender %in% c("male", "female"),
  ssa_classification_final := validated_gender]
stopifnot(nrow(x) == sum(original_annual[birth_year %in% years, total_records]),
          !anyDuplicated(x$record_id),
          all(x$ssa_classification_final %in% categories),
          same_numbers(x[birth_year >= 1930L, proportion_male],
                       x[birth_year >= 1930L, ssa_proportion_male]),
          same_numbers(x[birth_year >= 1930L, proportion_female],
                       x[birth_year >= 1930L, ssa_proportion_female]),
          all(x[birth_year >= 1930L, classification_final == ssa_classification_final]),
          all(x[is.na(first_name), ssa_classification_final] == "unreadable"),
          all(is.na(x$ssa_proportion_male) |
                abs(x$ssa_proportion_male + x$ssa_proportion_female - 1) < 1e-8),
          all(x[ssa_classification_raw %in% c("male", "female"),
                pmax(ssa_proportion_male, ssa_proportion_female)] >= threshold))

summarize_counts <- function(data, classification_col, grouping) {
  z <- copy(data)
  z[, label := get(classification_col)]
  z[, .(men = sum(label == "male"), women = sum(label == "female"),
         ambiguous = sum(label == "ambiguous"), unmatched = sum(label == "unmatched"),
         unreadable = sum(label == "unreadable"), total_records = .N), by = grouping]
}
add_metrics <- function(z) {
  z[, `:=`(classified = men + women, unresolved = ambiguous + unmatched + unreadable,
            male_female_count_gap = men - women)]
  z[, `:=`(coverage = safe_divide(classified, total_records),
            female_share_classified = safe_divide(women, classified),
            men_per_woman = safe_divide(men, women),
            female_per_male = safe_divide(women, men),
            relative_count_gap = safe_divide(men - women, classified))]
  stopifnot(all(z$classified + z$unresolved == z$total_records))
  z
}
annual_for <- function(classification_col, specification) {
  z <- merge(data.table(birth_year = years),
             summarize_counts(x, classification_col, "birth_year"),
             by = "birth_year", all.x = TRUE, sort = TRUE)
  for (nm in count_cols[1:6]) set(z, which(is.na(z[[nm]])), nm, 0L)
  add_metrics(z)
  z[, specification := specification]
  z
}
annual <- rbindlist(list(annual_for("classification_final", "primary"),
                         annual_for("ssa_classification_final", "ssa_only")))
stopifnot(nrow(annual) == 82L,
          identical(annual[specification == "primary", birth_year], years))
observed <- annual[specification == "primary"]
expected <- original_annual[birth_year %in% years][order(birth_year)]
stopifnot(all(as.matrix(observed[, ..count_cols]) == as.matrix(expected[, ..count_cols])))

baseline <- annual[birth_year %in% 1910L:1914L,
                   .(men_baseline_annual_mean = mean(men),
                     women_baseline_annual_mean = mean(women)), by = specification]
annual <- merge(annual, baseline, by = "specification", all.x = TRUE, sort = FALSE)
annual[, `:=`(men_index = 100 * safe_divide(men, men_baseline_annual_mean),
              women_index = 100 * safe_divide(women, women_baseline_annual_mean))]
setorder(annual, specification, birth_year)
stopifnot(all(abs(annual[birth_year %in% 1910L:1914L,
                         .(m = mean(men_index), f = mean(women_index)),
                         by = specification]$m - 100) < 1e-10),
          all(abs(annual[birth_year %in% 1910L:1914L,
                         .(m = mean(men_index), f = mean(women_index)),
                         by = specification]$f - 100) < 1e-10))

five_year <- annual[birth_year <= 1949L,
                    lapply(.SD, sum),
                    by = .(specification, cohort_start = 5L * (birth_year %/% 5L)),
                    .SDcols = count_cols[1:6]]
add_metrics(five_year)
five_year[, `:=`(cohort_end = cohort_start + 4L,
                 cohort = paste0(cohort_start, "-", cohort_start + 4L))]
setorder(five_year, specification, cohort_start)
stopifnot(nrow(five_year) == 16L,
          all(five_year[specification == "primary", cohort_start] == seq(1910L, 1945L, 5L)))
for (spec in c("primary", "ssa_only")) {
  stopifnot(all(colSums(five_year[specification == spec, ..count_cols]) ==
                  colSums(annual[specification == spec & birth_year <= 1949L, ..count_cols])))
}
known <- five_year[specification == "primary" & cohort_start %in% c(1920L, 1925L, 1930L, 1935L)]
stopifnot(identical(known$men, c(11097L, 9163L, 8412L, 8234L)),
          identical(known$women, c(421L, 424L, 347L, 471L)))
changes <- copy(five_year)
changes[, `:=`(previous_cohort = shift(cohort),
                previous_men = shift(men), previous_women = shift(women),
                previous_gap = shift(male_female_count_gap),
                previous_female_share = shift(female_share_classified),
                previous_ratio = shift(men_per_woman)), by = specification]
changes[, `:=`(men_change = men - previous_men, women_change = women - previous_women,
                men_change_pct = 100 * safe_divide(men - previous_men, previous_men),
                women_change_pct = 100 * safe_divide(women - previous_women, previous_women),
                gap_change = male_female_count_gap - previous_gap,
                female_share_change_pp = 100 * (female_share_classified - previous_female_share),
                men_per_woman_change = men_per_woman - previous_ratio)]
reference <- five_year[cohort_start == 1920L,
                       .(specification, reference_men = men, reference_women = women,
                         reference_gap = male_female_count_gap,
                         reference_female_share = female_share_classified)]
changes <- merge(changes, reference, by = "specification", all.x = TRUE, sort = FALSE)
changes[, `:=`(men_change_pct_vs_1920_1924 = 100 * safe_divide(men - reference_men, reference_men),
                women_change_pct_vs_1920_1924 = 100 * safe_divide(women - reference_women, reference_women),
                gap_change_vs_1920_1924 = male_female_count_gap - reference_gap,
                female_share_change_pp_vs_1920_1924 = 100 * (female_share_classified - reference_female_share))]
setorder(changes, specification, cohort_start)

edition <- merge(CJ(birth_year = years, edition = sort(unique(records$edition))),
                 summarize_counts(x, "classification_final", c("birth_year", "edition")),
                 by = c("birth_year", "edition"), all.x = TRUE)
for (nm in count_cols[1:6]) set(edition, which(is.na(edition[[nm]])), nm, 0L)
add_metrics(edition)
edition_check <- edition[, lapply(.SD, sum), by = birth_year, .SDcols = count_cols]
setorder(edition_check, birth_year)
stopifnot(all(as.matrix(edition_check[, ..count_cols]) == as.matrix(expected[, ..count_cols])))

audit <- x[, .(record_id, edition, birth_year, original_name, first_name, extraction_status,
                primary_method = method, year_min, year_max,
                primary_proportion_male = proportion_male,
                primary_proportion_female = proportion_female,
                primary_classification_raw = classification_raw,
                primary_classification_final = classification_final,
                ssa_package_prediction, ssa_proportion_male, ssa_proportion_female,
                ssa_classification_raw, ssa_classification_final,
                validated_gender, validation_source_url, threshold)]
audit[, classification_changed := primary_classification_final != ssa_classification_final]
sensitivity <- audit[, .(total_records = .N, changed_records = sum(classification_changed)),
                       by = .(birth_decade = 10L * (birth_year %/% 10L), primary_method)]
transitions <- audit[, .N, by = .(primary_classification_final, ssa_classification_final)]
stopifnot(sum(transitions$N) == nrow(x))
zero_check <- data.table(men = 0L, women = 0L, ambiguous = 0L, unmatched = 0L,
                         unreadable = 0L, total_records = 0L)
add_metrics(zero_check)
stopifnot(zero_check$male_female_count_gap == 0L,
          all(is.na(unlist(zero_check[, .(coverage, female_share_classified,
                                          men_per_woman, female_per_male, relative_count_gap)]))))

dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
for (nm in c("annual", "five_year", "changes", "edition", "audit", "lookup",
             "sensitivity", "transitions", "baseline")) {
  filename <- switch(nm, annual = "annual_counts.csv", five_year = "five_year_counts.csv",
                     changes = "five_year_changes.csv", edition = "edition_annual_counts.csv",
                     audit = "ssa_record_audit.csv", lookup = "ssa_name_reference.csv",
                     sensitivity = "sensitivity_summary.csv", transitions = "classification_transitions.csv",
                     baseline = "baseline_indices.csv")
  fwrite(get(nm), file.path(out_dir, filename), na = "")
}

count_labels <- function(v) format(v, big.mark = ",", scientific = FALSE, trim = TRUE)
sex_colors <- c("Men" = "#27647D", "Women" = "#B04B83")
spec_colors <- c("Primary" = "#27647D", "SSA only" = "#D28B32")
base_theme <- theme_minimal(base_size = 11, base_family = "Arial") +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 10.5, margin = margin(b = 12)),
        plot.caption = element_text(size = 9, hjust = 0, lineheight = 1.2, margin = margin(t = 14)),
        strip.text = element_text(face = "bold", hjust = 0),
        legend.position = "top", legend.title = element_blank(),
        plot.margin = margin(16, 20, 16, 16))
caption <- paste("Name-based inference, 95% reference cutoff; existing combined-panel record convention.",
                 "Shading: births 1920-1939. Dashed vertical line: primary method changes to SSA at 1930.",
                 "Descriptive evidence; cohort size, directory coverage and career timing affect counts.", sep = "\n")
decorate <- function(p) {
  p + scale_x_continuous(breaks = seq(1910, 1950, 5), limits = c(1910, 1950),
                         expand = expansion(mult = .015)) + base_theme
}
backdrop <- function() list(
  annotate("rect", xmin = 1919.5, xmax = 1939.5, ymin = -Inf, ymax = Inf,
           fill = "#E8E3D8", alpha = .5),
  geom_vline(xintercept = 1930, linetype = "dashed", color = "#888888", linewidth = .4))
save_figure <- function(p, name, height = 6.5) {
  ggsave(file.path(figure_dir, paste0(name, ".png")), p, width = 11, height = height,
         dpi = 300, bg = "white")
  ggsave(file.path(figure_dir, paste0(name, ".pdf")), p, width = 11, height = height,
         device = grDevices::cairo_pdf, bg = "white")
}
annual[, series := factor(specification, levels = c("primary", "ssa_only"),
                          labels = c("Primary", "SSA only"))]
gap_plot <- decorate(ggplot(annual, aes(birth_year, male_female_count_gap))) + backdrop() +
  geom_hline(yintercept = 0, color = "#777777", linewidth = .4) +
  geom_line(aes(color = series, linetype = series), linewidth = .85) +
  scale_color_manual(values = spec_colors) +
  scale_linetype_manual(values = c("Primary" = "solid", "SSA only" = "longdash")) +
  expand_limits(y = 0) +
  scale_y_continuous(labels = count_labels, expand = expansion(mult = c(.025, .06))) +
  labs(title = "Annual male-female gap in AMWS records",
       subtitle = "US-born birth cohorts, 1910-1950 | Combined editions; primary and SSA-only inference",
       x = "Birth year", y = "Inferred male records minus inferred female records", caption = caption)
save_figure(gap_plot, "annual_gender_gap")
relative_gap_plot <- decorate(ggplot(annual, aes(birth_year, 100 * relative_count_gap))) +
  backdrop() + geom_hline(yintercept = 0, color = "#777777", linewidth = .4) +
  geom_line(aes(color = series, linetype = series), linewidth = .85) +
  scale_color_manual(values = spec_colors) +
  scale_linetype_manual(values = c("Primary" = "solid", "SSA only" = "longdash")) +
  expand_limits(y = c(0, 100)) +
  scale_y_continuous(labels = function(v) paste0(v, "%"), breaks = seq(-100, 100, 20),
                     expand = expansion(mult = c(.025, .04))) +
  labs(title = "Relative male-female gap in AMWS records",
       subtitle = "(Men - women) / (men + women), expressed as a percentage | Birth cohorts 1910-1950",
       x = "Birth year", y = "Relative gap among classified records (%)",
       caption = paste(caption,
                       "Unresolved records excluded. 0% = equal counts; 100% = all classified records are male.",
                       sep = "\n"))
save_figure(relative_gap_plot, "annual_relative_count_gap")
female_male_plot <- decorate(ggplot(annual, aes(birth_year, female_per_male))) +
  backdrop() + geom_line(aes(color = series, linetype = series), linewidth = .85) +
  scale_color_manual(values = spec_colors) +
  scale_linetype_manual(values = c("Primary" = "solid", "SSA only" = "longdash")) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, .08))) +
  labs(title = "Female-to-male ratio in AMWS records",
       subtitle = "Inferred female records / inferred male records | 0.10 means 10 female records per 100 male records",
       x = "Birth year", y = "Female records / male records",
       caption = paste(caption, "Unresolved records excluded; ratios are undefined when male counts are zero.",
                       sep = "\n"))
save_figure(female_male_plot, "annual_female_male_ratio")
primary <- annual[specification == "primary"]
count_long <- melt(primary, id.vars = "birth_year", measure.vars = c("men", "women"),
                   variable.name = "sex", value.name = "count")
count_long[, sex := factor(sex, levels = c("men", "women"),
                           labels = c("Inferred men: record count", "Inferred women: record count"))]
count_plot <- decorate(ggplot(count_long, aes(birth_year, count))) + backdrop() +
  geom_line(aes(color = sex), linewidth = .85) +
  facet_wrap(~sex, ncol = 1, scales = "free_y") +
  scale_color_manual(values = unname(sex_colors), guide = "none") +
  scale_y_continuous(labels = count_labels, limits = c(0, NA), expand = expansion(mult = c(.02, .08))) +
  labs(title = "Male and female AMWS records by birth year",
       subtitle = "Combined editions, primary inference | Separate count scales for men and women",
       x = "Birth year", y = "Number of inferred records", caption = caption)
save_figure(count_plot, "annual_gender_counts", 8)
index_long <- melt(primary, id.vars = "birth_year", measure.vars = c("men_index", "women_index"),
                   variable.name = "sex", value.name = "index")
index_long[, sex := factor(sex, levels = c("men_index", "women_index"), labels = c("Men", "Women"))]
index_plot <- decorate(ggplot(index_long, aes(birth_year, index))) + backdrop() +
  geom_hline(yintercept = 100, color = "#777777", linewidth = .4) +
  geom_line(aes(color = sex), linewidth = .85) + scale_color_manual(values = sex_colors) +
  labs(title = "Relative evolution of male and female AMWS counts",
       subtitle = "Combined editions, primary inference | Each sex's 1910-1914 annual mean = 100",
       x = "Birth year", y = "Count index (baseline = 100)", caption = caption)
save_figure(index_plot, "annual_indexed_counts")
relative <- rbindlist(list(
  annual[, .(birth_year, series, metric = "Women among classified records (%)",
              value = 100 * female_share_classified)],
  annual[, .(birth_year, series, metric = "Male records per female record", value = men_per_woman)]))
relative[, metric := factor(metric, levels = unique(metric))]
relative_plot <- decorate(ggplot(relative, aes(birth_year, value))) + backdrop() +
  geom_line(aes(color = series, linetype = series), linewidth = .85) +
  facet_wrap(~metric, ncol = 1, scales = "free_y") + scale_color_manual(values = spec_colors) +
  scale_linetype_manual(values = c("Primary" = "solid", "SSA only" = "longdash")) +
  labs(title = "Relative gender gaps across AMWS birth cohorts",
       subtitle = "Combined editions | Denominators include male/female classifications only",
       x = "Birth year", y = "Value (units labeled in each panel)", caption = caption)
save_figure(relative_plot, "annual_relative_gaps", 8)
coverage_plot <- decorate(ggplot(annual, aes(birth_year, 100 * coverage))) + backdrop() +
  geom_line(aes(color = series, linetype = series), linewidth = .85) +
  scale_color_manual(values = spec_colors) +
  scale_linetype_manual(values = c("Primary" = "solid", "SSA only" = "longdash")) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
  labs(title = "Gender-classification coverage by birth cohort",
       subtitle = "Classified male/female records as a percentage of all included records",
       x = "Birth year", y = "Classification coverage (%)",
       caption = paste(caption, "Coverage measures completeness; classification precision is unmeasured.", sep = "\n"))
save_figure(coverage_plot, "annual_classification_coverage")
edition_long <- melt(edition, id.vars = c("birth_year", "edition"), measure.vars = c("men", "women"),
                     variable.name = "sex", value.name = "count")
edition_long[, `:=`(edition = factor(edition, levels = c(1906, 1938, 1955, 1986)),
                     sex = factor(sex, levels = c("men", "women"),
                                  labels = c("Inferred men: record count", "Inferred women: record count")))]
edition_plot <- decorate(ggplot(edition_long, aes(birth_year, count))) + backdrop() +
  geom_area(aes(fill = edition), alpha = .85, position = "stack") +
  facet_wrap(~sex, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = c("1906" = "#BBBBBB", "1938" = "#87966B",
                               "1955" = "#D28B32", "1986" = "#27647D")) +
  scale_y_continuous(labels = count_labels, limits = c(0, NA), expand = expansion(mult = c(.02, .08))) +
  labs(title = "Edition contributions to combined AMWS counts",
       subtitle = "Primary inference | Retained 1986 biographies are later-directory contributions",
       x = "Birth year", y = "Number of inferred records", caption = caption)
save_figure(edition_plot, "annual_edition_contributions", 8)

md5_after <- tools::md5sum(inputs)
stopifnot(identical(md5_before, md5_after))
fwrite(data.table(path = inputs, md5_before = unname(md5_before), md5_after = unname(md5_after),
                  unchanged = unname(md5_before == md5_after)), file.path(out_dir, "source_checksums.csv"))
metadata <- data.table(
  metric = c("run_time", "R_version", "gender_version", "genderdata_version", "threshold",
             "input_population", "analysis_population", "birth_year_min", "birth_year_max",
             "ssa_batches", "ssa_name_window_queries", "ssa_changed_records", "protected_files",
             "primary_rule", "gap_definition", "female_share_denominator", "precision", "causal_effect"),
  value = as.character(c(format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), R.version.string,
                         as.character(packageVersion("gender")),
                         as.character(packageVersion("genderdata")), threshold,
                         nrow(records), nrow(x), min(years), max(years), nrow(windows), nrow(queries),
                         sum(audit$classification_changed), length(inputs),
                         "Existing final labels and panel inclusion; no new extraction or deduplication",
                         "Male records minus female records for each birth year",
                         "Male plus female classified records; unresolved excluded",
                         "Unmeasured; reference proportions are not accuracy",
                         "Not estimated; sex-specific birth denominators and veteran exposure needed")))
fwrite(metadata, file.path(out_dir, "run_metadata.csv"))
cat("\nPrimary five-year results:\n")
print(five_year[specification == "primary",
                .(cohort, men, women, male_female_count_gap, female_share_classified, men_per_woman, coverage)])
cat("\nSSA sensitivity:\n")
print(sensitivity)
cat("\nAll validations passed;", length(inputs), "protected files unchanged.\nSaved to:", out_dir, "\n")
