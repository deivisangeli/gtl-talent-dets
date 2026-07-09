###############################################################################
# Plot AMWS US-born/geocoded births by year across all available editions.
#
# Reads:
#   output/amws/amws_combined_us_geocoded.csv
#   output/amws/regex_all_docs/amws_ed16_us_geocoded.csv
#
# Writes:
#   output/amws/amws_all_editions_stats_us_born/
#     births_per_year_by_edition_us_born.csv
#     births_per_year_by_edition_us_born_coverage.csv
#     births_per_year_by_edition_us_born.png
###############################################################################

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
})

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_arg)) {
  script_path <- normalizePath(sub("^--file=", "", script_arg[1]),
                               winslash = "/", mustWork = TRUE)
  repo_root <- normalizePath(file.path(dirname(script_path), "..", ".."),
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
  AMWS_OUTPUT <- file.path(DATA_OUTPUT, "amws")
}

out_dir <- file.path(AMWS_OUTPUT, "amws_all_editions_stats_us_born")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

combined_file <- file.path(AMWS_OUTPUT, "amws_combined_us_geocoded.csv")
ed16_file <- file.path(AMWS_OUTPUT, "regex_all_docs", "amws_ed16_us_geocoded.csv")

if (!file.exists(combined_file)) {
  stop("Missing combined AMWS geocoded file: ", combined_file)
}
if (!file.exists(ed16_file)) {
  stop("Missing Ed16 AMWS geocoded file: ", ed16_file)
}

combined <- fread(combined_file, select = c("edition", "kept", "birth_year", "geoid"))
combined <- combined[
  kept == TRUE &
    !is.na(birth_year) &
    !is.na(geoid) &
    trimws(as.character(birth_year)) != "" &
    trimws(as.character(geoid)) != ""
]
combined[, birth_year := suppressWarnings(as.integer(birth_year))]
combined <- combined[!is.na(birth_year)]
combined <- combined[, .(
  edition = as.character(edition),
  birth_year
)]

ed16 <- fread(ed16_file, select = c("birth_year", "geoid"))
ed16 <- ed16[
  !is.na(birth_year) &
    !is.na(geoid) &
    trimws(as.character(birth_year)) != "" &
    trimws(as.character(geoid)) != ""
]
ed16[, birth_year := suppressWarnings(as.integer(birth_year))]
ed16 <- ed16[!is.na(birth_year)]
ed16 <- ed16[, .(
  edition = "1986",
  birth_year
)]

editions <- rbindlist(list(combined, ed16), use.names = TRUE)
editions[, edition := factor(edition, levels = c("1906", "1938", "1955", "1986"))]

counts <- editions[, .(n = .N), by = .(edition, birth_year)]
setorder(counts, edition, birth_year)
fwrite(counts, file.path(out_dir, "births_per_year_by_edition_us_born.csv"))

coverage <- editions[, .(
  rows_with_birth_year_and_geoid = .N,
  min_birth_year = min(birth_year),
  max_birth_year = max(birth_year)
), by = edition]
setorder(coverage, edition)
fwrite(coverage, file.path(out_dir, "births_per_year_by_edition_us_born_coverage.csv"))

plot_min <- min(editions$birth_year)
plot_max <- max(editions$birth_year)

p <- ggplot(counts, aes(x = birth_year, y = n, fill = edition)) +
  geom_col(width = 0.95, position = "stack") +
  scale_fill_manual(
    values = c(
      "1906" = "#0072B2",
      "1938" = "#E69F00",
      "1955" = "#009E73",
      "1986" = "#CC79A7"
    )
  ) +
  scale_x_continuous(
    breaks = seq(1820, 1960, by = 20),
    limits = c(plot_min - 0.5, plot_max + 0.5)
  ) +
  labs(
    x = "Birth year",
    y = "AMWS entries",
    fill = "Edition",
    title = "AMWS US-born births by year across editions",
    subtitle = "Geocoded US records only; 1906/1938/1955 use post-dedup kept rows"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )

ggsave(file.path(out_dir, "births_per_year_by_edition_us_born.png"),
       p, width = 11, height = 6.5, dpi = 200)

cat("Wrote counts:",
    file.path(out_dir, "births_per_year_by_edition_us_born.csv"), "\n")
cat("Wrote coverage:",
    file.path(out_dir, "births_per_year_by_edition_us_born_coverage.csv"), "\n")
cat("Wrote plot:",
    file.path(out_dir, "births_per_year_by_edition_us_born.png"), "\n")
print(coverage)
