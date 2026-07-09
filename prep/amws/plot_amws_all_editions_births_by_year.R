###############################################################################
# Plot AMWS births by year across all available editions.
#
# Reads:
#   output/amws/
#     amws_1906_cleaned_corrected.csv
#     amws_1938_cleaned_corrected.csv
#     amws_1955_split.csv
#   output/amws/regex_all_docs/
#     amws_ed16_entries_regex_parsed_birth_year_city_expanded_all_corrected.csv
#
# Writes:
#   output/amws/amws_all_editions_stats/
#     births_per_year_by_edition.csv
#     births_per_year_by_edition_excluded_implausible.csv
#     births_per_year_by_edition.png
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

out_dir <- file.path(AMWS_OUTPUT, "amws_all_editions_stats")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

amws_1955_split_file <- file.path(AMWS_OUTPUT, "amws_1955_split_corrected.csv")
if (!file.exists(amws_1955_split_file)) {
  amws_1955_split_file <- file.path(AMWS_OUTPUT, "amws_1955_split.csv")
}

parse_1955_year <- function(date_str) {
  yr <- suppressWarnings(as.integer(sub(".*?([0-9]{1,4})\\s*$", "\\1", date_str)))
  out <- rep(NA_integer_, length(date_str))
  k4 <- !is.na(yr) & yr >= 1000L
  k1900 <- !is.na(yr) & yr >= 0L & yr <= 40L
  k1800 <- !is.na(yr) & yr >= 41L & yr <= 99L
  out[k4] <- yr[k4]
  out[k1900] <- 1900L + yr[k1900]
  out[k1800] <- 1800L + yr[k1800]
  out
}

read_edition_years <- function(path, edition, year_col = "birth_year") {
  data <- fread(path, colClasses = "character")
  if (year_col %in% names(data)) {
    year <- suppressWarnings(as.integer(data[[year_col]]))
  } else if ("date" %in% names(data)) {
    year <- parse_1955_year(data[["date"]])
  } else {
    stop("No birth_year/date column found in ", path)
  }
  data.table(
    edition = as.character(edition),
    birth_year = year
  )
}

editions <- rbindlist(list(
  read_edition_years(
    file.path(AMWS_OUTPUT, "amws_1906_cleaned_corrected.csv"),
    "1906"
  ),
  read_edition_years(
    file.path(AMWS_OUTPUT, "amws_1938_cleaned_corrected.csv"),
    "1938"
  ),
  read_edition_years(
    amws_1955_split_file,
    "1955"
  ),
  read_edition_years(
    file.path(
      AMWS_OUTPUT, "regex_all_docs",
      "amws_ed16_entries_regex_parsed_birth_year_city_expanded_all_corrected.csv"
    ),
    "1986"
  )
), use.names = TRUE)

editions <- editions[!is.na(birth_year) & birth_year >= 1800L]
editions[, edition := factor(edition, levels = c("1906", "1938", "1955", "1986"))]
editions[, edition_year := as.integer(as.character(edition))]

excluded <- editions[birth_year > edition_year]
fwrite(
  excluded[, .(edition, birth_year, exclusion_reason = "birth_year_after_edition")],
  file.path(out_dir, "births_per_year_by_edition_excluded_implausible.csv")
)

editions <- editions[birth_year <= edition_year]

counts <- editions[, .(n = .N), by = .(edition, birth_year)][order(edition, birth_year)]
fwrite(counts, file.path(out_dir, "births_per_year_by_edition.csv"))

coverage <- editions[, .(
  rows_with_birth_year = .N,
  min_birth_year = min(birth_year),
  max_birth_year = max(birth_year)
), by = edition][order(edition)]
fwrite(coverage, file.path(out_dir, "births_per_year_by_edition_coverage.csv"))

plot_data <- counts[birth_year >= 1800L & birth_year <= 1960L]

p <- ggplot(plot_data, aes(x = birth_year, y = n, fill = edition)) +
  geom_col(width = 0.95, position = "stack") +
  # Okabe-Ito colorblind-safe palette.
  scale_fill_manual(
    values = c(
      "1906" = "#0072B2",
      "1938" = "#E69F00",
      "1955" = "#009E73",
      "1986" = "#CC79A7"
    )
  ) +
  scale_x_continuous(breaks = seq(1800, 1960, by = 20)) +
  labs(
    x = "Birth year",
    y = "AMWS entries",
    fill = "Edition",
    title = "AMWS births by year across editions",
    subtitle = "Full cleaned/corrected edition files; rows after their edition year excluded"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )

ggsave(file.path(out_dir, "births_per_year_by_edition.png"),
       p, width = 11, height = 6.5, dpi = 200)

cat("Wrote counts:", file.path(out_dir, "births_per_year_by_edition.csv"), "\n")
cat("Wrote exclusions:",
    file.path(out_dir, "births_per_year_by_edition_excluded_implausible.csv"),
    "\n")
cat("Wrote coverage:", file.path(out_dir, "births_per_year_by_edition_coverage.csv"), "\n")
cat("Wrote plot:", file.path(out_dir, "births_per_year_by_edition.png"), "\n")
cat("Excluded implausible rows:", nrow(excluded), "\n")
print(coverage)
