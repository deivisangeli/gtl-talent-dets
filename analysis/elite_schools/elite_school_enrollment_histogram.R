###############################################################################
# Histogram of elite-high-school enrollment in the first decade of operation.
# Reads prep/output/elite_high_schools_enrollment.tsv and joins to the core
# schools file to flag access tier (high vs low historical access).
###############################################################################

rm(list = ls())

suppressPackageStartupMessages({
  library("tidyverse")
  library("ggplot2")
  library("scales")
})
args <- commandArgs(trailingOnly = FALSE)
file_arg <- args[grepl("^--file=", args)]
if (length(file_arg) > 0) {
  script_path <- normalizePath(sub("^--file=", "", file_arg[[1]]), winslash = "/", mustWork = TRUE)
  repo_root <- normalizePath(file.path(dirname(script_path), "..", ".."), winslash = "/", mustWork = TRUE)
} else {
  cwd <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  repo_root <- if (basename(cwd) == "analysis") dirname(cwd) else if (basename(dirname(cwd)) == "analysis") dirname(dirname(cwd)) else cwd
}
source(file.path(repo_root, "paths.R"))

results_dir <- file.path(TALENT_DETS_DATA_DIR, "results", "elite_schools", "elite_school_event_studies",
                         "elite_school_enrollment")
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

enroll <- read_tsv(file.path(SCHOOLS_OUTPUT, "elite_high_schools_enrollment.tsv"),
                   show_col_types = FALSE,
                   na = c("", "NA"))

schools_meta <- read_csv(
  file.path(SCHOOLS_OUTPUT, "elite_high_schools_core_1800_1930.csv"),
  show_col_types = FALSE
) %>%
  select(school, state_abbr, poor_access_historical, school_type)

merged <- enroll %>%
  left_join(schools_meta, by = c("school", "state_abbr")) %>%
  mutate(
    access = case_when(
      poor_access_historical == "high"   ~ "High access",
      poor_access_historical == "medium" ~ "Medium access",
      poor_access_historical == "low"    ~ "Low access",
      TRUE                               ~ "Unknown"
    ),
    access = factor(access,
                    levels = c("High access", "Medium access",
                               "Low access", "Unknown"))
  )

cat("Schools total: ", nrow(merged), "\n", sep = "")
cat("With year10_point: ", sum(!is.na(merged$year10_point)), "\n", sep = "")
cat("Unknown: ", sum(is.na(merged$year10_point)), "\n", sep = "")

###############################################################################
# Summary stats
###############################################################################

summary_by_access <- merged %>%
  filter(!is.na(year10_point)) %>%
  group_by(access) %>%
  summarise(
    n        = n(),
    mean     = round(mean(year10_point), 0),
    median   = median(year10_point),
    p25      = quantile(year10_point, 0.25),
    p75      = quantile(year10_point, 0.75),
    min      = min(year10_point),
    max      = max(year10_point),
    .groups  = "drop"
  )
write_csv(summary_by_access,
          file.path(results_dir, "summary_year10_by_access.csv"))
print(summary_by_access)

write_csv(merged,
          file.path(results_dir, "enrollment_with_access.csv"))

###############################################################################
# Histogram (linear)
###############################################################################

p_lin <- merged %>%
  filter(!is.na(year10_point)) %>%
  ggplot(aes(x = year10_point, fill = access)) +
  geom_histogram(binwidth = 50, color = "white", boundary = 0) +
  scale_fill_manual(values = c(
    "High access"   = "#2a9d8f",
    "Medium access" = "#e9c46a",
    "Low access"    = "#bc4749",
    "Unknown"       = "gray70"
  )) +
  scale_x_continuous(breaks = seq(0, 2500, 250)) +
  labs(
    x    = "Enrollment ~10 years after founding",
    y    = "Number of schools",
    fill = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

ggsave(file.path(results_dir, "histogram_year10_linear.png"),
       p_lin, width = 10, height = 6, dpi = 200)

###############################################################################
# Histogram (log scale) — handles the long tail (Brooklyn Tech 2400)
###############################################################################

p_log <- merged %>%
  filter(!is.na(year10_point), year10_point > 0) %>%
  ggplot(aes(x = year10_point, fill = access)) +
  geom_histogram(bins = 25, color = "white") +
  scale_x_log10(
    breaks = c(10, 25, 50, 100, 200, 500, 1000, 2500),
    labels = comma
  ) +
  scale_fill_manual(values = c(
    "High access"   = "#2a9d8f",
    "Medium access" = "#e9c46a",
    "Low access"    = "#bc4749",
    "Unknown"       = "gray70"
  )) +
  labs(
    x    = "Enrollment ~10 years after founding (log scale)",
    y    = "Number of schools",
    fill = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

ggsave(file.path(results_dir, "histogram_year10_log.png"),
       p_log, width = 10, height = 6, dpi = 200)

###############################################################################
# Faceted by access
###############################################################################

p_facet <- merged %>%
  filter(!is.na(year10_point), access != "Unknown") %>%
  ggplot(aes(x = year10_point, fill = access)) +
  geom_histogram(binwidth = 50, color = "white", boundary = 0) +
  facet_wrap(~ access, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = c(
    "High access"   = "#2a9d8f",
    "Medium access" = "#e9c46a",
    "Low access"    = "#bc4749"
  )) +
  scale_x_continuous(breaks = seq(0, 2500, 250)) +
  labs(
    x    = "Enrollment ~10 years after founding",
    y    = "Number of schools",
    fill = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

ggsave(file.path(results_dir, "histogram_year10_by_access.png"),
       p_facet, width = 10, height = 8, dpi = 200)

cat("\nFigures saved to: ", results_dir, "\n", sep = "")
