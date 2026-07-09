###############################################################################
# Plot AMWS Ed16 expanded corrected births by year.
#
# Reads:
#   output/amws/regex_all_docs/
#     amws_ed16_entries_regex_parsed_birth_year_city_expanded_all_corrected.csv
#
# Writes:
#   output/amws/regex_all_docs/
#     amws_ed16_births_by_year.csv
#     amws_ed16_births_by_year.png
###############################################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(readr)
})

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_arg)) {
  script_path <- normalizePath(sub("^--file=", "", script_arg[1]),
                               winslash = "/", mustWork = TRUE)
  repo_root <- normalizePath(file.path(dirname(script_path), "..", "..", ".."),
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

output_dir <- env_chr(
  "AMWS_ED16_BIRTH_YEAR_OUTPUT_DIR",
  file.path(DATA_OUTPUT, "amws", "regex_all_docs")
)
output_dir <- normalizePath(output_dir, winslash = "/", mustWork = TRUE)

input_file <- env_chr(
  "AMWS_ED16_BIRTH_YEAR_INPUT_FILE",
  file.path(
    output_dir,
    "amws_ed16_entries_regex_parsed_birth_year_city_expanded_all_corrected.csv"
  )
)
input_file <- normalizePath(input_file, winslash = "/", mustWork = TRUE)

counts_csv <- file.path(output_dir, "amws_ed16_births_by_year.csv")
plot_png <- file.path(output_dir, "amws_ed16_births_by_year.png")

data <- read_csv(input_file, col_types = cols(.default = col_character()),
                 show_col_types = FALSE)

counts <- data |>
  mutate(birth_year_int = suppressWarnings(as.integer(birth_year))) |>
  filter(!is.na(birth_year_int)) |>
  count(birth_year = birth_year_int, name = "n") |>
  arrange(birth_year)

write_excel_csv(counts, counts_csv, na = "")

p <- ggplot(counts, aes(x = birth_year, y = n)) +
  geom_col(fill = "#366c8f", width = 0.9) +
  scale_x_continuous(breaks = seq(1800, 1960, by = 20)) +
  labs(
    x = "Birth year",
    y = "AMWS Ed16 entries",
    title = "AMWS Ed16 births by year"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )

ggsave(plot_png, p, width = 11, height = 6.5, dpi = 200)

cat("Wrote counts:", counts_csv, "\n")
cat("Wrote plot:", plot_png, "\n")
cat("Max birth_year:", max(counts$birth_year), "\n")
cat("Rows birth_year >= 1970:", sum(counts$n[counts$birth_year >= 1970]), "\n")
