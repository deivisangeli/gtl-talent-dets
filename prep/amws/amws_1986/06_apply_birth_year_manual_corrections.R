###############################################################################
# Apply explicit AMWS Ed16 birth-year manual corrections.
#
# Reads/writes:
#   output/amws/regex_all_docs/
#     amws_ed16_entries_regex_parsed_birth_year_city_expanded_all_corrected.csv
#
# Writes logs:
#   output/amws/regex_all_docs/
#     amws_ed16_birth_year_manual_corrections_applied_log.csv
#     amws_ed16_birth_year_manual_corrections_summary.csv
###############################################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tibble)
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

normalize_text <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  trimws(gsub("\\s+", " ", x))
}

default_output_dir <- file.path(DATA_OUTPUT, "amws", "regex_all_docs")
output_dir <- env_chr("AMWS_ED16_BIRTH_YEAR_OUTPUT_DIR", default_output_dir)
output_dir <- normalizePath(output_dir, winslash = "/", mustWork = TRUE)

input_file <- env_chr(
  "AMWS_ED16_BIRTH_YEAR_INPUT_FILE",
  file.path(output_dir,
            "amws_ed16_entries_regex_parsed_birth_year_city_expanded_all_corrected.csv")
)
input_file <- normalizePath(input_file, winslash = "/", mustWork = TRUE)

applied_log_csv <- file.path(
  output_dir,
  "amws_ed16_birth_year_manual_corrections_applied_log.csv"
)
summary_csv <- file.path(
  output_dir,
  "amws_ed16_birth_year_manual_corrections_summary.csv"
)

corrections <- tribble(
  ~doc_id,                 ~lineid, ~birth_date_new,  ~birth_year_new, ~correction_note,
  "amws16_F_500_600",      "1565",  "Apr 18, 86",    "1886",
  "Raw date 'Apr 18, 86' is implausible as 1986 in AMWS Ed16; interpret as 1886.",

  "amws16_B_200_400",      "3456",  "June 18, 43",   "1943",
  "Raw date was parsed as 'June 1843'; education/career dates show this is day 18 plus two-digit year 43.",
  "amws16_B_800_1000",     "3584",  "Nov 18, 53",    "1953",
  "Raw date was parsed as 'Nov 1853'; BS 75 and PhD 79 show this is day 18 plus two-digit year 53.",
  "amws16_E_0_200",        "593",   "Feb 18, 40",    "1940",
  "Raw date was parsed as 'Feb 1840'; AB 61/MS 66/PhD 69 show this is day 18 plus two-digit year 40.",

  "amws16_A_0_200",        "2535",  "",              "",
  "Late year parsed from corrupted multi-entry OCR ('Oct 74'); no safe birth-year correction visible.",
  "amws16_A_0_200",        "3290",  "",              "",
  "Late year parsed from ambiguous OCR ('June 77'); no safe day/year split visible.",
  "amws16_A_0_200",        "3473",  "",              "",
  "Parsed 1960 conflicts with education/career dates; OCR does not support a safe replacement.",
  "amws16_A_200_400",      "150",   "",              "",
  "Late year parsed from ambiguous OCR ('Dee 78'); no safe birth-year correction visible.",
  "amws16_A_200_400",      "1058",  "Sept 13, 25",   "1925",
  "Raw date reads like 'Sept 13 75' but education/marriage dates in the entry support OCR 2->7, i.e. 1925.",
  "amws16_A_200_400",      "2116",  "Aug 25, 37",    "1937",
  "Raw date reads like 'Aug 25, 77' but the entry's 1960s education/marriage context supports OCR 3->7, i.e. 1937.",
  "amws16_B_0_200",        "115",   "",              "",
  "Late year parsed from ambiguous OCR ('Oct 184'); no safe birth-year correction visible.",
  "amws16_B_0_200",        "1935",  "Dec 8, 47",     "1947",
  "Raw date token '471' carries the visible '47'; BA 70 and PhD 75 support 1947 rather than 1971.",
  "amws16_B_0_200",        "2196",  "June 14, 25",   "1925",
  "Raw date reads like 'June 14, 75' but AB 47 and PhD 50 support OCR 2->7, i.e. 1925.",
  "amws16_B_0_200",        "3298",  "Apr 28, 30",    "1930",
  "Raw date reads like 'Apr 28 70' but BS 52/MS 60/PhD 66 support OCR 3->7, i.e. 1930.",
  "amws16_B_1000_1200",    "421",   "Mar 41",        "1941",
  "Raw date reads like 'Mar 71' but BA 63 and medical training dates support OCR 4->7, i.e. 1941.",
  "amws16_B_1000_1200",    "966",   "Aug 28, 31",    "1931",
  "Raw date has OCR debris after 'Aug 28'; BS 52/MS 53 support birth year 1931.",
  "amws16_B_1000_1200",    "1031",  "Oct 17, 20",    "1920",
  "Raw date token 'Oct 172' is read as Oct 17, 20; BA 42/MS 47 support 1920.",
  "amws16_B_1000_1200",    "1067",  "Oct 27, 27",    "1927",
  "Raw date reads like 'Oct 27 77' but BS 51/MS 55/PhD 58 support OCR 2->7, i.e. 1927.",
  "amws16_B_1000_1200",    "1790",  "Jan 12, 37",    "1937",
  "Raw date reads like 'Jan 12, 77' but BA 59 and later career dates support OCR 3->7, i.e. 1937.",
  "amws16_B_1000_1200",    "2298",  "",              "",
  "Corrupted multi-entry OCR with parsed 1961; no safe birth-year correction visible.",
  "amws16_B_1000_1200",    "2302",  "",              "",
  "Corrupted multi-entry OCR with parsed 1961; no safe birth-year correction visible.",
  "amws16_B_1000_1200",    "2966",  "",              "",
  "Late year parsed from ambiguous OCR ('Nov67') in multi-entry text; no safe replacement visible.",
  "amws16_B_1000_1200",    "2968",  "",              "",
  "Late year parsed from ambiguous OCR ('Nov67') in multi-entry text; no safe replacement visible.",
  "amws16_B_200_400",      "632",   "Apr 2, 18",     "1918",
  "Raw date 'Apr 2...18...' and BS 40/MA 41 support 1918 rather than 1981.",
  "amws16_B_200_400",      "954",   "July 15, 26",   "1926",
  "Raw date token '261' and BA 48/MD 52 support 1926 rather than 1961.",
  "amws16_B_200_400",      "957",   "July 15, 26",   "1926",
  "Raw date token '261' and BA 48/MD 52 support 1926 rather than 1961.",
  "amws16_B_200_400",      "959",   "July 15, 26",   "1926",
  "Raw date token '261' and BA 48/MD 52 support 1926 rather than 1961.",
  "amws16_B_200_400",      "1117",  "Oct 29, 21",    "1921",
  "Raw date reads like 'Oct 29 71' but BS 43/MS 49 support OCR 2->7, i.e. 1921.",
  "amws16_B_200_400",      "2340",  "",              "",
  "Late year parsed from ambiguous OCR ('Sept 74'); no safe birth-year correction visible.",
  "amws16_B_400_600",      "40",    "Sept 17, 25",   "1925",
  "Raw date 'Sent 1775' is read as Sept 17, 25; BA 49/PhD 51 support 1925.",
  "amws16_B_400_600",      "112",   "Sept 7, 21",    "1921",
  "Raw date reads like 'Sept 7 71' but AB 48/MS 50/ScD 55 support OCR 2->7, i.e. 1921.",
  "amws16_B_400_600",      "1260",  "",              "",
  "Parsed 1961 from compact OCR ('Sept61'); no safe day/year split visible.",
  "amws16_B_400_600",      "1261",  "",              "",
  "Parsed 1961 from compact OCR ('Sept61'); no safe day/year split visible.",
  "amws16_B_400_600",      "1872",  "",              "",
  "Parsed 1963 from ambiguous OCR ('Aug 63'); no safe day/year split visible.",
  "amws16_B_400_600",      "2747",  "Mar 8, 28",     "1928",
  "Raw date reads like 'Mar 8 78' but MA 49/PhD 50 support OCR 2->7, i.e. 1928.",
  "amws16_B_600_800",      "485",   "",              "",
  "Late year parsed from OCR token 'MayBS'; no safe date/year correction visible.",
  "amws16_B_600_800",      "3106",  "May 24, 26",    "1926",
  "Raw date token '26I' and training/career dates support 1926 rather than 1961.",
  "amws16_B_800_1000",     "155",   "Sept 30, 27",   "1927",
  "Raw date reads like 'Sept 30 77' but Univ Minn 55-59/career dates support OCR 2->7, i.e. 1927.",
  "amws16_B_800_1000",     "157",   "Sept 30, 27",   "1927",
  "Raw date reads like 'Sept 30 77' but Univ Minn 55-59/career dates support OCR 2->7, i.e. 1927.",
  "amws16_B_800_1000",     "1066",  "July 2, 35",    "1935",
  "Raw token 'bS' is read as 35 in date context; PhD 71/career dates support 1935 rather than 1985.",
  "amws16_B_800_1000",     "1552",  "Aug 6, 17",     "1917",
  "Raw date token '17i' and AB 48/MS 50/PhD 59 support 1917 rather than 1971.",
  "amws16_E_0_200",        "896",   "",              "",
  "Parsed 'Sept 65-75' is an appointment period, not a birth date; blank birth date/year."
)

csv_text_cols <- cols(.default = col_character())
input <- read_csv(input_file, col_types = csv_text_cols,
                  show_col_types = FALSE) |>
  mutate(across(everything(), ~ ifelse(is.na(.x), "", .x)))

required_cols <- c("doc_id", "lineid", "birth_year", "birth_date",
                   "raw_text_adjusted")
missing_cols <- setdiff(required_cols, names(input))
if (length(missing_cols)) {
  stop("Input missing required columns: ", paste(missing_cols, collapse = ", "))
}

if (n_distinct(paste(input$doc_id, input$lineid)) != nrow(input)) {
  stop("Input has duplicated doc_id + lineid.")
}

to_apply <- corrections |>
  left_join(input |>
              select(doc_id, lineid, birth_year_old = birth_year,
                     birth_date_old = birth_date, raw_text_adjusted),
            by = c("doc_id", "lineid"))

if (any(is.na(to_apply$birth_year_old))) {
  stop("At least one manual correction key was not found in input.")
}

bad_year <- to_apply |>
  filter(nzchar(birth_year_new) &
           (!grepl("^[0-9]{4}$", birth_year_new) |
              as.integer(birth_year_new) < 1800L |
              as.integer(birth_year_new) > 1986L))
if (nrow(bad_year)) {
  stop("Invalid birth_year_new in manual corrections.")
}

idx <- match(paste(corrections$doc_id, corrections$lineid),
             paste(input$doc_id, input$lineid))
input$birth_date[idx] <- corrections$birth_date_new
input$birth_year[idx] <- corrections$birth_year_new

applied_log <- to_apply |>
  mutate(
    changed_birth_date = normalize_text(birth_date_old) !=
      normalize_text(birth_date_new),
    changed_birth_year = normalize_text(birth_year_old) !=
      normalize_text(birth_year_new)
  )

birth_year_int <- suppressWarnings(as.integer(input$birth_year))

summary <- bind_rows(
  tibble(metric = "input_rows", value = nrow(input)),
  tibble(metric = "manual_correction_rows", value = nrow(corrections)),
  tibble(metric = "changed_birth_date",
         value = sum(applied_log$changed_birth_date)),
  tibble(metric = "changed_birth_year",
         value = sum(applied_log$changed_birth_year)),
  tibble(metric = "birth_year_1986_remaining",
         value = sum(input$birth_year == "1986")),
  tibble(metric = "birth_year_1970plus_remaining",
         value = sum(!is.na(birth_year_int) & birth_year_int >= 1970L)),
  tibble(metric = "birth_year_1960plus_remaining",
         value = sum(!is.na(birth_year_int) & birth_year_int >= 1960L)),
  tibble(metric = "birth_year_before_1860_remaining",
         value = sum(!is.na(birth_year_int) & birth_year_int < 1860L)),
  tibble(metric = "max_birth_year",
         value = max(birth_year_int, na.rm = TRUE)),
  tibble(metric = "birth_year_1886",
         value = sum(input$birth_year == "1886"))
) |>
  mutate(value = as.numeric(value))

write_excel_csv(input, input_file, na = "")
write_excel_csv(applied_log, applied_log_csv, na = "")
write_excel_csv(summary, summary_csv, na = "")

cat("Applied birth-year manual corrections:", nrow(corrections), "\n")
cat("Changed birth_date:", sum(applied_log$changed_birth_date), "\n")
cat("Changed birth_year:", sum(applied_log$changed_birth_year), "\n")
cat("Remaining 1986 rows:", sum(input$birth_year == "1986"), "\n")
cat("Updated:", input_file, "\n")
cat("Wrote log:", applied_log_csv, "\n")
cat("Wrote summary:", summary_csv, "\n")
