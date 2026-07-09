###############################################################################
# Apply explicit AMWS edition birth-year manual corrections.
#
# Reads/writes:
#   output/amws/
#     amws_1938_cleaned_corrected.csv
#
# Writes logs:
#   output/amws/
#     amws_1938_birth_year_manual_corrections_applied_log.csv
#     amws_1938_birth_year_manual_corrections_summary.csv
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

normalize_text <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  trimws(gsub("\\s+", " ", x))
}

edition <- "1938"
input_file <- file.path(AMWS_OUTPUT, "amws_1938_cleaned_corrected.csv")
applied_log_csv <- file.path(
  AMWS_OUTPUT,
  "amws_1938_birth_year_manual_corrections_applied_log.csv"
)
summary_csv <- file.path(
  AMWS_OUTPUT,
  "amws_1938_birth_year_manual_corrections_summary.csv"
)

corrections <- tribble(
  ~lineid, ~date_new,      ~birth_year_new, ~correction_note,
  "1598",  "Dec. 14, 73", "1873",
  "Raw text has 'Dae. 14, 73'; OCR/date parser incorrectly captured later education years as 9293.",
  "5634",  "Dec. 17, 92", "1892",
  "Raw text has 'Doc. 17, 92'; OCR/date parser incorrectly captured later education text as 1013.",
  "8744",  "79",          "1879",
  "Raw text has birthplace followed by '79'; parser incorrectly captured education years 99-02 as 9902.",
  "9864",  "Dec. 28, 67", "1867",
  "Raw text has 'Dec.28. 67'; parser incorrectly captured fellowship years 91-92 as 9192.",
  "9980",  "Oct. 91",     "1891",
  "Raw text has a corrupted October birth date with visible year 91; parser incorrectly captured career years 26-29 as 2629.",
  "13496", "April 19, 81", "1881",
  "Raw text has 'April 1981', read as April 19, 81; parser treated it as four-digit 1981.",
  "24255", "07",          "1907",
  "Raw text has birthplace followed by '07'; parser incorrectly captured education years 26-27 as 2627.",
  "17556", "Dec. 11, 91", "1891",
  "Raw text has 'Elkhart Co, Ind, Dec. 11, 91'; parser incorrectly captured high-school graduation year 10.",
  "5989",  "89",          "1889",
  "Raw text has 'Cincinnati, Ohio, 89'; parser incorrectly captured Kenyon College degree year 12.",
  "17012", "Sept. 6, 91", "1891",
  "Raw text has 'Dallas Co, Texas, Sept. 6, 91'; parser incorrectly captured teachers college certificate year 12.",
  "3145",  "97",          "1897",
  "Raw text has 'Baltimore, Md, 97'; parser incorrectly captured College of Notre Dame year 13.",
  "8453",  "June ?, 91",  "1891",
  "Raw text OCR has 'Eureka, Utah, June ..., 91'; parser incorrectly captured Chicago degree year 13.",
  "10343", "91",          "1891",
  "Raw text has 'Shamokin, Pa, 91'; parser incorrectly captured Lafayette Ph.B. year 13.",
  "14555", "",            "",
  "Suspect birth year removed: raw text/OCR begins 'Galva, Ill, 19-'; 1913 is a degree/career year and no safe birth year is recoverable.",
  "16513", "91",          "1891",
  "Raw text has 'Auxvasse, Mo, 91'; parser incorrectly captured Westminster College year 13.",
  "24121", "81",          "1881",
  "Raw text has 'Golconda, Ill, 81'; parser incorrectly captured Illinois State Normal B.Ed. year 13.",
  "79",    "March 28, 98", "1898",
  "Raw text has 'Delaware, N. J. March 28, 98'; parser incorrectly captured Mt. Holyoke degree year 14.",
  "1779",  "92",          "1892",
  "Raw text has 'Ft. Worth, Texas, 92'; parser incorrectly captured Baylor degree year 14.",
  "5453",  "Dec. 22, 97", "1897",
  "Raw text/OCR has 'Stoughton, Mass, Dec. 22, 97'; parser incorrectly captured Denver degree year 14.",
  "6933",  "85",          "1885",
  "Raw text has 'Irvington, Ind, 85'; parser incorrectly captured M.E. year 14.",
  "9479",  "90",          "1890",
  "Raw text has 'Selinsgrove, Pa, 90'; parser incorrectly captured Phar.D. year 14.",
  "13466", "Feb. 18, 84", "1884",
  "Raw text likely has 'Williamsport, Pa, Feb. 18, 84'; parser incorrectly captured malformed date/degree boundary as 1914.",
  "22795", "88",          "1888",
  "Raw text has 'Chicago, Ill, 88'; parser incorrectly captured conservatory year 14.",
  "4798",  "87",          "1887",
  "Raw text has 'Sumner, Ill, 87'; parser incorrectly captured Illinois State Normal Ed.B. year 15.",
  "5176",  "Aug. 5, 89",  "1889",
  "Raw text/OCR has 'Cincinnati, Ohio, Aug. 5, 89'; parser incorrectly captured Hopkins M.D. year 15.",
  "15356", "90",          "1890",
  "Raw text has 'Hamilton Co, Nebr, 90'; parser incorrectly captured Nebraska A.S. year 15.",
  "19015", "93",          "1893",
  "Raw text has 'Bee Co, Texas, 93'; parser incorrectly captured Texas E.E. year 15.",
  "7280",  "Oct. 1, 96",  "1896",
  "Raw text/OCR has 'Crystal Springs, Miss, Oct. 1, 96'; parser incorrectly captured Mississippi State College year 16.",
  "3729",  "Jan. ?, 90",  "1890",
  "Raw text/OCR has 'Silverton, Colo, Jan. ..., 90'; parser incorrectly captured Colorado degree year 17.",
  "4978",  "Jan. 4, 91",  "1891",
  "Raw text has 'Springfield, Mass, Jan. 4, 91'; parser incorrectly captured Mass. College M.Sc. year 17.",
  "5396",  "Sept. 22, 77", "1877",
  "Raw text has 'Aurora, Ill, Sept. 22, 77'; parser incorrectly captured Harvard years/career boundary as 17.",
  "13686", "May 28, 92",  "1892",
  "Raw text/OCR has 'Brooklyn, N. Y, May 28, 92'; parser incorrectly captured Cornell B.S. year 17.",
  "14576", "17",          "",
  "Suspect birth year removed: raw text has 'Waynesburg, Ohio, 17' followed by M.D. Ohio State, 92; no safe birth year is recoverable.",
  "1079",  "July ?, 04",  "1904",
  "Raw text has 'Washington, D. C. July ..., 04'; parser incorrectly captured Lehigh degree year 25.",
  "883",   ", 22",        "",
  "Suspect birth year removed: raw text has 'D.D.S, N. Y. Col. Dentistry, 22'; no recoverable birth year in preserved text.",
  "968",   "Feb. 28, 68", "1868",
  "Raw text has 'Philadelphia, Pa, Feb. 28, 68'; parser incorrectly captured Ph.M. year 19.",
  "4476",  "Feb. 22, 04", "1904",
  "Raw text has 'Dayton, Ohio, Feb. 22, ...'; degree chronology supports 1904, not Otterbein degree year 25.",
  "9758",  "90",          "1890",
  "Raw text has 'Grass Lake, Mich, 90'; parser incorrectly captured State College Washington/DVM degree year 25.",
  "2699",  "Feb. ?, 01",  "1901",
  "Raw text has 'Shelocta, Pa, Feb. ..., 01'; parser incorrectly captured Whitman College degree year 24.",
  "3509",  ", 20",        "",
  "Suspect birth year removed: OCR text is too corrupted to recover birth year; parser likely captured degree/career year 20.",
  "4675",  ", 24",        "",
  "Suspect birth year removed: raw text has 'B.S, California, 24'; no recoverable birth year in preserved text.",
  "25441", "May 1, 04",   "1904",
  "Raw text has 'Newton, Mass, May 1, 04'; parser incorrectly captured Radcliffe degree year 24.",
  "2520",  "Aug. 10, 01", "1901",
  "Raw text has 'Philadelphia, Pa, Aug. 10, 01'; parser incorrectly captured Pa. State degree year 23.",
  "7829",  "May 8, 98",   "1898",
  "Raw text has 'Terre Haute, Ind, May 8, 98'; parser incorrectly captured Indiana State Teachers College degree year 23.",
  "8123",  "May 14, 02",  "1902",
  "Raw text has 'Chicago, Ill, May 14, 02'; parser incorrectly captured Bryn Mawr degree year 23.",
  "5771",  "95",          "1895",
  "Raw text has 'Winthrop, Iowa, 95'; parser incorrectly captured Montreal S.T.L. year 22.",
  "9219",  "March 22, 99", "1899",
  "Raw text has 'South Bend, Ind, March 22, 99'; parser incorrectly captured Illinois M.S. year 22.",
  "7402",  "April 26, 00", "1900",
  "Raw text has 'Oberlin, Ohio, April 26, 00'; parser incorrectly captured Swarthmore degree year 21.",
  "8042",  "Oct. 03, 93", "1893",
  "Raw text has 'Clinton, Mo, Oct. 03, 93'; parser incorrectly captured Illinois M.S. year 21.",
  "9436",  "Sept. 21, 98", "1898",
  "Raw text has 'Marysville, Ohio, Sept. 21, 98'; parser incorrectly captured Case degree year 21.",
  "10394", "Jan. 18, 01", "1901",
  "Raw text/OCR has 'Gig Harbor, Wash, Jan. 18, 01'; parser incorrectly captured the corrupted date as 21.",
  "13052", "March 4, 91", "1891",
  "Raw text/OCR has 'Woodstown, N. J, March 4, 91'; parser incorrectly captured date/education boundary as 21.",
  "5725",  "Feb. 3, 95",  "1895",
  "Raw text/OCR has 'Hartford City, Ind, Feb. 3, 95'; parser incorrectly captured West Virginia A.B. year 18.",
  "18261", "Nov. 17, 18", "",
  "Suspect birth year removed: raw text has 'Nov. 17, 18' but education/career chronology makes 1918 impossible; no safe correction in preserved text.",
  "24062", "June 25, 96", "1896",
  "Raw text/OCR has 'Macon, Ga, June 25, 96'; parser incorrectly captured Georgia B.S. year 18.",
  "2034",  "94",          "1894",
  "Raw text has 'Davenport, Iowa, 94'; parser incorrectly captured California B.S. year 20.",
  "5681",  "97",          "1897",
  "Raw text has 'Waterloo, Ind, ... 97'; parser incorrectly captured Wisconsin degree year 20.",
  "8697",  "April 4, 97", "1897",
  "Raw text has 'Richfield, Ohio, April 4, 97'; parser incorrectly captured Oberlin degree year 20.",
  "11844", "Feb. 24, 97", "1897",
  "Raw text has 'Neosho Co, Kans, Feb. 24, 97'; parser incorrectly captured Kansas degree year 20.",
  "14243", "May 29, 19",  "",
  "Suspect birth year removed: raw text has 'May 29, 19' but education/career chronology makes 1919 impossible; no safe correction in preserved text.",
  "14955", "Oct. 98",     "1898",
  "Raw text has 'Armour, S. Dak, Oct. 98'; parser incorrectly retained trailing OCR/year artifact 19.",
  "15330", "June 23, 94", "1894",
  "Raw text/name identify Alfred C. Kinsey, born Hoboken, N.J., June 23, 1894; parser captured Harvard fellowship/Sc.D. year 20.",
  "15069", "Dec. 19, 19", "",
  "Suspect birth year removed: raw text has 'Dec. 19, 19' but MIT/Columbia chronology makes 1919 impossible; no safe correction in preserved text.",
  "5947",  "May",         "",
  "Suspect birth year removed: raw text has 'La Crosse, Va, May ... B, Richmond Col, 12'; parser captured a college year, and no safe birth year is recoverable.",
  "8536",  "",            "",
  "Suspect birth year removed: raw text has Cornell/Columbia degree years 12-13; parser captured a degree year, and no safe birth year is recoverable.",
  "8660",  "",            "",
  "Suspect birth year removed: raw text has Rensselaer/RPI degree years 10-11; parser captured a degree year, and no safe birth year is recoverable.",
  "12904", "Jun. 11, 10", "1910",
  "US-born 1910s review: raw text clearly has 'St. Louis, Mo, Jun. 11, 10'; parser kept only trailing year.",
  "2993",  "Sept. 1, 10", "1910",
  "US-born 1910s review: raw text clearly has 'Ansley, Nebr, Sept, 1, 10'; parser kept only trailing year.",
  "24759", "Feb. 17, 11", "1911",
  "US-born 1910s review: raw text has OCR 'Fek. 17, 11', interpreted as Feb. 17, 1911; parser kept only trailing year.",
  "11505", "March 2, 12", "1912",
  "US-born 1910s review: raw text has OCR 'Marclt 2, 12', interpreted as March 2, 1912; parser kept only trailing year.",
  "5889",  "Oct. 29",     "",
  "US-born 1910s review: raw text has 'Kiallo, Calif, Oct. 29, ... B.S California, 17'; parser captured degree year 17 and no safe birth year is recoverable.",
  "6052",  "Dec. 24",     "",
  "US-born 1910s review: raw text has impossible chronology for 1910 birth, with Pennsylvania degree in 03 and instructor role in 04-11; no safe birth year is recoverable.",
  "19614", "97",          "1897",
  "US-born 1910s review: raw text has 'N. J, 97. B.Sc, McGill, 19'; parser captured McGill degree year 19."
)

input <- read_csv(input_file, col_types = cols(.default = col_character()),
                  show_col_types = FALSE) |>
  mutate(across(everything(), ~ ifelse(is.na(.x), "", .x)))

required_cols <- c("lineid", "AMSname", "birthplace_orig", "date", "birth_year")
missing_cols <- setdiff(required_cols, names(input))
if (length(missing_cols)) {
  stop("Input missing required columns: ", paste(missing_cols, collapse = ", "))
}

if (n_distinct(input$lineid) != nrow(input)) {
  stop("Input has duplicated lineid.")
}

to_apply <- corrections |>
  left_join(
    input |>
      select(lineid, AMSname, birthplace_orig, date_old = date,
             birth_year_old = birth_year),
    by = "lineid"
  )

if (any(is.na(to_apply$birth_year_old))) {
  stop("At least one manual correction key was not found in input.")
}

bad_year <- to_apply |>
  mutate(birth_year_new_int = suppressWarnings(as.integer(birth_year_new))) |>
  filter(
    birth_year_new != "" &
      (!grepl("^[0-9]{4}$", birth_year_new) |
         birth_year_new_int < 1800L |
         birth_year_new_int > as.integer(edition))
  )
if (nrow(bad_year)) {
  stop("Invalid birth_year_new in manual corrections.")
}

idx <- match(corrections$lineid, input$lineid)
input$date[idx] <- corrections$date_new
input$birth_year[idx] <- corrections$birth_year_new

applied_log <- to_apply |>
  mutate(
    edition = edition,
    changed_date = normalize_text(date_old) != normalize_text(date_new),
    changed_birth_year = normalize_text(birth_year_old) !=
      normalize_text(birth_year_new)
  ) |>
  select(
    edition, lineid, AMSname, birthplace_orig,
    date_old, birth_year_old, date_new, birth_year_new,
    changed_date, changed_birth_year, correction_note
  )

birth_year_int <- suppressWarnings(as.integer(input$birth_year))
summary <- bind_rows(
  tibble(metric = "input_rows", value = nrow(input)),
  tibble(metric = "manual_correction_rows", value = nrow(corrections)),
  tibble(metric = "changed_date", value = sum(applied_log$changed_date)),
  tibble(metric = "changed_birth_year",
         value = sum(applied_log$changed_birth_year)),
  tibble(metric = "birth_year_before_1800_remaining",
         value = sum(!is.na(birth_year_int) & birth_year_int < 1800L)),
  tibble(metric = "birth_year_after_edition_remaining",
         value = sum(!is.na(birth_year_int) & birth_year_int > as.integer(edition))),
  tibble(metric = "min_birth_year",
         value = min(birth_year_int, na.rm = TRUE)),
  tibble(metric = "max_birth_year",
         value = max(birth_year_int, na.rm = TRUE))
) |>
  mutate(value = as.numeric(value))

write_excel_csv(input, input_file, na = "")
write_excel_csv(applied_log, applied_log_csv, na = "")
write_excel_csv(summary, summary_csv, na = "")

cat("Applied AMWS", edition, "birth-year manual corrections:",
    nrow(corrections), "\n")
cat("Changed date:", sum(applied_log$changed_date), "\n")
cat("Changed birth_year:", sum(applied_log$changed_birth_year), "\n")
cat("Remaining years before 1800:",
    sum(!is.na(birth_year_int) & birth_year_int < 1800L), "\n")
cat("Remaining years after edition:",
    sum(!is.na(birth_year_int) & birth_year_int > as.integer(edition)), "\n")
cat("Updated:", input_file, "\n")
cat("Wrote log:", applied_log_csv, "\n")
cat("Wrote summary:", summary_csv, "\n")
