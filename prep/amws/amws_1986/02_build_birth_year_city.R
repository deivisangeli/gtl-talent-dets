###############################################################################
# Add birth_year and birth_city to the regex-only AMWS edition 16 consolidated
# file.
#
# Input:
#   <Dropbox root>/output/amws/regex_all_docs/
#     amws_ed16_entries_regex_parsed.csv
#
# Outputs:
#   <Dropbox root>/output/amws/regex_all_docs/
#     amws_birth_year_city_sample_1000.csv
#     amws_birth_year_city_sample_1000.xlsx
#     amws_ed16_entries_regex_parsed_birth_year_city.csv
#     amws_ed16_entries_regex_parsed_birth_year_city.xlsx
#     amws_birth_year_city_summary.csv
#
# Environment overrides:
#   AMWS_REGEX_BIRTH_CITY_INPUT_FILE
#   AMWS_REGEX_BIRTH_CITY_OUTPUT_DIR
#   AMWS_REGEX_BIRTH_CITY_SAMPLE_N
#   AMWS_REGEX_BIRTH_CITY_SEED
###############################################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(tibble)
  library(writexl)
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
  DATA_INPUT <- file.path(TALENT_DETS_DATA_DIR, "input")
  DATA_OUTPUT <- file.path(TALENT_DETS_DATA_DIR, "output")
  AMWS_INPUT <- file.path(DATA_INPUT, "amws")
  AMWS_OUTPUT <- file.path(DATA_OUTPUT, "amws")
}

env_chr <- function(name, default = "") {
  value <- Sys.getenv(name, unset = "")
  if (nzchar(value)) value else default
}

env_int <- function(name, default) {
  value <- Sys.getenv(name, unset = "")
  if (!nzchar(value)) return(default)
  parsed <- suppressWarnings(as.integer(value))
  if (is.na(parsed)) {
    stop("Environment variable ", name, " must be an integer; got: ", value)
  }
  parsed
}

blank_na <- function(x) {
  x <- as.character(x)
  ifelse(is.na(x), "", x)
}

normalize_text <- function(x) {
  blank_na(x) |>
    str_replace_all("\u00a0", " ") |>
    str_replace_all("\\s+", " ") |>
    str_trim()
}

strip_edge_punct <- function(x) {
  normalize_text(x) |>
    str_replace("^[,;:.\\s'\"’“”\\-_*\\^]+", "") |>
    str_replace("[,;:.\\s'\"’“”\\-_*\\^]+$", "") |>
    normalize_text()
}

normalize_ocr_digits <- function(x) {
  chartr("OoIiLlSsZzBb", "001111552288", x)
}

amws_century_year <- function(yy) {
  yy <- as.integer(yy)
  ifelse(yy <= 86L, 1900L + yy, 1800L + yy)
}

parse_birth_year_one <- function(birth_date) {
  x <- normalize_text(birth_date)
  if (!nzchar(x)) {
    return(tibble(birth_year = NA_integer_,
                  birth_year_parse_flag = "no_birth_date"))
  }

  x <- str_replace(x, "[,;:.\\s]+$", "")
  year_token <- str_match(x, "([0-9OoIiLlSsZzBb]{2,4})[A-Za-z]?$")[, 2]
  if (is.na(year_token) || !nzchar(year_token)) {
    return(tibble(birth_year = NA_integer_,
                  birth_year_parse_flag = "no_year_token"))
  }

  normalized <- normalize_ocr_digits(year_token)
  if (!str_detect(normalized, "^[0-9]+$")) {
    return(tibble(birth_year = NA_integer_,
                  birth_year_parse_flag = "year_ocr_unresolved"))
  }

  if (nchar(normalized) == 2L) {
    year <- amws_century_year(normalized)
    flag <- "ok_2digit_amws"
  } else if (nchar(normalized) == 4L) {
    year <- as.integer(normalized)
    flag <- "ok_4digit"
    if (!is.na(year) && (year < 1800L || year > 1986L)) {
      compact_day <- suppressWarnings(as.integer(str_sub(normalized, 1, 2)))
      compact_yy <- str_sub(normalized, 3, 4)
      if (str_detect(year_token, "[A-Za-z]")) {
        year <- amws_century_year(str_sub(normalized, 1, 2))
        flag <- "ok_2digit_ocr_suffix"
      } else if (!is.na(compact_day) && compact_day >= 1L && compact_day <= 31L) {
        year <- amws_century_year(compact_yy)
        flag <- "ok_compact_day_year"
      }
    }
  } else if (nchar(normalized) == 3L) {
    compact_day <- suppressWarnings(as.integer(str_sub(normalized, 1, 1)))
    compact_yy <- str_sub(normalized, 2, 3)
    if (!is.na(compact_day) && compact_day >= 1L && compact_day <= 9L) {
      year <- amws_century_year(compact_yy)
      flag <- "ok_compact_day_year"
    } else {
      year <- as.integer(normalized)
      flag <- "year_token_implausible_length"
    }
  } else {
    year <- as.integer(normalized)
    flag <- "year_token_implausible_length"
  }

  if (is.na(year) || year < 1800L || year > 1986L) {
    return(tibble(birth_year = NA_integer_,
                  birth_year_parse_flag = "year_implausible"))
  }

  tibble(birth_year = as.integer(year), birth_year_parse_flag = flag)
}

state_or_region_tokens <- c(
  "Ala", "Alaska", "Ariz", "Ark", "Calif", "Colo", "Conn", "Del",
  "DC", "Fla", "Ga", "Hawaii", "Idaho", "Ill", "111", "Ind", "Iowa",
  "Kans", "Ky", "La", "Maine", "Md", "Mass", "Mich", "Minn", "Miss",
  "Mo", "Mont", "Nebr", "Nev", "NH", "NJ", "NMex", "NY", "NC", "NDak",
  "Ohio", "Okla", "Ore", "Pa", "RI", "SC", "SDak", "Tenn", "Tex",
  "Utah", "Vt", "Va", "Wash", "WVa", "Wis", "Wyo", "Ont", "Que",
  "BC", "NS", "NB", "Alta", "Sask", "Man", "PEI", "PR"
)

broad_place_tokens <- c(
  state_or_region_tokens,
  "Africa", "Argentina", "Austria", "Belgium", "Brazil", "Canada",
  "China", "CZ", "Denmark", "Egypt", "England", "Eng", "France", "Germany",
  "Ger", "Gcr", "Greece", "Hungary", "India", "Iran", "Iraq", "Ireland",
  "Israel", "Italy", "Japan", "Korea", "Lebanon", "Mexico", "Mex", "Netherlands",
  "Norway", "Pakistan", "Palestine", "Poland", "Portugal", "Russia",
  "Scotland", "Spain", "Sweden", "Switzerland", "Syria", "Taiwan",
  "Turkey", "UK", "US", "USA", "USSR", "Wales"
)

bad_place_markers <- regex(
  paste(
    "\\bEduc\\b", "\\bProf\\s+Exp\\b", "\\bConcurrent\\s+Pos\\b",
    "\\bHonors\\b", "\\bMailing\\s+Add\\b", "\\bPhD\\b", "\\bUniv\\b",
    "\\bMem\\b", "\\bRes\\b", "\\bAssoc\\b", "\\bSCIENCE\\b",
    "\\bCHEMISTRY\\b", "\\bPHYSICS\\b", "\\bMEDICINE\\b",
    sep = "|"
  ),
  ignore_case = TRUE
)

clean_city_candidate <- function(x) {
  x <- strip_edge_punct(x)
  x <- str_replace_all(x, "\\s+", " ")
  x <- str_replace(
    x,
    regex(paste0("\\s+(", paste(state_or_region_tokens, collapse = "|"),
                 ")$"), ignore_case = TRUE),
    ""
  )
  strip_edge_punct(x)
}

parse_birth_city_one <- function(birth_place) {
  x <- normalize_text(birth_place)
  if (!nzchar(x)) {
    return(tibble(birth_city = "", birth_city_parse_flag = "no_birth_place"))
  }

  x <- str_replace_all(x, "\\s*,\\s*", ", ")
  x <- strip_edge_punct(x)

  if (nchar(x) > 140L || str_detect(x, bad_place_markers)) {
    return(tibble(birth_city = "",
                  birth_city_parse_flag = "birth_place_contaminated"))
  }

  if (str_detect(x, ",")) {
    city <- str_split_fixed(x, ",", 2)[, 1]
    city <- clean_city_candidate(city)
    flag <- "ok_before_comma"
  } else if (str_detect(x, "\\.\\s*[A-Za-z0-9]{1,15}$")) {
    city <- str_split_fixed(x, "\\.", 2)[, 1]
    city <- clean_city_candidate(city)
    flag <- "ok_before_period"
  } else {
    city <- clean_city_candidate(x)
    flag <- "single_component"
  }

  if (!nzchar(city) || nchar(city) < 3L ||
      str_detect(city, regex("^[^A-Za-z]+$"))) {
    return(tibble(birth_city = "",
                  birth_city_parse_flag = "city_empty_after_cleaning"))
  }

  if (str_detect(city, regex(paste0("^(", paste(state_or_region_tokens,
                                             collapse = "|"), ")$"),
                           ignore_case = TRUE)) ||
      (flag == "single_component" &&
       str_detect(city, regex(paste0("^(", paste(broad_place_tokens,
                                                collapse = "|"), ")$"),
                              ignore_case = TRUE)))) {
    return(tibble(birth_city = "",
                  birth_city_parse_flag = "place_only_no_city"))
  }

  if (nchar(city) > 70L || str_detect(city, bad_place_markers)) {
    return(tibble(birth_city = "",
                  birth_city_parse_flag = "city_contaminated"))
  }

  tibble(birth_city = city, birth_city_parse_flag = flag)
}

add_birth_year_city <- function(data) {
  parse_birth_year_vec <- function(birth_date) {
    x <- normalize_text(birth_date)
    birth_year <- rep(NA_integer_, length(x))
    flag <- ifelse(nzchar(x), "no_year_token", "no_birth_date")

    x_clean <- str_replace(x, "[,;:.\\s]+$", "")
    year_token <- str_match(x_clean, "([0-9OoIiLlSsZzBb]{2,4})[A-Za-z]?$")[, 2]
    has_token <- !is.na(year_token) & nzchar(year_token)
    normalized <- rep("", length(x))
    normalized[has_token] <- normalize_ocr_digits(year_token[has_token])

    digit_token <- has_token & str_detect(normalized, "^[0-9]+$")
    flag[has_token & !digit_token] <- "year_ocr_unresolved"

    token_len <- nchar(normalized)

    idx2 <- digit_token & token_len == 2L
    birth_year[idx2] <- amws_century_year(normalized[idx2])
    flag[idx2] <- "ok_2digit_amws"

    idx4 <- digit_token & token_len == 4L
    full_year <- suppressWarnings(as.integer(normalized[idx4]))
    idx4_pos <- which(idx4)
    plausible_full <- !is.na(full_year) & full_year >= 1800L & full_year <= 1986L
    if (length(idx4_pos)) {
      birth_year[idx4_pos[plausible_full]] <- full_year[plausible_full]
      flag[idx4_pos[plausible_full]] <- "ok_4digit"

      implausible_pos <- idx4_pos[!plausible_full]
      if (length(implausible_pos)) {
        has_letter <- str_detect(year_token[implausible_pos], "[A-Za-z]")
        if (any(has_letter)) {
          letter_pos <- implausible_pos[has_letter]
          birth_year[letter_pos] <- amws_century_year(
            str_sub(normalized[letter_pos], 1, 2)
          )
          flag[letter_pos] <- "ok_2digit_ocr_suffix"
        }

        compact_pos <- implausible_pos[!has_letter]
        if (length(compact_pos)) {
          compact_day <- suppressWarnings(as.integer(str_sub(normalized[compact_pos], 1, 2)))
          compact_ok <- !is.na(compact_day) & compact_day >= 1L & compact_day <= 31L
          if (any(compact_ok)) {
            good_pos <- compact_pos[compact_ok]
            birth_year[good_pos] <- amws_century_year(
              str_sub(normalized[good_pos], 3, 4)
            )
            flag[good_pos] <- "ok_compact_day_year"
          }
        }
      }
    }

    idx3 <- digit_token & token_len == 3L
    if (any(idx3)) {
      idx3_pos <- which(idx3)
      compact_day <- suppressWarnings(as.integer(str_sub(normalized[idx3_pos], 1, 1)))
      compact_ok <- !is.na(compact_day) & compact_day >= 1L & compact_day <= 9L
      if (any(compact_ok)) {
        good_pos <- idx3_pos[compact_ok]
        birth_year[good_pos] <- amws_century_year(
          str_sub(normalized[good_pos], 2, 3)
        )
        flag[good_pos] <- "ok_compact_day_year"
      }
      if (any(!compact_ok)) {
        flag[idx3_pos[!compact_ok]] <- "year_token_implausible_length"
      }
    }

    other_digit <- digit_token & !(token_len %in% c(2L, 3L, 4L))
    flag[other_digit] <- "year_token_implausible_length"

    implausible <- !is.na(birth_year) &
      (birth_year < 1800L | birth_year > 1986L)
    birth_year[implausible] <- NA_integer_
    flag[implausible] <- "year_implausible"

    tibble(birth_year = birth_year, birth_year_parse_flag = flag)
  }

  parse_birth_city_vec <- function(birth_place) {
    x <- normalize_text(birth_place)
    birth_city <- rep("", length(x))
    flag <- ifelse(nzchar(x), "single_component", "no_birth_place")

    x <- str_replace_all(x, "\\s*,\\s*", ", ")
    x <- strip_edge_punct(x)

    contaminated <- nzchar(x) &
      (nchar(x) > 140L | str_detect(x, bad_place_markers))
    flag[contaminated] <- "birth_place_contaminated"

    eligible <- nzchar(x) & !contaminated
    has_comma <- eligible & str_detect(x, ",")
    has_period <- eligible & !has_comma & str_detect(x, "\\.\\s*[A-Za-z0-9]{1,15}$")
    single <- eligible & !has_comma & !has_period

    birth_city[has_comma] <- str_replace(x[has_comma], ",.*$", "")
    flag[has_comma] <- "ok_before_comma"

    birth_city[has_period] <- str_replace(x[has_period], "\\..*$", "")
    flag[has_period] <- "ok_before_period"

    birth_city[single] <- x[single]
    flag[single] <- "single_component"

    birth_city <- clean_city_candidate(birth_city)

    empty_after <- eligible &
      (!nzchar(birth_city) | nchar(birth_city) < 3L |
         str_detect(birth_city, regex("^[^A-Za-z]+$")))
    birth_city[empty_after] <- ""
    flag[empty_after] <- "city_empty_after_cleaning"

    state_only <- eligible & !empty_after &
      str_detect(birth_city,
                 regex(paste0("^(", paste(state_or_region_tokens,
                                          collapse = "|"), ")$"),
                       ignore_case = TRUE))
    broad_single <- single & !empty_after &
      str_detect(birth_city,
                 regex(paste0("^(", paste(broad_place_tokens,
                                          collapse = "|"), ")$"),
                       ignore_case = TRUE))
    place_only <- state_only | broad_single
    birth_city[place_only] <- ""
    flag[place_only] <- "place_only_no_city"

    city_bad <- eligible & !empty_after & !place_only &
      (nchar(birth_city) > 70L | str_detect(birth_city, bad_place_markers))
    birth_city[city_bad] <- ""
    flag[city_bad] <- "city_contaminated"

    birth_city[contaminated] <- ""

    tibble(birth_city = birth_city, birth_city_parse_flag = flag)
  }

  year_tbl <- parse_birth_year_vec(data$birth_date)
  city_tbl <- parse_birth_city_vec(data$birth_place)
  bind_cols(data, year_tbl, city_tbl)
}

output_dir <- env_chr(
  "AMWS_REGEX_BIRTH_CITY_OUTPUT_DIR",
  file.path(DATA_OUTPUT, "amws", "regex_all_docs")
)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
output_dir <- normalizePath(output_dir, winslash = "/", mustWork = TRUE)

input_file <- env_chr(
  "AMWS_REGEX_BIRTH_CITY_INPUT_FILE",
  file.path(output_dir, "amws_ed16_entries_regex_parsed.csv")
)
input_file <- normalizePath(input_file, winslash = "/", mustWork = TRUE)

sample_n <- env_int("AMWS_REGEX_BIRTH_CITY_SAMPLE_N", 1000L)
sample_seed <- env_int("AMWS_REGEX_BIRTH_CITY_SEED", 20260701L)

sample_csv <- file.path(output_dir, "amws_birth_year_city_sample_1000.csv")
sample_xlsx <- file.path(output_dir, "amws_birth_year_city_sample_1000.xlsx")
final_csv <- file.path(
  output_dir,
  "amws_ed16_entries_regex_parsed_birth_year_city.csv"
)
final_xlsx <- file.path(
  output_dir,
  "amws_ed16_entries_regex_parsed_birth_year_city.xlsx"
)
summary_csv <- file.path(output_dir, "amws_birth_year_city_summary.csv")

input <- readr::read_csv(input_file, show_col_types = FALSE,
                         col_types = cols(.default = col_character()))

required_cols <- c("birth_place", "birth_date", "raw_text")
missing_cols <- setdiff(required_cols, names(input))
if (length(missing_cols)) {
  stop("Input file is missing columns: ", paste(missing_cols, collapse = ", "))
}

set.seed(sample_seed)
sampled <- input |>
  slice_sample(n = min(sample_n, nrow(input))) |>
  arrange(doc_id, as.integer(lineid)) |>
  mutate(sample_id = row_number(), .before = 1)

sample_out <- add_birth_year_city(sampled)
final_out <- add_birth_year_city(input)

summary_tbl <- bind_rows(
  tibble(metric = "input_file", value = input_file),
  tibble(metric = "rows", value = as.character(nrow(final_out))),
  tibble(metric = "sample_seed", value = as.character(sample_seed)),
  tibble(metric = "sample_rows", value = as.character(nrow(sample_out))),
  tibble(metric = "birth_date_nonempty",
         value = as.character(sum(nzchar(normalize_text(final_out$birth_date))))),
  tibble(metric = "birth_place_nonempty",
         value = as.character(sum(nzchar(normalize_text(final_out$birth_place))))),
  tibble(metric = "birth_year_nonempty",
         value = as.character(sum(!is.na(final_out$birth_year)))),
  tibble(metric = "birth_city_nonempty",
         value = as.character(sum(nzchar(final_out$birth_city)))),
  final_out |>
    count(birth_year_parse_flag, name = "value") |>
    transmute(metric = paste0("birth_year_flag_", birth_year_parse_flag),
              value = as.character(value)),
  final_out |>
    count(birth_city_parse_flag, name = "value") |>
    transmute(metric = paste0("birth_city_flag_", birth_city_parse_flag),
              value = as.character(value))
)

readr::write_excel_csv(sample_out, sample_csv, na = "")
writexl::write_xlsx(list(sample = sample_out), sample_xlsx)
readr::write_excel_csv(final_out, final_csv, na = "")
writexl::write_xlsx(list(entries = final_out), final_xlsx)
readr::write_excel_csv(summary_tbl, summary_csv, na = "")

if (nrow(final_out) != nrow(input)) {
  stop("Output row count differs from input row count.")
}

if (any(!is.na(final_out$birth_year) &
        (final_out$birth_year < 1800L | final_out$birth_year > 1986L))) {
  stop("At least one parsed birth_year is outside 1800-1986.")
}

cat("Wrote sample CSV:", sample_csv, "\n")
cat("Wrote sample XLSX:", sample_xlsx, "\n")
cat("Wrote final CSV:", final_csv, "\n")
cat("Wrote final XLSX:", final_xlsx, "\n")
cat("Wrote summary:", summary_csv, "\n")
cat("Rows:", nrow(final_out), "\n")
cat("Birth year nonempty:", sum(!is.na(final_out$birth_year)), "\n")
cat("Birth city nonempty:", sum(nzchar(final_out$birth_city)), "\n")
cat("Birth year flags:\n")
print(final_out |> count(birth_year_parse_flag, sort = TRUE))
cat("Birth city flags:\n")
print(final_out |> count(birth_city_parse_flag, sort = TRUE))
