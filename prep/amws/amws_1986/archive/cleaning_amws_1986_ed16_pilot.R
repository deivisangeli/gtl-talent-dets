###############################################################################
# Pilot cleaner for AMWS edition 16 (1986) Word transcriptions.
#
# Input:
#   <Dropbox root>/Data/amws ed 16/16_A_0_200.docx
#
# Output:
#   <Dropbox root>/output/amws/amws_1986_ed16_A_0_200_pilot.csv
#
# Pilot scope:
#   Start at the first clear entry ("AABOE, ...") and process the first 10
#   detected Word page/chunk spans from that point.
###############################################################################

suppressPackageStartupMessages({
  library(xml2)
  library(stringr)
  library(dplyr)
  library(readr)
})

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_arg)) {
  script_path <- normalizePath(sub("^--file=", "", script_arg[1]),
                               winslash = "/", mustWork = TRUE)
  repo_root <- normalizePath(file.path(dirname(script_path), "..", "..", "..", ".."),
                             winslash = "/", mustWork = TRUE)
} else {
  repo_root <- normalizePath(Sys.getenv("GTL_REPO", unset = getwd()),
                             winslash = "/", mustWork = TRUE)
}

source(file.path(repo_root, "prep", "amws", "state_alias.R"))
source(file.path(repo_root, "paths.R"))

# paths.R is machine-specific in older checkouts. If it points to a missing
# Dropbox root, fall back to the current Windows user's canonical Dropbox path.
local_dropbox <- file.path("C:/Users", Sys.info()[["user"]],
                           "Globtalent Dropbox", "gtl_talent_dets")
if (!dir.exists(TALENT_DETS_DATA_DIR) && dir.exists(local_dropbox)) {
  TALENT_DETS_DATA_DIR <- normalizePath(local_dropbox, winslash = "/")
  DATA_INPUT <- file.path(TALENT_DETS_DATA_DIR, "input")
  DATA_OUTPUT <- file.path(TALENT_DETS_DATA_DIR, "output")
  AMWS_INPUT <- file.path(DATA_INPUT, "amws")
  AMWS_OUTPUT <- file.path(DATA_OUTPUT, "amws")
}

input_file <- file.path(TALENT_DETS_DATA_DIR, "Data", "amws ed 16",
                        "16_A_0_200.docx")
output_file <- file.path(AMWS_OUTPUT, "amws_1986_ed16_A_0_200_pilot.csv")

if (!file.exists(input_file)) {
  stop("Input file not found: ", input_file)
}
dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)

MONTH_RX <- paste(
  "January", "February", "March", "April", "September", "October",
  "November", "December", "June", "July", "August", "Sept", "Sep",
  "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Oct",
  "Nov", "Dec",
  sep = "|"
)

ENTRY_START_RX <- paste0(
  "^\\s*[A-Z][A-Z0-9'’.-]*(?:\\s+[A-Z][A-Z0-9'’.-]*)*,\\s+",
  "[A-Z0-9][A-Z0-9'’()., \\-]{0,95}",
  "(?:[,\\.]\\s*b\\b|\\bsee\\s+previous\\s+edition\\b|\\bdeceased\\b)"
)

normalize_text <- function(x) {
  x |>
    str_replace_all("\u00a0", " ") |>
    str_replace_all("([[:alnum:]])-\\s+([[:alnum:]])", "\\1\\2") |>
    str_replace_all("\\s+", " ") |>
    str_trim()
}

parse_ocr_integer <- function(x) {
  tok <- toupper(str_replace_all(x, "[^0-9A-Z]", ""))
  tok <- chartr("OISZLB", "015218", tok)
  suppressWarnings(as.integer(tok))
}

parse_birth_year <- function(year_token) {
  yr <- parse_ocr_integer(year_token)
  out <- rep(NA_integer_, length(yr))
  k4 <- !is.na(yr) & yr >= 1800 & yr <= 1986
  k1900 <- !is.na(yr) & yr >= 0 & yr <= 86
  k1800 <- !is.na(yr) & yr >= 87 & yr <= 99
  out[k4] <- yr[k4]
  out[k1900] <- 1900L + yr[k1900]
  out[k1800] <- 1800L + yr[k1800]
  out
}

canadian_province <- function(x) {
  k <- tolower(str_trim(str_replace_all(x, "[.;,]+$", "")))
  k <- str_replace_all(k, "\\s+", " ")
  k %in% c(
    "ab", "alta", "alta.", "alberta",
    "bc", "b c", "b. c", "b.c", "british columbia",
    "man", "man.", "manitoba",
    "nb", "n b", "n. b", "n.b", "new brunswick",
    "nfld", "newfoundland",
    "ns", "n s", "n. s", "n.s", "nova scotia",
    "ont", "ont.", "ontario",
    "pei", "p e i", "p. e. i", "prince edward island",
    "que", "que.", "quebec",
    "sask", "sask.", "saskatchewan"
  )
}

split_birth_place <- function(place_raw) {
  if (is.na(place_raw) || !nzchar(place_raw)) {
    return(list(city = "", state = "", country = "", flag = "missing_place"))
  }

  place_raw <- normalize_text(place_raw)
  toks <- str_split(place_raw, ",")[[1]] |>
    str_trim() |>
    (\(z) z[nzchar(z)])()

  if (!length(toks)) {
    return(list(city = "", state = "", country = "", flag = "missing_place"))
  }

  state_tokens <- normalize_state_vec(toks)
  state_idx <- which(!is.na(state_tokens))
  if (length(state_idx)) {
    j <- tail(state_idx, 1)
    city <- if (j > 1) paste(toks[seq_len(j - 1)], collapse = ", ") else ""
    return(list(city = city, state = state_tokens[j],
                country = "USA", flag = "ok"))
  }

  province_idx <- which(vapply(toks, canadian_province, logical(1)))
  if (length(province_idx)) {
    j <- tail(province_idx, 1)
    city <- if (j > 1) paste(toks[seq_len(j - 1)], collapse = ", ") else ""
    return(list(city = city, state = toks[j],
                country = "Canada", flag = "ok"))
  }

  if (length(toks) == 1) {
    city_state <- str_match(toks[1], "^(.+?)\\s+([A-Za-z. ]{1,15})$")
    if (!is.na(city_state[1, 1])) {
      usps <- normalize_state(city_state[1, 3])
      if (!is.na(usps)) {
        return(list(city = str_trim(city_state[1, 2]), state = usps,
                    country = "USA", flag = "ok"))
      }
    }
    return(list(city = "", state = "", country = toks[1], flag = "ok"))
  }

  list(city = paste(toks[-length(toks)], collapse = ", "),
       state = "", country = toks[length(toks)], flag = "ok")
}

read_docx_paragraphs <- function(path) {
  tmp <- tempfile("amws16_docx_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  unzip(path, files = "word/document.xml", exdir = tmp)
  doc <- read_xml(file.path(tmp, "word", "document.xml"))
  ns <- xml_ns(doc)
  paragraphs <- xml_find_all(doc, ".//w:p", ns)

  paragraph_text <- function(p) {
    paste(xml_text(xml_find_all(p, ".//w:t", ns)), collapse = "")
  }
  has_page_break <- function(p) {
    length(xml_find_all(
      p,
      ".//w:lastRenderedPageBreak | .//w:br[@w:type='page']",
      ns
    )) > 0
  }

  tibble(
    paragraph_id = seq_along(paragraphs),
    detected_page = cumsum(vapply(paragraphs, has_page_break, logical(1))) + 1L,
    text = vapply(paragraphs, paragraph_text, character(1))
  ) |>
    mutate(text = normalize_text(text)) |>
    filter(nzchar(text))
}

build_entries <- function(paragraphs) {
  first_entry <- which(str_detect(paragraphs$text, "^AABOE,\\s"))[1]
  if (is.na(first_entry)) {
    stop("Could not find pilot start entry: AABOE")
  }

  base_page <- paragraphs$detected_page[first_entry]
  pilot <- paragraphs |>
    slice(first_entry:n()) |>
    mutate(source_page = detected_page - base_page + 1L) |>
    filter(source_page >= 1L, source_page <= 10L)

  pilot <- pilot |>
    mutate(
      is_entry_start = str_detect(text, regex(ENTRY_START_RX,
                                             ignore_case = FALSE)),
      entry_id = cumsum(is_entry_start)
    ) |>
    filter(entry_id > 0)

  entries <- pilot |>
    group_by(entry_id) |>
    summarise(
      source_page = min(source_page),
      raw_entry = normalize_text(paste(text, collapse = " ")),
      .groups = "drop"
    ) |>
    distinct(raw_entry, .keep_all = TRUE)

  list(entries = entries, paragraphs = pilot)
}

parse_entry <- function(raw_entry) {
  raw_entry <- normalize_text(raw_entry)

  birth_match <- str_match(raw_entry, "^\\s*(.*?)[,\\.]\\s+b\\s+(.+)$")
  has_birth <- !is.na(birth_match[1, 1])

  if (has_birth) {
    name <- str_trim(birth_match[1, 2])
    after_birth <- birth_match[1, 3]
  } else {
    name <- str_match(
      raw_entry,
      "^\\s*([^,]+,\\s+.*?)(?:,\\s+|\\s+)(?:.*?\\b(?:see previous edition|deceased)\\b)"
    )[1, 2]
    if (is.na(name)) {
      name <- str_match(raw_entry, "^\\s*([^.;]{3,120})")[1, 2]
    }
    after_birth <- ""
  }

  date_rx <- paste0("\\b(", MONTH_RX, ")\\.?\\s+",
                    "([0-9OISZLB]{1,2})\\s*[,.'’`-]?\\s*",
                    "([0-9OISZLB]{1,4})\\b")
  date_loc <- str_locate(after_birth, regex(date_rx, ignore_case = TRUE))
  has_date <- has_birth && !is.na(date_loc[1, 1])

  if (has_date) {
    birth_place_raw <- str_sub(after_birth, 1, date_loc[1, 1] - 1) |>
      str_replace("[,;:.\\s]+$", "") |>
      normalize_text()
    birth_date_raw <- str_sub(after_birth, date_loc[1, 1], date_loc[1, 2]) |>
      normalize_text()
    date_parts <- str_match(birth_date_raw, regex(date_rx, ignore_case = TRUE))
    birth_year <- parse_birth_year(date_parts[1, 4])
    after_date <- str_sub(after_birth, date_loc[1, 2] + 1)
  } else if (has_birth) {
    birth_place_raw <- str_split(after_birth, ";", n = 2)[[1]][1] |>
      str_replace("[,;:.\\s]+$", "") |>
      normalize_text()
    birth_date_raw <- ""
    birth_year <- NA_integer_
    after_date <- after_birth
  } else {
    birth_place_raw <- ""
    birth_date_raw <- ""
    birth_year <- NA_integer_
    after_date <- raw_entry
  }

  place <- split_birth_place(birth_place_raw)

  educ_loc <- str_locate(raw_entry, regex("\\bEduc\\s*[:\\-!]", ignore_case = TRUE))
  has_educ <- !is.na(educ_loc[1, 1])
  before_educ <- if (has_educ) str_sub(raw_entry, 1, educ_loc[1, 1] - 1) else raw_entry
  description <- if (has_educ) str_sub(raw_entry, educ_loc[1, 1]) else ""

  field_source <- if (has_date) {
    str_sub(after_birth, date_loc[1, 2] + 1)
  } else if (has_birth) {
    str_split(after_birth, ";", n = 2)[[1]]
    str_sub(after_birth, nchar(birth_place_raw) + 1)
  } else {
    before_educ
  }
  if (has_educ) {
    rel_educ <- str_locate(field_source, regex("\\bEduc\\s*[:\\-!]", ignore_case = TRUE))
    if (!is.na(rel_educ[1, 1])) {
      field_source <- str_sub(field_source, 1, rel_educ[1, 1] - 1)
    }
  }

  field <- field_source |>
    str_replace_all("\\b(?:US|Can)\\s+citizen\\b", "") |>
    str_replace_all("\\bnat\\s+US\\b", "") |>
    str_replace_all("(?i)\\b(?:m|c|div|wid|sep)\\s*[0-9OISZLB]*\\b", "") |>
    str_replace_all("^[,;:.\\s0-9-]+", "") |>
    str_replace_all("[,;:.\\s]+$", "") |>
    normalize_text()

  # Keep the most field-like tail when residual marital/citizenship text remains.
  field_tail <- str_match(field, "([A-Z][A-Z0-9 &/,.'’\\-]{2,})$")[1, 2]
  if (!is.na(field_tail)) {
    field <- normalize_text(field_tail)
  }

  if (!nzchar(field) || nchar(field) > 180 ||
      str_detect(field, regex("\\bEduc\\b|\\bProf Exp\\b", ignore_case = TRUE))) {
    field <- ""
  }

  flags <- character()
  if (!has_birth) flags <- c(flags, "no_birth")
  if (has_birth && !has_date) flags <- c(flags, "no_date")
  if (!nzchar(field)) flags <- c(flags, "no_field")
  if (!length(flags) && place$flag != "ok") flags <- c(flags, place$flag)
  if (!length(flags)) flags <- "ok"

  tibble(
    Name = normalize_text(name),
    birth_city = place$city,
    birth_country = place$country,
    birth_date_raw = birth_date_raw,
    birth_year = birth_year,
    field = field,
    description = normalize_text(description),
    birth_place_raw = birth_place_raw,
    birth_state = place$state,
    parse_flag = paste(flags, collapse = "_")
  )
}

paragraphs <- read_docx_paragraphs(input_file)
built <- build_entries(paragraphs)
entries <- built$entries

parsed <- bind_rows(lapply(entries$raw_entry, parse_entry))

out <- bind_cols(
  tibble(
    lineid = seq_len(nrow(entries)),
    source_file = basename(input_file),
    source_page = entries$source_page,
    raw_entry = entries$raw_entry
  ),
  parsed
) |>
  select(
    lineid, Name, birth_city, birth_country, birth_date_raw, birth_year,
    field, description, source_file, source_page, raw_entry, birth_place_raw,
    birth_state, parse_flag
  )

write_csv(out, output_file)

cat("input:", input_file, "\n")
cat("output:", output_file, "\n")
cat("paragraphs read:", nrow(paragraphs), "\n")
cat("pilot paragraphs:", nrow(built$paragraphs), "\n")
cat("entries parsed:", nrow(out), "\n\n")
cat("parse flag distribution:\n")
print(sort(table(out$parse_flag), decreasing = TRUE))
cat("\nentries by source_page:\n")
print(table(out$source_page))
cat("\nfirst 20 parsed rows:\n")
print(out |>
        select(lineid, source_page, Name, birth_city, birth_country,
               birth_year, field, parse_flag) |>
        head(20),
      n = 20)
