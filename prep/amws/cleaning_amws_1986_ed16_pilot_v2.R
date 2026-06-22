###############################################################################
# Pilot cleaner v2 for AMWS edition 16 (1986) Word transcriptions.
#
# Input:
#   <Dropbox root>/Data/amws ed 16/16_A_0_200.docx
#
# Outputs:
#   <Dropbox root>/output/amws/amws_1986_ed16_A_0_200_first10_v2_intermediate.csv
#   <Dropbox root>/output/amws/amws_1986_ed16_A_0_200_first10_v2_parsed.csv
#   <Dropbox root>/output/amws/amws_1986_ed16_A_0_200_first10_v2_audit_sample100.csv
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
  repo_root <- normalizePath(file.path(dirname(script_path), "..", ".."),
                             winslash = "/", mustWork = TRUE)
} else {
  repo_root <- normalizePath(Sys.getenv("GTL_REPO", unset = getwd()),
                             winslash = "/", mustWork = TRUE)
}

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
intermediate_file <- file.path(
  AMWS_OUTPUT,
  "amws_1986_ed16_A_0_200_first10_v2_intermediate.csv"
)
parsed_file <- file.path(
  AMWS_OUTPUT,
  "amws_1986_ed16_A_0_200_first10_v2_parsed.csv"
)
audit_file <- file.path(
  AMWS_OUTPUT,
  "amws_1986_ed16_A_0_200_first10_v2_audit_sample100.csv"
)

if (!file.exists(input_file)) {
  stop("Input file not found: ", input_file)
}
dir.create(AMWS_OUTPUT, recursive = TRUE, showWarnings = FALSE)

MONTH_RX <- paste(
  "January", "February", "March", "April", "September", "October",
  "November", "December", "June", "July", "August", "Sept", "Sep",
  "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Oct",
  "Nov", "Dec",
  sep = "|"
)

ENTRY_LOOKAHEAD_RX <- paste0(
  "[A-Z][A-Z0-9'’.-]*(?:\\s+[A-Z][A-Z0-9'’.-]*)*,\\s+",
  "[A-Z0-9][A-Za-z0-9'’()., \\-]{0,120}",
  "(?:[,\\.]\\s*b\\b|\\bsee\\s+previous\\s+edition\\b|\\bdeceased\\b)"
)
ENTRY_START_RX <- paste0("^\\s*", ENTRY_LOOKAHEAD_RX)
EMBEDDED_ENTRY_RX <- paste0("\\s+(?=", ENTRY_LOOKAHEAD_RX, ")")

DATE_RX <- paste0(
  "\\b(", MONTH_RX, ")\\.?\\s+",
  "[0-9OISZLB]{1,2}\\s*[,.'’`-]?\\s*",
  "[0-9OISZLB]{1,4}[A-Za-z]?\\b"
)

normalize_text <- function(x) {
  x |>
    str_replace_all("\u00a0", " ") |>
    str_replace_all("([[:alnum:]])-\\s+([[:alnum:]])", "\\1\\2") |>
    str_replace_all("\\s+", " ") |>
    str_trim()
}

strip_edge_punct <- function(x) {
  x |>
    str_replace("^[,;:.\\s]+", "") |>
    str_replace("[,;:.\\s]+$", "") |>
    normalize_text()
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

split_embedded_entries <- function(x) {
  marked <- str_replace_all(x, regex(EMBEDDED_ENTRY_RX), "\n<<<ENTRY>>>")
  pieces <- unlist(str_split(marked, fixed("\n<<<ENTRY>>>")), use.names = FALSE)
  pieces <- normalize_text(pieces)
  pieces[nzchar(pieces)]
}

build_text_pieces <- function(paragraphs) {
  first_entry <- which(str_detect(paragraphs$text, "^AABOE,\\s"))[1]
  if (is.na(first_entry)) {
    stop("Could not find pilot start entry: AABOE")
  }

  base_page <- paragraphs$detected_page[first_entry]
  pilot <- paragraphs |>
    slice(first_entry:n()) |>
    mutate(source_page = detected_page - base_page + 1L) |>
    filter(source_page >= 1L, source_page <= 10L)

  bind_rows(lapply(seq_len(nrow(pilot)), function(i) {
    pieces <- split_embedded_entries(pilot$text[i])
    tibble(
      paragraph_id = pilot$paragraph_id[i],
      source_page = pilot$source_page[i],
      piece_order = seq_along(pieces),
      was_embedded_split = seq_along(pieces) > 1L,
      text = pieces
    )
  }))
}

build_intermediate <- function(pieces) {
  pieces <- pieces |>
    mutate(
      is_entry_start = str_detect(text, regex(ENTRY_START_RX)),
      entry_id = cumsum(is_entry_start)
    ) |>
    filter(entry_id > 0)

  entries <- pieces |>
    group_by(entry_id) |>
    summarise(
      source_page = min(source_page),
      entry_boundary_flag = if_else(any(was_embedded_split), "boundary_split", "ok"),
      entry_text = normalize_text(paste(text, collapse = " ")),
      .groups = "drop"
    ) |>
    distinct(entry_text, .keep_all = TRUE)

  parsed_header <- lapply(entries$entry_text, split_name_raw_info)

  bind_cols(
    tibble(
      lineid = seq_len(nrow(entries)),
      source_file = basename(input_file),
      source_page = entries$source_page,
      entry_boundary_flag = entries$entry_boundary_flag
    ),
    bind_rows(parsed_header)
  )
}

split_name_raw_info <- function(entry_text) {
  birth_match <- str_match(entry_text, "^\\s*(.*?)[,\\.]\\s*b\\s+(.+)$")
  if (!is.na(birth_match[1, 1])) {
    name <- normalize_text(birth_match[1, 2])
    raw_info <- paste("b", birth_match[1, 3]) |> normalize_text()
    return(tibble(Name = name, raw_info = raw_info))
  }

  prev_marker <- str_locate(
    entry_text,
    regex("\\b(?:see\\s+previous\\s+edition|deceased)\\b",
          ignore_case = TRUE)
  )
  if (!is.na(prev_marker[1, 1])) {
    prefix <- str_sub(entry_text, 1, prev_marker[1, 1] - 1)
    parts <- str_split(prefix, ",")[[1]] |> str_trim()
    if (length(parts) >= 2) {
      name_parts <- parts[1:2]
      if (length(parts) >= 3 &&
          str_detect(parts[3], regex("^(JR|SR|II|III|IV)\\b",
                                    ignore_case = TRUE))) {
        name_parts <- parts[1:3]
      }
      name <- paste(name_parts, collapse = ", ") |> normalize_text()
      raw_info <- str_sub(entry_text, nchar(name) + 1) |> strip_edge_punct()
      return(tibble(Name = name, raw_info = raw_info))
    }
  }

  prev_match <- str_match(
    entry_text,
    regex("^\\s*([^,]+,\\s+[^,]{1,90}?)(?:,\\s+|\\s+).{0,140}\\b(?:see\\s+previous\\s+edition|deceased)\\b",
          ignore_case = TRUE)
  )
  if (!is.na(prev_match[1, 1])) {
    name <- normalize_text(prev_match[1, 2])
    raw_info <- str_sub(entry_text, nchar(prev_match[1, 2]) + 1) |>
      strip_edge_punct()
    return(tibble(Name = name, raw_info = raw_info))
  }

  fallback <- str_match(entry_text, "^\\s*([^.;]{3,120})")[1, 2]
  tibble(
    Name = normalize_text(fallback),
    raw_info = str_sub(entry_text, nchar(fallback) + 1) |> strip_edge_punct()
  )
}

extract_birth <- function(raw_info) {
  after_birth <- str_match(raw_info, regex("^\\s*b\\s+(.+)$", ignore_case = TRUE))[1, 2]
  if (is.na(after_birth)) {
    return(tibble(place_of_birth = "", date_of_birth = "",
                  birth_flag = "no_birth"))
  }

  date_loc <- str_locate(after_birth, regex(DATE_RX, ignore_case = TRUE))
  if (is.na(date_loc[1, 1])) {
    place <- str_split(after_birth, ";|\\.\\s+[A-Z]{2,}", n = 2)[[1]][1] |>
      strip_edge_punct()
    return(tibble(place_of_birth = place, date_of_birth = "",
                  birth_flag = "no_date"))
  }

  place <- str_sub(after_birth, 1, date_loc[1, 1] - 1) |>
    strip_edge_punct()
  date <- str_sub(after_birth, date_loc[1, 1], date_loc[1, 2]) |>
    normalize_text()

  tibble(place_of_birth = place, date_of_birth = date, birth_flag = "ok")
}

extract_res_field <- function(raw_info) {
  strict_matches <- str_match_all(
    raw_info,
    regex("(?:^|\\.\\s+)Res\\s*[:;]\\s*(.*?)(?=\\s+Mailing\\s*Add\\s*:|\\s+MailingAdd\\s*:|\\s+Mailing\\s+Add\\b|$)",
          ignore_case = TRUE)
  )[[1]]

  if (nrow(strict_matches) > 0) {
    field <- strict_matches[nrow(strict_matches), 2] |> strip_edge_punct()
    return(tibble(field = field, field_flag = if_else(nzchar(field), "ok", "no_res_field")))
  }

  loose_matches <- str_match_all(
    raw_info,
    regex("(?:^|\\.\\s+)Res\\s+([A-Z][^:]{5,}?)(?=\\s+Mailing\\s*Add\\s*:|\\s+MailingAdd\\s*:|\\s+Mailing\\s+Add\\b|$)",
          ignore_case = FALSE)
  )[[1]]

  if (nrow(loose_matches) > 0) {
    field <- loose_matches[nrow(loose_matches), 2] |> strip_edge_punct()
    return(tibble(field = field, field_flag = if_else(nzchar(field), "ok", "no_res_field")))
  }

  tibble(field = "", field_flag = "no_res_field")
}

flag_possible_boundary <- function(raw_info, current_flag) {
  has_extra_entry <- str_detect(
    raw_info,
    regex(paste0("\\s", ENTRY_LOOKAHEAD_RX))
  )
  case_when(
    current_flag == "boundary_split" ~ "boundary_split",
    has_extra_entry ~ "possible_boundary_issue",
    TRUE ~ "ok"
  )
}

build_parsed <- function(intermediate) {
  birth <- bind_rows(lapply(intermediate$raw_info, extract_birth))
  fields <- bind_rows(lapply(intermediate$raw_info, extract_res_field))

  bind_cols(intermediate, birth, fields) |>
    mutate(
      entry_boundary_flag = mapply(
        flag_possible_boundary,
        raw_info,
        entry_boundary_flag,
        USE.NAMES = FALSE
      ),
      parse_flag = case_when(
        birth_flag == "ok" & field_flag == "ok" ~ "ok",
        birth_flag == "no_birth" & field_flag == "no_res_field" ~ "no_birth_no_res_field",
        birth_flag == "no_birth" ~ "no_birth",
        birth_flag == "no_date" & field_flag == "no_res_field" ~ "no_date_no_res_field",
        birth_flag == "no_date" ~ "no_date",
        field_flag == "no_res_field" ~ "no_res_field",
        TRUE ~ "ok"
      )
    ) |>
    select(
      lineid, Name, raw_info, place_of_birth, date_of_birth, field,
      source_file, source_page, parse_flag, entry_boundary_flag
    )
}

paragraphs <- read_docx_paragraphs(input_file)
pieces <- build_text_pieces(paragraphs)
intermediate <- build_intermediate(pieces)
parsed <- build_parsed(intermediate)

write_csv(
  intermediate |>
    select(lineid, Name, raw_info),
  intermediate_file
)
write_csv(parsed, parsed_file)

set.seed(160200)
audit_n <- min(100L, nrow(parsed))
audit_sample <- parsed |>
  slice_sample(n = audit_n) |>
  arrange(lineid) |>
  mutate(audit_status = "", audit_note = "")
write_csv(audit_sample, audit_file)

cat("input:", input_file, "\n")
cat("intermediate:", intermediate_file, "\n")
cat("parsed:", parsed_file, "\n")
cat("audit sample:", audit_file, "\n")
cat("paragraphs read:", nrow(paragraphs), "\n")
cat("text pieces in first 10 pages/chunks:", nrow(pieces), "\n")
cat("intermediate rows:", nrow(intermediate), "\n")
cat("parsed rows:", nrow(parsed), "\n\n")

cat("parse flag distribution:\n")
print(sort(table(parsed$parse_flag), decreasing = TRUE))
cat("\nentry boundary flag distribution:\n")
print(sort(table(parsed$entry_boundary_flag), decreasing = TRUE))
cat("\nentries by source_page:\n")
print(table(parsed$source_page))
cat("\nfirst 20 parsed rows:\n")
print(parsed |>
        select(lineid, source_page, Name, place_of_birth, date_of_birth,
               field, parse_flag, entry_boundary_flag) |>
        head(20),
      n = 20)
