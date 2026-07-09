###############################################################################
# Apply piloted AMWS Ed16 regex corrections to the all-country output.
#
# Reads:
#   output/amws/regex_all_docs/
#     amws_ed16_all_countries_geocoded_us_only.csv
#
# Writes:
#   output/amws/regex_all_docs/
#     amws_ed16_all_countries_geocoded_us_only_regex_enhanced.csv
#     amws_ed16_all_countries_geocoded_us_only_regex_enhanced_log.csv
#     amws_ed16_all_countries_geocoded_us_only_regex_enhanced_summary.csv
#   Data/processed/amws/
#     amws_ed86.csv
#     amws_ed86.xlsx
#     amws_ed86_filtered.csv
#     amws_ed86_filtered.xlsx
#   Data/intermediary/amws/
#     amws_ed86_full.csv
#     amws_ed86_full.xlsx
#
# The script preserves non-US rows. US rows are re-geocoded after corrections so
# geo_* columns remain aligned with corrected birth_city/birth_state.
###############################################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(stringdist)
  library(sf)
  library(tibble)
  library(data.table)
  library(openxlsx)
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
source(file.path(repo_root, "prep", "amws", "state_alias.R"))

local_dropbox <- file.path("C:/Users", Sys.info()[["user"]],
                           "Globtalent Dropbox", "gtl_talent_dets")
if (!dir.exists(TALENT_DETS_DATA_DIR) && dir.exists(local_dropbox)) {
  TALENT_DETS_DATA_DIR <- normalizePath(local_dropbox, winslash = "/")
  DATA_INPUT <- file.path(TALENT_DETS_DATA_DIR, "input")
  DATA_OUTPUT <- file.path(TALENT_DETS_DATA_DIR, "output")
}

env_chr <- function(name, default = "") {
  value <- Sys.getenv(name, unset = "")
  if (nzchar(value)) value else default
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

clean_na <- function(x) {
  x <- normalize_text(x)
  ifelse(str_to_upper(x) == "NA", "", x)
}

write_amws_xlsx <- function(data, path, sheet_name) {
  wb <- createWorkbook()
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, data)
  freezePane(wb, sheet_name, firstRow = TRUE)
  saveWorkbook(wb, path, overwrite = TRUE)
}

normalize_ocr_digits <- function(x) {
  chartr("OoIiLlSsZzBb|", "0011115522881", x)
}

amws_century_year <- function(yy) {
  yy <- as.integer(yy)
  ifelse(yy <= 86L, 1900L + yy, 1800L + yy)
}

parse_year_token <- function(x) {
  x <- normalize_ocr_digits(clean_na(x))
  if (!str_detect(x, "^([0-9]{2}|[0-9]{4})$")) return("")
  year <- if (nchar(x) == 2L) amws_century_year(x) else as.integer(x)
  if (is.na(year) || year < 1800L || year > 1986L) "" else as.character(year)
}

parse_day_token <- function(x) {
  x <- normalize_ocr_digits(clean_na(x)) |>
    str_replace_all("!", "1") |>
    str_replace_all(regex("u", ignore_case = TRUE), "11")
  if (!str_detect(x, "^[0-9]{1,2}$")) return(NA_integer_)
  day <- suppressWarnings(as.integer(x))
  if (is.na(day) || day < 1L || day > 31L) NA_integer_ else day
}

month_regex <- paste0(
  "(?:Jan(?:uary)?|Feb(?:ruary)?|Mar(?:ch)?|Apr(?:il)?|May|",
  "Jun(?:e)?|Jul(?:y)?|Aug|Aue|Sep(?:t)?|Sept|Oct|Oet|",
  "Nov|Nnv|Dec|Dee|Dcc|Mat|Mu)"
)

section_regex <- paste0(
  "(?:Educ|Fduc|Educl|Prof\\s*Exp|Prof\\s*tap|Mem|Honors|",
  "Concurrent\\s*Pos|Res:|Mailing\\s*Add|M[ae]m:|Research)"
)

field_regex <- paste0(
  "(?:PHYSICS|CHEMISTRY|BIOCHEMISTRY|BIOLOGY|ZOOLOGY|BOTANY|GENETICS|",
  "ECOLOGY|GEOLOGY|MATHEMATICS|STATISTICS|ENGINEERING|ASTRONOMY|",
  "PHARMACOLOGY|PHYSIOLOGY|PSYCHOLOGY|PSYCHIATRY|ENTOMOLOGY|",
  "VIROLOGY|MICROBIOLOGY|NEUROLOGY|ENDOCRINOLOGY|TOXICOLOGY|",
  "METALLURGY|GEOCHEMISTRY|OCEANOGRAPHY|COMPUTER\\s+SCIENCE|",
  "AERONAUTICAL|ASTRONAUTICAL|NUTRITION|MEDICINE|RADIOLOGY|",
  "SPECTROSCOPY|MECHANICS|FLUID\\s+MECHANICS|PUBLIC\\s+HEAI?L?TH|",
  "ANIMAL\\s+BEHAVIOR|INFECTIOUS\\s+DISEASE|MICROFLORA|ORGANIC|",
  "MAMMALOGY|FOREST\\s+PRODUCTS|ANALYSIS)"
)

COUNTRY_ALIAS <- c(
  "arg" = "Argentina", "argentina" = "Argentina",
  "australia" = "Australia",
  "austria" = "Austria", "auatna" = "Austria",
  "brazil" = "Brazil", "brasil" = "Brazil",
  "can" = "Canada", "can." = "Canada", "canada" = "Canada",
  "china" = "China",
  "denmark" = "Denmark",
  "eng" = "England", "england" = "England",
  "france" = "France",
  "ger" = "Germany", "germany" = "Germany",
  "greece" = "Greece",
  "holland" = "Netherlands", "netherlands" = "Netherlands",
  "hungary" = "Hungary",
  "india" = "India",
  "indonesia" = "Indonesia",
  "italy" = "Italy",
  "japan" = "Japan",
  "mex" = "Mexico", "mexico" = "Mexico",
  "taiwan" = "Taiwan",
  "turkey" = "Turkey",
  "uk" = "United Kingdom",
  "us" = "USA", "u.s" = "USA", "u.s." = "USA",
  "usa" = "USA", "u s" = "USA", "united states" = "USA",
  "yugoslavia" = "Yugoslavia"
)

CANADA_PROVINCE_ALIAS <- c(
  "ab" = "AB", "alta" = "AB", "alberta" = "AB",
  "bc" = "BC", "b c" = "BC", "b. c" = "BC", "b.c" = "BC",
  "british columbia" = "BC",
  "man" = "MB", "manitoba" = "MB",
  "nb" = "NB", "new brunswick" = "NB",
  "ns" = "NS", "nova scotia" = "NS",
  "ont" = "ON", "ontario" = "ON",
  "que" = "QC", "quebec" = "QC",
  "sask" = "SK", "saskatchewan" = "SK"
)

state_ocr_alias <- c(
  "111" = "IL", "iii" = "IL", "hi" = "IL", "i;" = "IL",
  "del" = "DE", "del." = "DE",
  "nmex" = "NM", "nmcx" = "NM",
  "calir" = "CA", "cahf" = "CA", "c>l'f" = "CA",
  "w'is" = "WI", "wig" = "WI", "w o" = "WI",
  "sdak" = "SD", "po" = "PA", "pa>" = "PA", "p." = "PA",
  "vy" = "NY", "ns v" = "NY",
  "w'va" = "WV", "svva" = "WV", "wva" = "WV",
  "u" = "LA", "m" = "MO", "m\u00bb" = "MO"
)

normalize_key <- function(x) {
  x <- str_to_lower(normalize_text(x))
  x <- str_replace_all(x, "[,;:]+$", "")
  x <- str_replace_all(x, "\\.", "")
  normalize_text(x)
}

state_lookup <- function(x) {
  key <- normalize_key(x)
  out <- normalize_state(key)
  if (!is.na(out) && nzchar(out)) return(out)
  ocr <- unname(state_ocr_alias[key])
  ifelse(is.na(ocr), "", ocr)
}

country_lookup <- function(x) {
  key <- normalize_key(x)
  out <- unname(COUNTRY_ALIAS[key])
  ifelse(is.na(out), "", out)
}

canada_province_lookup <- function(x) {
  key <- normalize_key(x)
  out <- unname(CANADA_PROVINCE_ALIAS[key])
  ifelse(is.na(out), "", out)
}

city_from_place <- function(place) {
  place <- normalize_text(place)
  if (!nzchar(place)) return("")
  if (str_detect(place, ",")) {
    city <- str_split_fixed(place, ",", 2)[, 1]
  } else if (str_detect(place, "\\.\\s*[A-Za-z0-9' ]{1,20}$")) {
    city <- str_split_fixed(place, "\\.", 2)[, 1]
  } else {
    city <- place
  }
  normalize_text(city)
}

extract_birth_segment <- function(raw_text_adjusted) {
  raw <- normalize_text(raw_text_adjusted)
  m <- str_match(raw, regex("(?:^|[ ,.;])b\\s+(.{0,220})",
                            ignore_case = TRUE))
  if (is.na(m[1, 2])) return("")
  seg <- m[1, 2]
  cut <- str_locate(seg, regex(paste0("\\b(", section_regex, "|",
                                      field_regex, ")\\b"),
                               ignore_case = TRUE))[1, 1]
  if (!is.na(cut) && cut > 4L) seg <- str_sub(seg, 1L, cut - 1L)
  normalize_text(seg)
}

first_cut <- function(x, patterns) {
  starts <- vapply(patterns, function(pat) {
    loc <- str_locate(x, regex(pat, ignore_case = TRUE))[1, 1]
    ifelse(is.na(loc) || loc < 4L, Inf, loc)
  }, numeric(1))
  if (all(is.infinite(starts))) return(list(pos = Inf, reason = ""))
  i <- which.min(starts)
  list(pos = starts[[i]], reason = names(patterns)[[i]])
}

trim_birth_place <- function(place) {
  x <- clean_na(place)
  if (!nzchar(x)) return(list(place = "", reason = "no_birth_place"))
  x <- str_replace(x, regex("^birth_place\\s*=\\s*", ignore_case = TRUE), "")
  x <- str_split_fixed(x, regex("\\s*;\\s*city\\s*=", ignore_case = TRUE), 2)[, 1]
  x <- normalize_text(x)
  reasons <- character()

  patterns <- c(
    month = paste0("(?:[,.;\\s]+|^)\\b", month_regex, "\\b"),
    section = paste0("\\b", section_regex, "\\b"),
    field = paste0("(?:[,.;]\\s*|\\b\\d{1,4}\\s+|\\s{2,})",
                   field_regex, "\\b")
  )

  for (i in seq_len(3L)) {
    cut <- first_cut(x, patterns)
    if (is.infinite(cut$pos)) break
    x_new <- str_sub(x, 1L, cut$pos - 1L) |>
      str_replace("[ ,.;:-]+$", "") |>
      normalize_text()
    if (identical(x_new, x)) break
    x <- x_new
    reasons <- c(reasons, cut$reason)
  }

  before_noise <- x
  x <- str_replace(
    x,
    regex("\\b(?:m\\s*\\d{2}|c\\s*\\d+|nat(?:l)?|citizen|US\\s+citizen|Can\\s+citizen)\\b.*$",
          ignore_case = TRUE),
    ""
  ) |>
    str_replace("[ ,.;:-]+$", "") |>
    normalize_text()
  if (!identical(x, before_noise)) reasons <- c(reasons, "demographic_noise")

  list(place = x, reason = ifelse(length(reasons), paste(reasons, collapse = "+"),
                                  "unchanged"))
}

parse_place_components <- function(place, old_city = "", old_state = "",
                                   old_country = "") {
  place <- normalize_text(place)
  city <- clean_na(old_city)
  state <- clean_na(old_state)
  country <- clean_na(old_country)
  rule <- ""
  if (!nzchar(place)) {
    return(tibble(birth_city = "", birth_state = "", birth_country = "",
                  component_rule = "empty_place"))
  }

  place_key <- normalize_key(place)
  country_only <- country_lookup(place_key)
  if (nzchar(country_only)) {
    return(tibble(birth_city = "", birth_state = "", birth_country = country_only,
                  component_rule = "country_only_not_city"))
  }

  province_only <- canada_province_lookup(place_key)
  if (nzchar(province_only)) {
    return(tibble(birth_city = "", birth_state = province_only,
                  birth_country = "Canada",
                  component_rule = "canada_province_only_not_city"))
  }

  parts <- str_split(place, "\\s*,\\s*|\\s+\\.\\s+|\\.\\s+", simplify = TRUE)
  parts <- parts[nzchar(parts)]
  if (length(parts) >= 2L) {
    suffix <- parts[[length(parts)]]
    prefix <- normalize_text(paste(parts[-length(parts)], collapse = ", "))
    st <- state_lookup(suffix)
    co <- country_lookup(suffix)
    prov <- canada_province_lookup(suffix)
    if (nzchar(st)) {
      return(tibble(birth_city = prefix, birth_state = st,
                    birth_country = "USA",
                    component_rule = "place_suffix_us_state"))
    }
    if (nzchar(prov)) {
      return(tibble(birth_city = prefix, birth_state = prov,
                    birth_country = "Canada",
                    component_rule = "place_suffix_canada_province"))
    }
    if (nzchar(co)) {
      return(tibble(birth_city = prefix, birth_state = "",
                    birth_country = co,
                    component_rule = "place_suffix_country"))
    }
  }

  m <- str_match(
    place,
    regex("^(.+?)\\s+([A-Za-z][A-Za-z.'>]{0,15})$", ignore_case = TRUE)
  )
  if (!is.na(m[1, 2])) {
    prefix <- normalize_text(m[1, 2])
    suffix <- normalize_text(m[1, 3])
    st <- state_lookup(suffix)
    co <- country_lookup(suffix)
    prov <- canada_province_lookup(suffix)
    if (nzchar(st)) {
      return(tibble(birth_city = prefix, birth_state = st,
                    birth_country = "USA",
                    component_rule = "space_separated_us_state"))
    }
    if (nzchar(prov)) {
      return(tibble(birth_city = prefix, birth_state = prov,
                    birth_country = "Canada",
                    component_rule = "space_separated_canada_province"))
    }
    if (nzchar(co)) {
      return(tibble(birth_city = prefix, birth_state = "",
                    birth_country = co,
                    component_rule = "space_separated_country"))
    }
  }

  if (nzchar(country) || nzchar(state) || nzchar(city)) {
    return(tibble(birth_city = city, birth_state = state,
                  birth_country = country,
                  component_rule = "preserve_existing_components"))
  }

  tibble(birth_city = city_from_place(place), birth_state = "",
         birth_country = "", component_rule = "city_only_from_place")
}

propose_birth_year_values <- function(birth_place, birth_date, birth_year,
                                      raw_text_adjusted) {
  current <- clean_na(birth_year)
  ctx <- normalize_text(paste(trim_birth_place(birth_place)$place,
                              clean_na(birth_date)))
  birth_seg <- extract_birth_segment(raw_text_adjusted)
  if (nchar(birth_seg) > nchar(ctx)) ctx <- birth_seg

  mdy <- str_match_all(
    ctx,
    regex(
      paste0("\\b(", month_regex, ")\\.?\\s*[,.;]?\\s*",
             "([0-9OoIiLlSsZzBb|!Uu]{1,4})\\s*[,.;_'’‘\\-^ ]+",
             "([0-9OoIiLlSsZzBb|]{2,4})\\b"),
      ignore_case = TRUE
    )
  )[[1]]
  if (nrow(mdy)) {
    for (i in seq_len(nrow(mdy))) {
      day <- parse_day_token(mdy[i, 3])
      year <- parse_year_token(mdy[i, 4])
      if (!is.na(day) && nzchar(year)) {
        return(list(birth_year = year, rule = "month_day_year_explicit",
                    context = ctx))
      }
    }
  }

  m_short <- str_match(
    ctx,
    regex("\\b([A-Z][A-Za-z .'-]{2,30}),\\s*([0-9OoIiLlSsZzBb|]{2})\\s*;\\s*(?:Can|US) citizen",
          ignore_case = TRUE)
  )
  if (!is.na(m_short[1, 3])) {
    year <- parse_year_token(m_short[1, 3])
    if (nzchar(year)) {
      return(list(birth_year = year,
                  rule = "short_birth_clause_year_before_citizen",
                  context = ctx))
    }
  }

  four_year <- str_match(ctx, "(?<!\\d)(18\\d{2}|19\\d{2})(?!\\d)")[, 2]
  if (!is.na(four_year)) {
    year <- parse_year_token(four_year)
    if (nzchar(year)) {
      return(list(birth_year = year,
                  rule = "four_digit_year_in_birth_clause",
                  context = ctx))
    }
  }

  day_only <- str_match(
    ctx,
    regex(
      paste0("\\b(", month_regex, ")\\.?\\s*[,.;]?\\s*",
             "([0-9OoIiLlSsZzBb|]{1,2})\\b",
             "(?!\\s*[,.;_'’‘\\-^ ]+[0-9OoIiLlSsZzBb|]{2,4}\\b)"),
      ignore_case = TRUE
    )
  )
  if (!is.na(day_only[1, 2]) && nzchar(current)) {
    day <- parse_day_token(day_only[1, 3])
    current_int <- suppressWarnings(as.integer(current))
    if (!is.na(day) && !is.na(current_int) && current_int %% 100L == day) {
      return(list(birth_year = "", rule = "suppress_day_as_year",
                  context = ctx))
    }
  }

  list(birth_year = current, rule = "unchanged", context = ctx)
}

has_city_format_symbol <- function(x) {
  x <- normalize_text(x)
  nzchar(x) & str_detect(x, "[[:cntrl:]<>\\^_*=+/\\\\|\\[\\]{}#@&%$~;:!?()]")
}

birth_location_format_reasons <- function(birth_city, birth_state, birth_year) {
  city <- normalize_text(birth_city)
  state <- normalize_text(birth_state)
  year <- normalize_text(birth_year)
  year_int <- suppressWarnings(as.integer(year))
  city_bad_symbol <- has_city_format_symbol(city)
  city_has_digit <- nzchar(city) & str_detect(city, "[0-9]")
  city_has_separator_debris <- nzchar(city) & str_detect(city, "[,.]")
  city_has_section_text <- nzchar(city) & str_detect(
    city,
    regex("\\b(Educ|Prof|Univ|Dept|Chemistry|Physics|Biology|Engineering|Medicine|Research|SCIENCE|MATH|MATHEMATICS|BIOCHEMISTRY|GENETICS|GEOLOGY)\\b",
          ignore_case = TRUE)
  )
  city_has_date_marker <- nzchar(city) & str_detect(
    city,
    regex(paste0("\\b", month_regex, "\\b"), ignore_case = TRUE)
  )
  city_too_long <- nchar(city) > 45L
  state_malformed <- nzchar(state) &
    !(str_detect(state, "^[A-Z]{2}$") | state %in% unname(CANADA_PROVINCE_ALIAS))
  year_malformed <- nzchar(year) &
    (!str_detect(year, "^[0-9]{4}$") |
       is.na(year_int) |
       year_int < 1800L |
       year_int > 1986L)

  vapply(seq_along(city), function(i) {
    reasons <- c(
      if (city_bad_symbol[[i]]) "birth_city_symbols",
      if (city_has_digit[[i]]) "birth_city_digits",
      if (city_has_separator_debris[[i]]) "birth_city_punct_or_separator",
      if (city_has_section_text[[i]]) "birth_city_section_text",
      if (city_has_date_marker[[i]]) "birth_city_date_text",
      if (city_too_long[[i]]) "birth_city_too_long",
      if (state_malformed[[i]]) "birth_state_not_clean_code",
      if (year_malformed[[i]]) "birth_year_not_plausible_4digit"
    )
    paste(reasons, collapse = "; ")
  }, character(1))
}

apply_regex_corrections <- function(data) {
  place_text <- clean_na(data$birth_place)
  date_text <- clean_na(data$birth_date)
  year_text <- clean_na(data$birth_year)
  country_text <- clean_na(data$birth_country)
  year_parse_flag <- clean_na(data$birth_year_parse_flag)
  format_problem <- str_to_upper(clean_na(data$birth_location_format_problem)) %in%
    c("TRUE", "T", "1", "YES")
  raw_head <- substr(clean_na(data$raw_text_adjusted), 1L, 260L)

  place_candidate_pattern <- paste0(
    month_regex, "|", section_regex, "|", field_regex,
    "|citizen|\\b111\\b|\\bIII\\b|\\bHI\\b|NMcx|Calir|Cahf|Wig|W'is|",
    "SDak|W'Va|SVVa|\\bUS\\b|\\bCan\\b|Indonesia|Holland|Alberta|",
    "Australia|India|\\bNJ\\b|\\bNY\\b|\\bPa\\b|\\bOhio\\b"
  )
  year_candidate_text <- normalize_text(paste(place_text, date_text, raw_head))
  place_candidate <- nzchar(place_text) &
    (str_detect(place_text, regex(place_candidate_pattern, ignore_case = TRUE)) |
       (!nzchar(country_text) & str_detect(place_text, "[A-Za-z]")))
  needs_year_review <- !nzchar(year_text) |
    year_parse_flag %in% c("no_birth_date", "no_year_token") |
    format_problem
  year_candidate <- needs_year_review &
    str_detect(year_candidate_text, regex(month_regex, ignore_case = TRUE))
  candidate_idx <- which(place_candidate | year_candidate)
  rows <- vector("list", length(candidate_idx))
  out_i <- 0L

  for (i in candidate_idx) {
    old_place <- place_text[[i]]
    old_year <- year_text[[i]]

    trim <- trim_birth_place(old_place)
    proposed_place <- if (nzchar(trim$place)) trim$place else ""
    if (!nzchar(old_place) && trim$reason == "no_birth_place") {
      comp <- tibble(
        birth_city = clean_na(data$birth_city[[i]]),
        birth_state = clean_na(data$birth_state[[i]]),
        birth_country = clean_na(data$birth_country[[i]]),
        component_rule = "preserve_existing_empty_place"
      )
    } else {
      comp <- parse_place_components(
        proposed_place,
        old_city = data$birth_city[[i]],
        old_state = data$birth_state[[i]],
        old_country = data$birth_country[[i]]
      )
    }
    if (year_candidate[[i]]) {
      year <- propose_birth_year_values(
        data$birth_place[[i]],
        data$birth_date[[i]],
        data$birth_year[[i]],
        data$raw_text_adjusted[[i]]
      )
    } else {
      year <- list(birth_year = old_year, rule = "unchanged", context = "")
    }

    place_changed <- !identical(old_place, proposed_place)
    city_changed <- !identical(clean_na(data$birth_city[[i]]), comp$birth_city[[1]])
    state_changed <- !identical(clean_na(data$birth_state[[i]]), comp$birth_state[[1]])
    country_changed <- !identical(clean_na(data$birth_country[[i]]), comp$birth_country[[1]])
    year_changed <- !identical(old_year, year$birth_year)

    rule_parts <- c(
      if (place_changed && !trim$reason %in% c("unchanged", "no_birth_place")) {
        paste0("trim_place:", trim$reason)
      },
      if (city_changed || state_changed || country_changed) {
        paste0("components:", comp$component_rule[[1]])
      },
      if (year_changed && year$rule != "unchanged") {
        paste0("birth_year:", year$rule)
      }
    )
    if (length(rule_parts) > 0L) {
      out_i <- out_i + 1L
      rows[[out_i]] <- tibble(
        row_id = i,
        doc_id = data$doc_id[[i]],
        lineid = data$lineid[[i]],
        correction_rule = paste(rule_parts, collapse = "; "),
        old_birth_place = old_place,
        new_birth_place = proposed_place,
        old_birth_city = clean_na(data$birth_city[[i]]),
        new_birth_city = comp$birth_city[[1]],
        old_birth_state = clean_na(data$birth_state[[i]]),
        new_birth_state = comp$birth_state[[1]],
        old_birth_country = clean_na(data$birth_country[[i]]),
        new_birth_country = comp$birth_country[[1]],
        old_birth_year = old_year,
        new_birth_year = year$birth_year,
        birth_year_context = year$context,
        changed = TRUE
      )
    }
  }
  if (!out_i) {
    return(tibble(
      row_id = integer(), doc_id = character(), lineid = character(),
      correction_rule = character(), old_birth_place = character(),
      new_birth_place = character(), old_birth_city = character(),
      new_birth_city = character(), old_birth_state = character(),
      new_birth_state = character(), old_birth_country = character(),
      new_birth_country = character(), old_birth_year = character(),
      new_birth_year = character(), birth_year_context = character(),
      changed = logical()
    ))
  }
  bind_rows(rows[seq_len(out_i)])
}

strip_punct <- function(x) {
  x <- gsub("\\.", "", x)
  x <- gsub("[[:punct:]]", " ", x)
  trimws(gsub("\\s+", " ", x))
}

expand_abbrev <- function(x) {
  x <- gsub("\\bSt\\b\\.?", "Saint", x, ignore.case = TRUE)
  x <- gsub("\\bSte\\b\\.?", "Sainte", x, ignore.case = TRUE)
  x <- gsub("\\bMt\\b\\.?", "Mount", x, ignore.case = TRUE)
  x <- gsub("\\bFt\\b\\.?", "Fort", x, ignore.case = TRUE)
  x
}

strip_suffix <- function(x) {
  x <- gsub("\\b(city|town|township|village|borough|cdp)\\b", "", x,
            ignore.case = TRUE)
  trimws(gsub("\\s+", " ", x))
}

norm_place_key <- function(x) {
  x <- expand_abbrev(x)
  x <- strip_suffix(x)
  x <- strip_punct(x)
  tolower(trimws(x))
}

norm_for_check <- function(s) {
  s <- tolower(ifelse(is.na(s), "", s))
  s <- gsub("[^a-z ]", " ", s)
  trimws(gsub("\\s+", " ", s))
}

state_to_fp <- c(
  AL = "01", AK = "02", AZ = "04", AR = "05", CA = "06", CO = "08",
  CT = "09", DE = "10", DC = "11", FL = "12", GA = "13", HI = "15",
  ID = "16", IL = "17", IN = "18", IA = "19", KS = "20", KY = "21",
  LA = "22", ME = "23", MD = "24", MA = "25", MI = "26", MN = "27",
  MS = "28", MO = "29", MT = "30", NE = "31", NV = "32", NH = "33",
  NJ = "34", NM = "35", NY = "36", NC = "37", ND = "38", OH = "39",
  OK = "40", OR = "41", PA = "42", RI = "44", SC = "45", SD = "46",
  TN = "47", TX = "48", UT = "49", VT = "50", VA = "51", WA = "53",
  WV = "54", WI = "55", WY = "56", PR = "72"
)

geocode_us_rows <- function(input, gaz_file, geon_file, county_shp) {
  gaz <- read_tsv(gaz_file, show_col_types = FALSE,
                  locale = locale(encoding = "UTF-8")) |>
    rename_with(~ trimws(.x)) |>
    transmute(
      state = USPS,
      geoid_place = GEOID,
      name = NAME,
      lat = INTPTLAT,
      lon = INTPTLONG,
      key = norm_place_key(name)
    )

  geon_cols <- c("geonameid", "name", "asciiname", "alternatenames",
                 "latitude", "longitude", "feature_class", "feature_code",
                 "country", "cc2", "admin1", "admin2", "admin3", "admin4",
                 "population", "elevation", "dem", "timezone",
                 "modification_date")
  geon <- read_tsv(geon_file, col_names = geon_cols, show_col_types = FALSE,
                   quote = "", locale = locale(encoding = "UTF-8")) |>
    filter(feature_class %in% c("P", "A")) |>
    transmute(
      state = admin1,
      name = asciiname,
      lat = latitude,
      lon = longitude,
      admin2_fips = admin2,
      population = suppressWarnings(as.integer(population)),
      key = norm_place_key(asciiname)
    )

  candidates <- input |>
    mutate(
      format_problem_bool =
        tolower(normalize_text(birth_location_format_problem)) %in%
        c("true", "t", "1", "yes"),
      geocode_candidate = birth_country == "USA" &
        nzchar(normalize_text(birth_city)) &
        nzchar(normalize_text(birth_state)) &
        !format_problem_bool
    ) |>
    filter(geocode_candidate)

  pairs <- candidates |>
    distinct(city = birth_city, state = birth_state) |>
    mutate(key = norm_place_key(city))

  if (!nrow(pairs)) return(tibble())

  m1 <- pairs |>
    inner_join(gaz |> select(key, state, lat, lon, geoid_place,
                             gaz_name = name),
               by = c("key", "state"), relationship = "many-to-many") |>
    group_by(city, state) |>
    slice(1L) |>
    ungroup() |>
    mutate(match_source = "gazetteer_exact",
           admin2_fips = NA_character_,
           matched_name = gaz_name,
           jw = 0)

  remain <- pairs |> anti_join(m1, by = c("city", "state"))
  m2 <- remain |>
    inner_join(geon |> select(key, state, lat, lon, admin2_fips,
                              geon_name = name, population),
               by = c("key", "state"), relationship = "many-to-many") |>
    arrange(city, state, desc(population)) |>
    group_by(city, state) |>
    slice(1L) |>
    ungroup() |>
    mutate(match_source = "geonames_exact",
           geoid_place = NA_character_,
           matched_name = geon_name,
           jw = 0)

  remain <- remain |> anti_join(m2, by = c("city", "state"))
  fuzzy_one <- function(key_in, state_in) {
    cands <- bind_rows(
      gaz |> filter(state == state_in) |>
        transmute(key, name, lat, lon, src = "gazetteer_fuzzy",
                  admin2_fips = NA_character_, population = NA_integer_),
      geon |> filter(state == state_in) |>
        transmute(key, name, lat, lon, src = "geonames_fuzzy",
                  admin2_fips, population)
    )
    if (!nrow(cands)) return(NULL)
    dist <- stringdist::stringdist(key_in, cands$key, method = "jw", p = 0.1)
    i <- which.min(dist)
    if (!length(i) || dist[[i]] > 0.10) return(NULL)
    cands[i, ] |> mutate(jw = dist[[i]])
  }

  if (nrow(remain) > 0L) {
    fuzzy_rows <- vector("list", nrow(remain))
    for (i in seq_len(nrow(remain))) {
      row <- remain[i, ]
      res <- fuzzy_one(row$key, row$state)
      if (!is.null(res)) fuzzy_rows[[i]] <- bind_cols(row, res |> select(-key))
    }
    m3 <- bind_rows(fuzzy_rows) |>
      rename(match_source = src, matched_name = name) |>
      mutate(geoid_place = NA_character_)
  } else {
    m3 <- tibble()
  }

  matched <- bind_rows(
    m1 |> transmute(city, state, key, lat, lon, geoid_place, admin2_fips,
                    matched_name, match_source, jw),
    m2 |> transmute(city, state, key, lat, lon, geoid_place, admin2_fips,
                    matched_name, match_source, jw),
    if (nrow(m3)) {
      m3 |> transmute(city, state, key, lat, lon, geoid_place, admin2_fips,
                      matched_name, match_source, jw)
    } else {
      tibble()
    }
  ) |>
    mutate(lat = as.numeric(lat), lon = as.numeric(lon)) |>
    filter(!is.na(lat), !is.na(lon))

  counties <- st_read(county_shp, quiet = TRUE) |> st_transform(4326)
  pts <- st_as_sf(matched, coords = c("lon", "lat"), crs = 4326,
                  remove = FALSE)
  sj <- st_join(pts, counties |> select(STATEFP, COUNTYFP, GEOID, NAME),
                left = TRUE)
  matched$geoid <- as.character(sj$GEOID)
  matched$county_name <- as.character(sj$NAME)

  snap_idx <- which(is.na(matched$geoid) | !nzchar(matched$geoid))
  if (length(snap_idx)) {
    by_state <- split(snap_idx, matched$state[snap_idx])
    for (st in names(by_state)) {
      st_fp <- state_to_fp[st]
      if (is.na(st_fp)) next
      county_state <- counties[counties$STATEFP == st_fp, ]
      if (!nrow(county_state)) next
      nearest <- sf::st_nearest_feature(pts[by_state[[st]], ], county_state)
      matched$geoid[by_state[[st]]] <- as.character(county_state$GEOID[nearest])
      matched$county_name[by_state[[st]]] <-
        as.character(county_state$NAME[nearest])
    }
  }

  needs_fb <- (is.na(matched$geoid) | !nzchar(matched$geoid)) &
    !is.na(matched$admin2_fips) & nzchar(matched$admin2_fips)
  county_lookup <- counties |>
    st_drop_geometry() |>
    transmute(state_fp = STATEFP, county_fp = COUNTYFP,
              geoid_fb = GEOID, name_fb = NAME)
  for (i in which(needs_fb)) {
    st_fp <- state_to_fp[matched$state[[i]]]
    if (is.na(st_fp)) next
    hit <- county_lookup |>
      filter(state_fp == st_fp, county_fp == matched$admin2_fips[[i]])
    if (nrow(hit) == 1L) {
      matched$geoid[[i]] <- as.character(hit$geoid_fb[[1]])
      matched$county_name[[i]] <- as.character(hit$name_fb[[1]])
    }
  }

  overrides <- tribble(
    ~city,            ~state, ~geoid_new, ~county_new,
    "New York",       "NY",   "36061",    "New York",
    "Manhattan",      "NY",   "36061",    "New York",
    "Bronx",          "NY",   "36005",    "Bronx",
    "Brooklyn",       "NY",   "36047",    "Kings",
    "Queens",         "NY",   "36081",    "Queens",
    "Staten Island",  "NY",   "36085",    "Richmond",
    "Jersey City",    "NJ",   "34017",    "Hudson",
    "Hoboken",        "NJ",   "34017",    "Hudson",
    "San Francisco",  "CA",   "06075",    "San Francisco"
  )
  for (i in seq_len(nrow(overrides))) {
    hit <- matched$city == overrides$city[[i]] &
      matched$state == overrides$state[[i]]
    if (any(hit)) {
      matched$geoid[hit] <- overrides$geoid_new[[i]]
      matched$county_name[hit] <- overrides$county_new[[i]]
    }
  }

  matched <- matched |>
    mutate(geoid = ifelse(nzchar(geoid), str_pad(geoid, 5, pad = "0"), ""))

  candidates |>
    select(doc_id, lineid, birth_place, birth_city, birth_state) |>
    left_join(matched |>
                select(birth_city = city, birth_state = state, lat, lon,
                       geoid, county_name, matched_name, match_source, jw),
              by = c("birth_city", "birth_state")) |>
    mutate(
      birth_place_check = norm_for_check(birth_place),
      city_check = norm_for_check(birth_city),
      city_substr_hit = mapply(function(city_norm, place_norm) {
        if (!nzchar(city_norm)) return(FALSE)
        grepl(paste0("\\b", gsub("\\s+", "\\\\s+", city_norm), "\\b"),
              place_norm)
      }, city_check, birth_place_check, USE.NAMES = FALSE),
      cleaning_jw = stringdist::stringdist(
        city_check,
        substr(birth_place_check, 1, 25),
        method = "jw",
        p = 0.1
      )
    )
}

default_output_dir <- file.path(DATA_OUTPUT, "amws", "regex_all_docs")
output_dir <- env_chr("AMWS_ED16_ALL_COUNTRIES_REGEX_OUTPUT_DIR",
                      default_output_dir)
output_dir <- normalizePath(output_dir, winslash = "/", mustWork = TRUE)

input_file <- env_chr(
  "AMWS_ED16_ALL_COUNTRIES_REGEX_INPUT_FILE",
  file.path(output_dir, "amws_ed16_all_countries_geocoded_us_only.csv")
)
input_file <- normalizePath(input_file, winslash = "/", mustWork = TRUE)

output_csv <- file.path(
  output_dir,
  "amws_ed16_all_countries_geocoded_us_only_regex_enhanced.csv"
)
log_csv <- file.path(
  output_dir,
  "amws_ed16_all_countries_geocoded_us_only_regex_enhanced_log.csv"
)
summary_csv <- file.path(
  output_dir,
  "amws_ed16_all_countries_geocoded_us_only_regex_enhanced_summary.csv"
)

data_dir <- file.path(TALENT_DETS_DATA_DIR, "Data")
processed_amws_dir <- file.path(data_dir, "processed", "amws")
intermediary_amws_dir <- file.path(data_dir, "intermediary", "amws")

processed_csv <- file.path(processed_amws_dir, "amws_ed86.csv")
processed_xlsx <- file.path(processed_amws_dir, "amws_ed86.xlsx")
processed_filtered_csv <- file.path(processed_amws_dir, "amws_ed86_filtered.csv")
processed_filtered_xlsx <- file.path(processed_amws_dir, "amws_ed86_filtered.xlsx")
full_csv <- file.path(intermediary_amws_dir, "amws_ed86_full.csv")
full_xlsx <- file.path(intermediary_amws_dir, "amws_ed86_full.xlsx")

processed_cols <- c(
  "doc_id", "source_file", "lineid", "entry_instance", "name_raw", "field",
  "raw_text_adjusted", "birth_place", "birth_date", "birth_year",
  "birth_city", "birth_state", "birth_country", "is_us_birth",
  "is_us_geocoded", "geo_lat", "geo_lon", "geo_geoid", "geo_county_name",
  "geo_matched_name", "geocoding_status"
)

manual_exclusions <- tribble(
  ~doc_id, ~lineid, ~entry_instance, ~expected_name_raw,
  ~expected_birth_year, ~manual_exclusion_reason,
  "amws16_A_0_200", "597", "1", "",
  "1824", "corrupted_1824_birth_year_parse",
  "amws16_D_0_200", "796", "1", "JENKINS, KENNETH JAMES WILLIAM",
  "1969", "implausible_1969_birth_year"
)

gaz_file <- file.path(DATA_INPUT, "2024_Gaz_place_national.txt")
geon_file <- file.path(DATA_INPUT, "geonames_US.txt")
county_shp <- file.path(Sys.getenv("LOCALAPPDATA"), "tigris", "tigris",
                        "Cache", "cb_2020_us_county_20m.shp")
stopifnot(file.exists(gaz_file), file.exists(geon_file), file.exists(county_shp))

csv_text_cols <- cols(.default = col_character())
input <- read_csv(input_file, col_types = csv_text_cols,
                  show_col_types = FALSE) |>
  mutate(across(everything(), ~ ifelse(is.na(.x), "", .x)))

required_cols <- c(
  "doc_id", "lineid", "entry_instance", "name_raw", "raw_text_adjusted",
  "birth_place", "birth_date", "birth_year", "birth_city", "birth_state",
  "birth_country", "geo_lat", "geo_lon", "geo_geoid", "geo_county_name",
  "geo_matched_name", "geo_match_source", "geo_jw", "geo_cleaning_jw",
  "is_us_birth", "is_us_geocoded", "geocoding_status"
)
missing_cols <- setdiff(required_cols, names(input))
if (length(missing_cols)) {
  stop("Input is missing required columns: ", paste(missing_cols, collapse = ", "))
}

raw_input_rows <- nrow(input)
manual_drop <- input |>
  mutate(.row_id = row_number()) |>
  inner_join(manual_exclusions,
             by = c("doc_id", "lineid", "entry_instance")) |>
  filter(
    !nzchar(expected_name_raw) | name_raw == expected_name_raw,
    !nzchar(expected_birth_year) | birth_year == expected_birth_year
  )
if (nrow(manual_drop) != nrow(manual_exclusions)) {
  stop("Manual exclusions did not match expected rows. Expected ",
       nrow(manual_exclusions), " matches, found ", nrow(manual_drop), ".")
}
manual_exclusion_log <- manual_drop |>
  select(.row_id, doc_id, lineid, entry_instance, name_raw, birth_place,
         birth_date, birth_year, manual_exclusion_reason)
input <- input[-manual_exclusion_log$.row_id, , drop = FALSE]

if (n_distinct(paste(input$doc_id, input$lineid)) != nrow(input)) {
  stop("Input has duplicated doc_id + lineid.")
}

cat("Raw input rows:", raw_input_rows, "\n")
cat("Manual exclusion rows:", nrow(manual_exclusion_log), "\n")
cat("Input rows after manual exclusions:", nrow(input), "\n")
correction_log <- apply_regex_corrections(input)

enhanced <- input
log_changed <- filter(correction_log, changed)
changed_idx <- log_changed$row_id
enhanced$birth_place[changed_idx] <- log_changed$new_birth_place
enhanced$birth_city[changed_idx] <- log_changed$new_birth_city
enhanced$birth_state[changed_idx] <- log_changed$new_birth_state
enhanced$birth_country[changed_idx] <- log_changed$new_birth_country
enhanced$birth_year[changed_idx] <- log_changed$new_birth_year

format_reasons <- birth_location_format_reasons(
  enhanced$birth_city,
  enhanced$birth_state,
  enhanced$birth_year
)
enhanced$birth_location_format_problem <- ifelse(nzchar(format_reasons),
                                                 "TRUE", "FALSE")
enhanced$birth_location_format_problem_reason <- format_reasons

cat("Changed rows before geocode:", length(changed_idx), "\n")

geo_cols <- c("geo_lat", "geo_lon", "geo_geoid", "geo_county_name",
              "geo_matched_name", "geo_match_source", "geo_jw",
              "geo_cleaning_jw")

location_key_changed <- rep(FALSE, nrow(enhanced))
if (nrow(log_changed)) {
  location_key_changed[log_changed$row_id] <-
    log_changed$old_birth_city != log_changed$new_birth_city |
    log_changed$old_birth_state != log_changed$new_birth_state |
    log_changed$old_birth_country != log_changed$new_birth_country
}
format_flag_changed <-
  clean_na(input$birth_location_format_problem) !=
  clean_na(enhanced$birth_location_format_problem)
needs_regeo <- location_key_changed | format_flag_changed
for (col in geo_cols) {
  enhanced[[col]][needs_regeo] <- ""
}

regeo_input <- enhanced[
  needs_regeo &
    enhanced$birth_country == "USA" &
    nzchar(enhanced$birth_city) &
    nzchar(enhanced$birth_state) &
    enhanced$birth_location_format_problem != "TRUE",
]
cat("Rows needing geocode refresh:", nrow(regeo_input), "\n")
cat("Re-geocoding changed eligible US rows after corrections...\n")
geo <- geocode_us_rows(regeo_input, gaz_file, geon_file, county_shp)

if (nrow(geo)) {
  idx <- match(paste(geo$doc_id, geo$lineid),
               paste(enhanced$doc_id, enhanced$lineid))
  matched <- !is.na(idx) & !is.na(geo$geoid) & nzchar(geo$geoid)
  idx <- idx[matched]
  geo_m <- geo[matched, ]
  enhanced$geo_lat[idx] <- as.character(geo_m$lat)
  enhanced$geo_lon[idx] <- as.character(geo_m$lon)
  enhanced$geo_geoid[idx] <- geo_m$geoid
  enhanced$geo_county_name[idx] <- geo_m$county_name
  enhanced$geo_matched_name[idx] <- geo_m$matched_name
  enhanced$geo_match_source[idx] <- geo_m$match_source
  enhanced$geo_jw[idx] <- as.character(geo_m$jw)
  enhanced$geo_cleaning_jw[idx] <- as.character(geo_m$cleaning_jw)
}

enhanced <- enhanced |>
  mutate(
    is_us_birth = birth_country == "USA",
    is_us_geocoded = birth_country == "USA" & nzchar(geo_geoid),
    geocoding_status = case_when(
      !nzchar(birth_country) ~ "missing_country",
      birth_country != "USA" ~ "not_usa",
      birth_location_format_problem == "TRUE" ~ "format_problem",
      !nzchar(birth_city) | !nzchar(birth_state) ~ "missing_city_or_state",
      nzchar(geo_geoid) ~ "geocoded",
      TRUE ~ "no_geocoder_match"
    )
  )

log_out <- correction_log |>
  filter(changed) |>
  mutate(
    old_geo_geoid = input$geo_geoid[row_id],
    new_geo_geoid = enhanced$geo_geoid[row_id],
    old_geocoding_status = input$geocoding_status[row_id],
    new_geocoding_status = enhanced$geocoding_status[row_id]
  )

year_int <- suppressWarnings(as.integer(enhanced$birth_year))
bad_year <- nzchar(enhanced$birth_year) &
  (is.na(year_int) | year_int < 1800L | year_int > 1986L)
if (any(bad_year)) {
  stop("Enhanced output has invalid birth_year values: ", sum(bad_year))
}
if (nrow(enhanced) != nrow(input)) {
  stop("Row count changed unexpectedly.")
}
if (n_distinct(paste(enhanced$doc_id, enhanced$lineid)) != nrow(enhanced)) {
  stop("Enhanced output has duplicated doc_id + lineid.")
}

missing_processed_cols <- setdiff(processed_cols, names(enhanced))
if (length(missing_processed_cols)) {
  stop("Enhanced output is missing processed columns: ",
       paste(missing_processed_cols, collapse = ", "))
}

processed <- enhanced |>
  select(all_of(processed_cols))

processed_filtered <- processed |>
  filter(nzchar(clean_na(birth_city)), nzchar(clean_na(birth_year)))

summary <- bind_rows(
  tibble(metric = "input_file", value = input_file),
  tibble(metric = "raw_input_rows", value = as.character(raw_input_rows)),
  tibble(metric = "manual_exclusion_rows",
         value = as.character(nrow(manual_exclusion_log))),
  manual_exclusion_log |>
    count(manual_exclusion_reason, name = "value") |>
    transmute(metric = paste0("manual_exclusion:", manual_exclusion_reason),
              value = as.character(value)),
  tibble(metric = "input_rows", value = as.character(nrow(input))),
  tibble(metric = "output_rows_after_manual_exclusions",
         value = as.character(nrow(input))),
  tibble(metric = "output_rows", value = as.character(nrow(enhanced))),
  tibble(metric = "final_full_csv", value = full_csv),
  tibble(metric = "final_full_xlsx", value = full_xlsx),
  tibble(metric = "final_processed_csv", value = processed_csv),
  tibble(metric = "final_processed_xlsx", value = processed_xlsx),
  tibble(metric = "final_processed_filtered_csv",
         value = processed_filtered_csv),
  tibble(metric = "final_processed_filtered_xlsx",
         value = processed_filtered_xlsx),
  tibble(metric = "final_processed_rows",
         value = as.character(nrow(processed))),
  tibble(metric = "final_processed_filtered_rows",
         value = as.character(nrow(processed_filtered))),
  tibble(metric = "changed_rows", value = as.character(nrow(log_out))),
  tibble(metric = "birth_place_changed_rows",
         value = as.character(sum(log_out$old_birth_place != log_out$new_birth_place))),
  tibble(metric = "birth_city_changed_rows",
         value = as.character(sum(log_out$old_birth_city != log_out$new_birth_city))),
  tibble(metric = "birth_state_changed_rows",
         value = as.character(sum(log_out$old_birth_state != log_out$new_birth_state))),
  tibble(metric = "birth_country_changed_rows",
         value = as.character(sum(log_out$old_birth_country != log_out$new_birth_country))),
  tibble(metric = "birth_year_changed_rows",
         value = as.character(sum(log_out$old_birth_year != log_out$new_birth_year))),
  tibble(metric = "us_birth_rows",
         value = as.character(sum(enhanced$birth_country == "USA"))),
  tibble(metric = "us_geocoded_rows",
         value = as.character(sum(enhanced$is_us_geocoded %in% c(TRUE, "TRUE", "true")))),
  tibble(metric = "non_us_rows",
         value = as.character(sum(nzchar(enhanced$birth_country) &
                                    enhanced$birth_country != "USA"))),
  tibble(metric = "missing_country_rows",
         value = as.character(sum(!nzchar(enhanced$birth_country)))),
  tibble(metric = "valid_birth_year_rows",
         value = as.character(sum(nzchar(enhanced$birth_year)))),
  log_out |>
    count(correction_rule, name = "value") |>
    transmute(metric = paste0("rule:", correction_rule),
              value = as.character(value)),
  enhanced |>
    count(geocoding_status, name = "value") |>
    transmute(metric = paste0("geocoding_status:", geocoding_status),
              value = as.character(value))
)

dir.create(processed_amws_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(intermediary_amws_dir, recursive = TRUE, showWarnings = FALSE)

write_excel_csv(enhanced, output_csv, na = "")
write_excel_csv(log_out, log_csv, na = "")
write_excel_csv(summary, summary_csv, na = "")
write_excel_csv(enhanced, full_csv, na = "")
write_excel_csv(processed, processed_csv, na = "")
write_excel_csv(processed_filtered, processed_filtered_csv, na = "")

write_amws_xlsx(enhanced, full_xlsx, "amws_ed86_full")
write_amws_xlsx(processed, processed_xlsx, "amws_ed86")
write_amws_xlsx(processed_filtered, processed_filtered_xlsx,
                "amws_ed86_filtered")

cat("Wrote enhanced output:", output_csv, "\n")
cat("Wrote log:", log_csv, "\n")
cat("Wrote summary:", summary_csv, "\n")
cat("Wrote full AMWS ed86:", full_csv, "\n")
cat("Wrote processed AMWS ed86:", processed_csv, "\n")
cat("Wrote filtered AMWS ed86:", processed_filtered_csv, "\n")
print(summary |> filter(!str_starts(metric, "rule:")))
