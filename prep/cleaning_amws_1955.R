library(haven)
library(readr)
library(dplyr)

repo_root <- Sys.getenv("GTL_REPO", unset = "")
if (!nzchar(repo_root)) {
  stop("GTL_REPO is not set. Set it in .Renviron to the repository root.")
}

source(file.path(repo_root, "paths.R"))

# Load Stata file
df <- read_dta(file.path(AMWS_INPUT, "amws_1955.dta"))

# Fix multibyte encoding on all character columns
char_cols <- names(df)[sapply(df, is.character)]
for (col in char_cols) {
  df[[col]] <- iconv(df[[col]], from = "latin1", to = "UTF-8", sub = "?")
}

# Cleaning function
parse_birth_year <- function(date_raw) {
  yr <- suppressWarnings(as.integer(sub(".*?([0-9]{1,4})\\s*$", "\\1", date_raw)))
  out <- rep(NA_integer_, length(date_raw))
  k4 <- !is.na(yr) & yr >= 1856 & yr <= 1935
  k1900 <- !is.na(yr) & yr >= 0 & yr <= 35
  k1800 <- !is.na(yr) & yr >= 56 & yr <= 99
  out[k4] <- yr[k4]
  out[k1900] <- 1900L + yr[k1900]
  out[k1800] <- 1800L + yr[k1800]
  out
}

parse_ocr_number <- function(x) {
  tok <- toupper(gsub("[^0-9A-Z]", "", x))
  tok <- chartr("OISZLB", "015218", tok)
  suppressWarnings(as.integer(tok))
}

parse_year_token <- function(x) {
  has_digit <- grepl("[0-9]", x)
  tok <- toupper(gsub("[^0-9A-Z]", "", x))
  tok <- chartr("OISZLB", "015218", tok)
  yr <- suppressWarnings(as.integer(tok))
  out <- rep(NA_integer_, length(tok))
  weak_single_ocr <- !has_digit & nchar(tok) == 1
  k4 <- !is.na(yr) & yr >= 1856 & yr <= 1935
  k1900 <- !is.na(yr) & !weak_single_ocr & nchar(tok) <= 2 & yr >= 0 & yr <= 35
  k1800 <- !is.na(yr) & !weak_single_ocr & nchar(tok) <= 2 & yr >= 56 & yr <= 99
  out[k4] <- yr[k4]
  out[k1900] <- 1900L + yr[k1900]
  out[k1800] <- 1800L + yr[k1800]
  out
}

parse_day_token <- function(x) {
  day <- parse_ocr_number(x)
  ifelse(!is.na(day) & day >= 1 & day <= 31, day, NA_integer_)
}

extract_year_match <- function(x, pattern) {
  m <- regexec(pattern, x, ignore.case = TRUE, perl = TRUE)
  r <- regmatches(x, m)
  vapply(r, function(z) {
    if (length(z) < 2) return(NA_integer_)
    parse_year_token(z[length(z)])
  }, integer(1))
}

extract_date_year_match <- function(x, pattern) {
  m <- regexec(pattern, x, ignore.case = TRUE, perl = TRUE)
  r <- regmatches(x, m)
  vapply(r, function(z) {
    if (length(z) < 3) return(NA_integer_)
    day <- parse_day_token(z[length(z) - 1])
    if (is.na(day)) return(NA_integer_)
    parse_year_token(z[length(z)])
  }, integer(1))
}

extract_month_year_match <- function(x, pattern) {
  m <- regexec(pattern, x, ignore.case = TRUE, perl = TRUE)
  r <- regmatches(x, m)
  vapply(r, function(z) {
    if (length(z) < 2) return(NA_integer_)
    year_raw <- parse_ocr_number(z[length(z)])
    if (is.na(year_raw) || year_raw <= 31) return(NA_integer_)
    parse_year_token(z[length(z)])
  }, integer(1))
}

parse_birth_year_fallback <- function(x) {
  x <- gsub("([A-Za-z])-\\s+([A-Za-z])", "\\1\\2", x)
  x <- gsub("@", "", x)
  x <- gsub("\\s+", " ", x)
  x <- trimws(x)

  degree_pattern <- paste0(
    "\\b(",
    "A[\\.,]?\\s?B|B[\\.,]?\\s?A|B[\\.,]?\\s?Sc|B[\\.,]?\\s?S|B[\\.,]?\\s?E|B[\\.,]?\\s?C\\.?\\s?E|",
    "C[\\.,]?\\s?E|M[\\.,]?\\s?A|M[\\.,]?\\s?Sc|M[\\.,]?\\s?S|M\\.\\s?D|D\\.?\\s?Sc|",
    "Ph\\.?\\s?D|Sc\\.?\\s?D|LL\\.?\\s?D|D\\.?\\s?Eng|A[\\.,]\\s?M|Dipl\\.?|",
    "fellow|Asst|Prof|Research|Instr",
    ")\\b"
  )
  degree_pos <- regexpr(degree_pattern, x, ignore.case = TRUE, perl = TRUE)
  before_degree <- ifelse(degree_pos > 0,
                          ifelse(degree_pos > 1, substr(x, 1, degree_pos - 1), ""),
                          x)
  before_degree <- sub(";.*$", "", before_degree)
  before_degree <- trimws(before_degree)

  month_pattern <- paste(
    "January", "February", "March", "April", "September",
    "October", "November", "December", "June", "July", "August",
    "Sept", "Sep", "Jan", "Feb", "Mar", "Apr", "May", "Jun",
    "Jul", "Aug", "Oct", "Nov", "Dec", "Nor", "Ian", "Mav",
    "Jfuly", "Jiaiy", "8ept", "AprU", "Aprl", "Apri1",
    sep = "|"
  )
  year_token <- "([0-9OISZLB]{1,4})"
  day_token <- "([0-9OISZLB]{1,2})"

  out <- extract_date_year_match(
    before_degree,
    paste0("\\b(", month_pattern, ")\\s*[\\.,]?\\s*", day_token,
           "\\s*[,.'’`-]+\\s*", year_token, "\\b")
  )

  missing <- is.na(out)
  out[missing] <- extract_date_year_match(
    before_degree[missing],
    paste0("\\b(", month_pattern, ")\\s*[\\.,]?\\s*\\d{1,2}\\s+", year_token, "\\b")
  )

  missing <- is.na(out)
  out[missing] <- extract_month_year_match(
    before_degree[missing],
    paste0("\\b(", month_pattern, ")\\s*[\\.,]\\s*", year_token, "\\b")
  )

  missing <- is.na(out)
  out[missing] <- extract_month_year_match(
    before_degree[missing],
    paste0("\\b(", month_pattern, ")\\.\\s*([0-9OISZLB]{2})\\b")
  )

  missing <- is.na(out)
  out[missing] <- extract_year_match(
    before_degree[missing],
    "^[A-Za-z][^,;]{2,60},\\s*[A-Za-z. ]{1,25},\\s*([0-9]{2})[.;]?\\s*$"
  )

  out
}

clean_birthplace <- function(bp) {
  if (is.na(bp)) return(list(place = "", date = "", nat = "", flag = "na"))

  orig <- bp
  flag <- "ok"

  # Step 1: de-hyphenate line breaks
  bp <- gsub("([A-Za-z])-\\s+([A-Za-z])", "\\1\\2", bp)

  # Step 2: strip stray @
  bp <- gsub("@", "", bp)

  # Step 3: collapse whitespace, trim
  bp <- gsub("\\s+", " ", bp)
  bp <- trimws(bp)

  # Step 4: truncate bleed-through (degree tokens or career roles after offset 15)
  # Degree regex: A\.?B, B\.?A, etc.
  degree_pattern <- "\\b(A[\\.,]?B|B[\\.,]?A|B[\\.,]?Sc|B[\\.,]?S|B[\\.,]?E|M[\\.,]?A|M[\\.,]?Sc|M[\\.,]?S|M\\.\\s?D|D\\.?Sc|Ph\\.?\\s?D|Sc\\.?D|LL\\.?D|D\\.?Eng|A[\\.,]\\s?M)\\b"
  career_pattern <- "\\b(fellow|Asst|Prof|Research|Instr)\\b"

  degree_pos <- regexpr(degree_pattern, bp, ignore.case = TRUE)
  if (degree_pos[1] > 0) {
    bp <- substr(bp, 1, degree_pos[1] - 1)
    flag <- "bleed_truncated"
  } else {
    # Only check career tokens after offset 15
    career_pos <- regexpr(career_pattern, bp, ignore.case = TRUE)
    if (career_pos[1] > 15) {
      bp <- substr(bp, 1, career_pos[1] - 1)
      flag <- "bleed_truncated"
    }
  }

  bp <- trimws(bp)
  clean_bp <- bp

  # Step 5: find date (Month Day, YY pattern)
  month_pattern <- paste(
    "January", "February", "March", "April", "September",
    "October", "November", "December", "June", "July", "August",
    "Sept", "Sep", "Jan", "Feb", "Mar", "Apr", "May", "Jun",
    "Jul", "Aug", "Oct", "Nov", "Dec", "Nor", "Jfuly", "Jiaiy", "8ept",
    "AprU", "Aprl", "Apri1",
    sep = "|"
  )
  date_pattern <- paste0(
    "\\b(", month_pattern, ")\\s*[\\.,]?\\s*",
    "\\d{1,2}(?:\\s*[,.]\\s*|\\s+)\\d{1,2}\\b"
  )
  date_match <- regexpr(date_pattern, bp, ignore.case = TRUE)

  if (date_match[1] > 0) {
    date_len <- attr(date_match, "match.length")
    place_raw <- substr(bp, 1, date_match[1] - 1)
    date_raw <- substr(bp, date_match[1], date_match[1] + date_len - 1)
    nat_raw_tail <- substr(bp, date_match[1] + date_len, nchar(bp))

    # Step 6: extract naturalization from tail
    nat_pattern <- "\\bnat\\b(\\.?\\s*\\d{2})?"
    nat_match <- regexpr(nat_pattern, nat_raw_tail, ignore.case = TRUE)
    if (nat_match[1] > 0) {
      nat_len <- attr(nat_match, "match.length")
      nat_raw <- substr(nat_raw_tail, nat_match[1], nat_match[1] + nat_len - 1)
    } else {
      nat_raw <- ""
    }
  } else {
    place_raw <- bp
    date_raw <- ""
    nat_raw <- ""
    flag <- "no_date_found"
  }

  # Step 8: trim and strip trailing punctuation
  place_raw <- trimws(gsub("[;,.]\\s*$", "", place_raw))
  date_raw <- trimws(gsub("[;,.]\\s*$", "", date_raw))
  nat_raw <- trimws(gsub("[;,.]\\s*$", "", nat_raw))

  list(place = place_raw, date = date_raw, nat = nat_raw, flag = flag, clean = clean_bp)
}

# Apply to all rows
results <- lapply(df$birthplace, clean_birthplace)
date_raw_vec <- sapply(results, function(x) x$date)
birth_year_full <- parse_birth_year(date_raw_vec)
birth_year_fallback <- parse_birth_year_fallback(df$birthplace)
birth_year <- ifelse(is.na(birth_year_full), birth_year_fallback, birth_year_full)
birth_year_source <- ifelse(!is.na(birth_year_full), "full_date",
                            ifelse(!is.na(birth_year), "year_only", "missing"))

# Build output dataframe
out <- data.frame(
  lineid = df$lineid,
  star = df$star,
  last = df$last,
  first = df$first,
  specialization = df$specialization,
  birthplace_orig = df$birthplace,
  birthplace_clean = sapply(results, function(x) x$clean),
  place_raw = sapply(results, function(x) x$place),
  date_raw = date_raw_vec,
  birth_year = birth_year,
  birth_year_source = birth_year_source,
  nat_raw = sapply(results, function(x) x$nat),
  parse_flag = sapply(results, function(x) x$flag),
  stringsAsFactors = FALSE
)

# Write output
write_csv(out, file.path(AMWS_OUTPUT, "amws_1955_split.csv"))

# Diagnostics
cat("\n=== AMWS 1955 Birthplace Cleaning Diagnostics ===\n")
cat("Total rows:", nrow(out), "\n")
cat("Parse flag distribution:\n")
print(table(out$parse_flag))
cat("Rows with date_raw:", sum(nzchar(out$date_raw)), "\n")
cat("Rows with birth_year:", sum(!is.na(out$birth_year)), "\n")
cat("Rows with birth_year from full_date:", sum(out$birth_year_source == "full_date"), "\n")
cat("Rows with birth_year from year_only:", sum(out$birth_year_source == "year_only"), "\n")

date_signal_pattern <- paste0(
  "\\b(",
  "January|February|March|April|September|October|November|December|",
  "June|July|August|Sept|Sep|Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Oct|Nov|Dec|Nor|Jfuly|Jiaiy|8ept|",
  "AprU|Aprl|Apri1",
  ")[\\.,]?\\s*\\d{1,2}(?:\\s*[,.]\\s*|\\s+)\\d{1,2}\\b"
)
no_date_with_signal <- out |>
  filter(parse_flag == "no_date_found",
         grepl(date_signal_pattern, birthplace_clean, ignore.case = TRUE))
cat("no_date_found with apparent date signal:", nrow(no_date_with_signal), "\n")

cat("\n--- Sample 10 rows with parse_flag == 'ok' ---\n")
ok_rows <- filter(out, parse_flag == "ok")
if (nrow(ok_rows) > 0) {
  sample_ok <- ok_rows[sample(nrow(ok_rows), min(10, nrow(ok_rows))), ]
  for (i in 1:nrow(sample_ok)) {
    cat(sprintf("%d. %s\n  → %s | %s | %s\n",
                i, sample_ok$birthplace_orig[i],
                sample_ok$place_raw[i], sample_ok$date_raw[i], sample_ok$nat_raw[i]))
  }
}

cat("\n--- Sample rows with parse_flag != 'ok' ---\n")
notok_rows <- filter(out, parse_flag != "ok")
if (nrow(notok_rows) > 0) {
  sample_notok <- notok_rows[sample(nrow(notok_rows), min(10, nrow(notok_rows))), ]
  for (i in 1:nrow(sample_notok)) {
    cat(sprintf("%d. [%s] %s\n  → %s | %s | %s\n",
                i, sample_notok$parse_flag[i], sample_notok$birthplace_orig[i],
                sample_notok$place_raw[i], sample_notok$date_raw[i], sample_notok$nat_raw[i]))
  }
}

cat("\n")
