library(haven)
library(readr)
library(dplyr)

source("../paths.R")

# Load Stata file
df <- read_dta(file.path(AMWS_INPUT, "amws_1955.dta"))

# Fix multibyte encoding on all character columns
char_cols <- names(df)[sapply(df, is.character)]
for (col in char_cols) {
  df[[col]] <- iconv(df[[col]], from = "latin1", to = "UTF-8", sub = "?")
}

# Cleaning function
clean_birthplace <- function(bp) {
  if (is.na(bp)) return(list(place = "", date = "", nat = "", flag = "na"))

  orig <- bp
  flag <- "ok"

  # Step 1: de-hyphenate line breaks
  bp <- gsub("(\\S)-\\s+(\\S)", "\\1\\2", bp)

  # Step 2: strip stray @
  bp <- gsub("@", "", bp)

  # Step 3: collapse whitespace, trim
  bp <- gsub("\\s+", " ", bp)
  bp <- trimws(bp)

  # Step 4: truncate bleed-through (degree tokens or career roles after offset 15)
  # Degree regex: A\.?B, B\.?A, etc.
  degree_pattern <- "\\b(A\\.?B|B\\.?A|B\\.?S|B\\.?E|M\\.?A|M\\.?S|M\\.?D|D\\.?Sc|Ph\\.?\\s?D|Sc\\.?D|LL\\.?D|D\\.?Eng|A\\.?M)\\b"
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
  date_pattern <- "(Jan|Feb|Mar|Apr|May|June?|July?|Aug|Sept?|Oct|Nov|Dec)\\.?\\s+\\d{1,2}\\s*[,.]?\\s*\\d{2}"
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
  date_raw = sapply(results, function(x) x$date),
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
