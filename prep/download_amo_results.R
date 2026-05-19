###############################################################################
# Project: Determinants of Talent Production
# Goal: Download AMO (American Mathematics Olympiad) winner PDFs by year & grade
# Source: https://amo.simcc.org/amo-results/
#
# Output layout (under DET_DIR/raw/):
#   AMO winners/<year>/Grade<2..12>/<file>.pdf
#   AMO winners/_manifest.csv     (one row per downloaded PDF)
#   AMO winners/skipped.csv       (PDF links that could not be classified)
###############################################################################

options(timeout = 600)
source("raw_paths.R")

library("rvest")
library("httr2")

# ---------------------------------------------------------------------------
# HTTP client with browser-like headers (the site is behind Cloudflare and
# rejects vanilla download.file / curl with a 403).
# ---------------------------------------------------------------------------

amo_user_agent <- paste(
 "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36",
 "(KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36",
 "GTL-talent-research-scraper/1.0 (marques@globtalent.org)"
)

amo_request <- function(url) {
 httr2::request(url) |>
  httr2::req_headers(
   `User-Agent` = amo_user_agent,
   Accept = "text/html,application/xhtml+xml,application/xml;q=0.9,application/pdf,*/*;q=0.8",
   `Accept-Language` = "en-US,en;q=0.9",
   `Upgrade-Insecure-Requests` = "1"
  ) |>
  httr2::req_retry(max_tries = 3, backoff = ~ 2)
}

fetch_html <- function(url) {
 resp <- tryCatch(
  httr2::req_perform(amo_request(url)),
  error = function(e) e
 )
 if (inherits(resp, "error") ||
     httr2::resp_status(resp) %in% c(403, 503)) {
  return(fetch_html_chromote(url))
 }
 httr2::resp_body_html(resp)
}

# Cloudflare fallback: render page with a real headless Chrome via chromote.
fetch_html_chromote <- function(url) {
 if (!requireNamespace("chromote", quietly = TRUE)) {
  stop(
   "AMO site returned 403/503 (likely Cloudflare).\n",
   "Install the 'chromote' package to use a real headless browser:\n",
   "  install.packages(\"chromote\")"
  )
 }
 message("  Falling back to headless Chrome (chromote) for ", url)
 b <- chromote::ChromoteSession$new()
 on.exit(try(b$close(), silent = TRUE), add = TRUE)
 b$Page$navigate(url)
 b$Page$loadEventFired()
 Sys.sleep(2)
 html <- b$Runtime$evaluate("document.documentElement.outerHTML")$result$value
 xml2::read_html(html)
}

# ---------------------------------------------------------------------------
# Year and grade parsing
# ---------------------------------------------------------------------------

# Singapore -> US grade mapping (P2..P6 = G2..G6, S1..S4 = G7..G10, JC1..2 = G11..12)
sg_grade_map <- c(
 P2 = 2, P3 = 3, P4 = 4, P5 = 5, P6 = 6,
 S1 = 7, S2 = 8, S3 = 9, S4 = 10,
 JC1 = 11, JC2 = 12
)

parse_year <- function(text) {
 m <- regmatches(text, regexpr("\\b(20[12][0-9])\\b", text))
 if (length(m) && nzchar(m[1])) as.integer(m[1]) else NA_integer_
}

parse_grade <- function(text) {
 t <- gsub("\\s+", " ", trimws(toupper(text)))

 m <- regmatches(t, regexpr("GRADE\\s*([0-9]+)", t))
 if (length(m) && nzchar(m[1])) {
  n <- as.integer(sub("[^0-9]", "", m[1]))
  if (!is.na(n) && n >= 2 && n <= 12) return(n)
 }
 m <- regmatches(t, regexpr("PRIMARY\\s*([2-6])", t))
 if (length(m) && nzchar(m[1])) return(as.integer(sub("[^0-9]", "", m[1])))
 m <- regmatches(t, regexpr("SECONDARY\\s*([1-4])", t))
 if (length(m) && nzchar(m[1])) return(as.integer(sub("[^0-9]", "", m[1])) + 6L)
 m <- regmatches(t, regexpr("(JUNIOR\\s*COLLEGE|JC)\\s*([12])", t))
 if (length(m) && nzchar(m[1])) {
  return(as.integer(regmatches(m[1], regexpr("[12]$", m[1]))) + 10L)
 }

 sep <- "(^|[^A-Z0-9])"; tail <- "([^0-9A-Z]|$)"
 for (key in names(sg_grade_map)) {
  if (grepl(paste0(sep, key, tail), t)) return(unname(sg_grade_map[key]))
 }
 NA_integer_
}

# ---------------------------------------------------------------------------
# Crawler
# ---------------------------------------------------------------------------

is_pdf <- function(href) grepl("\\.pdf(\\?.*)?$", href, ignore.case = TRUE)

extract_links <- function(page, base_url) {
 nodes <- rvest::html_elements(page, "a[href]")
 if (length(nodes) == 0) {
  return(data.frame(text = character(), href = character(), stringsAsFactors = FALSE))
 }
 hrefs <- xml2::url_absolute(rvest::html_attr(nodes, "href"), base_url)
 data.frame(
  text = trimws(rvest::html_text2(nodes)),
  href = hrefs,
  stringsAsFactors = FALSE
 )
}

crawl_for_pdfs <- function(start_url, max_depth = 2) {
 seen <- character(0)
 queue <- data.frame(
  url = start_url, depth = 0L, parent_text = "",
  stringsAsFactors = FALSE
 )
 pdfs <- data.frame(
  text = character(), href = character(), source_text = character(),
  stringsAsFactors = FALSE
 )

 while (nrow(queue) > 0) {
  cur <- queue[1, , drop = FALSE]
  queue <- queue[-1, , drop = FALSE]
  if (cur$url %in% seen) next
  seen <- c(seen, cur$url)

  message("Fetching: ", cur$url)
  page <- tryCatch(fetch_html(cur$url), error = function(e) {
   message("  ERROR: ", conditionMessage(e)); NULL
  })
  if (is.null(page)) next

  links <- extract_links(page, cur$url)
  if (nrow(links) == 0) next

  pdf_rows <- links[is_pdf(links$href), , drop = FALSE]
  if (nrow(pdf_rows) > 0) {
   pdf_rows$source_text <- cur$parent_text
   pdfs <- rbind(pdfs, pdf_rows)
  }

  if (cur$depth < max_depth) {
   sub <- links[
    grepl("^https?://amo\\.simcc\\.org/", links$href) &
     !is_pdf(links$href) &
     grepl("(amo-?results|winners?|[12][0-9]{3})", links$href, ignore.case = TRUE) &
     !(links$href %in% seen),
    , drop = FALSE
   ]
   if (nrow(sub) > 0) {
    queue <- rbind(queue, data.frame(
     url = sub$href,
     depth = cur$depth + 1L,
     parent_text = paste(cur$parent_text, sub$text),
     stringsAsFactors = FALSE
    ))
   }
  }

  Sys.sleep(1)
 }
 unique(pdfs)
}

# ---------------------------------------------------------------------------
# PDF download (idempotent)
# ---------------------------------------------------------------------------

download_pdf <- function(url, dest) {
 if (file.exists(dest)) {
  message(basename(dest), " already present, skipping.")
  return(dest)
 }
 message("Downloading ", basename(dest), "...")
 resp <- tryCatch(
  httr2::req_perform(amo_request(url)),
  error = function(e) {
   message("  ERROR: ", conditionMessage(e)); NULL
  }
 )
 if (is.null(resp)) return(NA_character_)
 if (httr2::resp_status(resp) >= 400) {
  message("  HTTP ", httr2::resp_status(resp))
  return(NA_character_)
 }
 ensure_dir(dirname(dest))
 writeBin(httr2::resp_body_raw(resp), dest)
 message("  Saved to: ", dest)
 dest
}

# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

amo_index_url <- "https://amo.simcc.org/amo-results/"

message("Crawling AMO results index for PDF links...")
all_pdfs <- crawl_for_pdfs(amo_index_url, max_depth = 2)
message("Found ", nrow(all_pdfs), " PDF link(s).")

if (nrow(all_pdfs) == 0) {
 stop(
  "No PDFs found. The site may be blocking access or the page structure ",
  "may have changed. Try opening ", amo_index_url, " in a browser to check."
 )
}

ctx <- paste(all_pdfs$text, all_pdfs$href, all_pdfs$source_text)
all_pdfs$year <- vapply(ctx, parse_year, integer(1))
all_pdfs$grade <- vapply(ctx, parse_grade, integer(1))

base_dir <- raw_dir("AMO winners")

skipped <- all_pdfs[is.na(all_pdfs$year) | is.na(all_pdfs$grade), , drop = FALSE]
if (nrow(skipped) > 0) {
 skipped_path <- file.path(base_dir, "skipped.csv")
 write.csv(skipped, skipped_path, row.names = FALSE)
 message(
  "Wrote ", nrow(skipped),
  " unclassified link(s) to: ", skipped_path
 )
}

classified <- all_pdfs[!is.na(all_pdfs$year) & !is.na(all_pdfs$grade), , drop = FALSE]
if (nrow(classified) == 0) {
 stop(
  "Found PDFs but none could be classified by year and grade. ",
  "Inspect skipped.csv and adjust the parsers."
 )
}

manifest <- data.frame(
 year = integer(),
 grade = character(),
 original_url = character(),
 local_path = character(),
 downloaded_at = character(),
 stringsAsFactors = FALSE
)

for (i in seq_len(nrow(classified))) {
 row <- classified[i, ]
 fname <- basename(sub("\\?.*$", "", row$href))
 dest <- raw_file_path(
  "AMO winners",
  as.character(row$year),
  paste0("Grade", row$grade),
  fname
 )
 result <- download_pdf(row$href, dest)
 manifest <- rbind(manifest, data.frame(
  year = row$year,
  grade = paste0("Grade", row$grade),
  original_url = row$href,
  local_path = if (is.na(result)) NA_character_ else result,
  downloaded_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
  stringsAsFactors = FALSE
 ))
 Sys.sleep(1)
}

manifest_path <- file.path(base_dir, "_manifest.csv")
write.csv(manifest, manifest_path, row.names = FALSE)
message("Wrote manifest: ", manifest_path)

message("\nSummary (PDFs per year x grade):")
print(table(classified$year, paste0("Grade", classified$grade)))
