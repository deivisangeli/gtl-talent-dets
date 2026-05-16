###############################################################################
# Fetch English Wikipedia article text for a random sample of US STEM
# scientists. For each Wikidata Q-code:
#   1. Query Wikidata Special:EntityData for the enwiki sitelink (title).
#   2. Query the Wikipedia REST API for the plain-text extract.
#   3. Save the full plain text to a per-scientist .txt file.
# Also writes a metadata CSV pairing each file with name, birth year,
# birth coords, and the resolved Wikipedia URL.
###############################################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(data.table)
  library(sf)
  library(httr2)
  library(jsonlite)
})

source("../prep/stem_labels.R")
source("../paths.R")
sf::sf_use_s2(FALSE)
set.seed(42)

SAMPLE_SIZE <- 20
OUT_DIR     <- "ses_probe_articles"
dir.create(OUT_DIR, showWarnings = FALSE)

raw_path <- Sys.getenv("ELITE_RAW_PATH",
                       unset = file.path(DATA_INPUT, "cross-verified-database.csv"))

cat("Loading raw database...\n")
raw <- fread(
  raw_path,
  select = c("wikidata_code", "name", "birth", "bplo1", "bpla1",
             "level1_main_occ", "level3_main_occ", "level3_all_occ"),
  showProgress = TRUE
)

cat("Filtering to US STEM 1800-2000...\n")
us_stem <- raw %>%
  drop_na(birth, bplo1, bpla1) %>%
  filter(level1_main_occ == "Discovery/Science",
         birth >= 1800, birth <= 2000) %>%
  add_stem_dummy() %>%
  filter(stem == 1) %>%
  filter(bplo1 < -60, bplo1 > -170, bpla1 > 20, bpla1 < 72)

county_shp <- file.path(Sys.getenv("LOCALAPPDATA"), "tigris", "tigris",
                       "Cache", "cb_2020_us_county_20m.shp")
counties <- st_read(county_shp, quiet = TRUE) %>%
  st_transform(5070) %>%
  filter(as.integer(STATEFP) <= 56) %>%
  select(GEOID, geometry)
us_stem <- us_stem %>%
  st_as_sf(coords = c("bplo1", "bpla1"), crs = 4326, remove = FALSE) %>%
  st_transform(5070) %>%
  st_join(counties["GEOID"], join = st_within) %>%
  filter(!is.na(GEOID)) %>%
  st_drop_geometry() %>%
  as_tibble()

sample_df <- us_stem %>%
  filter(!is.na(wikidata_code), nzchar(wikidata_code)) %>%
  slice_sample(n = SAMPLE_SIZE)

cat("Sampled ", nrow(sample_df), " scientists.\n", sep = "")

###############################################################################
# Resolve Wikidata Q-code -> English Wikipedia title (via Special:EntityData)
###############################################################################

ua <- "elite-schools-ses-probe (deivisangeli@gmail.com)"

get_enwiki_title <- function(qcode) {
  url <- paste0("https://www.wikidata.org/wiki/Special:EntityData/", qcode, ".json")
  resp <- tryCatch(
    request(url) %>% req_user_agent(ua) %>% req_retry(max_tries = 3, backoff = ~3) %>% req_perform(),
    error = function(e) NULL
  )
  if (is.null(resp)) return(NA_character_)
  parsed <- jsonlite::fromJSON(rawToChar(resp$body), simplifyVector = FALSE)
  ent <- parsed$entities[[qcode]]
  if (is.null(ent) || is.null(ent$sitelinks$enwiki)) return(NA_character_)
  ent$sitelinks$enwiki$title
}

get_wiki_extract <- function(title) {
  resp <- tryCatch(
    request("https://en.wikipedia.org/w/api.php") %>%
      req_url_query(
        action       = "query",
        prop         = "extracts",
        explaintext  = 1,
        format       = "json",
        redirects    = 1,
        titles       = title
      ) %>%
      req_user_agent(ua) %>%
      req_retry(max_tries = 3, backoff = ~3) %>%
      req_perform(),
    error = function(e) NULL
  )
  if (is.null(resp)) return(NA_character_)
  parsed <- jsonlite::fromJSON(rawToChar(resp$body), simplifyVector = FALSE)
  pages <- parsed$query$pages
  if (length(pages) == 0) return(NA_character_)
  page <- pages[[1]]
  if (!is.null(page$missing)) return(NA_character_)
  page$extract %||% NA_character_
}

# Sanitize Q-code into a filename.
slug <- function(s) str_replace_all(s, "[^A-Za-z0-9_-]", "_")

records <- sample_df %>%
  mutate(enwiki_title = NA_character_,
         article_path = NA_character_,
         article_len  = NA_integer_)

for (i in seq_len(nrow(records))) {
  q <- records$wikidata_code[i]
  cat(sprintf("[%2d/%d] %-12s ", i, nrow(records), q))
  title <- get_enwiki_title(q)
  if (is.na(title)) { cat("(no enwiki sitelink)\n"); next }
  records$enwiki_title[i] <- title

  extract <- get_wiki_extract(title)
  if (is.na(extract) || !nzchar(extract)) { cat("(no extract)\n"); next }

  fn <- file.path(OUT_DIR, paste0(slug(q), ".txt"))
  writeLines(paste0("# Wikidata: ", q, "\n# Title: ", title, "\n# Birth: ",
                    records$birth[i], "\n\n", extract), fn)
  records$article_path[i] <- fn
  records$article_len[i]  <- nchar(extract)
  cat(sprintf("'%s' -> %s (%d chars)\n", title, fn, records$article_len[i]))
  Sys.sleep(0.3)
}

write_csv(records, "ses_probe_records.csv")
cat("\nWrote ses_probe_records.csv and ", sum(!is.na(records$article_path)),
    " article files to ", OUT_DIR, "/\n", sep = "")
