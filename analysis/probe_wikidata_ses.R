###############################################################################
# Wikidata coverage probe: do we get SES proxies for our US STEM scientists?
#
# Pulls a random sample of US STEM scientists from the cross-verified
# database, queries the public Wikidata SPARQL endpoint for:
#   P22 (father)
#   P25 (mother)
#   P69 (educated at)
#   P106 (occupation, on the father if present)
# Reports per-property coverage overall and by birth-year decade, plus the
# top father-occupations in the sample.
#
# Free, public API. Polite headers (user-agent + email) and small batches.
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
SAMPLE_SIZE <- 500
BATCH       <- 40

raw_path <- Sys.getenv("ELITE_RAW_PATH",
                       unset = file.path(DATA_INPUT, "cross-verified-database.csv"))

cat("Loading raw database...\n")
raw <- fread(
  raw_path,
  select = c("wikidata_code", "birth", "bplo1", "bpla1",
             "level1_main_occ", "level3_main_occ", "level3_all_occ"),
  showProgress = TRUE
)

cat("Filtering to US-born STEM scientists 1800-2000...\n")
us_stem <- raw %>%
  drop_na(birth, bplo1, bpla1) %>%
  filter(level1_main_occ == "Discovery/Science",
         birth >= 1800, birth <= 2000) %>%
  add_stem_dummy() %>%
  filter(stem == 1) %>%
  filter(bplo1 < -60, bplo1 > -170, bpla1 > 20, bpla1 < 72)

# Sanity-check against US county footprint using the cached shapefile.
county_shp <- file.path(
  Sys.getenv("LOCALAPPDATA"),
  "tigris", "tigris", "Cache", "cb_2020_us_county_20m.shp"
)
counties <- st_read(county_shp, quiet = TRUE) %>%
  st_transform(5070) %>%
  filter(as.integer(STATEFP) <= 56) %>%
  select(GEOID, geometry)

us_stem_sf <- us_stem %>%
  st_as_sf(coords = c("bplo1", "bpla1"), crs = 4326, remove = FALSE) %>%
  st_transform(5070)
us_stem_sf <- st_join(us_stem_sf, counties["GEOID"], join = st_within) %>%
  filter(!is.na(GEOID)) %>%
  st_drop_geometry() %>%
  as_tibble()

cat("US STEM scientists in panel: ", nrow(us_stem_sf), "\n", sep = "")

sample_df <- us_stem_sf %>%
  filter(!is.na(wikidata_code), nzchar(wikidata_code)) %>%
  slice_sample(n = SAMPLE_SIZE) %>%
  mutate(birth_decade = (birth %/% 10) * 10)

cat("Sampled: ", nrow(sample_df), "\n", sep = "")
cat("Birth-decade range: ", min(sample_df$birth_decade), " to ",
    max(sample_df$birth_decade), "\n", sep = "")

###############################################################################
# SPARQL queries (batched)
###############################################################################

endpoint <- "https://query.wikidata.org/sparql"

run_batch <- function(codes) {
  values <- paste0("wd:", codes, collapse = " ")
  query <- sprintf("
    SELECT ?person
           (SAMPLE(?father) AS ?father)
           (SAMPLE(?father_occ) AS ?father_occ)
           (SAMPLE(?mother) AS ?mother)
           (GROUP_CONCAT(DISTINCT ?school; separator='|') AS ?schools)
    WHERE {
      VALUES ?person { %s }
      OPTIONAL { ?person wdt:P22 ?father.
                 OPTIONAL { ?father wdt:P106 ?father_occ. } }
      OPTIONAL { ?person wdt:P25 ?mother. }
      OPTIONAL { ?person wdt:P69 ?school. }
    }
    GROUP BY ?person
  ", values)

  resp <- request(endpoint) %>%
    req_url_query(query = query, format = "json") %>%
    req_user_agent("elite-schools-research-probe (deivisangeli@gmail.com)") %>%
    req_retry(max_tries = 3, backoff = ~5) %>%
    req_perform()

  parsed <- jsonlite::fromJSON(rawToChar(resp$body), simplifyVector = FALSE)
  bindings <- parsed$results$bindings

  if (length(bindings) == 0) return(tibble())

  map_dfr(bindings, function(b) {
    extract <- function(k) if (!is.null(b[[k]])) b[[k]]$value else NA_character_
    tibble(
      wikidata_uri = extract("person"),
      father_uri   = extract("father"),
      father_occ   = extract("father_occ"),
      mother_uri   = extract("mother"),
      schools_uris = extract("schools")
    )
  }) %>%
    mutate(wikidata_code = str_remove(wikidata_uri, ".*/"))
}

codes <- sample_df$wikidata_code
results <- tibble()
n_batches <- ceiling(length(codes) / BATCH)
cat("Running ", n_batches, " SPARQL batches of ", BATCH, " IDs each...\n",
    sep = "")
for (i in seq_len(n_batches)) {
  chunk <- codes[((i - 1) * BATCH + 1):min(i * BATCH, length(codes))]
  cat(sprintf("  batch %d / %d (n=%d)\n", i, n_batches, length(chunk)))
  batch_res <- tryCatch(run_batch(chunk),
                        error = function(e) {
                          message("  ! batch ", i, " failed: ",
                                  conditionMessage(e))
                          tibble()
                        })
  results <- bind_rows(results, batch_res)
  Sys.sleep(0.5)
}

cat("\nResults returned: ", nrow(results), " / ", length(codes), "\n", sep = "")

merged <- sample_df %>%
  left_join(results %>% select(-wikidata_uri), by = "wikidata_code") %>%
  mutate(
    has_father  = !is.na(father_uri),
    has_mother  = !is.na(mother_uri),
    has_school  = !is.na(schools_uris) & nzchar(schools_uris),
    has_father_occ = !is.na(father_occ)
  )

cat("\n=== Coverage (overall) ===\n")
merged %>%
  summarise(
    P22_father   = mean(has_father),
    P25_mother   = mean(has_mother),
    P69_school   = mean(has_school),
    P106_father_occ = mean(has_father_occ)
  ) %>%
  mutate(across(everything(), ~ round(. * 100, 1))) %>%
  print()

cat("\n=== Coverage by birth decade ===\n")
merged %>%
  group_by(birth_decade) %>%
  summarise(
    n = n(),
    P22 = round(100 * mean(has_father), 1),
    P25 = round(100 * mean(has_mother), 1),
    P69 = round(100 * mean(has_school), 1),
    P106_father_occ = round(100 * mean(has_father_occ), 1),
    .groups = "drop"
  ) %>%
  print(n = 30)

# Decode father_occ Q-codes to labels in a second SPARQL pass.
father_occs <- merged %>%
  filter(has_father_occ) %>%
  mutate(occ_qid = str_remove(father_occ, ".*/")) %>%
  pull(occ_qid) %>%
  unique()

if (length(father_occs) > 0) {
  cat("\nResolving ", length(father_occs), " father-occupation Q-codes...\n",
      sep = "")
  label_batches <- split(father_occs,
                         ceiling(seq_along(father_occs) / BATCH))
  occ_labels <- map_dfr(label_batches, function(qs) {
    values <- paste0("wd:", qs, collapse = " ")
    q <- sprintf("
      SELECT ?occ ?occLabel WHERE {
        VALUES ?occ { %s }
        SERVICE wikibase:label { bd:serviceParam wikibase:language 'en'. }
      }
    ", values)
    resp <- request(endpoint) %>%
      req_url_query(query = q, format = "json") %>%
      req_user_agent("elite-schools-research-probe (deivisangeli@gmail.com)") %>%
      req_retry(max_tries = 3, backoff = ~5) %>%
      req_perform()
    parsed <- jsonlite::fromJSON(rawToChar(resp$body), simplifyVector = FALSE)
    map_dfr(parsed$results$bindings, function(b) {
      tibble(
        father_occ = b$occ$value,
        occ_label  = if (!is.null(b$occLabel)) b$occLabel$value else NA_character_
      )
    })
  })

  cat("\n=== Top 25 father occupations (sample of US STEM scientists) ===\n")
  merged %>%
    filter(has_father_occ) %>%
    left_join(occ_labels, by = "father_occ") %>%
    count(occ_label, sort = TRUE) %>%
    head(25) %>%
    print()
}

###############################################################################
# Save the joined sample for inspection.
###############################################################################

out_path <- "wikidata_ses_probe_sample.csv"
write_csv(merged, out_path)
cat("\nWrote sample to ", out_path, "\n", sep = "")
