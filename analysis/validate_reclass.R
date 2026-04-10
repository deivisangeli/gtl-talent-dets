suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(httr2))

# ---------------------------------------------------------------------------
# Load data and apply reclassification (same logic as taxonomy_us_scientists.R)
# ---------------------------------------------------------------------------
data <- read_csv("../prep/input/cross-verified-database.csv", show_col_types=FALSE)

reclass_keywords <- c(
  "computer_scientist", "astrophysicist", "biochemist", "bacteriologist",
  "climatologist", "crystallographer", "epidemiologist", "immunologist",
  "microbiologist", "neuroscientist", "oceanographer", "pharmacologist",
  "seismologist", "virologist",
  "physicist", "chemist", "mathematician", "biologist", "astronomer",
  "engineer", "inventor", "geologist", "zoologist", "botanist",
  "entomologist", "geneticist", "physiologist", "astronaut", "aerospace",
  "statistician", "ecologist", "palaeontologist", "meteorologist",
  "mineralogist", "naturalist", "ichthyologist", "mycologist",
  "ornithologist", "anatomist", "agronomist", "programmer",
  "physician", "surgeon", "psychiatrist", "paediatrician", "dermatologist",
  "cardiologist", "pathologist", "ophthalmologist", "neurologist",
  "dentist", "pharmacist", "nurse", "homeopath",
  "economist", "psychologist", "sociologist", "political_scientist",
  "art_historian", "historian", "philosopher", "theologian", "philologist",
  "linguist", "anthropologist", "archaeologist", "romanist", "orientalist",
  "lexicographer", "jurist", "lawyer"
)
kw_regex <- str_c(reclass_keywords, collapse = "|")

us <- data %>%
  filter(citizenship_1_b == "US", birth >= 1800, birth <= 1999) %>%
  filter(level1_main_occ == "Discovery/Science") %>%
  filter(level3_main_occ %in% c("academic", "professor")) %>%
  mutate(
    level3_occ = coalesce(str_extract(level3_all_occ, kw_regex), level3_main_occ),
    reclassified = level3_occ != level3_main_occ,
    group = paste0(level3_main_occ, "_", ifelse(reclassified, "reclassified", "unchanged"))
  )

# ---------------------------------------------------------------------------
# Stratified sample: 25 per group = 100 total
# ---------------------------------------------------------------------------
set.seed(123)
sample_df <- us %>%
  group_by(group) %>%
  slice_sample(n = 25) %>%
  ungroup() %>%
  select(name, birth, original = level3_main_occ, new_label = level3_occ,
         reclassified, group, level3_all_occ)

cat("Sample sizes by group:\n")
print(count(sample_df, group))

# ---------------------------------------------------------------------------
# Fetch Wikipedia summary API for each person
# Endpoint: https://en.wikipedia.org/api/rest_v1/page/summary/{title}
# Returns JSON with 'description' (short Wikidata label) and 'extract' (lede)
# ---------------------------------------------------------------------------
fetch_wiki_summary <- function(name) {
  url <- paste0(
    "https://en.wikipedia.org/api/rest_v1/page/summary/",
    utils::URLencode(name, reserved = TRUE)
  )
  tryCatch({
    resp <- request(url) %>%
      req_headers("User-Agent" = "gtl-taxonomy-validation/1.0 (research)") %>%
      req_timeout(10) %>%
      req_perform()
    body <- resp_body_json(resp)
    list(
      description = body$description %||% NA_character_,
      extract     = str_trunc(body$extract %||% NA_character_, 200)
    )
  }, error = function(e) {
    list(description = NA_character_, extract = NA_character_)
  })
}

cat("\nFetching Wikipedia summaries for", nrow(sample_df), "people...\n")
results <- vector("list", nrow(sample_df))
for (i in seq_len(nrow(sample_df))) {
  if (i %% 10 == 0) cat("  ...", i, "/", nrow(sample_df), "\n")
  results[[i]] <- fetch_wiki_summary(sample_df$name[i])
  Sys.sleep(0.15)  # polite crawl delay
}

sample_df <- sample_df %>%
  mutate(
    wiki_description = map_chr(results, "description"),
    wiki_extract     = map_chr(results, "extract")
  )

# ---------------------------------------------------------------------------
# Automated agreement check:
# For reclassified rows: does wiki_description contain our new_label (or synonym)?
# For unchanged rows: does wiki_description suggest a specific label we missed?
# ---------------------------------------------------------------------------

# Map labels to regex patterns (handles plurals, adjective forms, synonyms)
label_patterns <- c(
  physicist        = "physic",
  chemist          = "chem",
  mathematician    = "mathemat",
  biologist        = "biolog",
  astronomer       = "astronom",
  engineer         = "engineer|engineering",
  inventor         = "inventor",
  geologist        = "geolog",
  zoologist        = "zoolog",
  botanist         = "botan",
  entomologist     = "entomolog",
  geneticist       = "genetic",
  biochemist       = "biochem",
  physiologist     = "physiolog",
  astronaut        = "astronaut",
  aerospace        = "aerospace",
  astrophysicist   = "astrophys",
  statistician     = "statistic",
  ecologist        = "ecolog",
  neuroscientist   = "neurosci|neurolog",
  computer_scientist = "computer scien|computing|software",
  climatologist    = "climat",
  immunologist     = "immunolog",
  microbiologist   = "microbiolog",
  epidemiologist   = "epidemiolog",
  physician        = "physician|medical doctor|doctor of medicine",
  surgeon          = "surgeon|surgery",
  psychiatrist     = "psychiatr",
  neurologist      = "neurolog",
  psychologist     = "psycholog",
  economist        = "econom",
  sociologist      = "sociolog",
  political_scientist = "political scien|political theor",
  historian        = "historian|history",
  philosopher      = "philosoph",
  theologian       = "theolog",
  linguist         = "linguist",
  anthropologist   = "anthropolog",
  archaeologist    = "archaeolog",
  jurist           = "jurist|legal scholar|legal theorist",
  lawyer           = "lawyer|attorney|legal"
)

check_agreement <- function(wiki_desc, our_label) {
  if (is.na(wiki_desc)) return(NA)
  pattern <- label_patterns[our_label]
  if (is.na(pattern)) return(NA)
  str_detect(str_to_lower(wiki_desc), pattern)
}

sample_df <- sample_df %>%
  mutate(
    # For reclassified: did we get the new label right?
    # For unchanged (still academic/professor): does wiki suggest something specific?
    wiki_has_our_label = map2_lgl(wiki_description, new_label, check_agreement),
    wiki_specific_occ  = str_extract(
      str_to_lower(coalesce(wiki_description, "")),
      str_c(reclass_keywords, collapse = "|")
    ),
    # Agreement: reclassified → wiki confirms; unchanged → wiki is also generic
    agreement = case_when(
      reclassified & isTRUE(wiki_has_our_label)  ~ "correct",
      reclassified & isFALSE(wiki_has_our_label) ~ "wrong",
      reclassified & is.na(wiki_has_our_label)   ~ "unverifiable",
      !reclassified & is.na(wiki_specific_occ)   ~ "correctly_generic",
      !reclassified & !is.na(wiki_specific_occ)  ~ "missed",
      TRUE ~ "unverifiable"
    )
  )

# ---------------------------------------------------------------------------
# Results
# ---------------------------------------------------------------------------
cat("\n=== Agreement by group ===\n")
sample_df %>%
  count(group, agreement) %>%
  group_by(group) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  print()

cat("\n=== Overall accuracy on reclassified rows ===\n")
sample_df %>%
  filter(reclassified) %>%
  count(agreement) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  print()

cat("\n=== Missed reclassifications (unchanged but wiki has specific label) ===\n")
sample_df %>%
  filter(agreement == "missed") %>%
  select(name, birth, original, new_label, wiki_description, wiki_specific_occ) %>%
  print(width = 160)

cat("\n=== Wrong reclassifications ===\n")
sample_df %>%
  filter(agreement == "wrong") %>%
  select(name, birth, original, new_label, wiki_description, wiki_extract) %>%
  print(width = 160)

cat("\n=== Full results (sorted by group) ===\n")
sample_df %>%
  arrange(group, agreement) %>%
  select(name, birth, group, new_label, agreement, wiki_description) %>%
  print(n = 100, width = 160)

write_csv(
  sample_df %>% select(name, birth, group, original, new_label,
                       reclassified, agreement, wiki_description, wiki_extract),
  "results/taxonomy_validation_systematic.csv"
)
cat("\nSaved: results/taxonomy_validation_systematic.csv\n")
