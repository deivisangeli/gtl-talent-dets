suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(httr2))

# ---------------------------------------------------------------------------
# Rebuild classified dataset (same as taxonomy_us_scientists.R)
# ---------------------------------------------------------------------------
data <- read_csv("../prep/input/cross-verified-database.csv", show_col_types=FALSE)

reclass_keywords <- c(
  "computer_scientist","political_scientist","art_historian",
  "astrophysicist","biochemist","bacteriologist","climatologist","crystallographer",
  "epidemiologist","immunologist","microbiologist","neuroscientist","oceanographer",
  "pharmacologist","seismologist","virologist",
  "physicist","chemist","mathematician","biologist","astronomer","engineer","inventor",
  "geologist","zoologist","botanist","entomologist","geneticist","physiologist",
  "astronaut","aerospace","statistician","ecologist","palaeontologist","meteorologist",
  "mineralogist","naturalist","ichthyologist","mycologist","ornithologist","anatomist",
  "agronomist","programmer","physician","surgeon","psychiatrist","paediatrician",
  "dermatologist","cardiologist","pathologist","ophthalmologist","neurologist",
  "dentist","pharmacist","nurse","homeopath","economist","psychologist","sociologist",
  "historian","philosopher","theologian","philologist","linguist","anthropologist",
  "archaeologist","romanist","orientalist","lexicographer","jurist","lawyer"
)
kw_regex <- str_c(reclass_keywords, collapse = "|")

stem_l3 <- c("physicist","chemist","mathematician","biologist","astronomer","engineer",
              "inventor","geologist","zoologist","botanist","entomologist","geneticist",
              "biochemist","physiologist","astronaut","aerospace","astrophysicist",
              "statistician","ecologist","palaeontologist","bacteriologist","meteorologist",
              "mineralogist","naturalist","ichthyologist","mycologist","ornithologist",
              "anatomist","agronomist","immunologist","neuroscientist","computer_scientist",
              "programmer","climatologist","epidemiologist","microbiologist","pharmacologist",
              "virologist","seismologist","crystallographer","oceanographer")

humanities_l3 <- c("historian","philosopher","theologian","philologist","linguist",
                   "anthropologist","archaeologist","art_historian","scholar","educator",
                   "teacher","lecturer","political_scientist","romanist","orientalist",
                   "lexicographer","education","jurist","lawyer")

medicine_l3 <- c("physician","surgeon","dentist","psychiatrist","nurse","paediatrician",
                 "dermatologist","cardiologist","neurologist","pathologist","ophthalmologist",
                 "pharmacist","homeopath","medical")

ss_l3 <- c("economist","sociologist","psychologist","political_scientist")

classify_field <- function(occ) {
  case_when(
    occ %in% stem_l3       ~ "Hard STEM",
    occ %in% medicine_l3   ~ "Medicine/Health",
    occ %in% ss_l3         ~ "Social Science",
    occ %in% humanities_l3 ~ "Humanities/Law",
    TRUE                   ~ "Other/Unclassified"
  )
}

us <- data %>%
  filter(citizenship_1_b == "US", birth >= 1800, birth <= 1999,
         level1_main_occ == "Discovery/Science") %>%
  mutate(
    century = ifelse(birth < 1900, "1800s", "1900s"),
    level3_occ = if_else(
      level3_main_occ %in% c("academic", "professor"),
      coalesce(str_extract(level3_all_occ, kw_regex), level3_main_occ),
      level3_main_occ
    ),
    field = classify_field(level3_occ)
  )

# ---------------------------------------------------------------------------
# Stratified sample: 4 per field category = 20 total
# Exclude "academic"/"professor" (already validated separately)
# ---------------------------------------------------------------------------
set.seed(99)
sample_df <- us %>%
  filter(!level3_occ %in% c("academic", "professor")) %>%
  group_by(field) %>%
  slice_sample(n = 4) %>%
  ungroup() %>%
  select(name, birth, century, level3_occ, field)

cat("Sample by field:\n")
print(count(sample_df, field))

# ---------------------------------------------------------------------------
# Fetch Wikipedia summary for each
# ---------------------------------------------------------------------------
fetch_wiki <- function(name) {
  url <- paste0("https://en.wikipedia.org/api/rest_v1/page/summary/",
                utils::URLencode(name, reserved = TRUE))
  tryCatch({
    resp <- request(url) %>%
      req_headers("User-Agent" = "gtl-taxonomy-validation/1.0 (research)") %>%
      req_timeout(10) %>%
      req_perform()
    body <- resp_body_json(resp)
    list(description = body$description %||% NA_character_,
         extract     = str_trunc(body$extract %||% NA_character_, 300))
  }, error = function(e) list(description = NA_character_, extract = NA_character_))
}

cat("\nFetching Wikipedia bios...\n")
wiki <- map(sample_df$name, function(nm) { Sys.sleep(0.15); fetch_wiki(nm) })

sample_df <- sample_df %>%
  mutate(wiki_description = map_chr(wiki, "description"),
         wiki_extract     = map_chr(wiki, "extract"))

# ---------------------------------------------------------------------------
# Print full bios for manual review
# ---------------------------------------------------------------------------
cat("\n=== Full validation table ===\n")
sample_df %>%
  mutate(wiki_extract = str_trunc(wiki_extract, 250)) %>%
  select(name, birth, field, level3_occ, wiki_description, wiki_extract) %>%
  print(n = 20, width = 200)

write_csv(sample_df, "results/taxonomy_validation_full_classification.csv")
cat("\nSaved: results/taxonomy_validation_full_classification.csv\n")
