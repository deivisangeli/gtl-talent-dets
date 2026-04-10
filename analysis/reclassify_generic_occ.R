suppressPackageStartupMessages(library(tidyverse))

data <- read_csv("../prep/input/cross-verified-database.csv", show_col_types=FALSE)
us <- data %>%
  filter(citizenship_1_b == "US", birth >= 1800, birth <= 1999) %>%
  mutate(century = ifelse(birth < 1900, "1800s", "1900s"))

# ---------------------------------------------------------------------------
# Keyword priority list — order matters: more specific first, generic last.
# Multi-word terms use underscores as they appear in level3_all_occ.
# ---------------------------------------------------------------------------
priority_keywords <- c(
  # Hard STEM (compound first to avoid partial matches)
  "computer_scientist", "political_scientist", "art_historian",
  "astrophysicist", "biochemist", "bacteriologist",
  "climatologist", "crystallographer", "epidemiologist", "immunologist",
  "microbiologist", "neuroscientist", "oceanographer", "pharmacologist",
  "seismologist", "virologist",
  "physicist", "chemist", "mathematician", "biologist", "astronomer",
  "engineer", "inventor", "geologist", "zoologist", "botanist",
  "entomologist", "geneticist", "physiologist", "astronaut", "aerospace",
  "statistician", "ecologist", "palaeontologist", "meteorologist",
  "mineralogist", "naturalist", "ichthyologist", "mycologist",
  "ornithologist", "anatomist", "agronomist", "programmer",
  # Medicine
  "physician", "surgeon", "psychiatrist", "paediatrician", "dermatologist",
  "cardiologist", "pathologist", "ophthalmologist", "neurologist",
  "dentist", "pharmacist", "nurse", "homeopath", "doctor",
  # Social science
  "economist", "psychologist", "sociologist",
  # Humanities / law
  "historian", "philosopher", "theologian", "philologist",
  "linguist", "anthropologist", "archaeologist", "romanist", "orientalist",
  "lexicographer", "jurist", "lawyer"
)

# Single vectorized regex — matches first keyword found in string
kw_regex <- str_c(priority_keywords, collapse = "|")

# ---------------------------------------------------------------------------
# Apply reclassification to "academic" and "professor" — fully vectorized
# ---------------------------------------------------------------------------
us_reclass <- us %>%
  mutate(
    level3_reclass = if_else(
      level3_main_occ %in% c("academic", "professor"),
      coalesce(str_extract(level3_all_occ, kw_regex), level3_main_occ),
      level3_main_occ
    )
  )

# How many got reclassified?
cat("=== Reclassification coverage ===\n")
us_reclass %>%
  filter(level1_main_occ == "Discovery/Science",
         level3_main_occ %in% c("academic", "professor")) %>%
  mutate(reclassified = level3_reclass != level3_main_occ) %>%
  count(level3_main_occ, reclassified) %>%
  group_by(level3_main_occ) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  print()

# ---------------------------------------------------------------------------
# Validation sample: 50 from "academic", 50 from "professor" (reclassified only)
# ---------------------------------------------------------------------------
set.seed(42)

validation <- us_reclass %>%
  filter(level1_main_occ == "Discovery/Science",
         level3_main_occ %in% c("academic", "professor"),
         level3_reclass != level3_main_occ) %>%
  group_by(level3_main_occ) %>%
  slice_sample(n = 50) %>%
  ungroup() %>%
  select(name, birth, original = level3_main_occ, new_label = level3_reclass,
         all_occ_snippet = level3_all_occ) %>%
  mutate(all_occ_snippet = str_trunc(all_occ_snippet, 90))

cat("\n=== Validation sample (50 academic + 50 professor, reclassified) ===\n")
print(validation, n = 100, width = 160)

write_csv(validation, "results/taxonomy_reclass_validation.csv")
cat("\nSaved: results/taxonomy_reclass_validation.csv\n")

# ---------------------------------------------------------------------------
# Field breakdown before vs after reclassification
# ---------------------------------------------------------------------------
stem_l3 <- c("physicist", "chemist", "mathematician", "biologist", "astronomer",
              "engineer", "inventor", "geologist", "zoologist", "botanist",
              "entomologist", "geneticist", "biochemist", "physiologist",
              "astronaut", "aerospace", "astrophysicist", "statistician",
              "ecologist", "palaeontologist", "bacteriologist", "meteorologist",
              "mineralogist", "naturalist", "ichthyologist", "mycologist",
              "ornithologist", "anatomist", "agronomist", "immunologist",
              "neuroscientist", "computer_scientist", "programmer",
              "climatologist", "epidemiologist", "microbiologist",
              "pharmacologist", "virologist", "seismologist", "crystallographer",
              "oceanographer")

humanities_l3 <- c("historian", "philosopher", "theologian", "philologist",
                   "linguist", "anthropologist", "archaeologist", "art_historian",
                   "scholar", "educator", "teacher", "professor", "lecturer",
                   "academic", "political_scientist", "romanist", "orientalist",
                   "lexicographer", "education", "jurist", "lawyer")

medicine_l3 <- c("physician","surgeon","dentist","psychiatrist","nurse",
                 "paediatrician","dermatologist","cardiologist","neurologist",
                 "pathologist","ophthalmologist","pharmacist","homeopath",
                 "doctor","medical")

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

cat("\n=== Field breakdown BEFORE reclassification ===\n")
us %>%
  filter(level1_main_occ == "Discovery/Science") %>%
  mutate(field = classify_field(level3_main_occ)) %>%
  count(century, field) %>%
  group_by(century) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  arrange(century, desc(n)) %>%
  print()

cat("\n=== Field breakdown AFTER reclassification ===\n")
us_reclass %>%
  filter(level1_main_occ == "Discovery/Science") %>%
  mutate(field = classify_field(level3_reclass)) %>%
  count(century, field) %>%
  group_by(century) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  arrange(century, desc(n)) %>%
  print()
