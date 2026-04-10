###############################################################################
# Project: Determinants of Talent Production
# Goal: STEM occupation labels, regex, and reclassification helper
#
# Defines:
#   stem_occ         — character vector of all level3_main_occ values that
#                      count as STEM (used for %in% matching)
#   stem_regex       — single regex string for level3_all_occ disambiguation
#   add_stem_dummy() — adds level3_occ and stem (0/1) columns to a dataframe
###############################################################################

library(tidyverse)

###############################################################################
# 1.  stem_occ — full list of STEM occupation strings
#     Covers English labels + non-English cognates present in level3_main_occ
###############################################################################

stem_occ <- c(
  # ---- English: hard sciences ----
  "physicist",
  "chemist",
  "mathematician",
  "biologist",
  "astronomer",
  "geologist",
  "zoologist",
  "botanist",
  "entomologist",
  "ornithologist",
  "mycologist",
  "mineralogist",
  "ecologist",
  "meteorologist",
  "naturalist",
  "agronomist",
  "geneticist",
  "biochemist",
  "physiologist",
  "anatomist",
  "bacteriologist",
  "astrophysicist",
  "immunologist",
  "epidemiologist",
  "virologist",
  "oceanographer",
  "climatologist",
  "neuroscientist",
  "seismologist",
  "crystallographer",
  "statistician",
  "palaeontologist",
  "paleontologist",
  "pharmacologist",
  "herpetologist",
  "ichthyologist",
  "malacologist",
  "lepidopterist",
  "arachnologist",
  "bryologist",
  "mammalogist",
  "embryologist",
  "histologist",
  "ethologist",
  "algologist",
  "conchologist",
  "taxonomist",
  "parasitologist",
  "acarologist",
  "carcinologist",
  "myriapodologist",
  "ostracodologist",
  "spongiologist",
  "planktologist",
  "limnologist",
  "geodesist",
  "cartographer",
  "hydrologist",
  "topographer",
  "hydrographer",
  # ---- English: engineering / technology ----
  "engineer",
  "inventor",
  "programmer",
  "computer_scientist",
  "aerospace",
  "astronaut",
  "cosmonaut",
  "aeronautical",
  "cryptanalyst",
  "bioinformatician",
  "dynamicist",
  "aerodynamics",
  "innovator",
  # ---- English: applied / natural history / interdisciplinary ----
  "scientist",
  "horticulturist",
  "geographer",
  "conservationist",
  "speleologist",
  "toxicologist",
  "endocrinologist",
  "oncologist",
  "demographer",
  "ethnologist",
  "biophysics",
  "developer",
  "gemologist",
  "limnolog",
  # ---- German / Scandinavian / pan-European ----
  "forscher",           # German: researcher
  "wissenschaftler",   # German: scientist
  "naturforscher",     # German: naturalist
  "naturforskare",     # Scandinavian: naturalist
  "forschungsreisender", # German: research traveler
  "vetenskap",         # Swedish: science
  "vetenskaplig",      # Swedish: scientific
  "forskning",         # Swedish/Norwegian: research
  "wissenschaft",      # German: science
  "forschung",         # German: research
  "erfinder",          # German: inventor
  "erfindung",         # German: invention
  # ---- Physics (multilingual) ----
  "physiker",
  "fysiker",
  "fyzik",
  "physik",
  "physique",
  "physicien",
  "físic",
  "físico",
  "física",
  "fisico",
  "fisic",
  # ---- Chemistry (multilingual) ----
  "chemiker",
  "kemist",
  "chimiste",
  "químic",
  "químico",
  "chimico",
  # ---- Mathematics (multilingual) ----
  "mathematiker",
  "matematik",
  "mathématicien",
  "matemátic",
  "matemático",
  "matemática",
  "matematiker",
  "matematico",
  # ---- Botany (multilingual) ----
  "botaniker",
  "botánica",
  "botánic",
  "botanico",
  "botaniste",
  # ---- Biology / biochemistry / biophysics (multilingual) ----
  "biologe",
  "biolog",
  "biólog",
  "biokemist",
  "biochemiker",
  "biochimiste",
  "bioquímic",
  "biofysik",
  # ---- Zoology (multilingual) ----
  "zoologe",
  "zoolog",
  "zoólog",
  "zoologue",
  # ---- Geology (multilingual) ----
  "geologe",
  "geolog",
  "géologue",
  "geólog",
  # ---- Physiology (multilingual) ----
  "physiologe",
  "fysiolog",
  "physiologiste",
  # ---- Anatomy (multilingual) ----
  "anatom",
  "anatomiste",
  "anatomista",
  # ---- Palaeontology (multilingual) ----
  "paläontologe",
  "paleontolog",
  "paleontólog",
  # ---- Neuroscience (multilingual) ----
  "neurowissenschaftler",
  "neurocientífic",
  # ---- Immunology (multilingual) ----
  "immunolog",
  "immunologe",
  # ---- Genetics (multilingual) ----
  "genetiker",
  "genetik",
  "genetica",
  # ---- Bacteriology (multilingual) ----
  "bakteriolog",
  "bacteriolog",
  # ---- Invention / engineering (multilingual) ----
  "inventeur",
  "uppfinnare",
  # ---- Aeronautics / aviation (multilingual) ----
  "aéronautique",
  "aeronautico",
  "aeronáutic",
  "aeronáutica",
  "aerodynamik",
  "flygteknik",
  # ---- Programming (multilingual) ----
  "programmerare",
  "programador",
  # ---- Agronomy (multilingual) ----
  "agronom",
  "agrónom",
  "agrônom",
  "agronome",
  # ---- Naturalist (multilingual) ----
  "naturaliste",
  "naturalista",
  # ---- Science / scientist (multilingual) ----
  "científic",
  "cientifico",
  "científico",
  "ciencia",
  "ciência",
  # ---- Specialty sciences (multilingual) ----
  "entomólog",
  "meteorólog",
  "mineralog",
  "zoólog",
  "geógraf",
  # ---- Astronomy (multilingual) ----
  "astrónomo",
  "astronome",
  "astronomo",
  "astrônom",
  "astronom",
  # ---- Earth / cave / volcano sciences (multilingual) ----
  "spéléologue",
  "vulcanologo",
  "vulcólog",
  # ---- Swedish science terms ----
  "físiker"   # included if present
)

# Remove any accidental duplicates
stem_occ <- unique(stem_occ)


###############################################################################
# 2.  stem_regex — for level3_all_occ disambiguation (English only)
#     Used to identify STEM signal within the all_occ string.
###############################################################################

stem_keywords <- c(
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
  "toxicologist", "geographer", "herpetologist", "limnologist",
  "hydrologist", "geodesist", "mammalogist", "embryologist",
  "histologist", "ethologist", "parasitologist", "bioinformatician",
  "cryptanalyst", "cosmonaut"
)

stem_regex <- str_c(unique(stem_keywords), collapse = "|")


###############################################################################
# 3.  add_stem_dummy() — reclassify and flag STEM individuals
#
#     Input:  dataframe with columns level3_main_occ and level3_all_occ
#     Output: same dataframe with two new columns:
#       level3_occ  — reclassified occupation (academic/professor resolved via
#                     level3_all_occ; all others unchanged)
#       stem        — integer 1/0: 1 if level3_occ %in% stem_occ
#
#     The reclassification regex covers ALL disciplines (not just STEM) so that
#     historians, physicians, etc. coded as "academic" are correctly labelled
#     rather than falling through to stem == 0 for the wrong reason.
###############################################################################

add_stem_dummy <- function(df) {

  # Full keyword list — all disciplines, compound terms first to avoid
  # partial matches (same ordering as taxonomy_us_scientists.R)
  full_reclass_regex <- str_c(c(
    "computer_scientist", "political_scientist", "art_historian",
    "astrophysicist", "biochemist", "bacteriologist", "climatologist",
    "crystallographer", "epidemiologist", "immunologist", "microbiologist",
    "neuroscientist", "oceanographer", "pharmacologist", "seismologist",
    "virologist",
    "physicist", "chemist", "mathematician", "biologist", "astronomer",
    "engineer", "inventor", "geologist", "zoologist", "botanist",
    "entomologist", "geneticist", "physiologist", "astronaut", "aerospace",
    "statistician", "ecologist", "palaeontologist", "meteorologist",
    "mineralogist", "naturalist", "ichthyologist", "mycologist",
    "ornithologist", "anatomist", "agronomist", "programmer",
    "physician", "surgeon", "psychiatrist", "paediatrician", "dermatologist",
    "cardiologist", "pathologist", "ophthalmologist", "neurologist",
    "dentist", "pharmacist", "nurse", "homeopath",
    "economist", "psychologist", "sociologist",
    "historian", "philosopher", "theologian", "philologist", "linguist",
    "anthropologist", "archaeologist", "romanist", "orientalist",
    "lexicographer", "jurist", "lawyer"
  ), collapse = "|")

  df %>%
    mutate(
      level3_occ = if_else(
        level3_main_occ %in% c("academic", "professor"),
        coalesce(str_extract(level3_all_occ, full_reclass_regex), level3_main_occ),
        level3_main_occ
      ),
      stem = as.integer(level3_occ %in% stem_occ)
    )
}
