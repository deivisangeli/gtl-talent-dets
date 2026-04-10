###############################################################################
# Project: Determinants of Talent Production
# Goal: Taxonomy of US-born scientists in the cross-verified database
#       by birth century (1800s vs 1900s)
###############################################################################

library(tidyverse)

data <- read_csv("../prep/input/cross-verified-database.csv",
                 show_col_types = FALSE)

# US-born: citizenship at birth == United States
us_raw <- data %>%
  filter(citizenship_1_b == "US") %>%
  filter(birth >= 1800 & birth <= 1999) %>%
  mutate(century = ifelse(birth < 1900, "1800s", "1900s"))

###############################################################################
# Reclassify generic occupation labels ("academic", "professor") using
# level3_all_occ signal.  Validation on 100 obs gave 92% accuracy;
# main errors were "doctor" (PhD false positive, dropped) and priority order.
###############################################################################
reclass_keywords <- c(
  # Hard STEM — compound terms first to avoid partial matches
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
  # Medicine ("doctor" dropped: matches PhD titles, not physicians)
  "physician", "surgeon", "psychiatrist", "paediatrician", "dermatologist",
  "cardiologist", "pathologist", "ophthalmologist", "neurologist",
  "dentist", "pharmacist", "nurse", "homeopath",
  # Social science
  "economist", "psychologist", "sociologist", "political_scientist",
  # Humanities / law ("linguist" before "anthropologist" to fix priority errors)
  "art_historian", "historian", "philosopher", "theologian", "philologist",
  "linguist", "anthropologist", "archaeologist", "romanist", "orientalist",
  "lexicographer", "jurist", "lawyer"
)

kw_regex <- str_c(reclass_keywords, collapse = "|")

us <- us_raw %>%
  mutate(
    level3_occ = if_else(
      level3_main_occ %in% c("academic", "professor"),
      coalesce(str_extract(level3_all_occ, kw_regex), level3_main_occ),
      level3_main_occ
    )
  )

cat("=== US-born individuals in cross-verified database (1800-1999) ===\n\n")
cat("Total:", nrow(us), "\n")
cat("1800s:", sum(us$century == "1800s"), "\n")
cat("1900s:", sum(us$century == "1900s"), "\n\n")

###############################################################################
# Level 1 occupation breakdown
###############################################################################
cat("=== Level 1 Occupation ===\n")
us %>%
  count(century, level1_main_occ) %>%
  group_by(century) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  arrange(century, desc(n)) %>%
  print(n = 50)

###############################################################################
# Among Discovery/Science: Level 2 breakdown
###############################################################################
cat("\n=== Level 2 Occupation (Discovery/Science only) ===\n")
us %>%
  filter(level1_main_occ == "Discovery/Science") %>%
  count(century, level2_main_occ) %>%
  group_by(century) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  arrange(century, desc(n)) %>%
  print(n = 60)

###############################################################################
# Among Discovery/Science: Level 3 breakdown (STEM granularity)
###############################################################################
cat("\n=== Level 3 Occupation (Discovery/Science only, after reclassification) ===\n")
us %>%
  filter(level1_main_occ == "Discovery/Science") %>%
  count(century, level3_occ) %>%
  group_by(century) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  arrange(century, desc(n)) %>%
  print(n = 80)

###############################################################################
# STEM vs humanities within Discovery/Science, using Level 3
# Level 2 only distinguishes "Academia" vs "Explorer/Inventor/Developer" —
# it lumps historians and physicists together. Level 3 is needed.
# Uses reclassified level3_occ (academic/professor disambiguated via all_occ).
###############################################################################
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
                   "scholar", "educator", "teacher", "lecturer",
                   "political_scientist", "romanist", "orientalist",
                   "lexicographer", "education", "jurist", "lawyer")
# "academic" and "professor" intentionally excluded: unreclassified entries
# are genuinely unclassified and go to Other/Unclassified

medicine_l3 <- c("physician","surgeon","dentist","psychiatrist","nurse",
                 "paediatrician","dermatologist","cardiologist","neurologist",
                 "pathologist","ophthalmologist","pharmacist","homeopath","medical")

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

cat("\n=== STEM vs Humanities vs Medicine within Discovery/Science (Level 3) ===\n")
us %>%
  filter(level1_main_occ == "Discovery/Science") %>%
  mutate(field = classify_field(level3_occ)) %>%
  count(century, field) %>%
  group_by(century) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  arrange(century, desc(n)) %>%
  print()

###############################################################################
# Gender breakdown among Discovery/Science
###############################################################################
cat("\n=== Gender (Discovery/Science only) ===\n")
us %>%
  filter(level1_main_occ == "Discovery/Science") %>%
  count(century, gender) %>%
  group_by(century) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  arrange(century, desc(n)) %>%
  print()

###############################################################################
# Top names (visibility) in Discovery/Science — a sanity check
###############################################################################
cat("\n=== Top 20 most visible US scientists (by Wikipedia page views) ===\n")
us %>%
  filter(level1_main_occ == "Discovery/Science") %>%
  arrange(desc(wiki_readers_2015_2018)) %>%
  select(name, birth, level2_main_occ, level3_occ,
         citizenship_1_b, wiki_readers_2015_2018) %>%
  slice_head(n = 20) %>%
  print(width = 120)

###############################################################################
# Birth decade distribution
###############################################################################
cat("\n=== Discovery/Science by birth decade ===\n")
us %>%
  filter(level1_main_occ == "Discovery/Science") %>%
  mutate(decade = floor(birth / 10) * 10) %>%
  count(decade) %>%
  print(n = 30)

###############################################################################
# FIGURES
###############################################################################
library(ggplot2)
library(scales)

theme_clean <- theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"),
        legend.position = "bottom")

# --- Figure 1: Level 1 occupation share by century ---
fig1_data <- us %>%
  count(century, level1_main_occ) %>%
  group_by(century) %>%
  mutate(pct = n / sum(n),
         level1_main_occ = fct_reorder(level1_main_occ, pct, .fun = max))

fig1 <- ggplot(fig1_data, aes(x = level1_main_occ, y = pct, fill = century)) +
  geom_col(position = "dodge", width = 0.65) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_manual(values = c("1800s" = "#2166ac", "1900s" = "#d6604d"),
                    name = "Birth century") +
  labs(title = "Occupation mix of US-born notables by birth century",
       subtitle = "Cross-verified Wikipedia database, births 1800–1999",
       x = NULL, y = "Share of US-born individuals") +
  coord_flip() +
  theme_clean

ggsave("results/taxonomy_fig1_occ_level1.png", fig1,
       width = 7, height = 4.5, dpi = 150)

# --- Figure 2: STEM / Humanities / Medicine within Discovery/Science ---
fig2_data <- us %>%
  filter(level1_main_occ == "Discovery/Science") %>%
  mutate(field = classify_field(level3_occ)) %>%
  count(century, field) %>%
  group_by(century) %>%
  mutate(pct = n / sum(n),
         field = fct_reorder(field, pct, .fun = max))

fig2 <- ggplot(fig2_data, aes(x = field, y = pct, fill = century)) +
  geom_col(position = "dodge", width = 0.65) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_manual(values = c("1800s" = "#2166ac", "1900s" = "#d6604d"),
                    name = "Birth century") +
  labs(title = "Field composition of US Discovery/Science individuals",
       subtitle = "Cross-verified Wikipedia database, births 1800–1999",
       x = NULL, y = "Share within Discovery/Science") +
  coord_flip() +
  theme_clean

ggsave("results/taxonomy_fig2_stem_fields.png", fig2,
       width = 7, height = 4.5, dpi = 150)

# --- Figure 3: Top 15 Level 3 occupations, 1800s vs 1900s (dot plot) ---
top_l3 <- us %>%
  filter(level1_main_occ == "Discovery/Science",
         !level3_occ %in% c("academic", "professor")) %>%
  count(century, level3_occ) %>%
  group_by(century) %>%
  mutate(pct = n / sum(n)) %>%
  slice_max(pct, n = 15) %>%
  ungroup()

keep_occ <- unique(top_l3$level3_occ)

fig3_data <- us %>%
  filter(level1_main_occ == "Discovery/Science",
         level3_occ %in% keep_occ,
         !level3_occ %in% c("academic", "professor")) %>%
  count(century, level3_occ) %>%
  group_by(century) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup() %>%
  mutate(level3_occ = fct_reorder(level3_occ, pct, .fun = max))

fig3 <- ggplot(fig3_data, aes(x = pct, y = level3_occ, color = century)) +
  geom_line(aes(group = level3_occ), color = "grey70", linewidth = 0.8) +
  geom_point(size = 3) +
  scale_x_continuous(labels = percent_format(accuracy = 0.1)) +
  scale_color_manual(values = c("1800s" = "#2166ac", "1900s" = "#d6604d"),
                     name = "Birth century") +
  labs(title = "Top occupations among US scientists: 1800s vs 1900s",
       subtitle = "Share within Discovery/Science category",
       x = "Share", y = NULL) +
  theme_clean

ggsave("results/taxonomy_fig3_top_occ_dotplot.png", fig3,
       width = 7.5, height = 6, dpi = 150)

# --- Figure 4: Birth decade trend ---
fig4_data <- us %>%
  filter(level1_main_occ == "Discovery/Science") %>%
  mutate(decade = floor(birth / 10) * 10)

fig4 <- ggplot(fig4_data, aes(x = decade)) +
  geom_bar(fill = "#4393c3", width = 9) +
  scale_x_continuous(breaks = seq(1800, 1990, 20)) +
  scale_y_continuous(labels = comma) +
  labs(title = "US Discovery/Science individuals by birth decade",
       subtitle = "Cross-verified Wikipedia database",
       x = "Birth decade", y = "Count") +
  theme_clean

ggsave("results/taxonomy_fig4_birth_decade.png", fig4,
       width = 8, height = 4, dpi = 150)

cat("\nFigures saved to results/:\n")
cat("  taxonomy_fig1_occ_level1.png\n")
cat("  taxonomy_fig2_stem_fields.png\n")
cat("  taxonomy_fig3_top_occ_dotplot.png\n")
cat("  taxonomy_fig4_birth_decade.png\n")
