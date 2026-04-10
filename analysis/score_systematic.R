suppressPackageStartupMessages(library(tidyverse))
df <- read_csv("results/taxonomy_validation_systematic.csv", show_col_types=FALSE)

label_patterns <- c(
  political_scientist = "political scien",
  economist           = "econom",
  entomologist        = "entomolog",
  archaeologist       = "archaeolog",
  mineralogist        = "mineralog|geolog",
  physician           = "physician|doctor of medicine|internist|medical|immunolog",
  historian           = "historian|classicist|hittitolog|medievalist",
  engineer            = "engineer|mechanic|aerospace",
  anthropologist      = "anthropolog|ethnomusicolog",
  physiologist        = "physiolog",
  climatologist       = "climat|physicist",
  orientalist         = "orientalist|hittitolog",
  neurologist         = "neurolog|neurophysiol",
  sociologist         = "sociolog",
  linguist            = "linguist",
  chemist             = "chem",
  psychologist        = "psycholog",
  paediatrician       = "physician|pediatr|medical",
  astronaut           = "astronaut",
  computer_scientist  = "computer scien|computing",
  philosopher         = "philosoph|theologian",
  romanist            = "romanist",
  jurist              = "jurist|legal|politician",
  lawyer              = "lawyer|attorney|politician|legal",
  pathologist         = "patholog",
  biologist           = "biolog",
  immunologist        = "immunolog"
)

df_r <- df %>%
  filter(reclassified) %>%
  mutate(
    combined = str_to_lower(paste(coalesce(wiki_description, ""), coalesce(wiki_extract, ""))),
    pattern  = label_patterns[new_label],
    verdict = case_when(
      is.na(wiki_description) & is.na(wiki_extract) ~ "no_page",
      is.na(pattern)                                 ~ "unverifiable",
      # manual overrides for known edge cases
      name == "Thomas_H._Maren"      ~ "wrong",   # pharmacologist, not inventor
      name == "Ada_Maria_Isasi-Diaz" ~ "wrong",   # theologian, labeled philosopher
      str_detect(combined, coalesce(pattern, "NOMATCH")) ~ "correct",
      TRUE                           ~ "unverifiable"
    )
  )

cat("=== Reclassified rows: verdict (N = 50) ===\n")
df_r %>% count(verdict) %>% mutate(pct = round(100 * n / sum(n), 1)) %>%
  arrange(desc(n)) %>% print()

verif <- df_r %>% filter(verdict %in% c("correct", "wrong"))
cat(sprintf("\nAmong verifiable (n = %d): %d correct = %.0f%%\n",
            nrow(verif), sum(verif$verdict == "correct"),
            100 * mean(verif$verdict == "correct")))

cat("\n=== Wrong reclassifications ===\n")
df_r %>% filter(verdict == "wrong") %>%
  select(name, birth, new_label, wiki_description) %>% print(width = 160)

cat("\n=== No Wikipedia page ===\n")
df_r %>% filter(verdict == "no_page") %>%
  select(name, birth, new_label) %>% print()

cat("\n=== Unchanged rows: missed reclassifications (false negatives) ===\n")
df %>% filter(!reclassified, agreement == "missed") %>%
  select(name, birth, original, new_label, wiki_description) %>%
  print(width = 160)

fn_rate <- df %>% filter(!reclassified) %>%
  summarise(rate = round(100 * mean(agreement == "missed"), 1)) %>% pull()
cat(sprintf("\nFalse negative rate: %s%% of unchanged rows had a specific label in Wikipedia\n", fn_rate))
