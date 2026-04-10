suppressPackageStartupMessages(library(tidyverse))

# Manual scoring of the 100-obs validation sample
# Based on known facts about each person + all_occ_snippet
# Errors annotated with reason

errors <- tribble(
  ~row, ~name,                      ~new_label,          ~correct_label,      ~reason,
  16,   "Haddon_Robinson",           "doctor",            "theologian",        "doctor=PhD, not physician; theologian appears in all_occ",
  24,   "Gordon_A._Haaland",         "doctor",            "academic",          "doctor=PhD; college president, no specific discipline",
  35,   "S._I._Hayakawa",            "anthropologist",    "linguist",          "priority: anthropologist before linguist; he's primarily a semanticist/linguist",
  40,   "Thaddeus_Seymour",          "doctor",            "academic",          "doctor=PhD; college president",
  55,   "Dale_Jamieson",             "jurist",            "philosopher",       "Wikidata label error; he's an environmental philosopher",
  75,   "David_Cordle",              "doctor",            "academic",          "doctor=PhD; literature professor",
  84,   "Charles_Figley",            "immunologist",      "psychologist",      "Wikidata label error; he's a trauma psychologist",
  98,   "Ira_Shor",                  "doctor",            "academic",          "doctor=PhD; composition/rhetoric professor"
)

cat("=== Validation errors (out of 100) ===\n")
print(errors, width = 120)

cat("\nTotal errors:", nrow(errors), "/ 100\n")
cat("Accuracy:", 100 - nrow(errors), "%\n\n")

cat("=== Errors by cause ===\n")
errors %>% count(reason) %>% arrange(desc(n)) %>% print(width=120)

cat("\n=== doctor false positives ===\n")
errors %>% filter(new_label == "doctor") %>% nrow() %>% cat("out of", nrow(errors), "total errors\n")
