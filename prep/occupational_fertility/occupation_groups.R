# OCC1950 -> analysis group, shared by build_professor_fertility.R (1910) and
# build_professor_nchild_decades.R (1850-1910) so both use identical groups.
#
# Order matters: the first matching rule wins, so the professional subgroups
# come before the residual professional class. A blank OCC1950 counts as no
# occupation.

OCC_GROUPS <- data.frame(
  group = c("professor", "pres_dean", "physician", "lawyer", "clergy", "teacher",
            "engineer", "natural_scientist", "other_professional",
            "farmer", "manager", "clerical", "sales", "craftsman", "operative",
            "service", "farm_laborer", "laborer", "no_occupation"),
  sql = c("OCC1950 BETWEEN 12 AND 29", "OCC1950 = 10", "OCC1950 = 75", "OCC1950 = 55",
          "OCC1950 = 9", "OCC1950 = 93", "OCC1950 BETWEEN 41 AND 49",
          "OCC1950 IN (7, 61, 62, 63, 67, 68, 69)", "OCC1950 BETWEEN 0 AND 99",
          "OCC1950 BETWEEN 100 AND 123", "OCC1950 BETWEEN 200 AND 290",
          "OCC1950 BETWEEN 300 AND 390", "OCC1950 BETWEEN 400 AND 490",
          "OCC1950 BETWEEN 500 AND 595", "OCC1950 BETWEEN 600 AND 690",
          "OCC1950 BETWEEN 700 AND 790", "OCC1950 BETWEEN 810 AND 890",
          "OCC1950 BETWEEN 900 AND 970", "OCC1950 >= 979 OR OCC1950 IS NULL"),
  stringsAsFactors = FALSE)

# The professional codes pooled as `all_professional` in the analyses.
PROFESSIONAL <- c("professor", "pres_dean", "physician", "lawyer", "clergy", "teacher",
                  "engineer", "natural_scientist", "other_professional")

group_case <- function(prefix = "") {
  rules <- gsub("OCC1950", paste0(prefix, "OCC1950"), OCC_GROUPS$sql, fixed = TRUE)
  paste0("CASE ", paste0("WHEN ", rules, " THEN '", OCC_GROUPS$group, "'", collapse = " "),
         " END")
}
