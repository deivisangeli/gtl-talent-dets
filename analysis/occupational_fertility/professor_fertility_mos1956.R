# Fertility of science professors from American Men of Science 1956, scientist
# by scientist, against the other scientists in the same directory.
#
# Source: the cleaned MoS 1956 file in Kim & Moser's replication package for
# "Women in Science: Lessons from the Baby Boom" (Econometrica,
# D:/ECMA_MS-22741_Replication.zip): 82,094 scientists with number of
# children, year of marriage, birth year, gender and up to 60 career entries,
# plus the authors' dictionary mapping raw career titles to 22 clean ranks.
# Only the two CSVs are extracted; none of the package's code is run.
#
# Definitions:
#   professor  ever held a faculty rank: Assistant / Associate / Full Professor
#              (medical versions included), Department Head, Dean or Professor
#              Emeritus. Instructors, teaching and research assistants,
#              fellows and clinical / adjunct / visiting titles alone do not count.
#   other      every other scientist; never_academic is the subset with no
#              academic title at all (Kim & Moser's academic == 0).
#   children   `child`, blank counted as 0, as Kim & Moser treat blanks as
#              non-parents; married = year of marriage reported.
#   completed  born 1905 or earlier, aged 50+ when the 1955-56 edition was compiled.
#
# Output: <Dropbox>/results/occupational_fertility/mos1956/ (summary.md first)
#
# Run from any working directory:
#   Rscript analysis/occupational_fertility/professor_fertility_mos1956.R

suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
  library(ggplot2)
})

repo_root <- local({
  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  candidate <- if (length(file_arg)) {
    dirname(dirname(normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/")))
  } else {
    Sys.getenv("GTL_REPO", unset = getwd())
  }
  while (!file.exists(file.path(candidate, "paths.R"))) {
    parent <- dirname(candidate)
    if (identical(parent, candidate)) stop("Could not locate the repo root (no paths.R found)")
    candidate <- parent
  }
  normalizePath(candidate, winslash = "/")
})
data_root <- local({
  env <- new.env()
  suppressWarnings(try(sys.source(file.path(repo_root, "paths.R"), envir = env), silent = TRUE))
  candidates <- c(
    get0("TALENT_DETS_DATA_DIR", envir = env, ifnotfound = ""),
    Sys.getenv("DET_DIR"),
    file.path("C:/Users", Sys.info()[["user"]], "Globtalent Dropbox/gtl_talent_dets")
  )
  hit <- candidates[nzchar(candidates) & dir.exists(candidates)]
  if (!length(hit)) stop("Could not locate the Dropbox data root. Set TALENT_DETS_DATA_DIR or DET_DIR.")
  normalizePath(hit[1], winslash = "/", mustWork = TRUE)
})

OUT_DIR <- Sys.getenv("MOS1956_RESULTS_DIR",
                      unset = file.path(data_root, "results/occupational_fertility/mos1956"))
COMPLETED_BY <- 1905L
DECADES <- seq(1870L, 1920L, 10L)

say <- function(...) message(format(Sys.time(), "%H:%M:%S"), "  ", ...)
stop_if <- function(condition, ...) if (isTRUE(condition)) stop(..., call. = FALSE)
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
write_out <- function(x, name) {
  fwrite(x, file.path(OUT_DIR, name))
  say("wrote ", name)
}

# Extraction, validation and professor status are shared with
# prep/occupational_fertility/build_professor_birth_schedule.R.
source(file.path(repo_root, "prep/occupational_fertility/mos1956_helpers.R"))
loaded <- mos1956_load(data_root)
d <- loaded$d
rank_counts <- loaded$rank_counts
excluded <- d[, .(unknown_gender = sum(!gender %in% c("male", "female")),
                  missing_birthyear = sum(gender %in% c("male", "female") & is.na(birthyear)),
                  outside_1870_1929 = sum(gender %in% c("male", "female") & !is.na(birthyear) &
                                            !decade %in% DECADES))]
d <- d[gender %in% c("male", "female") & decade %in% DECADES]
d[, group := fifelse(professor, "professor", "other")]
say(d[gender == "male", sum(professor)], " male professors of ", d[gender == "male", .N], " men")

# --------------------------------------------------------------------------- #
# 1. Group summaries by gender x group x birth decade
# --------------------------------------------------------------------------- #
summarise_cell <- function(x) {
  x[, .(n = .N, share_married = mean(married),
        mean_children = mean(children), se_children = sd(children) / sqrt(.N),
        n_married = sum(married),
        mean_children_married = mean(children[married]),
        se_children_married = sd(children[married]) / sqrt(sum(married)),
        share_childless = mean(children == 0),
        share_childless_married = mean(children[married] == 0),
        share_0 = mean(children == 0), share_1 = mean(children == 1), share_2 = mean(children == 2),
        share_3 = mean(children == 3), share_4 = mean(children == 4), share_5plus = mean(children >= 5))]
}
groups <- rbind(d, copy(d[academic == 0])[, group := "never_academic"])
summary_tab <- rbindlist(list(
  groups[, summarise_cell(.SD), by = .(gender, group, cohort = as.character(decade))],
  groups[birthyear <= COMPLETED_BY, summarise_cell(.SD), by = .(gender, group)][
    , cohort := paste0("completed (born <= ", COMPLETED_BY, ")")],
  groups[, summarise_cell(.SD), by = .(gender, group)][, cohort := "all 1870-1929"]
), use.names = TRUE)
summary_tab[, group := factor(group, levels = c("professor", "other", "never_academic"))]
setorder(summary_tab, gender, cohort, group)
summary_tab[, group := as.character(group)]

# --------------------------------------------------------------------------- #
# 2. Poisson IRRs: professors against other scientists, birth-year FE
# --------------------------------------------------------------------------- #
irr <- function(label, data, fml) {
  m <- fepois(fml, data = data, vcov = "hetero")
  ct <- coeftable(m)
  row <- grep("^professor", rownames(ct))
  b <- ct[row, 1]; se <- ct[row, 2]
  data.table(model = label, n = nobs(m), professors = sum(data[[all.vars(fml)[2]]]),
             irr = exp(b), ci_low = exp(b - 1.96 * se), ci_high = exp(b + 1.96 * se), p_value = ct[row, 4])
}
men <- d[gender == "male"]
men[, field := fifelse(is.na(field_visher) | !nzchar(field_visher), "missing", field_visher)]
irrs <- rbindlist(list(
  irr("men, all", men, children ~ professor | birthyear),
  irr("men, married", men[married == TRUE], children ~ professor | birthyear),
  irr(paste0("men, completed (born <= ", COMPLETED_BY, ")"), men[birthyear <= COMPLETED_BY],
      children ~ professor | birthyear),
  irr(paste0("men, married, completed (born <= ", COMPLETED_BY, ")"),
      men[married == TRUE & birthyear <= COMPLETED_BY], children ~ professor | birthyear),
  irr("men, all, field FE", men, children ~ professor | birthyear + field),
  irr("men, all, vs never-academic only", men[professor | academic == 0], children ~ professor | birthyear),
  irr("men, all, professor by age 40 (later professors dropped)", men[professor_by40 | !professor],
      children ~ professor_by40 | birthyear),
  irr("women, all", d[gender == "female"], children ~ professor | birthyear),
  irr("women, married", d[gender == "female" & married == TRUE], children ~ professor | birthyear)
))
irrs[, model := factor(model, levels = model)]

# --------------------------------------------------------------------------- #
# 3. Figure: mean children by birth decade, men
# --------------------------------------------------------------------------- #
SURFACE <- "#fcfcfb"; INK <- "#0b0b0b"; INK_2 <- "#52514e"; GRID <- "#e4e3df"
BLUE <- "#2a78d6"; ORANGE <- "#eb6834"
fig_d <- rbind(
  summary_tab[gender == "male" & group %in% c("professor", "other") & cohort %in% as.character(DECADES),
              .(group, decade = as.integer(cohort), panel = "All men", mean = mean_children, se = se_children)],
  summary_tab[gender == "male" & group %in% c("professor", "other") & cohort %in% as.character(DECADES),
              .(group, decade = as.integer(cohort), panel = "Married men", mean = mean_children_married,
                se = se_children_married)])
fig_d[, `:=`(group = factor(fifelse(group == "professor", "Professors", "Other scientists"),
                            levels = c("Professors", "Other scientists")),
             incomplete = decade + 9L > COMPLETED_BY)]
p <- ggplot(fig_d, aes(decade, mean, colour = group, group = group)) +
  geom_line(linewidth = 0.7, position = position_dodge(width = 2.5)) +
  geom_errorbar(aes(ymin = mean - 1.96 * se, ymax = mean + 1.96 * se, alpha = incomplete), width = 1.5,
                linewidth = 0.5, position = position_dodge(width = 2.5)) +
  geom_point(aes(alpha = incomplete), size = 2.4, shape = 21, fill = SURFACE, stroke = 0.8,
             position = position_dodge(width = 2.5)) +
  facet_wrap(~ panel, nrow = 1) +
  scale_colour_manual(values = c(BLUE, ORANGE), name = NULL) +
  scale_alpha_manual(values = c(`FALSE` = 1, `TRUE` = 0.45), guide = "none") +
  scale_x_continuous(breaks = DECADES, labels = paste0(DECADES, "s")) +
  labs(title = "Children per scientist, American Men of Science 1956",
       subtitle = "Men; professors = ever held a faculty rank; 95% confidence intervals",
       x = "Birth decade", y = "Mean number of children",
       caption = paste0("Lighter points: cohorts born after ", COMPLETED_BY,
                        ", whose fertility was not complete by 1956. Blank child counts as 0.")) +
  theme_minimal(base_size = 10) +
  theme(plot.background = element_rect(fill = SURFACE, colour = NA),
        panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = GRID, linewidth = 0.3),
        axis.text = element_text(colour = INK_2), axis.title = element_text(colour = INK_2),
        strip.text = element_text(colour = INK, face = "bold", hjust = 0),
        plot.title = element_text(colour = INK, face = "bold"),
        plot.subtitle = element_text(colour = INK_2), plot.caption = element_text(colour = INK_2),
        legend.position = "top", legend.justification = "left")
ggsave(file.path(OUT_DIR, "fig_children_by_cohort.png"), p, width = 9, height = 5, dpi = 200)
ggsave(file.path(OUT_DIR, "fig_children_by_cohort_pdf.pdf"),
       p + labs(title = NULL, subtitle = NULL, caption = NULL), width = 9, height = 4.6)

# --------------------------------------------------------------------------- #
# Write
# --------------------------------------------------------------------------- #
write_out(summary_tab, "group_summary.csv")
write_out(irrs, "poisson_irr.csv")
write_out(rank_counts, "faculty_rank_counts.csv")
write_out(cbind(excluded, kept = nrow(d), kept_men = nrow(men)), "sample_exclusions.csv")

pick <- function(g, grp, coh, col) summary_tab[gender == g & group == grp & cohort == coh][[col]]
comp <- paste0("completed (born <= ", COMPLETED_BY, ")")
f2 <- function(x) formatC(x, format = "f", digits = 2)
pc <- function(x) paste0(formatC(100 * x, format = "f", digits = 1), "%")
irr_line <- function(label) {
  r <- irrs[model == label]
  sprintf("%s [%s, %s] (n = %s, professors %s)", f2(r$irr), f2(r$ci_low), f2(r$ci_high),
          format(r$n, big.mark = ","), format(r$professors, big.mark = ","))
}
blank_married <- men[married == TRUE, mean(is.na(child))]
lines <- c(
  "# Professors' fertility in American Men of Science 1956", "",
  "*Built by `analysis/occupational_fertility/professor_fertility_mos1956.R` from the cleaned",
  "MoS 1956 file in Kim & Moser's replication package (Econometrica, ECMA MS-22741).*", "",
  "## Headline (men)", "",
  sprintf("- **Completed fertility (born <= %d).** Professors %s children (married: %s), other scientists %s (married: %s).",
          COMPLETED_BY, f2(pick("male", "professor", comp, "mean_children")),
          f2(pick("male", "professor", comp, "mean_children_married")),
          f2(pick("male", "other", comp, "mean_children")),
          f2(pick("male", "other", comp, "mean_children_married"))),
  sprintf("- **Childless.** Professors %s, other scientists %s (completed cohorts).",
          pc(pick("male", "professor", comp, "share_childless")),
          pc(pick("male", "other", comp, "share_childless"))),
  sprintf("- **Married.** Professors %s, other scientists %s (completed cohorts).",
          pc(pick("male", "professor", comp, "share_married")),
          pc(pick("male", "other", comp, "share_married"))),
  "- **Poisson IRR, professors vs other scientists, birth-year FE:**",
  paste0("  - all men: ", irr_line("men, all")),
  paste0("  - married men: ", irr_line("men, married")),
  paste0("  - completed cohorts: ", irr_line(paste0("men, completed (born <= ", COMPLETED_BY, ")"))),
  paste0("  - married, completed: ", irr_line(paste0("men, married, completed (born <= ", COMPLETED_BY, ")"))),
  paste0("  - with field FE: ", irr_line("men, all, field FE")),
  paste0("  - against never-academic scientists only: ", irr_line("men, all, vs never-academic only")),
  paste0("  - professor by age 40: ", irr_line("men, all, professor by age 40 (later professors dropped)")),
  "", "## Women", "",
  paste0("- IRR all women: ", irr_line("women, all"), "; married women: ", irr_line("women, married"), "."),
  "", "## Definitions and caveats", "",
  "- **Professor:** ever held Assistant / Associate / Full Professor (medical included), Department Head,",
  "  Dean or Professor Emeritus, via Kim & Moser's title dictionary. Instructors, assistants, fellows",
  "  and clinical / adjunct / visiting titles alone do not count. Counts by rank: `faculty_rank_counts.csv`.",
  "- **Children:** `child`, blank = 0 as in Kim & Moser. A blank may also be an unreported count: ",
  sprintf("  %s of married men have a blank.", pc(blank_married)),
  "- **Coverage:** AMS lists scientists only, so these are science faculty, not all college faculty;",
  "  and only scientists alive and listed in 1955-56.",
  sprintf("- **Cohorts:** births 1870-1929; men born after %d were still having children in 1956.", COMPLETED_BY),
  sprintf("- **Excluded:** %s of unknown gender, %s without birth year, %s born outside 1870-1929.",
          format(excluded$unknown_gender, big.mark = ","), format(excluded$missing_birthyear, big.mark = ","),
          format(excluded$outside_1870_1929, big.mark = ",")),
  "", "Tables: `group_summary.csv` (gender x group x cohort), `poisson_irr.csv`. Figure: `fig_children_by_cohort.png`.")
writeLines(lines, file.path(OUT_DIR, "summary.md"), useBytes = TRUE)
say("wrote summary.md")
print(irrs)
