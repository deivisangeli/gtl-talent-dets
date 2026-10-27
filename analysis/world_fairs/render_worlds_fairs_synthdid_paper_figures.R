###############################################################################
# Render the preferred lead-20 pooled synthetic-DiD figures used in the paper.
#
# The estimator is not rerun here. This script reads the validated Dube-Zipperer
# CSV outputs and produces compact, publication-ready composites for main.tex.
# Run from any working directory:
#   Rscript analysis/world_fairs/render_worlds_fairs_synthdid_paper_figures.R
###############################################################################

rm(list = ls()); gc()

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
})

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (!length(script_arg)) stop("Run this file with Rscript.")
script_path <- normalizePath(
  sub("^--file=", "", script_arg[[1L]]), winslash = "/", mustWork = TRUE
)
repo_root <- normalizePath(
  file.path(dirname(script_path), "..", ".."), winslash = "/", mustWork = TRUE
)
source(file.path(repo_root, "paths.R"))

if (!dir.exists(TALENT_DETS_DATA_DIR)) {
  candidate <- file.path(
    "C:/Users", Sys.info()[["user"]], "Globtalent Dropbox", "gtl_talent_dets"
  )
  if (dir.exists(candidate)) TALENT_DETS_DATA_DIR <- candidate
}
if (!dir.exists(TALENT_DETS_DATA_DIR)) {
  stop("Set TALENT_DETS_DATA_DIR to the Dropbox project root.")
}

results_dir <- file.path(
  TALENT_DETS_DATA_DIR, "results", "worlds_fair", "synthdid",
  "ukds_1911_lgd_wikipedia_donor_pools", "match_v4_balanced_e0", "pooled_dz"
)
file_names <- c(
  att = "dz_pooled_att.csv",
  dynamic = "dz_pooled_dynamic_effects.csv",
  dynamic_trim = "dz_pooled_dynamic_by_trim.csv",
  trimming = "dz_progressive_trimming.csv",
  qc = "dz_qc.csv"
)
files <- setNames(file.path(results_dir, unname(file_names)), names(file_names))
if (!all(file.exists(files))) {
  stop("Preferred lead-20 Dube-Zipperer outputs are missing under: ", results_dir)
}

output_dir <- file.path(repo_root, "project", "figures")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

definition_levels <- c("Standard decades", "Alternative decades")
model_map <- c(
  lead20_standard = "Standard decades",
  lead20_alternative = "Alternative decades"
)

paper_theme <- theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    plot.margin = margin(8, 12, 8, 18)
  )

# Figure 7: the effect panels contain inverted confidence intervals for the
# reference-normalized effect. The rank panels retain randomization-null
# envelopes because a rank has no effect-scale confidence interval. Event time
# -10 is fixed by construction and is omitted from both forms of inference.
dynamic <- fread(files[["dynamic"]])[model_id %chin% names(model_map)]
if (nrow(dynamic) != 14L || any(dynamic$n_events != 4L) ||
    any(dynamic$reference_event_time != -10L)) {
  stop("Unexpected preferred lead-20 dynamic sample or reference period.")
}
dynamic[, decade_definition := factor(
  unname(model_map[model_id]), levels = definition_levels
)]

effect_panel <- dynamic[, .(
  model_id, decade_definition, event_time,
  panel = "Reference-normalized pooled effect",
  estimate = pooled_effect,
  lower = effect_ci_lower,
  upper = effect_ci_upper,
  p_value = randomization_p,
  reference = 0,
  band_type = "95% effect confidence interval"
)]
rank_panel <- dynamic[, .(
  model_id, decade_definition, event_time,
  panel = "Mean placebo percentile rank",
  estimate = mean_percentile_rank,
  lower = rank_null_lower,
  upper = rank_null_upper,
  p_value = rank_randomization_p,
  reference = 16 / 31,
  band_type = "95% rank null envelope"
)]
plot_dynamic <- rbindlist(list(effect_panel, rank_panel), use.names = TRUE)
plot_dynamic[, panel := factor(
  panel,
  levels = c(
    "Reference-normalized pooled effect",
    "Mean placebo percentile rank"
  )
)]
plot_dynamic[, significant := fifelse(
  is.finite(p_value), p_value < 0.05, FALSE
)]
plot_dynamic[, interval_segment := fifelse(
  event_time == -10L, "reference",
  fifelse(event_time < 0L, "pre", "post")
)]

pooled_plot <- ggplot(
  plot_dynamic,
  aes(x = event_time, y = estimate,
      group = interaction(panel, decade_definition))
) +
  geom_ribbon(
    data = plot_dynamic[is.finite(lower) & is.finite(upper)],
    aes(ymin = lower, ymax = upper, fill = band_type,
        group = interaction(panel, decade_definition, interval_segment)),
    alpha = 0.72, colour = NA
  ) +
  geom_hline(aes(yintercept = reference), colour = "grey45", linewidth = 0.35) +
  geom_vline(xintercept = -10, linetype = "longdash", colour = "grey45") +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "#b2182b") +
  geom_vline(xintercept = 20, linetype = "dotted", colour = "#2166ac") +
  geom_line(linewidth = 0.55, na.rm = TRUE) +
  geom_point(aes(colour = significant), size = 2.1, na.rm = TRUE) +
  facet_grid(panel ~ decade_definition, scales = "free_y") +
  scale_fill_manual(
    name = "Band",
    values = c(
      `95% effect confidence interval` = "grey76",
      `95% rank null envelope` = "#b7d4e8"
    )
  ) +
  scale_colour_manual(
    name = "Horizon test",
    values = c(`FALSE` = "black", `TRUE` = "#b22222"),
    labels = c(`FALSE` = "p >= 0.05", `TRUE` = "p < 0.05")
  ) +
  scale_x_continuous(breaks = seq(-30, 30, 10)) +
  labs(
    title = "Pooled synthetic-DiD event study: 20-year cohort lead",
    subtitle = paste0(
      "Reference e=-10 (grey long-dash); treatment e=0 (red dash); ",
      "fair e=+20 (blue dot)"
    ),
    x = "Event time (years relative to the shifted fair decade)",
    y = NULL
  ) +
  paper_theme

ggsave(
  file.path(output_dir, "WF_synthdid_lead20_pooled_event_study.png"),
  pooled_plot, width = 11.5, height = 8.2, units = "in", dpi = 300,
  bg = "white"
)

# Figures 8 and 9: event-study paths after each of the first three pre-fit
# trimming steps. These are deliberately not scalar ATT trimming plots.
dynamic_trim <- fread(files[["dynamic_trim"]])[
  model_id %chin% names(model_map) & n_events >= 2L
]
if (nrow(dynamic_trim) != 42L ||
    any(dynamic_trim$reference_event_time != -10L) ||
    any(dynamic_trim[worst_fitting_events_dropped == 0L]$n_events != 4L)) {
  stop("Unexpected preferred lead-20 trimming event-study sample.")
}

render_trim_event_study <- function(target_model_id, output_name, decade_label) {
  d <- copy(dynamic_trim[model_id == target_model_id])
  stage_order <- sort(unique(d$worst_fitting_events_dropped))
  stage_n <- d[, unique(n_events), by = worst_fitting_events_dropped]
  stage_labels <- setNames(
    sprintf("Drop %d (n = %d)",
            stage_n$worst_fitting_events_dropped, stage_n$V1),
    stage_n$worst_fitting_events_dropped
  )
  d[, panel := factor(
    stage_labels[as.character(worst_fitting_events_dropped)],
    levels = stage_labels[as.character(stage_order)]
  )]
  d[, significant := fifelse(
    is.finite(randomization_p), randomization_p < 0.05, FALSE
  )]
  d[, interval_segment := fifelse(
    event_time == -10L, "reference",
    fifelse(event_time < 0L, "pre", "post")
  )]

  p <- ggplot(d, aes(event_time, pooled_effect, group = panel)) +
    geom_ribbon(
      data = d[is.finite(effect_ci_lower) & is.finite(effect_ci_upper)],
      aes(ymin = effect_ci_lower, ymax = effect_ci_upper,
          group = interaction(panel, interval_segment)),
      fill = "grey76", alpha = 0.72, colour = NA
    ) +
    geom_hline(yintercept = 0, colour = "grey45", linewidth = 0.35) +
    geom_vline(xintercept = -10, linetype = "longdash", colour = "grey45") +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "#b2182b") +
    geom_vline(xintercept = 20, linetype = "dotted", colour = "#2166ac") +
    geom_line(linewidth = 0.55) +
    geom_point(aes(colour = significant), size = 2) +
    facet_wrap(~panel, ncol = 2, scales = "free_y") +
    scale_colour_manual(
      name = "Horizon test",
      values = c(`FALSE` = "black", `TRUE` = "#b22222"),
      labels = c(`FALSE` = "p >= 0.05", `TRUE` = "p < 0.05")
    ) +
    scale_x_continuous(breaks = seq(-30, 30, 10)) +
    labs(
      title = paste0(
        "Pooled event studies by pre-fit trimming stage: ", decade_label
      ),
      subtitle = paste0(
        "Grey ribbons: 95% inverted CIs; reference e=-10, treatment e=0, ",
        "fair e=+20"
      ),
      x = "Event time (years relative to the shifted fair decade)",
      y = "Reference-normalized pooled effect"
    ) +
    paper_theme

  ggsave(
    file.path(output_dir, output_name), p,
    width = 11.5, height = 7.4, units = "in", dpi = 300, bg = "white"
  )
}

render_trim_event_study(
  "lead20_standard", "WF_synthdid_lead20_standard_trim_event_studies.png",
  "standard decades"
)
render_trim_event_study(
  "lead20_alternative",
  "WF_synthdid_lead20_alternative_trim_event_studies.png",
  "alternative decades"
)

# Retain the earlier scalar trimming summary as a supplementary output. The
# manuscript's trimming figures are the event-study composites rendered above.
trimming <- fread(files[["trimming"]])[
  model_id %chin% names(model_map) & n_events >= 2L
]
trimming[, decade_definition := factor(
  unname(model_map[model_id]), levels = definition_levels
)]
trimming[, significant := randomization_p_rank < 0.05]
trimming[, point_label := sprintf("n=%d, p=%.3f", n_events,
                                  randomization_p_rank)]

trimming_plot <- ggplot(
  trimming,
  aes(x = worst_fitting_events_dropped, y = pooled_att)
) +
  geom_hline(yintercept = 0, colour = "grey45", linewidth = 0.35) +
  geom_line(linewidth = 0.55) +
  geom_point(aes(colour = significant), size = 2.3) +
  geom_text(aes(label = point_label), vjust = -0.8, size = 3.1) +
  facet_wrap(~decade_definition, nrow = 1) +
  scale_colour_manual(
    name = "Joint rank test",
    values = c(`FALSE` = "black", `TRUE` = "#b22222"),
    labels = c(`FALSE` = "p >= 0.05", `TRUE` = "p < 0.05")
  ) +
  scale_x_continuous(breaks = 0:2, limits = c(-0.28, 2.28)) +
  scale_y_continuous(expand = expansion(mult = c(0.12, 0.22))) +
  labs(
    title = "Progressive pre-fit trimming: 20-year cohort lead",
    x = "Worst pre-fitting events dropped",
    y = "Reference-normalized pooled ATT"
  ) +
  paper_theme

ggsave(
  file.path(output_dir, "WF_synthdid_lead20_progressive_trimming.png"),
  trimming_plot, width = 11.5, height = 5.4, units = "in", dpi = 300,
  bg = "white"
)

###############################################################################
# Manuscript tables: generated from the same validated CSVs as the figures.
###############################################################################

table_dir <- file.path(repo_root, "project", "tables")
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

fmt3 <- function(x) sprintf("%.3f", as.numeric(x))
effect_tex <- function(x) paste0("$", fmt3(x), "$")
definition_label <- function(x) fifelse(
  x == "standard", "Standard", "Alternative"
)

preferred_att <- fread(files[["att"]])[model_id %chin% names(model_map)]
if (nrow(preferred_att) != 2L || any(preferred_att$n_events != 4L)) {
  stop("Unexpected preferred pooled-ATT table input.")
}
preferred_att[, decade_order := match(decade_definition,
                                      c("standard", "alternative"))]
setorder(preferred_att, decade_order)
preferred_rows <- preferred_att[, paste(
  definition_label(decade_definition), n_events, effect_tex(pooled_att),
  fmt3(mean_percentile_rank), fmt3(randomization_p_two_sided),
  fmt3(randomization_p_any_post_horizon),
  fmt3(randomization_p_any_post_horizon_rank), sep = " & "
)]
preferred_rows <- paste0(preferred_rows, " \\\\")

trim_table <- fread(files[["trimming"]])[
  model_id %chin% names(model_map) & n_events >= 2L
]
trim_table[, decade_definition := fifelse(
  model_id == "lead20_standard", "standard", "alternative"
)]
trim_table[, decade_order := match(decade_definition,
                                    c("standard", "alternative"))]
setorder(trim_table, decade_order, worst_fitting_events_dropped)
trim_table[, removed_display := fifelse(
  worst_fitting_events_dropped == 0L, "None",
  gsub("; ", ", ", dropped_event_names, fixed = TRUE)
)]
trim_table[, removed_display := gsub(
  "BRISTOL", "Bristol", removed_display, fixed = TRUE
)]
trim_rows <- trim_table[, paste(
  definition_label(decade_definition), worst_fitting_events_dropped,
  removed_display, n_events, effect_tex(pooled_att),
  fmt3(mean_percentile_rank), fmt3(randomization_p_rank), sep = " & "
)]
trim_rows <- paste0(trim_rows, " \\\\")
trim_break <- sum(trim_table$decade_definition == "standard")
trim_rows <- append(trim_rows, "\\addlinespace", after = trim_break)

lead20_table <- c(
  "\\begin{table}[H]",
  "\\centering",
  "\\caption{Pooled synthetic-DiD estimates: preferred 20-year cohort-lead specification}",
  "\\label{tab:wf_synthdid_lead20}",
  "\\small",
  "\\setlength{\\tabcolsep}{4pt}",
  "\\begin{tabular}{lrrrrrr}",
  "\\toprule",
  paste0(
    "Decade definition & Events & Pooled effect & Mean rank & Rank $p$ & ",
    "Any-post $p$ & Any-post rank $p$ ", "\\\\"
  ),
  "\\midrule",
  preferred_rows,
  "\\bottomrule",
  "\\end{tabular}",
  paste0(
    "\\caption*{\\footnotesize Notes: Corrected balanced-window, rebuilt-donor-pool specification with exposure shifted twenty years before the fair. ",
    "Each event path subtracts the final untreated gap at $e=-10$; the pooled effect is the equal-weighted mean over $e=0,+10,+20,+30$. ",
    "Mean rank is the average event-specific percentile rank of the RMSPE-standardized effect in its in-space placebo distribution. ",
    "Rank $p$ is the two-sided joint-randomization $p$-value for the mean rank. The two any-post columns jointly test the four treated horizons. ",
    "All $p$-values use 1{,}000 joint randomization draws. Wembley is excluded because its corrected pre-panel is identically zero.}"
  ),
  "\\end{table}",
  "",
  "\\begin{table}[H]",
  "\\centering",
  "\\caption{Progressive pre-treatment-fit trimming: preferred 20-year cohort-lead specification}",
  "\\label{tab:wf_synthdid_lead20_trimming}",
  "\\small",
  "\\setlength{\\tabcolsep}{3.8pt}",
  "\\begin{tabular}{ll>{\\raggedright\\arraybackslash}p{4.4cm}rrrr}",
  "\\toprule",
  paste0(
    "Decades & Worst dropped & Events removed & Events & Pooled effect & ",
    "Mean rank & Rank $p$ ", "\\\\"
  ),
  "\\midrule",
  trim_rows,
  "\\bottomrule",
  "\\end{tabular}",
  "\\caption*{\\footnotesize Notes: Events are ranked from worst to best normalized pre-treatment RMSPE and removed progressively. The ordering uses no post-treatment outcomes and is reproduced inside every randomization draw. The manuscript stops before a one-event estimate; the full audit output retains that final stage.}",
  "\\end{table}"
)
writeLines(
  lead20_table,
  file.path(table_dir, "wf_synthdid_lead20_results.tex")
)

base_results <- file.path(
  TALENT_DETS_DATA_DIR, "results", "worlds_fair", "synthdid",
  "ukds_1911_lgd_wikipedia_donor_pools"
)
robustness_sources <- list(
  "Full window" = file.path(
    base_results, "match_v2_lagged", "pooled_dz", "dz_pooled_att.csv"
  ),
  "Balanced, original pools" = file.path(
    base_results, "match_v2_lagged", "pooled_dz_balanced_3p4",
    "dz_pooled_att.csv"
  ),
  "Balanced, rebuilt pools" = files[["att"]]
)
if (!all(file.exists(unlist(robustness_sources)))) {
  stop("A design-robustness pooled-ATT input is missing.")
}
robustness <- rbindlist(lapply(names(robustness_sources), function(label) {
  x <- fread(robustness_sources[[label]])
  x[, design := label]
  x
}), fill = TRUE, use.names = TRUE)
robustness[, design := factor(
  design, levels = names(robustness_sources)
)]
robustness[, decade_order := match(decade_definition,
                                    c("standard", "alternative"))]
setorder(robustness, design, cohort_lead_years, decade_order)
robustness_rows <- robustness[, paste(
  as.character(design), cohort_lead_years,
  definition_label(decade_definition), n_events, effect_tex(pooled_att),
  fmt3(mean_percentile_rank), fmt3(randomization_p_two_sided), sep = " & "
)]
robustness_rows <- paste0(robustness_rows, " \\\\")
robustness_rows <- append(robustness_rows, "\\addlinespace", after = 4L)
robustness_rows <- append(robustness_rows, "\\addlinespace", after = 9L)

robustness_table <- c(
  "\\begin{table}[H]",
  "\\centering",
  "\\caption{Pooled synthetic-DiD estimates across donor-pool and event-window designs}",
  "\\label{tab:wf_synthdid_design_robustness}",
  "\\small",
  "\\setlength{\\tabcolsep}{5pt}",
  "\\begin{tabular}{lllrrrr}",
  "\\toprule",
  paste0(
    "Design & Cohort lead & Decades & Events & Pooled effect & Mean rank & ",
    "Rank $p$ ", "\\\\"
  ),
  "\\midrule",
  robustness_rows,
  "\\bottomrule",
  "\\end{tabular}",
  paste0(
    "\\caption*{\\footnotesize Notes: The full-window design uses every available 1850--1960 bin and already begins treatment at $e=0$. ",
    "The balanced designs use the corrected three-pre/four-post window. The middle design retains the original lagged-outcome pools; the preferred design rebuilds pools from the fixed five-variable match. ",
    "Every path subtracts its final untreated gap, $e=-10$ in the corrected balanced designs. Rank $p$ is the two-sided joint-randomization $p$-value for the mean placebo percentile rank.}"
  ),
  "\\end{table}"
)
writeLines(
  robustness_table,
  file.path(table_dir, "wf_synthdid_design_robustness.tex")
)

message("Wrote paper figures to: ", output_dir)
message("Wrote paper tables to: ", table_dir)
