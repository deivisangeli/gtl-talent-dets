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
  "ukds_1911_lgd_wikipedia_donor_pools", "match_v3_balanced", "pooled_dz"
)
dynamic_file <- file.path(results_dir, "dz_pooled_dynamic_effects.csv")
trimming_file <- file.path(results_dir, "dz_progressive_trimming.csv")
if (!all(file.exists(dynamic_file, trimming_file))) {
  stop("Preferred lead-20 Dube-Zipperer outputs are missing under: ", results_dir)
}

output_dir <- file.path(repo_root, "project", "figures")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

definition_levels <- c("Standard decades", "Alternative decades")
model_map <- c(
  lead20_standard = "Standard decades",
  lead20_alternative = "Alternative decades"
)

dynamic <- fread(dynamic_file)[model_id %chin% names(model_map)]
if (nrow(dynamic) != 14L || any(dynamic$n_events != 5L)) {
  stop("Unexpected preferred lead-20 dynamic sample.")
}
dynamic[, decade_definition := factor(
  unname(model_map[model_id]), levels = definition_levels
)]

effect_panel <- dynamic[, .(
  model_id, decade_definition, event_time,
  panel = "Pooled ATT",
  estimate = pooled_effect,
  lower = null_lower,
  upper = null_upper,
  p_value = randomization_p,
  reference = 0
)]
rank_panel <- dynamic[, .(
  model_id, decade_definition, event_time,
  panel = "Mean placebo percentile rank",
  estimate = mean_percentile_rank,
  lower = rank_null_lower,
  upper = rank_null_upper,
  p_value = rank_randomization_p,
  reference = 16 / 31
)]
plot_dynamic <- rbindlist(list(effect_panel, rank_panel))
plot_dynamic[, panel := factor(
  panel, levels = c("Pooled ATT", "Mean placebo percentile rank")
)]
plot_dynamic[, significant := p_value < 0.05]

paper_theme <- theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10),
    legend.position = "none",
    plot.margin = margin(8, 12, 8, 8)
  )

pooled_plot <- ggplot(
  plot_dynamic,
  aes(x = event_time, y = estimate, group = interaction(panel, decade_definition))
) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey82", alpha = 0.9) +
  geom_hline(aes(yintercept = reference), colour = "grey45", linewidth = 0.35) +
  geom_vline(xintercept = 10, linetype = "dashed", colour = "grey45") +
  geom_line(linewidth = 0.55) +
  geom_point(aes(colour = significant), size = 2.1) +
  facet_grid(panel ~ decade_definition, scales = "free_y") +
  scale_colour_manual(values = c(`FALSE` = "black", `TRUE` = "#b22222")) +
  scale_x_continuous(breaks = seq(-30, 30, 10)) +
  labs(
    title = "Pooled synthetic-DiD event study: 20-year cohort lead",
    subtitle = paste0(
      "Grey areas are 95% joint-randomization null bands; red points have ",
      "uncorrected horizon p < 0.05"
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

trimming <- fread(trimming_file)[model_id %chin% names(model_map)]
if (nrow(trimming) != 8L || any(!trimming$n_events %in% 2:5)) {
  stop("Unexpected preferred lead-20 trimming sample.")
}
trimming[, decade_definition := factor(
  unname(model_map[model_id]), levels = definition_levels
)]
trimming[, significant := randomization_p_rank < 0.05]
trimming[, point_label := sprintf("n=%d, p=%.3f", n_events, randomization_p_rank)]

trimming_plot <- ggplot(
  trimming,
  aes(x = worst_fitting_events_dropped, y = pooled_att)
) +
  geom_hline(yintercept = 0, colour = "grey45", linewidth = 0.35) +
  geom_line(linewidth = 0.55) +
  geom_point(aes(colour = significant), size = 2.3) +
  geom_text(
    aes(label = point_label), vjust = -0.8, size = 3.1,
    check_overlap = TRUE
  ) +
  facet_wrap(~decade_definition, nrow = 1) +
  scale_colour_manual(values = c(`FALSE` = "black", `TRUE` = "#b22222")) +
  scale_x_continuous(breaks = 0:3, limits = c(-0.28, 3.28)) +
  scale_y_continuous(expand = expansion(mult = c(0.12, 0.22))) +
  labs(
    title = "Progressive pre-fit trimming: 20-year cohort lead",
    subtitle = "Events are dropped from worst to best normalized pre-treatment fit; p is the joint rank-randomization p-value",
    x = "Worst pre-fitting events dropped",
    y = "Pooled ATT"
  ) +
  paper_theme

ggsave(
  file.path(output_dir, "WF_synthdid_lead20_progressive_trimming.png"),
  trimming_plot, width = 11.5, height = 5.4, units = "in", dpi = 300,
  bg = "white"
)

message("Wrote paper figures to: ", output_dir)
