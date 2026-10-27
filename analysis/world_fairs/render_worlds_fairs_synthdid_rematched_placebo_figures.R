###############################################################################
# Render the separate UK lead-20 synthetic-DiD specification whose placebo
# donor pools are rebuilt around every pseudo-treated LGD.
###############################################################################

rm(list = ls()); gc()
suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
})

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (!length(script_arg)) stop("Run this file with Rscript.")
script_path <- normalizePath(sub("^--file=", "", script_arg[[1L]]),
                            winslash = "/", mustWork = TRUE)
repo_root <- normalizePath(file.path(dirname(script_path), "..", ".."),
                           winslash = "/", mustWork = TRUE)
source(file.path(repo_root, "paths.R"))
if (!dir.exists(TALENT_DETS_DATA_DIR)) {
  candidate <- file.path("C:/Users", Sys.info()[["user"]],
                         "Globtalent Dropbox", "gtl_talent_dets")
  if (dir.exists(candidate)) TALENT_DETS_DATA_DIR <- candidate
}

results_dir <- file.path(
  TALENT_DETS_DATA_DIR, "results", "worlds_fair", "synthdid",
  "ukds_1911_lgd_wikipedia_donor_pools", "match_v4_balanced_e0",
  "pooled_dz_rematched_placebos"
)
dynamic_file <- file.path(results_dir, "dz_pooled_dynamic_effects.csv")
trim_file <- file.path(results_dir, "dz_pooled_dynamic_by_trim.csv")
att_file <- file.path(results_dir, "dz_pooled_att.csv")
event_file <- file.path(results_dir, "dz_event_effects.csv")
pool_qc_file <- file.path(results_dir, "dz_rematched_placebo_pool_qc.csv")
comparison_file <- file.path(results_dir, "dz_baseline_comparison.csv")
files <- c(dynamic_file, trim_file, att_file, event_file, pool_qc_file,
           comparison_file)
if (any(!file.exists(files))) stop("Missing rematched-placebo result file(s).")

figure_dir <- file.path(repo_root, "project", "figures")
table_dir <- file.path(repo_root, "project", "tables")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

model_labels <- c(
  lead20_standard = "Standard decades",
  lead20_alternative = "Alternative decades"
)
definition_levels <- unname(model_labels)
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

dynamic <- fread(dynamic_file)
if (nrow(dynamic) != 14L || any(dynamic$n_events != 4L) ||
    any(dynamic$reference_event_time != -10L)) {
  stop("Unexpected rematched-placebo dynamic sample.")
}
dynamic[, definition := factor(unname(model_labels[model_id]),
                               levels = definition_levels)]
effect_panel <- dynamic[, .(
  model_id, definition, event_time,
  panel = "Reference-normalized pooled effect", estimate = pooled_effect,
  lower = effect_ci_lower, upper = effect_ci_upper,
  p_value = randomization_p, reference = 0,
  band_type = "95% effect confidence interval"
)]
rank_panel <- dynamic[, .(
  model_id, definition, event_time,
  panel = "Mean rematched-placebo percentile rank",
  estimate = mean_percentile_rank, lower = rank_null_lower,
  upper = rank_null_upper, p_value = rank_randomization_p,
  reference = 0.5, band_type = "95% rank null envelope"
)]
plot_data <- rbindlist(list(effect_panel, rank_panel))
plot_data[, panel := factor(panel, levels = c(
  "Reference-normalized pooled effect",
  "Mean rematched-placebo percentile rank"
))]
plot_data[, significant := is.finite(p_value) & p_value < .05]
plot_data[, segment := fifelse(event_time == -10L, "reference",
                               fifelse(event_time < 0L, "pre", "post"))]

pooled_plot <- ggplot(plot_data, aes(event_time, estimate,
  group = interaction(panel, definition))) +
  geom_ribbon(
    data = plot_data[is.finite(lower) & is.finite(upper)],
    aes(ymin = lower, ymax = upper, fill = band_type,
        group = interaction(panel, definition, segment)),
    alpha = .72, colour = NA
  ) +
  geom_hline(aes(yintercept = reference), colour = "grey45", linewidth = .35) +
  geom_vline(xintercept = -10, linetype = "longdash", colour = "grey45") +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "#b2182b") +
  geom_vline(xintercept = 20, linetype = "dotted", colour = "#2166ac") +
  geom_line(linewidth = .55, na.rm = TRUE) +
  geom_point(aes(colour = significant), size = 2.1, na.rm = TRUE) +
  facet_grid(panel ~ definition, scales = "free_y") +
  scale_fill_manual(name = "Band", values = c(
    `95% effect confidence interval` = "grey76",
    `95% rank null envelope` = "#b7d4e8"
  )) +
  scale_colour_manual(name = "Horizon test",
    values = c(`FALSE` = "black", `TRUE` = "#b22222"),
    labels = c(`FALSE` = "p >= 0.05", `TRUE` = "p < 0.05")) +
  scale_x_continuous(breaks = seq(-30, 30, 10)) +
  labs(
    title = "Pooled synthetic DiD with rematched placebo donor pools",
    subtitle = paste0(
      "Lead 20; reference e=-10 (grey); treatment e=0 (red); ",
      "fair e=+20 (blue)"
    ),
    x = "Event time (years relative to shifted fair decade)", y = NULL
  ) + paper_theme

ggsave(file.path(figure_dir,
  "WF_synthdid_lead20_pooled_event_study_rematched_placebos.png"),
  pooled_plot, width = 11.5, height = 8.2, units = "in", dpi = 300,
  bg = "white")

trim <- fread(trim_file)[n_events >= 2L]
render_trim <- function(target_model, filename) {
  z <- trim[model_id == target_model]
  z[, trim_stage := factor(
    paste0("Worst-fit events dropped: ", worst_fitting_events_dropped,
           " (N=", n_events, ")"),
    levels = unique(paste0("Worst-fit events dropped: ",
      worst_fitting_events_dropped, " (N=", n_events, ")"))
  )]
  z[, segment := fifelse(event_time == -10L, "reference",
                         fifelse(event_time < 0L, "pre", "post"))]
  p <- ggplot(z, aes(event_time, pooled_effect, group = trim_stage)) +
    geom_ribbon(
      data = z[is.finite(effect_ci_lower) & is.finite(effect_ci_upper)],
      aes(ymin = effect_ci_lower, ymax = effect_ci_upper,
          group = interaction(trim_stage, segment)),
      fill = "grey77", alpha = .72
    ) +
    geom_hline(yintercept = 0, colour = "grey45", linewidth = .35) +
    geom_vline(xintercept = -10, linetype = "longdash", colour = "grey45") +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "#b2182b") +
    geom_vline(xintercept = 20, linetype = "dotted", colour = "#2166ac") +
    geom_line(linewidth = .55) + geom_point(size = 1.8) +
    facet_wrap(~trim_stage, ncol = 1, scales = "fixed") +
    scale_x_continuous(breaks = seq(-30, 30, 10)) +
    labs(title = paste0(model_labels[[target_model]],
                        ": progressive pre-fit trimming"),
         subtitle = "95% intervals use jointly assigned rematched placebos",
         x = "Event time", y = "Reference-normalized pooled effect") +
    paper_theme + theme(
      legend.position = "none", plot.title = element_text(size = 12),
      plot.margin = margin(8, 12, 8, 30)
    )
  ggsave(file.path(figure_dir, filename), p, width = 9, height = 9,
         units = "in", dpi = 300, bg = "white")
}
render_trim("lead20_standard",
            "WF_synthdid_lead20_standard_trim_event_studies_rematched_placebos.png")
render_trim("lead20_alternative",
            "WF_synthdid_lead20_alternative_trim_event_studies_rematched_placebos.png")

fmt3 <- function(x) sprintf("%.3f", as.numeric(x))
att <- fread(att_file)
events <- fread(event_file)
pool_qc <- fread(pool_qc_file)
comparison <- fread(comparison_file)
att[, order := match(decade_definition, c("standard", "alternative"))]
setorder(att, order)
label <- function(x) fifelse(x == "standard", "Standard", "Alternative")
main_rows <- att[, paste0(
  label(decade_definition), " & ", n_events, " & $", fmt3(pooled_att),
  "$ & ", fmt3(mean_percentile_rank), " & ",
  fmt3(randomization_p_two_sided), " & ",
  fmt3(randomization_p_att_two_sided), " \\\\"
)]
pool_qc[, display_order := match(model_id,
  c("lead20_standard", "lead20_alternative"))]
setorder(pool_qc, display_order, treatment_decade, treated_lgd_name)
qc_rows <- pool_qc[, paste0(
  fifelse(model_id == "lead20_standard", "Standard", "Alternative"),
  " & ", treated_lgd_name, " & ", treatment_decade, " & ",
  placebo_candidates, " & ", placebo_fits_requested, " & ",
  placebo_fits_successful, " & ", placebo_fits_failed, " \\\\"
)]
comparison[, definition := fifelse(model_id == "lead20_standard",
                                   "Standard", "Alternative")]
comparison[, definition_order := match(definition,
                                       c("Standard", "Alternative"))]
setorder(comparison, definition_order)
comparison_rows <- comparison[, paste0(
  definition, " & ", baseline_placebos_per_event, " & ",
  rematched_placebos_per_event_min, "--", rematched_placebos_per_event_max,
  " & ", fmt3(baseline_mean_rank), " & ", fmt3(rematched_mean_rank),
  " & ", fmt3(baseline_rank_p), " & ", fmt3(rematched_rank_p), " \\\\"
)]

tex <- c(
  "\\begin{table}[H]", "\\centering",
  "\\caption{UK pooled synthetic-DiD inference with rematched placebo donor pools}",
  "\\label{tab:wf_synthdid_rematched_placebos}", "\\small",
  "\\begin{tabular}{lrrrrr}", "\\toprule",
  "Decades & Events & Pooled effect & Mean rank & Rank $p$ & Effect $p$ \\\\",
  "\\midrule", main_rows, "\\bottomrule", "\\end{tabular}",
  paste0("\\caption*{\\footnotesize Notes: The treated-event estimates and ",
    "30-unit donor pools are unchanged. For every pseudo-treated LGD, a fresh ",
    "30-unit donor pool is selected using the same five matching variables and ",
    "a 50 km exclusion around the pseudo-LGD representative point. Inference ",
    "uses 1{,}000 joint draws with distinct pseudo units across the four events.}"),
  "\\end{table}", "",
  "\\begin{table}[H]", "\\centering",
  "\\caption{Rematched-placebo estimation sample by treated-event context}",
  "\\label{tab:wf_synthdid_rematched_placebo_counts}", "\\small",
  "\\begin{tabular}{llrrrrr}", "\\toprule",
  "Decades & Treated event & $T$ & Candidates & Attempted & Fit & Failed \\\\",
  "\\midrule", qc_rows, "\\bottomrule", "\\end{tabular}",
  paste0("\\caption*{\\footnotesize Notes: Every valid never-host candidate ",
    "is attempted. Numerical solver failures---concentrated among sparse, ",
    "degenerate pseudo-treated pre-period paths---are logged and excluded from ",
    "joint assignment.}"),
  "\\end{table}", "",
  "\\begin{table}[H]", "\\centering",
  "\\caption{Fixed-pool and rematched-placebo inference comparison}",
  "\\label{tab:wf_synthdid_placebo_comparison}", "\\small",
  "\\begin{tabular}{lrrrrrr}", "\\toprule",
  "Decades & Fixed $J$ & Rematched $J$ & Fixed rank & Rematched rank & Fixed $p$ & Rematched $p$ \\\\",
  "\\midrule", comparison_rows, "\\bottomrule", "\\end{tabular}",
  "\\caption*{\\footnotesize Notes: Point estimates are identical across procedures; only the placebo construction and resulting ranks and randomization inference change.}",
  "\\end{table}"
)
writeLines(tex, file.path(table_dir,
                          "wf_synthdid_lead20_rematched_placebos.tex"))
message("Wrote rematched-placebo figures to: ", figure_dir)
message("Wrote rematched-placebo tables to: ", table_dir)
