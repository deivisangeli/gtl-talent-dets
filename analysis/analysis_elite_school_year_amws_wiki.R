###############################################################################
# Yearly event-study spec for the elite-school analysis using AMWS 1955 births
# alongside Wikipedia STEM births.
#
# Events in [1880, 1910]; event window e in [-10, +10] years.
# Three treatment definitions, run separately:
#   (A) HIGH-ACCESS: first qualifying (crit_high_access_strict) school
#       in county opens in [1880, 1910]. Sample = treated + never-treated.
#   (B) LOW-ACCESS:  first non-strict school in county opens in [1880, 1910]
#       AND no high-access school ever in county. Sample = treated + never-treated.
#   (C) HIGH vs LOW: head-to-head -- high-access treated counties vs the
#       low-access counties as the comparison group. Tests the "elite-ness"
#       margin holding "had a school built" fixed.
###############################################################################

suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
  library(ggplot2)
  library(patchwork)
})
source("../paths.R")

if (basename(getwd()) != "analysis" && dir.exists("analysis")) setwd("analysis")

out_dir <- "results/elite_school_event_studies_year_amws"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

EVENT_LO <- 1880L
EVENT_HI <- 1910L
E_WIN    <- 10L
REF_E    <- -1L
NEVER_G  <- .Machine$integer.max

# ---- Inputs -----------------------------------------------------------------
yr_panel <- fread(file.path(DATA_OUTPUT, "us_panel_county_stem_year_1800.csv"))
amws     <- fread(file.path(AMWS_OUTPUT, "amws_1955_county_year.csv"))
schools  <- fread(file.path(SCHOOLS_OUTPUT, "elite_high_schools_core_1800_1930.csv"))

# ---- Two treatment vectors --------------------------------------------------
schools[, county_geoid := as.integer(county_geoid)]
schools[, founding_year_used := as.integer(founding_year_used)]

g_high <- schools[crit_high_access_strict == "yes",
                  .(g = min(founding_year_used)), by = county_geoid]
g_low  <- schools[crit_high_access_strict == "no",
                  .(g = min(founding_year_used)), by = county_geoid]

contaminator <- unique(schools[contaminates_county == "yes" |
                                 contaminates_county == TRUE, county_geoid])
contaminator <- contaminator[!is.na(contaminator)]

cat("contaminator counties:", paste(contaminator, collapse=", "), "\n")
cat("high-access counties (n =", nrow(g_high), "):\n"); print(g_high[order(g)])
cat("low-access counties (n =", nrow(g_low), ")\n")
cat("  low-access with first school in 1880-1910:",
    g_low[g >= EVENT_LO & g <= EVENT_HI, .N], "\n")

# All counties that ever appear in either school list
ever_school_geoid <- unique(c(g_high$county_geoid, g_low$county_geoid))

# ---- Base panel -------------------------------------------------------------
base <- merge(yr_panel, amws, by = c("GEOID", "year"), all.x = TRUE)
base[is.na(n_amws), n_amws := 0L]
base[, n_wiki := n_stem]
base <- base[!GEOID %in% contaminator]
base <- base[!is.na(population) & population > 0]
setnames(base, "stem_per_1000_pop", "wiki_per_1000_pop")
base[, amws_per_1000_pop := 1000 * n_amws / population]

# Never-treated = no school of either type
never_treated <- base[!GEOID %in% ever_school_geoid, unique(GEOID)]

# ---- Spec runner ------------------------------------------------------------
run_spec <- function(spec_name, treated_geoid_g, control_geoid) {
  cat("\n========== ", spec_name, " ==========\n")
  cat("treated:", length(treated_geoid_g$GEOID),
      "  controls:", length(control_geoid), "\n")

  p <- base[GEOID %in% c(treated_geoid_g$GEOID, control_geoid)]
  p <- merge(p, treated_geoid_g, by = "GEOID", all.x = TRUE)
  p[is.na(g), g := NEVER_G]

  # Restrict year ranges
  p <- p[(g != NEVER_G & abs(year - g) <= E_WIN) |
         (g == NEVER_G & year >= EVENT_LO - E_WIN & year <= EVENT_HI + E_WIN)]

  cat("panel rows:", nrow(p), " counties:", uniqueN(p$GEOID),
      " years:", min(p$year), "-", max(p$year), "\n")

  outcomes <- list(
    list(y = "n_amws",            lab = "AMWS births (count)",       log = TRUE),
    list(y = "amws_per_1000_pop", lab = "AMWS / 1000 pop",           log = FALSE),
    list(y = "n_wiki",            lab = "Wikipedia STEM (count)",    log = TRUE),
    list(y = "wiki_per_1000_pop", lab = "Wikipedia STEM / 1000 pop", log = FALSE)
  )

  results <- rbindlist(lapply(outcomes, function(o) {
    dt <- copy(p)
    dt[, yy := if (o$log) log1p(get(o$y)) else get(o$y)]
    fml <- as.formula(sprintf("yy ~ sunab(g, year, ref.p = %d) | GEOID + year", REF_E))
    mod <- feols(fml, data = dt, cluster = ~GEOID)
    ip  <- iplot(mod, only.params = TRUE)$prms
    data.table(spec = spec_name, outcome = o$lab,
               e = as.integer(round(ip$x)),
               estimate = ip$y, ci_low = ip$ci_low, ci_high = ip$ci_high)
  }))
  results
}

# ---- Run the three specs ----------------------------------------------------
# (A) HIGH-ACCESS vs never-treated
high_in <- g_high[g >= EVENT_LO & g <= EVENT_HI]
setnames(high_in, "county_geoid", "GEOID")
# (B) LOW-ACCESS vs never-treated (exclude counties that ever got a high-access)
low_in <- g_low[g >= EVENT_LO & g <= EVENT_HI &
                !county_geoid %in% g_high$county_geoid]
setnames(low_in, "county_geoid", "GEOID")

res_A <- run_spec("A. high-access vs never-treated",
                  treated_geoid_g = high_in,
                  control_geoid = never_treated)
res_B <- run_spec("B. low-access vs never-treated",
                  treated_geoid_g = low_in,
                  control_geoid = never_treated)


# (C) HIGH-ACCESS vs LOW-ACCESS (head to head)
# Use the same low-access set as the control. Low-access counties enter as
# never-treated relative to "high-access" treatment.
res_C <- run_spec("C. high-access vs low-access",
                  treated_geoid_g = high_in,
                  control_geoid = low_in$GEOID)

all_res <- rbindlist(list(res_A, res_B, res_C))
all_res[, spec := factor(spec, levels = unique(spec))]
all_res[, outcome := factor(outcome, levels = c("AMWS births (count)",
                                                "AMWS / 1000 pop",
                                                "Wikipedia STEM (count)",
                                                "Wikipedia STEM / 1000 pop"))]
fwrite(all_res, file.path(out_dir, "event_study_estimates_threespec.csv"))

# ---- Plot: 3 specs x 4 outcomes ---------------------------------------------
make_plot <- function(d, title) {
  ggplot(d, aes(e, estimate)) +
    geom_hline(yintercept = 0, color = "grey50") +
    geom_vline(xintercept = -0.5, linetype = "dashed", color = "grey50") +
    geom_pointrange(aes(ymin = ci_low, ymax = ci_high), size = 0.25) +
    labs(x = "Years since school opening", y = "Coefficient", title = title) +
    scale_x_continuous(breaks = seq(-E_WIN, E_WIN, 5)) +
    theme_minimal(base_size = 9) +
    theme(plot.title = element_text(size = 9))
}

plots <- list()
for (sp in levels(all_res$spec)) {
  for (oc in levels(all_res$outcome)) {
    d <- all_res[spec == sp & outcome == oc]
    plots[[paste(sp, oc)]] <- make_plot(d, sprintf("%s\n%s", sp, oc))
  }
}

big <- wrap_plots(plots, ncol = 4) +
  plot_annotation(
    title = "Elite high schools — yearly event study, AMWS vs Wikipedia STEM",
    subtitle = sprintf("Events %d-%d, +/-%d year window. Rows: A=high vs never, B=low vs never, C=high vs low",
                       EVENT_LO, EVENT_HI, E_WIN))
ggsave(file.path(out_dir, "event_study_threespec.png"), big,
       width = 18, height = 12, dpi = 160)

# Spec-by-spec focused plots
for (sp in levels(all_res$spec)) {
  d <- all_res[spec == sp]
  p_sp <- ggplot(d, aes(e, estimate)) +
    geom_hline(yintercept = 0, color = "grey50") +
    geom_vline(xintercept = -0.5, linetype = "dashed", color = "grey50") +
    geom_pointrange(aes(ymin = ci_low, ymax = ci_high), size = 0.35) +
    facet_wrap(~ outcome, scales = "free_y", ncol = 2) +
    scale_x_continuous(breaks = seq(-E_WIN, E_WIN, 2)) +
    labs(x = "Years since school opening", y = "Coefficient",
         title = sp) +
    theme_minimal(base_size = 11)
  fn <- gsub("[^A-Za-z0-9]+", "_", sp)
  ggsave(file.path(out_dir, sprintf("event_study_%s.png", fn)),
         p_sp, width = 10, height = 7, dpi = 150)
}

# ---- Raw-data overlay: high vs low vs never ---------------------------------
high_geoid <- high_in$GEOID[1]
low_geoids <- low_in$GEOID
nev_geoids <- never_treated
raw <- base[year >= EVENT_LO - E_WIN & year <= EVENT_HI + E_WIN]
raw[, group := fcase(
  GEOID == high_geoid,  "high-access (Hamilton OH)",
  GEOID %in% low_geoids,"low-access counties",
  GEOID %in% nev_geoids,"never-treated",
  default = NA_character_)]
raw <- raw[!is.na(group)]
agg <- raw[, .(amws_rate = 1000 * sum(n_amws) / sum(population),
               wiki_rate = 1000 * sum(n_wiki) / sum(population),
               n_counties = uniqueN(GEOID)),
           by = .(group, year)]

p_amws <- ggplot(agg, aes(year, amws_rate, color = group)) +
  geom_vline(xintercept = high_in$g[1], linetype = "dashed", color = "grey40") +
  geom_line(linewidth = 0.6) +
  scale_color_manual(values = c("high-access (Hamilton OH)" = "#d62728",
                                "low-access counties" = "#2ca02c",
                                "never-treated" = "#1f77b4")) +
  labs(x = "Birth year", y = "AMWS births / 1000 pop", color = NULL,
       title = "AMWS rate: high-access vs low-access vs never-treated") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

p_wiki <- ggplot(agg, aes(year, wiki_rate, color = group)) +
  geom_vline(xintercept = high_in$g[1], linetype = "dashed", color = "grey40") +
  geom_line(linewidth = 0.6) +
  scale_color_manual(values = c("high-access (Hamilton OH)" = "#d62728",
                                "low-access counties" = "#2ca02c",
                                "never-treated" = "#1f77b4")) +
  labs(x = "Birth year", y = "Wikipedia STEM / 1000 pop", color = NULL,
       title = "Wikipedia STEM rate: high vs low vs never") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

raw_combined <- p_amws / p_wiki
ggsave(file.path(out_dir, "raw_data_high_vs_low_vs_never.png"),
       raw_combined, width = 10, height = 9, dpi = 150)

cat("\n--- Totals in 1885-1905 window ---\n")
agg_tot <- raw[, .(amws_births = sum(n_amws), wiki_stem = sum(n_wiki),
                   pop_yr_avg = mean(population), n_counties = uniqueN(GEOID)),
               by = group]
print(agg_tot)

cat("\nWrote:", out_dir, "\n")
