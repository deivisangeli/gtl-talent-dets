###############################################################################
# Shared donor-pool matching specification and event-window geometry for the
# fixed-1911 England and Wales Wikipedia synthetic-DiD pipeline.
#
# The matching-specification string is stamped into every donor-pool row and is
# then compared for exact equality by the estimation scripts. Keeping the
# string, its directory tag and the event-window geometry in one place prevents
# the four scripts from drifting apart when a new specification is added.
#
# Specifications:
#   pooled_rate_density        Original two-variable match. One scalar pooled
#                              pre-treatment Wikipedia rate plus baseline
#                              population density. Writes to the historical
#                              paths, so leaving SYNTHDID_MATCHING_SPEC unset
#                              reproduces the existing outputs exactly.
#   lagged_rate_growth_density One lagged-outcome column per pre-treatment
#                              decade bin, the pre-treatment annualized
#                              population growth rate, and baseline population
#                              density. Writes under match_v2_lagged/.
#   balanced3_rate_growth_density
#                              Fixed-dimension match for the balanced event
#                              window: exactly three strictly pre-fair lags, a
#                              uniform 20-year population growth rate, and
#                              baseline density. Five columns for every treated
#                              unit, so the equal-per-column weighting is
#                              comparable across events. Writes under
#                              match_v3_balanced/ and implies the balanced
#                              event window.
#
# Event windows:
#   full               Every decade bin in the 1850-1960 panel. The treatment
#                      indicator turns on at the fair decade, so T_0 runs 3 to
#                      10 and T_1 runs 2 to 9 across events.
#   balanced_4pre_3post
#                      Four pre bins at offsets -30, -20, -10 and 0 decades,
#                      and three post bins at +10, +20 and +30, relative to the
#                      fair decade T. The fair decade is the LAST PRE-TREATMENT
#                      period, so treatment starts at T+10. Requires T in
#                      [1880, 1920]: T-30 must reach no earlier than the 1850
#                      bin and T+30 no later than 1950, which keeps the 9-year
#                      1850 bin but excludes the 2-year 1960 bin.
###############################################################################

wf_matching_spec_ids <- c(
  "pooled_rate_density",
  "lagged_rate_growth_density",
  "balanced3_rate_growth_density"
)

wf_event_window_ids <- c("full", "balanced_4pre_3post")

wf_matching_spec_id <- function() {
  id <- tolower(trimws(Sys.getenv(
    "SYNTHDID_MATCHING_SPEC", unset = "pooled_rate_density"
  )))
  if (!nzchar(id)) id <- "pooled_rate_density"
  if (!id %in% wf_matching_spec_ids) {
    stop("SYNTHDID_MATCHING_SPEC must be one of: ",
         paste(wf_matching_spec_ids, collapse = ", "), ".")
  }
  id
}

wf_matching_spec_string <- function(id = wf_matching_spec_id()) {
  switch(
    id,
    pooled_rate_density = paste0(
      "equal_z_euclidean_log1p_pooled_pre_treatment_wikipedia_rate_",
      "plus_log_baseline_population_density"
    ),
    lagged_rate_growth_density = paste0(
      "equal_z_euclidean_log1p_pre_treatment_wikipedia_rate_lags_",
      "plus_log1p_pre_treatment_population_cagr_",
      "plus_log_baseline_population_density"
    ),
    balanced3_rate_growth_density = paste0(
      "equal_z_euclidean_log1p_3_balanced_pre_treatment_wikipedia_rate_lags_",
      "plus_log1p_20y_pre_treatment_population_cagr_",
      "plus_log_baseline_population_density"
    ),
    stop("Unsupported matching specification: ", id, ".")
  )
}

# NULL keeps the historical directory layout byte-identical for the original
# specification; any other specification is isolated in its own subtree.
wf_matching_spec_dir_tag <- function(id = wf_matching_spec_id()) {
  switch(
    id,
    pooled_rate_density = NULL,
    lagged_rate_growth_density = "match_v2_lagged",
    balanced3_rate_growth_density = "match_v3_balanced",
    stop("Unsupported matching specification: ", id, ".")
  )
}

wf_matching_spec_dir <- function(parent_dir, id = wf_matching_spec_id()) {
  tag <- wf_matching_spec_dir_tag(id)
  if (is.null(tag)) parent_dir else file.path(parent_dir, tag)
}

# TRUE when the distance is built from one column per pre-treatment bin rather
# than from a single pooled pre-treatment scalar.
wf_matching_spec_uses_lags <- function(id = wf_matching_spec_id()) {
  id %in% c("lagged_rate_growth_density", "balanced3_rate_growth_density")
}

# TRUE when the matched lags are a fixed count anchored on the baseline decade
# rather than every available pre-treatment bin.
wf_matching_spec_fixed_lags <- function(id = wf_matching_spec_id()) {
  identical(id, "balanced3_rate_growth_density")
}

# The window a specification is designed for. A specification whose native
# window is not "full" cannot be estimated on another window, because its
# matching columns are defined relative to that window geometry.
wf_matching_spec_native_window <- function(id = wf_matching_spec_id()) {
  switch(
    id,
    pooled_rate_density = "full",
    lagged_rate_growth_density = "full",
    balanced3_rate_growth_density = "balanced_4pre_3post",
    stop("Unsupported matching specification: ", id, ".")
  )
}

wf_event_window_id <- function(spec_id = wf_matching_spec_id()) {
  native <- wf_matching_spec_native_window(spec_id)
  raw <- tolower(trimws(Sys.getenv("SYNTHDID_EVENT_WINDOW", unset = "")))
  if (!nzchar(raw)) return(native)
  if (!raw %in% wf_event_window_ids) {
    stop("SYNTHDID_EVENT_WINDOW must be one of: ",
         paste(wf_event_window_ids, collapse = ", "), ".")
  }
  if (!identical(native, "full") && !identical(raw, native)) {
    stop("Matching specification ", spec_id, " is defined only for the ",
         native, " event window, but SYNTHDID_EVENT_WINDOW requests ", raw, ".")
  }
  raw
}

# All event-window geometry lives here so the builder, the per-event estimator
# and the pooled stage cannot disagree about it. Offsets are decades relative to
# the fair decade T.
wf_event_window_spec <- function(id = wf_event_window_id()) {
  switch(
    id,
    full = list(
      id = "full",
      dir_tag = NULL,
      balanced = FALSE,
      pre_offsets = NULL,
      post_offsets = NULL,
      treatment_offset = 0L,
      matched_lag_offsets = NULL,
      min_treatment_decade = NA_integer_,
      max_treatment_decade = NA_integer_,
      n_pre_periods = NA_integer_,
      n_post_periods = NA_integer_
    ),
    balanced_4pre_3post = list(
      id = "balanced_4pre_3post",
      dir_tag = "balanced_4p3",
      balanced = TRUE,
      pre_offsets = c(-30L, -20L, -10L, 0L),
      post_offsets = c(10L, 20L, 30L),
      treatment_offset = 10L,
      matched_lag_offsets = c(-30L, -20L, -10L),
      min_treatment_decade = 1880L,
      max_treatment_decade = 1920L,
      n_pre_periods = 4L,
      n_post_periods = 3L
    ),
    stop("Unsupported event window: ", id, ".")
  )
}

# Decade bins used for estimation, in order.
wf_event_window_decades <- function(treatment_decade, window) {
  if (!window$balanced) return(NULL)
  as.integer(treatment_decade) +
    as.integer(c(window$pre_offsets, window$post_offsets))
}

# The decade at which the treatment indicator turns on.
wf_event_window_treatment_start <- function(treatment_decade, window) {
  as.integer(treatment_decade) + as.integer(window$treatment_offset)
}

# TRUE when the fair decade admits the complete balanced window.
wf_event_window_admits <- function(treatment_decade, window) {
  if (!window$balanced) return(rep(TRUE, length(treatment_decade)))
  treatment_decade >= window$min_treatment_decade &
    treatment_decade <= window$max_treatment_decade
}

# Pooled-stage output directory: the plain name when the window is the
# specification native one, otherwise tagged so the full-window result is never
# overwritten.
wf_window_output_dir <- function(parent_dir, base_name,
                                spec_id = wf_matching_spec_id(),
                                window_id = wf_event_window_id(spec_id)) {
  if (identical(window_id, wf_matching_spec_native_window(spec_id))) {
    return(file.path(parent_dir, base_name))
  }
  tag <- wf_event_window_spec(window_id)$dir_tag
  if (is.null(tag)) return(file.path(parent_dir, base_name))
  file.path(parent_dir, paste0(base_name, "_", tag))
}
