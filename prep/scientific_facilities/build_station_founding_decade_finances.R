#!/usr/bin/env Rscript
# Offline publisher of manually sourced station financial accounts.
# No downloads, automatic historical imputations, or analysis changes.
args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grepl("^--file=", args)])
repo_root <- if (length(file_arg)) {
  normalizePath(file.path(dirname(file_arg[[1]]), "../.."), winslash = "/", mustWork = TRUE)
} else {
  candidate <- Sys.getenv("GTL_REPO", unset = getwd())
  if (!file.exists(file.path(candidate, "paths.R"))) candidate <- file.path(candidate, "..")
  normalizePath(candidate, winslash = "/", mustWork = TRUE)
}
source(file.path(repo_root, "paths.R"))
if (.Platform$OS.type == "windows" && !isTRUE(l10n_info()[["UTF-8"]])) {
  invisible(Sys.setlocale("LC_CTYPE", "English_United States.utf8"))
}
stopifnot(requireNamespace("data.table", quietly = TRUE))
library(data.table)
root <- normalizePath(TALENT_DETS_DATA_DIR, winslash = "/", mustWork = TRUE)
cur <- file.path(root, "Data/intermediary/scientific_facilities/station_finances")
raw <- file.path(root, "Data/raw/scientific_facilities/station_finances")
out <- file.path(root, "Data/processed/scientific_facilities")
prefix <- "station_founding_decade_finances"
dir.create(out, recursive = TRUE, showWarnings = FALSE)
read_input <- function(name) fread(file.path(cur, name), colClasses = "character", na.strings = "", encoding = "UTF-8")
scope <- read_input("station_scope_curated.csv")
accounts <- read_input("financial_accounts_curated.csv")
lines <- read_input("financial_lines_curated.csv")
pairs <- read_input("annual_pairings_curated.csv")
adjustments <- read_input("pair_adjustments_curated.csv")
searches <- read_input("year_search_curated.csv")
sources <- read_input("sources_curated.csv")
reviews <- read_input("page_reviews_curated.csv")
inventory_path <- file.path(out, "usda_1910_university_agricultural_stations.csv")
inventory <- fread(inventory_path, colClasses = "character", encoding = "UTF-8")
protected <- list.files(out, pattern = "^(usda_1910_university_agricultural_stations|kantor_whalley_2019_stations)", full.names = TRUE)
protected_before <- tools::md5sum(protected)

checks <- data.table(check = character(), passed = logical(), detail = character())
check <- function(name, ok, detail = "") {
  checks <<- rbind(checks, data.table(check = name, passed = isTRUE(ok), detail = as.character(detail)))
  if (!isTRUE(ok)) stop(name, ": ", detail, call. = FALSE)
}
blank <- function(x) is.na(x) | !nzchar(x)
check("scope_15_station_identities", nrow(scope) == 15L && uniqueN(scope$station_id) == 15L)
expected <- c("arkansas", "illinois", "purdue", "lsu", "cornell", "new_hampshire", "clemson", "tennessee", "texas_am", "virginia_tech")
check("scope_exactly_10_requested_universities", setequal(unique(scope$university_id), expected))
check("scope_preserves_existing_inventory_ids", all(scope$station_id %in% inventory$station_id))
check("scope_university_matches_inventory", all(scope$university_id == inventory$university_id[match(scope$station_id, inventory$station_id)]))
check("unique_account_ids", !anyDuplicated(accounts$account_id))
check("unique_line_ids", !anyDuplicated(lines$line_id))
check("unique_observation_ids", !anyDuplicated(pairs$observation_id))
check("all_line_accounts_known", all(lines$account_id %in% accounts$account_id))
check("all_account_sources_known", all(accounts$source_id %in% sources$source_id))
check("pair_numerator_references", all(pairs$numerator_account_id[!blank(pairs$numerator_account_id)] %in% accounts$account_id))
check("pair_denominator_references", all(pairs$denominator_account_id[!blank(pairs$denominator_account_id)] %in% accounts$account_id))
check("all_pairs_station_in_scope", all(pairs$station_id %in% scope$station_id))
check("all_15_stations_have_pair_candidate", setequal(scope$station_id, pairs$station_id))
check("all_15_stations_have_3_search_slots", nrow(searches) == 45L && all(searches[, .N, by = station_id]$N == 3L))
check("usd_nominal_only", all(lines$currency == "USD" & lines$price_basis == "nominal"))
check("no_unreviewed_transcribed_amounts", all(lines$visual_review == "yes"))
check("all_amounts_have_pdf_page", all(!blank(lines$source_pdf_page) & !blank(lines$source_printed_page)))
check("all_amounts_have_source_url", all(grepl("^https://", lines$source_url)))
check("visually_verified_account_page_recorded", all(paste(accounts$source_id, accounts$source_pdf_page) %in% paste(reviews$source_id, reviews$source_pdf_page)))
check("review_images_exist", all(file.exists(file.path(cur, reviews$review_file))))
check("downloaded_source_documents_exist", all(file.exists(file.path(raw, sources$raw_file[!blank(sources$raw_file)]))))
downloaded <- sources[!blank(raw_file)]
check("downloaded_source_checksums_match", identical(unname(tools::md5sum(file.path(raw, downloaded$raw_file))), downloaded$md5))

money <- function(x) {
  # Only punctuation normalization. Do not silently parse OCR or infer digits.
  ok <- blank(x) | grepl("^[0-9]+(,[0-9]{3})*(\\.[0-9]{1,2})?$", x)
  if (!all(ok)) stop("Invalid manually transcribed monetary string: ", paste(x[!ok], collapse = ", "))
  as.numeric(gsub(",", "", x, fixed = TRUE))
}
lines[, amount_usd := money(reported_amount)]
check("amounts_finite_nonnegative", all(is.finite(lines$amount_usd) & lines$amount_usd >= 0))
scope[, `:=`(operational_anchor_year = as.integer(operational_anchor_year), founding_decade = as.integer(founding_decade))]
accounts[, financial_year := as.integer(financial_year)]
pairs[, financial_year := as.integer(financial_year)]
searches[, `:=`(target_year = as.integer(target_year), selection_slot = as.integer(selection_slot))]
check("founding_decade_matches_anchor", all(is.na(scope$operational_anchor_year) | scope$founding_decade == 10L * (scope$operational_anchor_year %/% 10L)))
check("cornell_keeps_1870s_anchor", scope[station_id == "ny_ithaca", founding_decade] == 1870L)
check("clemson_unresolved_decade_not_imputed", is.na(scope[station_id == "sc_coast", founding_decade]))
check("target_years_distinct_per_station", !anyDuplicated(searches[!is.na(target_year), .(station_id, target_year)]))
target_check <- merge(searches[!is.na(target_year)], scope[, .(station_id, operational_anchor_year, founding_decade)], by = "station_id")
check("search_targets_within_calendar_decade", all(target_check$target_year >= target_check$operational_anchor_year & target_check$target_year <= target_check$founding_decade + 9L))
dated <- accounts[!blank(period_start) & !blank(period_end)]
check("source_period_date_order", all(as.IDate(dated$period_start) <= as.IDate(dated$period_end)))

sum_class <- function(x, category) sum(x[flow_class %in% category, amount_usd])
rollup <- lines[, .(
  reported_total_usd = if (any(flow_class == "reported_total")) amount_usd[flow_class == "reported_total"][[1]] else NA_real_,
  identified_current_receipts_usd = sum(amount_usd[flow_class %in% c("current_receipt", "included_federal_station_receipt")]),
  identified_operating_expenditure_usd = sum(amount_usd[flow_class == "operating_expenditure"]),
  identified_capital_expenditure_usd = sum(amount_usd[flow_class == "capital_expenditure"]),
  identified_mixed_capital_expenditure_usd = sum(amount_usd[flow_class == "capital_or_mixed_expenditure"]),
  identified_opening_balance_usd = sum(amount_usd[flow_class == "opening_balance"]),
  identified_internal_transfer_usd = sum(amount_usd[flow_class == "internal_transfer"]),
  n_transcribed_lines = .N
), by = account_id]
accounts <- merge(accounts, rollup, by = "account_id", sort = FALSE)
accounts[, subtotal_interpretation := "Identified components only; a zero subtotal means no line in that class was transcribed, not a historical zero. These are not certified complete operating totals."]
accounts[, certified_current_receipts_usd := NA_real_]
accounts[, certified_operating_expenditure_usd := NA_real_]
check("no_opening_balance_added_to_current_receipts", abs(accounts[account_id == "lsu_network_1909", identified_current_receipts_usd] - 51973.95) < .005)
check("virginia_carryover_removed", abs(accounts[account_id == "vt_station_1909", identified_current_receipts_usd] - 32250.64) < .005)
check("tennessee_first_year_expenditure_reconciles", abs(lines[account_id == "tn_hatch_1888" & flow_class != "reported_total", sum(amount_usd)] - 15000) < .005)
check("illinois_1889_expenditure_reconciles", abs(lines[account_id == "il_station_1889_actual" & flow_class != "reported_total", sum(amount_usd)] - 15000) < .005)
check("texas_station_1894_revenue_reconciles", abs(lines[account_id == "texas_network_1894" & flow_class != "reported_total", sum(amount_usd)] - 17398.46) < .005)
check("nh_explicit_inclusion_no_second_addition", lines[line_id == "new_hampshire_university_1889_education_station_included", amount_usd] == 15000)

paircols <- c("account_id", "measure", "account_scope", "period_start", "period_end", "period_label", "source_id", "source_url", "source_pdf_page", "source_printed_page", "account_notes", "reported_total_usd", "identified_current_receipts_usd", "identified_operating_expenditure_usd")
for (side in c("numerator", "denominator")) {
  a <- copy(accounts[, ..paircols])
  setnames(a, names(a), paste0(side, "_", names(a)))
  pairs <- merge(pairs, a, by = paste0(side, "_account_id"), all.x = TRUE, sort = FALSE)
}
pairs <- merge(pairs, scope[, .(station_id, station_name, station_type, operational_anchor_year, founding_decade, operational_anchor_status)], by = "station_id", all.x = TRUE, sort = FALSE)
pairs[, within_founding_decade := !is.na(founding_decade) & financial_year >= operational_anchor_year & financial_year <= founding_decade + 9L]
pairs[, explicit_period_match := !blank(numerator_period_end) & !blank(denominator_period_end) & numerator_period_end == denominator_period_end & numerator_period_start == denominator_period_start]
pairs[is.na(explicit_period_match), explicit_period_match := FALSE]
check("pair_status_vocabulary", all(pairs$publication_status %in% c("unresolved", "accepted")))
check("adjustment_side_vocabulary", all(adjustments$side %in% c("numerator", "denominator")))
check("adjustments_link_existing_lines", all(adjustments$line_id %in% lines$line_id))
check("adjustments_link_existing_pairs", all(adjustments$observation_id %in% pairs$observation_id))
check("no_duplicate_adjustment_terms", !anyDuplicated(adjustments[, .(observation_id, side, line_id)]))
adjustments[, coefficient := as.numeric(coefficient)]
check("adjustment_coefficients_documented", all(is.finite(adjustments$coefficient) & adjustments$coefficient != 0 & !blank(adjustments$adjustment_note)))
pairs[, `:=`(numerator_operating_or_current_usd = NA_real_, denominator_university_operating_or_current_usd = NA_real_, share_pct = NA_real_, selected_for_summary = FALSE)]
accepted <- pairs[publication_status == "accepted"]
check("accepted_pairs_in_decade", all(accepted$within_founding_decade))
check("accepted_pairs_fiscal_periods_match", all(accepted$explicit_period_match & accepted$period_match == "yes"))
check("accepted_pairs_boundaries_reconciled", all(accepted$whole_university_boundary == "yes" & accepted$operating_current_flow_reconciled == "yes" & accepted$duplicate_funding_reconciled == "yes"))
check("accepted_pairs_have_consistent_basis", all(accepted$share_basis %in% c("operating_expenditure", "current_annual_revenue")))
check("accepted_pairs_have_scope", all(accepted$share_scope %in% c("individual_station", "station_network")))
if (nrow(accepted)) {
  for (pid in accepted$observation_id) {
    p <- pairs[observation_id == pid]
    for (side_label in c("numerator", "denominator")) {
      terms <- merge(adjustments[observation_id == pid & side == side_label], lines[, .(line_id, account_id, amount_usd, flow_class)], by = "line_id", all.x = TRUE)
      component_accounts <- accounts[account_id %in% terms$account_id]
      check(paste0(pid, "_", side_label, "_sourced_adjustment_terms"), nrow(terms) > 0L && all(component_accounts$university_id == p$university_id) && all(!blank(component_accounts$period_end) & component_accounts$period_end == p[[paste0(side_label, "_period_end")]]))
      amount <- sum(terms$coefficient * terms$amount_usd)
      check(paste0(pid, "_", side_label, "_finite_positive"), is.finite(amount) && amount > 0)
      pairs[observation_id == pid, (paste0(side_label, "_operating_or_current_usd")) := amount]
    }
  }
}
# Denominator uses a longer public name to prevent confusion with a station subtotal.
if ("denominator_operating_or_current_usd" %in% names(pairs)) {
  pairs[!is.na(denominator_operating_or_current_usd), denominator_university_operating_or_current_usd := denominator_operating_or_current_usd]
  pairs[, denominator_operating_or_current_usd := NULL]
}
pairs[publication_status == "accepted", share_pct := 100 * numerator_operating_or_current_usd / denominator_university_operating_or_current_usd]
check("unresolved_shares_remain_missing", all(is.na(pairs[publication_status != "accepted", share_pct])))
check("shares_above100_investigated", !any(pairs$share_pct > 100, na.rm = TRUE), "An above100 result requires reopening boundary/capital/transfer reconciliation before acceptance.")
check("no_1910_finance_substitution", all(pairs$financial_year != 1910L))
check("individual_branch_scope_not_network", !any(pairs$publication_status == "accepted" & pairs$share_scope == "individual_station" & grepl("network", pairs$numerator_account_scope), na.rm = TRUE))
choose_years <- function(year, last) {
  y <- sort(unique(year)); if (length(y) <= 2L) return(y)
  earliest <- y[[1]]; latest <- tail(y, 1L); mid <- (earliest + last) / 2
  remaining <- setdiff(y, c(earliest, latest))
  c(earliest, remaining[which.min(abs(remaining - mid))], latest)
}
if (nrow(pairs[publication_status == "accepted"])) {
  check("accepted_annual_identities_unique", !anyDuplicated(pairs[publication_status == "accepted", .(university_id, station_id, financial_year, share_basis, share_scope)]))
  selected <- pairs[publication_status == "accepted", .SD[financial_year %in% choose_years(financial_year, founding_decade[[1]] + 9L)], by = .(university_id, station_id, share_basis, share_scope)]
  pairs[observation_id %in% selected$observation_id, selected_for_summary := TRUE]
}
setorder(pairs, university_id, station_id, financial_year, observation_id)

university <- scope[, .(
  university_name = university_name[[1]], n_scoped_stations = .N,
  first_qualifying_station_ids = if (all(is.na(operational_anchor_year))) paste(station_id, collapse = "|") else paste(station_id[operational_anchor_year == min(operational_anchor_year, na.rm = TRUE)], collapse = "|"),
  first_operational_anchor_year = if (all(is.na(operational_anchor_year))) NA_integer_ else min(operational_anchor_year, na.rm = TRUE),
  first_station_founding_decade = if (all(is.na(founding_decade))) NA_integer_ else min(founding_decade, na.rm = TRUE),
  founding_decade_status = if (all(is.na(founding_decade))) "unresolved" else "supported_decade_with_date_qualifications"
), by = university_id]
coverage <- pairs[, .(n_candidate_observations = .N, n_observations_with_station_amount = sum(!is.na(numerator_reported_total_usd)), n_candidates_with_both_reported_totals = sum(!is.na(numerator_reported_total_usd) & !is.na(denominator_reported_total_usd)), n_qualifying_annual_shares = sum(!is.na(share_pct))), by = university_id]
university <- merge(university, coverage, by = "university_id", all.x = TRUE, sort = FALSE)
university[, `:=`(observed_annual_shares = NA_character_, summary_basis = NA_character_, median_share_pct = NA_real_, minimum_share_pct = NA_real_, maximum_share_pct = NA_real_, coverage_status = "incomplete_research_no_reconciled_pair", research_complete = FALSE)]
for (uid in university$university_id) {
  first_ids <- strsplit(university[university_id == uid, first_qualifying_station_ids], "|", fixed = TRUE)[[1]]
  obs <- pairs[university_id == uid & station_id %in% first_ids & selected_for_summary]
  if (nrow(obs)) {
    preferred <- if (any(obs$share_basis == "operating_expenditure")) "operating_expenditure" else "current_annual_revenue"
    z <- obs[share_basis == preferred]
    check(paste0(uid, "_summary_scope_consistent"), uniqueN(z$share_scope) == 1L)
    university[university_id == uid, `:=`(summary_basis = preferred, observed_annual_shares = paste(sprintf("%s: %.6f%% (%s)", z$financial_year, z$share_pct, z$share_scope), collapse = "; "), median_share_pct = median(z$share_pct), minimum_share_pct = min(z$share_pct), maximum_share_pct = max(z$share_pct), coverage_status = "partial_qualifying_observations")]
  }
}
branch <- merge(scope[station_type == "branch"], university[, .(university_id, first_station_founding_decade)], by = "university_id", all.x = TRUE)
branch[, later_than_first_station_decade := fifelse(!is.na(founding_decade) & !is.na(first_station_founding_decade), founding_decade > first_station_founding_decade, NA)]
branch <- merge(branch, pairs[, .(station_id, observation_id, financial_year, numerator_account_id, numerator_account_scope, numerator_reported_total_usd, share_pct, exclusion_or_missing_reason)], by = "station_id", all.x = TRUE, allow.cartesian = TRUE)
check("summary_10_institutions", nrow(university) == 10L)
check("all15_preserved_in_published_scope", nrow(scope) == 15L)
check("no_false_zero_median", all(is.na(university[n_qualifying_annual_shares == 0L, median_share_pct])))
check("summary_share_counts_match_annual", sum(university$n_qualifying_annual_shares) == sum(!is.na(pairs$share_pct)))
check("existing_inventory_outputs_unchanged", identical(protected_before, tools::md5sum(protected)))

pub <- function(suffix, x) fwrite(x, file.path(out, paste0(prefix, suffix)), na = "", quote = TRUE, bom = FALSE)
pub("_annual.csv", pairs)
pub("_universities.csv", university)
pub("_branches.csv", branch)
pub("_accounts.csv", accounts)
pub("_reported_lines.csv", lines)
pub("_station_scope.csv", scope)
pub("_year_search.csv", searches)
pub("_sources.csv", sources)
pub("_validation.csv", checks)
input_paths <- c(list.files(cur, pattern = "_curated.csv$", full.names = TRUE), inventory_path, file.path(repo_root, "paths.R"), file.path(repo_root, "prep/scientific_facilities/build_station_founding_decade_finances.R"))
manifest <- data.table(file = vapply(input_paths, function(p) {
  p <- normalizePath(p, winslash = "/"); if (startsWith(p, root)) substring(p, nchar(root) + 2L) else p
}, ""), bytes = file.info(input_paths)$size, md5 = unname(tools::md5sum(input_paths)))
pub("_input_manifest.csv", manifest)

descriptions <- c(
  share_pct = "100 times reconciled station operating expenditure / whole-university operating expenditure, or separately labeled current annual revenue. Missing until fiscal period, capital and transfers are reconciled.",
  share_basis = "operating_expenditure or current_annual_revenue only when certified. No mixed basis or funding-rate proxy.",
  reported_total_usd = "Numeric transcription of source total; may contain capital, carryovers, financing, or be a multi-year equality. Never automatically an operating measure.",
  reported_amount = "Exact monetary digits and punctuation transcribed from the printed source. Preserves apparent source arithmetic discrepancies.",
  amount_usd = "Numeric parsing of reported_amount, nominal contemporary US dollars. No inflation adjustment needed for a same-period ratio.",
  identified_current_receipts_usd = "Sum of explicitly classified current-receipt components only; excludes carried balances and internal transfers. Not a certified complete income total.",
  identified_operating_expenditure_usd = "Sum of explicitly current spending categories; mixed supplies/equipment categories omitted. Not a complete operating budget.",
  certified_current_receipts_usd = "Full reconciled current annual receipts. Missing in this initial research release.",
  certified_operating_expenditure_usd = "Full reconciled annual operating spending. Missing in this initial research release.",
  founding_decade = "Calendar decade containing supported operational founding year. Unresolved for continuing Clemson Coast identity;1908 relocation is not assigned as foundation.",
  operational_anchor_year = "Research anchor for operational start; read operational_anchor_status and founding_notes. Not a newly imputed legal establishment date.",
  financial_year = "Source reference year; consult period labels and dates. A report year is not proof of matching fiscal periods.",
  period_start = "Source-specific start date when established; blank otherwise.",
  period_end = "Source-specific fiscal end date when established; blank otherwise.",
  observation_role = "Founding-decade candidate versus explicit later-Hatch or multiyear context. Context rows never substitute for requested years.",
  publication_status = "unresolved until source accounts satisfy requested comparability. Does not mean no historical station spending.",
  target_year = "Provisional archival search anchor, not a claimed earliest available paired year. Three slots may have blank dates when the decade/date cannot support three years.",
  selection_slot = "One of three planned search positions. No repeated year is created to meet a three-observation target.",
  selected_for_summary = "TRUE only for a qualifying paired annual observation selected by earliest, midpoint, latest rule. FALSE for every unresolved candidate.",
  source_pdf_page = "One-based PDF page in archived source, distinct from printed pagination.",
  source_printed_page = "Printed page citation visible in scan.",
  source_url = "Direct source or authoritative catalog URL; source_id links archived PDF and checksum.",
  visual_review = "yes only when monetary digits were compared to rendered source page. Page images are retained with curated inputs.",
  flow_class = "Manual classification distinguishing current receipts/spending, capital/mixed, carried balances, internal transfers and financing; subtotals are not additive components.",
  n_qualifying_annual_shares = "Count of comparable annual shares, not count of sourced dollar totals. Zero here means no accepted estimate, not zero station spending.",
  research_complete = "FALSE in this initial archival research release; requested paired-finance collection is unfinished."
)
outputs <- list(annual = pairs, universities = university, branches = branch, accounts = accounts, reported_lines = lines, station_scope = scope, year_search = searches, sources = sources, validation = checks, input_manifest = manifest)
dict <- rbindlist(lapply(names(outputs), function(n) data.table(table = n, field = names(outputs[[n]]), storage_type = vapply(outputs[[n]], function(x) class(x)[[1]], ""))))
dict[, description := vapply(field, function(f) {
  base <- sub("^(numerator|denominator)_", "", f)
  if (f %in% names(descriptions)) descriptions[[f]] else if (base %in% names(descriptions)) descriptions[[base]] else if (grepl("_id$|_ids$", f)) "Stable linkage identifier(s); pipe-separated where multiple identities share an account." else if (grepl("notes|reason|status|label|scope|kind|interpretation", f)) "Source-specific evidence, definition or research status; retain with the financial observation." else "Source or derived descriptive field; see methodology and source-specific notes."
}, "")]
pub("_field_dictionary.csv", dict)
readme <- c(
  "# Station founding-decade finances: initial archival audit",
  "", "**Research is incomplete. No fully reconciled annual station/university budget share is certified in this release.**",
  "The files preserve useful original accounts and specific barriers to comparability. Missing shares are not zero spending. The requested ten-university estimation exercise is not complete.",
  "", "## Scope and dates",
  "Exactly15 existing confirmed station identities at Arkansas, Illinois, Purdue, LSU, Cornell, New Hampshire, Clemson, Tennessee, Texas A&M and Virginia Tech. Existing station inventories and analyses are read-only.",
  "First operational decades are1870s Cornell;1880s for the other eight non-Clemson universities. Clemson Coast has an unresolved continuing-identity founding decade: USDA1908 describes a new site replacing an older local station. Its1908 reorganization/relocation cannot be substituted for original foundation. Later branches retain individual IDs and legal/operational differences.",
  "", "## Measurement",
  "Preferred measure: station operating expenditure / whole-university operating expenditure in the same fiscal period. Fallback: separately labeled current annual revenue / whole-university current annual revenue. Endowment interest is a flow; endowment principal, asset values, capital appropriations/spending, opening balances and financing are excluded. Internal transfers must not be added to consolidated receipts.",
  "Reported financial lines are stored verbatim. Numeric normalization strips thousands separators only. Source balancing totals, statutory funding rates, and annual appropriations are not automatically realized current income or expenditure. Identified component subtotals are not certified complete operating totals.",
  "No shares are calculated merely by dividing two printed totals. Acceptance needs matched fiscal periods, whole-university coverage, full current/operating classification and reconciliation of duplicated funds. Federal-only expenditure accounts do not measure all station spending.",
  "", "## Concrete reconciliation findings",
  "Illinois: university year ends August31, station June30. University receipts include1196.11 from the station even though its heading excludes the station. Its115884.95 balancing total includes23151.21 opening balance and40479.45 closing balance. Neither is an operating flow. Station mixed apparatus/supplies and buildings/repairs require further detail.",
  "Tennessee1888:15000 is a Hatch-only account, including3000 for a building,282.68 furniture/equipment and3325.67 apparatus/supplies. Subtracting only the building does not recover full station operating expenditure.",
  "New Hampshire:1888-89 education return reports25253 total income and explicitly includes15000 for the station (footnote c). A second addition of15000 is wrong. Exact institutional fiscal dates and realized receipts remain unverified, so no59.4% budget estimate is published.",
  "Arkansas:November/December cash accounts differ from June30 federal returns and include prior deficits.1889 printed equality15322.85 differs by0.10 from the two credit figures1607.20+13715.55. Preserve source figures.1888 national station income is a network including historical branches beyond the15 current scope identities.",
  "LSU1909:reported network income57550 includes5576.05 carryover; identified current lines total51973.95. Network includes Sugar (excluded founding affiliation) and Calhoun (unresolved). The1910 original report was reviewed only to establish that federal and nonfederal accounts used different fiscal periods; none of its finances replace an earlier founding-decade observation.",
  "Clemson1909:Coast annual10000 explicitly covers buildings, equipment and maintenance. Main-station31077.18 includes an unsplit carryover in farm receipts and does not establish Coast coverage. Trustees report PDFs were located but retrieval returned403.",
  "Virginia1909:37405.82 includes5155.18 carried balance; identified current lines total32250.64. Tobacco-network cooperator funding and individual branch accounts remain unresolved. Tennessee1909 does not establish full Jackson coverage.",
  "Texas1894:station network income17398.46 and college income48530.01 are recorded, but fiscal/current/capital reconciliation is unfinished. College components sum48530.00, a0.01 printed discrepancy. Beeville ledger4596.21 spans1898-1900 and is not an annual1899 numerator; its5000 appropriation is biennial, not2500 annual income by assumption.",
  "", "## Files",
  "_annual.csv: candidate paired observations, amounts, source citations, comparability decisions and missing shares.",
  "_universities.csv: ten institutions, first operational decade, candidate coverage, accepted-share count and missing medians/ranges.",
  "_branches.csv: branch supplement with shared-account context. Network money is never divided among branches.",
  "_accounts.csv and _reported_lines.csv: source definitions, periods, original monetary strings and classified components.",
  "_station_scope.csv: all15 station identities, founding evidence and date qualifications.",
  "_year_search.csv: three search slots per identity. Dates are provisional search anchors, not claimed earliest available paired years. Empty targets preserve date uncertainty or the single possible Cornell1879 year.",
  "_sources.csv: authoritative URLs and SHA256 of downloaded PDFs. Curated page_reviews contains the visually checked financial pages.",
  "_validation.csv, _input_manifest.csv and _field_dictionary.csv document checks, exact inputs and fields.",
  "", "## Selection and unfinished research",
  "Once comparable accounts are recovered, seek the earliest year after opening, midpoint of the remaining founding decade, and latest year. Use distinct years and fewer than three when evidence permits fewer. Do not claim archive completeness from the current online searches. Current provisional search anchors may change when accounts become available.",
  "Further work:recover founding-period institutional ledgers for all ten universities; establish whole-university boundaries and fiscal dates; separate capital/current accounts; reconcile network receipts with university totals and cooperator money. Review1887/1888/1889 or the appropriate earlier years, and1894-1899/1906-1909 branch periods. Missing institutional accounts cannot be repaired with1910 finances.",
  "", "## Rebuild offline",
  "```powershell",
  "$env:TALENT_DETS_DATA_DIR = 'C:/Users/megaj/Globtalent Dropbox/gtl_talent_dets'",
  "& 'C:/Program Files/R/R-4.5.2/bin/x64/Rscript.exe' 'C:/Users/megaj/repos/gtl_talent_dets/prep/scientific_facilities/build_station_founding_decade_finances.R'",
  "```",
  "Requires data.table and curated Dropbox inputs. Resolves repo_root and sources paths.R; no network is used. Research access/review date is preserved in inputs, not generated from run time.",
  "", "Interpret any future share as financial scale. Earmarked station money does not imply discretionary spending displaced other university activities."
)
writeLines(readme, file.path(out, paste0(prefix, "_README.md")), useBytes = TRUE)
cat(sprintf("Published initial research audit: %d universities, %d stations, %d accounts, %d lines, %d candidates. %d checks passed. Qualifying shares:%d; archival research incomplete.\n", nrow(university), nrow(scope), nrow(accounts), nrow(lines), nrow(pairs), nrow(checks), sum(!is.na(pairs$share_pct))))
