#!/usr/bin/env Rscript
# Offline publisher of manually researched university expenditure accounts.
# Research and source transcription remain in curated Dropbox inputs.
args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grepl("^--file=", args)])
repo_root <- if (length(file_arg)) {
  normalizePath(file.path(dirname(file_arg[[1]]), "../.."), winslash = "/", mustWork = TRUE)
} else {
  normalizePath(Sys.getenv("GTL_REPO", unset = getwd()), winslash = "/", mustWork = TRUE)
}
source(file.path(repo_root, "paths.R"))
if (.Platform$OS.type == "windows" && !isTRUE(l10n_info()[["UTF-8"]])) {
  invisible(Sys.setlocale("LC_CTYPE", "English_United States.utf8"))
}
stopifnot(requireNamespace("data.table", quietly = TRUE), requireNamespace("digest", quietly = TRUE))
library(data.table)
root <- normalizePath(TALENT_DETS_DATA_DIR, winslash = "/", mustWork = TRUE)
cur <- file.path(root, "Data/intermediary/scientific_facilities/university_expenditures")
out <- file.path(root, "Data/processed/scientific_facilities")
prefix <- "university_founding_decade_expenditures"
read <- function(n) fread(file.path(cur, n), colClasses = "character", na.strings = "", encoding = "UTF-8")
scope <- read("station_scope_curated.csv")
accounts <- read("financial_accounts_curated.csv")
lines <- read("financial_lines_curated.csv")
obs <- read("observations_curated.csv")
terms <- read("calculation_terms_curated.csv")
sources <- read("sources_curated.csv")
reviews <- read("page_reviews_curated.csv")
coverage <- read("coverage_curated.csv")
searches <- read("search_log_curated.csv")
blank <- function(x) is.na(x) | !nzchar(x)
checks <- data.table(check = character(), passed = logical(), detail = character())
check <- function(n, ok, detail = "") {
  checks <<- rbind(checks, data.table(check = n, passed = isTRUE(ok), detail = detail))
  if (!isTRUE(ok)) stop(n, ": ", detail, call. = FALSE)
}
protected <- list.files(out, pattern = "^(kantor_whalley_2019_stations|usda_1910_university_agricultural_stations|station_founding_decade_finances)", full.names = TRUE)
protected_before <- tools::md5sum(protected)
existing_scope <- fread(file.path(root, "Data/intermediary/scientific_facilities/station_finances/station_scope_curated.csv"), colClasses = "character", na.strings = "", encoding = "UTF-8")
check("original_station_scope_unchanged", identical(scope, existing_scope))
expected <- c("arkansas", "illinois", "purdue", "lsu", "cornell", "new_hampshire", "clemson", "tennessee", "texas_am", "virginia_tech")
check("ten_universities", setequal(unique(scope$university_id), expected) && setequal(coverage$university_id, expected))
check("fifteen_unique_stations", nrow(scope) == 15L && uniqueN(scope$station_id) == 15L)
for (x in c("accounts", "lines", "obs", "sources", "reviews", "searches")) {
  d <- get(x); key <- switch(x, accounts = "account_id", lines = "line_id", obs = "observation_id", sources = "source_id", reviews = "review_id", searches = "search_id")
  check(paste0(x, "_unique_ids"), !anyDuplicated(d[[key]]))
}
check("known_account_sources", all(accounts$source_id %in% sources$source_id))
check("known_line_accounts", all(lines$account_id %in% accounts$account_id))
check("known_calculation_lines", all(terms$line_id %in% lines$line_id))
check("known_calculation_observations", all(terms$observation_id %in% obs$observation_id))
check("no_repeated_terms", !anyDuplicated(terms[, .(observation_id, output_field, line_id)]))
check("source_urls", all(grepl("^https://", lines$source_url)))
check("visual_review_every_amount", all(lines$visual_review == "yes"))
check("account_pages_visually_reviewed", all(paste(accounts$source_id, accounts$source_locator) %in% paste(reviews$source_id, reviews$source_locator)))
check("review_images_exist", all(file.exists(file.path(cur, reviews$review_image))))
sha <- function(p) digest::digest(file = p, algo = "sha256", serialize = FALSE)
downloaded <- sources[!blank(raw_relative_path)]
check("source_files_exist", all(file.exists(file.path(root, downloaded$raw_relative_path))))
check("source_sha256", all(vapply(file.path(root, downloaded$raw_relative_path), sha, character(1)) == downloaded$sha256))
check("review_image_sha256", all(vapply(file.path(cur, reviews$review_image), sha, character(1)) == reviews$review_image_sha256))
originals <- reviews[!blank(original_image_relative_path)]
check("original_page_images_md5", identical(unname(tools::md5sum(file.path(root, originals$original_image_relative_path))), originals$original_image_md5))
money <- function(s) {
  check("valid_manually_transcribed_money", all(grepl("^[0-9]+(,[0-9]{3})*(\\.[0-9]{1,2})?$", s)))
  as.numeric(gsub(",", "", s, fixed = TRUE))
}
lines[, amount_usd := money(reported_amount)]
check("nominal_usd", all(lines$currency == "USD" & lines$price_basis == "nominal"))
check("nonnegative_finite_amounts", all(is.finite(lines$amount_usd) & lines$amount_usd >= 0))
terms[, coefficient := as.numeric(coefficient)]
check("signed_terms", all(terms$coefficient %in% c(-1, 1)))
scope[, `:=`(operational_anchor_year = as.integer(operational_anchor_year), founding_decade = as.integer(founding_decade))]
obs[, financial_year := as.integer(financial_year)]
accounts[, financial_year := as.integer(financial_year)]
check("period_type", all(obs$period_kind %in% c("annual", "multiyear", "partial_year")))
dated <- obs[!blank(period_start) & !blank(period_end)]
check("period_order", all(as.IDate(dated$period_end) > as.IDate(dated$period_start)))
annual_dated <- dated[period_kind == "annual"]
days <- as.integer(as.IDate(annual_dated$period_end) - as.IDate(annual_dated$period_start)) + 1L
check("annual_period_lengths", all(days %in% c(365L, 366L)))
check("fiscal_end_year", all(as.integer(substr(dated$period_end, 1, 4)) == dated$financial_year))
check("no_duplicate_financial_period_boundary", !anyDuplicated(obs[, .(university_id, financial_year, period_kind, institutional_boundary)]))

tot <- function(a) lines[account_id == a & flow_class == "reported_total", amount_usd][[1]]
sum_components <- function(a) lines[account_id == a & flow_class %in% c("operating", "capital", "mixed"), sum(amount_usd)]
for (a in c("pur_general1889", "pur_electrical1889", "pur_mechanical1889", "pur_station_building1889", "clemson_statework1908")) {
  check(paste0("reconcile_", a), abs(sum_components(a) - tot(a)) < .005)
}
check("station_credit_total_includes_cash", abs(sum_components("pur_station1889") + lines[line_id == "pur_station1889_closing", amount_usd] - tot("pur_station1889")) < .005)
for (a in c("il_core1888", "il_core1889")) {
  check(paste0("cash_identity_", a), abs(lines[account_id == a & flow_class %in% c("expenditure_subtotal", "closing_balance"), sum(amount_usd)] - tot(a)) < .005)
}
check("texas_cash_identity", abs(lines[account_id == "tx_warrants1889" & flow_class %in% c("expenditure_subtotal", "closing_balance"), sum(amount_usd)] - tot("tx_warrants1889")) < .005)
check("texas_receipt_identity", abs(lines[account_id == "tx_warrants1889" & flow_class %in% c("opening_balance", "revenue_control"), sum(amount_usd)] - tot("tx_warrants1889")) < .005)
check("clemson_cash_identity", abs(lines[account_id == "clemson_treasury1908" & flow_class != "reported_total", sum(amount_usd)] - tot("clemson_treasury1908")) < .005)

expanded <- merge(terms, lines[, .(line_id, account_id, amount_usd, flow_class, source_id, source_locator, printed_page)], by = "line_id", sort = FALSE)
fields <- c("reported_account_expenditure_usd", "total_university_expenditure_usd", "account_operating_expenditure_usd", "operating_university_expenditure_usd", "identified_capital_expenditure_usd")
check("known_calculated_fields", all(terms$output_field %in% fields))
check("no_balances_or_transfers_added_to_spending", !any(expanded$coefficient > 0 & expanded$flow_class %in% c("opening_balance", "closing_balance", "internal_transfer", "revenue_control")))
values <- expanded[, .(value = round(sum(amount_usd * coefficient), 2)), by = .(observation_id, output_field)]
for (f in fields) {
  v <- values[output_field == f, .(observation_id, value)]
  obs[, (f) := v$value[match(observation_id, v$observation_id)]]
}
evidence <- expanded[, .(source_ids = paste(sort(unique(source_id)), collapse = ";"), source_locators = paste(sort(unique(paste(source_id, source_locator, sep = ":"))), collapse = ";"), printed_pages = paste(sort(unique(printed_page)), collapse = ";")), by = observation_id]
obs <- merge(obs, evidence, by = "observation_id", sort = FALSE)
obs[, source_urls := vapply(strsplit(source_ids, ";", fixed = TRUE), function(s) paste(sources$source_url[match(s, sources$source_id)], collapse = ";"), character(1))]
obs[, `:=`(currency = "USD", price_basis = "nominal", accounting_basis = "reported_cash_expenditure")]
check("whole_university_total_only_if_verified", all(is.na(obs$total_university_expenditure_usd) | obs$whole_university_verified == "yes"))
check("no_fabricated_operating_totals", all(is.na(obs$operating_university_expenditure_usd)))
check("purdue_total", abs(obs[observation_id == "purdue1889", total_university_expenditure_usd] - 68113.32) < .005)
check("purdue_cash_balance_removed", abs(sum_components("pur_station1889") - 14988.28) < .005)
check("clemson_running_expense_kept_partial", obs[observation_id == "clemson1908", account_operating_expenditure_usd] == 111221.24 && is.na(obs[observation_id == "clemson1908", total_university_expenditure_usd]))
check("illinois_balancing_totals_not_spending", abs(obs[observation_id == "il_core1888", reported_account_expenditure_usd] - 72467.91) < .005 && abs(obs[observation_id == "il_core1889", reported_account_expenditure_usd] - 75405.50) < .005)

# Only complete annual institution totals may fill the three requested slots.
select_periods <- function(o, decade_end) {
  o <- copy(o)
  setorder(o, financial_year, observation_id)
  selected <- rep(NA_character_, 3L)
  if (!nrow(o)) return(selected)
  selected[1] <- o$observation_id[1]
  remaining <- o[observation_id != selected[1]]
  if (nrow(remaining)) {
    selected[3] <- tail(o$observation_id, 1)
    middle <- remaining[observation_id != selected[3]]
    if (nrow(middle)) {
      target <- (o$financial_year[1] + decade_end) / 2
      middle[, distance := abs(financial_year - target)]
      setorder(middle, distance, financial_year)
      selected[2] <- middle$observation_id[1]
    }
  }
  selected
}
selection_fixture <- data.table(observation_id = c("a", "b", "c", "d"), financial_year = c(1882L, 1885L, 1886L, 1889L))
check("selection_midpoint_tie_earlier", identical(select_periods(selection_fixture, 1889L), c("a", "b", "d")))
check("selection_two_distinct_years", identical(select_periods(selection_fixture[c(1L, 4L)], 1889L), c("a", NA_character_, "d")))
check("selection_one_year_not_triplicated", identical(select_periods(selection_fixture[1], 1889L), c("a", NA_character_, NA_character_)))
check("selection_no_year_not_zero", all(is.na(select_periods(selection_fixture[0], 1889L))))
cross <- list()
for (i in seq_len(nrow(scope))) {
  s <- scope[i]
  o <- obs[university_id == s$university_id & period_kind == "annual" & whole_university_verified == "yes" & !is.na(total_university_expenditure_usd)]
  if (!is.na(s$founding_decade)) {
    o <- o[financial_year >= s$operational_anchor_year & financial_year <= s$founding_decade + 9L]
    if (!blank(s$first_operation_date_reported) && grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", s$first_operation_date_reported)) {
      o <- o[!blank(period_end) & period_end >= s$first_operation_date_reported]
    }
  } else o <- o[0]
  setorder(o, financial_year, observation_id)
  if (nrow(o)) {
    # Alternative boundary versions must be resolved in curation, not selected twice.
    check(paste0("one_verified_account_per_year_", s$station_id), !anyDuplicated(o$financial_year))
  }
  selected <- select_periods(o, s$founding_decade + 9L)
  for (slot in 1:3) {
    id <- selected[slot]; r <- obs[match(id, observation_id)]
    reason <- if (!is.na(id)) "verified_annual_whole_university_total" else if (is.na(s$founding_decade)) "founding_decade_unresolved" else if (s$founding_decade + 9L - s$operational_anchor_year + 1L < slot) "fewer_distinct_calendar_years_available" else coverage[university_id == s$university_id, review_status]
    exposure <- "unfilled"
    partial <- NA
    if (!is.na(id)) {
      opening <- s$first_operation_date_reported
      if (!blank(opening) && grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", opening) && !blank(r$period_start)) {
        partial <- r$period_start < opening
        exposure <- if (partial) "opening_within_fiscal_period" else "fiscal_period_after_opening"
      } else if (!blank(r$period_start) && as.integer(substr(r$period_start, 1, 4)) > s$operational_anchor_year) {
        partial <- FALSE; exposure <- "fiscal_period_after_opening_year"
      } else exposure <- "possible_partial_exposure_opening_date_imprecise"
    }
    cross[[length(cross) + 1L]] <- data.table(station_id = s$station_id, university_id = s$university_id, station_name = s$station_name, station_type = s$station_type, operational_anchor_year = s$operational_anchor_year, founding_decade = s$founding_decade, selection_slot = slot, selection_role = c("earliest", "midpoint", "latest")[slot], observation_id = id, financial_year = r$financial_year, selection_status = if (is.na(id)) "unfilled" else "selected", selection_reason = reason, opening_year_partial_exposure = partial, opening_exposure_status = exposure, coverage_notes = coverage[university_id == s$university_id, coverage_notes])
  }
}
cross <- rbindlist(cross)
check("45_selection_slots", nrow(cross) == 45L && all(cross[, .N, by = station_id]$N == 3L))
check("no_account_repeated_within_station", !anyDuplicated(cross[!blank(observation_id), .(station_id, observation_id)]))
check("selected_within_founding_decade", all(cross[selection_status == "selected", financial_year >= operational_anchor_year & financial_year <= founding_decade + 9L]))
check("cornell_stays_1870s", scope[station_id == "ny_ithaca", founding_decade] == 1870L)
check("clemson_not_selected_without_decade", all(cross[university_id == "clemson", selection_status] == "unfilled"))
check("no_biennial_total_selected", !any(cross$observation_id %in% obs[period_kind != "annual", observation_id]))
obs[, selected_for_station := observation_id %in% cross[selection_status == "selected", observation_id]]
setcolorder(obs, c("university_id", "financial_year", "observation_id", "period_start", "period_end", "period_kind", "total_university_expenditure_usd", "operating_university_expenditure_usd", "reported_account_expenditure_usd", "account_operating_expenditure_usd", "identified_capital_expenditure_usd", "institutional_boundary", "whole_university_verified", "verification_status", "observation_role", "selected_for_station", "currency", "price_basis", "accounting_basis", "source_ids", "printed_pages", "source_locators", "source_urls", "notes"))
for (uid in expected) {
  z <- obs[university_id == uid & period_kind == "annual"]
  ids <- scope[university_id == uid, station_id]
  coverage[university_id == uid, `:=`(n_stations = length(ids), n_annual_source_accounts = nrow(z), n_verified_whole_university_years = sum(!is.na(z$total_university_expenditure_usd)), selected_years = paste(sort(unique(cross[university_id == uid & selection_status == "selected", financial_year])), collapse = ";"), n_filled_station_slots = nrow(cross[university_id == uid & selection_status == "selected"]), n_unfilled_station_slots = nrow(cross[university_id == uid & selection_status == "unfilled"]))]
}
check("coverage_accounts_for_every_slot", sum(coverage$n_filled_station_slots + coverage$n_unfilled_station_slots) == 45L)
check("all_universities_have_search_audit", setequal(searches$university_id, expected))

write_output <- function(x, suffix) {
  path <- file.path(out, paste0(prefix, suffix))
  fwrite(x, path, na = "", quote = TRUE, bom = FALSE, eol = "\n")
}
setorder(obs, university_id, financial_year, observation_id)
setorder(cross, university_id, station_id, selection_slot)
setorder(coverage, university_id)
setorder(accounts, university_id, financial_year, account_id)
setorder(lines, account_id, line_id)
dir.create(out, recursive = TRUE, showWarnings = FALSE)
annual <- obs[period_kind == "annual"]
write_output(annual, ".csv")
write_output(cross, "_station_crosswalk.csv")
write_output(coverage, "_coverage.csv")
write_output(accounts, "_account_audit.csv")
write_output(obs, "_period_audit.csv")
write_output(lines, "_reported_lines.csv")
write_output(terms, "_calculation_terms.csv")
write_output(sources, "_sources.csv")
write_output(searches, "_search_log.csv")

descriptions <- c(observation_id = "Stable university-period-boundary record ID.", university_id = "Existing allocation/station university ID.", financial_year = "Fiscal-year ending year, not report publication year.", period_kind = "annual, multiyear or partial_year; no annualization.", period_start = "First day of account period; blank when unverified.", period_end = "Last day of account period; blank when unverified.", institutional_boundary = "Activities and funds included or excluded by the account.", whole_university_verified = "yes only for a reconciled whole institution denominator.", observation_role = "Founding-decade candidate, unresolved decade or explicit contextual period.", verification_status = "Verification of original account or reconstructed consolidation.", notes = "Accounting interpretation and remaining limitations.", reported_account_expenditure_usd = "Actual expenditure/cash warrants in the stated account, excluding closing balances; may cover only part of university.", total_university_expenditure_usd = "Reconciled whole-university actual expenditure including capital; blank until institutional coverage is verified.", account_operating_expenditure_usd = "Source-defined running/operating expenditure for the stated account; not necessarily the whole university.", operating_university_expenditure_usd = "Whole-university operating expenditure; blank if capital/mixed items cannot be separated.", identified_capital_expenditure_usd = "Individually documented capital spending; partial identified amount, not necessarily complete capital spending.", source_ids = "Semicolon-separated source register IDs.", source_locators = "PDF page numbers or original CONTENTdm page IDs, prefixed by source ID.", printed_pages = "Printed page numbers of calculation evidence.", source_urls = "Stable document landing URLs; page images also recorded in review inputs.", currency = "USD.", price_basis = "Nominal contemporary dollars; no inflation adjustment.", accounting_basis = "Historical reported cash expenditure; not modern accrual accounting.", selected_for_station = "TRUE when referenced by at least one selected station slot.")
dictionary <- data.table(file = paste0(prefix, ".csv"), field = names(annual), description = unname(descriptions[names(annual)]))
check("every_main_field_documented", all(!blank(dictionary$description)))
support_descriptions <- c(
  station_id = "Existing1910 station identity; preserved without renumbering.", station_name = "Existing station name.", station_type = "Main or separately established branch station.", operational_anchor_year = "Existing operational founding-year anchor, preserving uncertainty.", founding_decade = "Calendar founding decade, e.g.1880; blank if unresolved.", selection_slot = "1 earliest,2 midpoint,3 latest; only distinct observed years fill slots.", selection_role = "Requested temporal role, not proof a year was recovered.", selection_status = "selected or unfilled.", selection_reason = "Reason selected or unfilled; absence of recovery is not zero spending.", opening_year_partial_exposure = "TRUE if known opening lies inside the fiscal period; blank when uncertain/unfilled.", opening_exposure_status = "Evidence-based assessment of whether account includes pre-opening months.", coverage_notes = "Source-specific scope, period and archival limitations.", university_name = "Institution name preserved from existing scope.", sources_reviewed = "Semicolon-separated source IDs considered for this institution.", review_status = "unresolved or unavailable_online_in_sources_reviewed; neither certifies historical absence.", next_research_step = "Concrete missing archive/account reconciliation task.", research_complete = "no while founding-decade archival research remains incomplete.", checked_date = "Date the source or institutional search was reviewed.", n_stations = "Number of existing scoped station identities at institution.", n_annual_source_accounts = "Annual observation rows for limited or whole institution boundary; not a count of verified university totals.", n_verified_whole_university_years = "Annual rows with a verified whole-university expenditure value.", selected_years = "Distinct fiscal ending years selected for at least one scoped station.", n_filled_station_slots = "Selected station-to-account slots, allowing shared accounts across stations.", n_unfilled_station_slots = "Unfilled station slots, including insufficient possible years and unresolved coverage.", account_id = "Stable source-account ID, referenced by monetary lines.", source_id = "Source register ID.", source_locator = "One-based PDF page or original CONTENTdm page ID.", printed_page = "Page label printed in the source.", reported_account_label = "Original heading or faithful short transcription of account heading.", line_id = "Stable manually transcribed source line ID.", reported_label = "Source monetary line wording, with punctuation/spacing normalized where necessary.", reported_amount = "Manually verified original monetary string; commas preserved.", flow_class = "Classification: reported_total, expenditure_subtotal, operating, capital, mixed, internal_transfer, opening_balance, closing_balance or revenue_control. Source-defined totals may be balancing totals.", source_url = "Stable source document landing URL.", visual_review = "yes means monetary amount checked against original rendered page.", amount_usd = "Numeric nominal USD parsed from reported_amount; no imputation.", output_field = "Calculated expenditure field to which signed source line contributes.", coefficient = "+1 to include or -1 to exclude a cited amount; no fractions/annualization.", title = "Source title and actual period where catalog metadata is misleading.", source_kind = "Original financial report, catalog, finding aid or inaccessible report lead.", access_status = "Retrieved document or explicit access/metadata-only status.", raw_relative_path = "Archived file path relative to TALENT_DETS_DATA_DIR; blank when no original document was retrieved.", sha256 = "SHA256 checksum of archived file or curated input.", search_id = "Stable institution search-audit ID.", search_date = "Date of research review.", search_channels = "Channels used in the source search.", query_description = "Human-readable description of institution and account sought.", result_status = "Search outcome without claiming exhaustive archival absence.", result_notes = "What the reviewed sources establish and fail to establish.", input_file = "Curated input CSV filename.", md5 = "MD5 checksum of exact input bytes.", check = "Validation or accounting acceptance condition.", passed = "TRUE only when validation condition passes.", detail = "Additional validation explanation when applicable.")
support <- list("_station_crosswalk.csv" = cross, "_coverage.csv" = coverage, "_account_audit.csv" = accounts, "_period_audit.csv" = obs, "_reported_lines.csv" = lines, "_calculation_terms.csv" = terms, "_sources.csv" = sources, "_search_log.csv" = searches)
for (suffix in names(support)) {
  nm <- names(support[[suffix]])
  ds <- descriptions[nm]
  missing <- is.na(ds)
  ds[missing] <- support_descriptions[nm[missing]]
  check(paste0("dictionary_", suffix), all(!blank(ds)))
  dictionary <- rbind(dictionary, data.table(file = paste0(prefix, suffix), field = nm, description = unname(ds)))
}
write_output(dictionary, "_field_dictionary.csv")
inputs <- sort(list.files(cur, pattern = "\\.csv$", full.names = TRUE))
manifest <- data.table(input_file = basename(inputs), md5 = unname(tools::md5sum(inputs)), sha256 = vapply(inputs, sha, character(1)))
write_output(manifest, "_input_manifest.csv")
check("existing_processed_outputs_preserved", identical(protected_before, tools::md5sum(protected)))
write_output(checks, "_validation.csv")
readme <- c(
  "# University expenditure in agricultural-station founding decades", "",
  sprintf("Release reviewed 2026-10-22: %d universities, %d scoped station identities, %d annual source-account observations, %d verified whole-university annual total(s).", nrow(coverage), nrow(scope), nrow(annual), nrow(annual[!is.na(total_university_expenditure_usd)])),
  "", "## Status and interpretation", "",
  "This is a sourced dataset with incomplete archival coverage. Only Purdue FY1889 currently has a reconciled whole-university total. Other observed amounts are explicitly partial university accounts; missing values never mean zero. No university has three completed founding-decade years. Source catalogs and unsuccessful searches are leads, not financial observations. Research is not complete.",
  "", "The main CSV keeps verified annual account spending alongside a separate total_university_expenditure_usd field. Use that field for a whole-university denominator. Do not use reported_account_expenditure_usd as a substitute. Amounts are nominal contemporary USD, mainly cash-based. Actual expenditure including capital is distinct from operating expenditure, appropriations, income, and cash balancing totals.",
  "", "## Verified accounts and limitations", "",
  "- Purdue FY1889: general fund 49,444.01 + electrical capital 306.00 + mechanical capital 1,071.03 + station/farm capital 2,304.00 + federal station spending (15,000.00 minus 11.72 unspent) = 68,113.32. The five funds cover the annual report's university expenditure accounts for July1888-June1889. No within-year transfer is among these expense lines. Prior-year station reimbursements are not added again. The general fund contains combined rent/fee rebates, apparatus, supplies and other mixed items; a complete operating-only total is not inferred. Source pp12,14,15,41, original CONTENTdm IDs269,271,272,298.",
  "- Illinois FY1888: 27,935.45 + 44,532.46 = 72,467.91 for the stated university account; excludes 23,151.21 closing cash from the 95,619.12 balancing total. FY1889: 29,062.54 + 46,342.96 = 75,405.50; excludes 40,479.45 closing cash. Separate research funds and fiscal periods prevent whole-university consolidation. Bond premiums and mixed capital remain in source-defined spending. The 1888 heading explicitly excludes the Natural History Laboratory; a 500.00 station receipt is not evidence that all station expense is included.",
  "- Texas FY1888-89: 96,548.93 cash warrants paid, rather than the 97,494.06 balancing total. Exact fiscal boundary and external-spending coverage remain unresolved; maintenance subtotal is not added twice. No whole-university total certified.",
  "- Clemson FY1908: 56,366.12 State work + 38,621.83 permanent improvements + 111,221.24 college running expenses = 206,209.19 in this treasury boundary. The separate Hatch/Adams federal station account is not in treasury receipts; no full university total certified. State work contains 4,046.31 of Coast Experiments expenditure, plus scholarships/regulatory services. Coast's original founding decade remains unresolved. A separate State-work detail is nested within the treasury summary and is not counted again.",
  "- Purdue FY1886-88 total92,854.08 is preserved only in the period audit as multiyear, including an explicitly identified480.80 internal transfer. It is not annualized or selected.",
  "- New Hampshire1890 college report lies outside the founding decade; its financial statement explicitly warns that farm cash flows include advances repaid by the station. No1890 amounts substitute for1880s values. Earlier1887 report endsApril30 before the station's August organization. Cornell remains anchored to1879, not later Hatch reorganization. LSU and Virginia ledger catalogs establish leads, not amounts. The1909 federal Table82 disbursements cover Morrill/Nelson grants only; equipment values in Table83 are asset stocks, not expenditure.",
  "", "## Selection and coverage", "",
  "Exactly three slots per existing station: earliest verified annual whole-university total after opening, latest within the founding calendar decade, and a distinct available year nearest the midpoint between earliest year and decade end (ties earlier). With one observation only earliest fills; with two only earliest/latest fill. Shared denominators are stored once and referenced through the station crosswalk. Fiscal end year drives selection, exact source dates are retained, and opening-year periods are flagged. Unknown founding decade blocks selection. Fewer possible calendar years, inaccessible sources and unresolved accounts remain explicitly unfilled. The coverage and search-log files distinguish these reasons. Reviewed sources do not establish exhaustive archival absence.",
  "", "## Files", "",
  "- Main CSV: verified annual observations with explicit account and whole-university fields.",
  "- _station_crosswalk.csv: 45 station selection slots; blank observation_id means unfilled, not zero expenditure.",
  "- _coverage.csv: all10 universities, observed/selected counts, gaps, next archival steps and research-completeness flags.",
  "- _account_audit.csv, _period_audit.csv, _reported_lines.csv, _calculation_terms.csv: account definitions, annual/multiyear audit, verbatim monetary strings and signed source-line calculations.",
  "- _sources.csv and _search_log.csv: provenance, checksums/access results and searches reviewed. Catalog-only leads have no financial amount.",
  "- _field_dictionary.csv: complete main-table definitions; _input_manifest.csv and _validation.csv document reproducibility and checks.",
  "", "## Offline rebuild", "",
  "Curated inputs and page images: Data/intermediary/scientific_facilities/university_expenditures/. Newly retrieved originals: Data/raw/scientific_facilities/university_expenditures/. Reused source PDFs remain unchanged in Data/raw/scientific_facilities/station_finances/ and are referenced by relative project path. Purdue original JPEGs and OCR/compound metadata are retained unmodified. Review PNGs were rendered from originals; every monetary page was visually reviewed.",
  "", "Run prep/scientific_facilities/build_university_founding_decade_expenditures.R with TALENT_DETS_DATA_DIR explicitly set to C:/Users/megaj/Globtalent Dropbox/gtl_talent_dets. Requires data.table and digest. The script sources paths.R, makes no network calls, derives amounts from signed terms, validates source/review checksums and preserves previous inventories, station-finance outputs and analysis files. The pipeline README gives the PowerShell command. Station shares are not calculated by this release.")
writeLines(readme, file.path(out, paste0(prefix, "_README.md")), useBytes = TRUE)
cat(sprintf("Published %d annual observations; %d verified whole-university total(s); %d filled of45 station slots. %d checks passed.\n", nrow(annual), sum(!is.na(annual$total_university_expenditure_usd)), sum(cross$selection_status == "selected"), nrow(checks)))
