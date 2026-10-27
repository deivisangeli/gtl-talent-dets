#!/usr/bin/env Rscript
# Reduced annual audit and sourced 1889 funding components; no imputations.
args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grepl("^--file=", args)])
repo_root <- if (length(file_arg)) {
  normalizePath(file.path(dirname(file_arg[[1]]), "../.."), winslash = "/", mustWork = TRUE)
} else {
  normalizePath(Sys.getenv("GTL_REPO", unset = getwd()), winslash = "/", mustWork = TRUE)
}
source(file.path(repo_root, "paths.R"))
stopifnot(requireNamespace("data.table", quietly = TRUE))
library(data.table)
folder <- file.path(TALENT_DETS_DATA_DIR, "Data/processed/scientific_facilities")
input <- file.path(folder, "station_founding_decade_finances_annual.csv")
before <- tools::md5sum(input)
x <- fread(input, colClasses = "character", na.strings = "", encoding = "UTF-8")
cur <- file.path(TALENT_DETS_DATA_DIR, "Data/intermediary/scientific_facilities/station_finances")
lines_path <- file.path(folder, "station_founding_decade_finances_reported_lines.csv")
accounts_path <- file.path(folder, "station_founding_decade_finances_accounts.csv")
components_path <- file.path(cur, "annual_funding_components_curated.csv")
lines <- fread(lines_path, colClasses = "character", na.strings = "", encoding = "UTF-8")
accounts <- fread(accounts_path, colClasses = "character", na.strings = "", encoding = "UTF-8")
components <- fread(components_path, colClasses = "character", na.strings = "", encoding = "UTF-8")
components[, amount_usd := as.numeric(amount_usd)]
funding_accounts <- accounts[measure == "reported_annual_funding"]
stopifnot(nrow(funding_accounts) == 9L, !anyDuplicated(funding_accounts$account_id),
          !anyDuplicated(components$component_id), all(is.finite(components$amount_usd)),
          all(components$amount_usd >= 0), all(components$visual_review == "yes"),
          all(components$line_id %in% lines$line_id),
          all(components$category %in% c("federal", "state_appropriation", "fertilizer_tax", "farm_proceeds")))
components[, account_id := lines$account_id[match(line_id, lines$line_id)]]
stopifnot(setequal(components$account_id, funding_accounts$account_id))
eligible_lines <- lines[account_id %in% funding_accounts$account_id & flow_class == "reported_annual_funding"]
stopifnot(setequal(components$line_id, eligible_lines$line_id))
# A single source line can be split only into explicitly reported components.
# LSU's printed b2,000 + g1,400 entries were originally stored as one 3,400 line.
line_totals <- components[, .(component_total = sum(amount_usd)), by = line_id]
stopifnot(all(abs(line_totals$component_total - as.numeric(lines$amount_usd[match(line_totals$line_id, lines$line_id)])) < 0.005))
reported_sum <- function(amount, category, wanted) {
  if (any(category %in% wanted)) sum(amount[category %in% wanted]) else NA_real_
}
breakdown <- components[, .(
  federal_funding_reported_usd = reported_sum(amount_usd, category, "federal"),
  state_appropriation_reported_usd = reported_sum(amount_usd, category, "state_appropriation"),
  fertilizer_tax_reported_usd = reported_sum(amount_usd, category, "fertilizer_tax"),
  farm_proceeds_reported_usd = reported_sum(amount_usd, category, "farm_proceeds"),
  nonfederal_funding_reported_usd = reported_sum(amount_usd, category, c("state_appropriation", "fertilizer_tax", "farm_proceeds")),
  components_total_usd = sum(amount_usd)
), by = account_id]
breakdown <- breakdown[match(funding_accounts$account_id, account_id)]
breakdown[, total_funding_reported_usd := as.numeric(funding_accounts$reported_total_usd)]
stopifnot(all(abs(breakdown$components_total_usd - breakdown$total_funding_reported_usd) < 0.005))
breakdown[, funding_coverage_status := ifelse(is.na(nonfederal_funding_reported_usd),
  "no_additional_funding_reported", "additional_funding_reported")]
breakdown[, nonfederal_to_federal_pct := 100 * nonfederal_funding_reported_usd / federal_funding_reported_usd]
breakdown[, components_total_usd := NULL]
universities <- fread(file.path(folder, "station_founding_decade_finances_universities.csv"), colClasses = "character", encoding = "UTF-8")
stopifnot(!anyDuplicated(universities$university_id), all(x$university_id %in% universities$university_id))
funding <- x[numerator_measure == "reported_annual_funding"]
stopifnot(nrow(funding) == 9L, !anyDuplicated(funding$numerator_account_id),
          setequal(funding$numerator_account_id, breakdown$account_id))
funding <- funding[match(breakdown$account_id, numerator_account_id)]
detail <- cbind(funding[, .(
  station_id, university_id,
  university = universities$university_name[match(university_id, universities$university_id)],
  station = station_name, year = financial_year, operational_anchor_year,
  founding_decade, within_founding_decade, account_id = numerator_account_id
)], breakdown[, !"account_id"])
detail[, `:=`(
  funding_period = funding$numerator_period_label,
  funding_period_end = funding$numerator_period_end,
  source_url = funding$numerator_source_url,
  source_pdf_page = funding$numerator_source_pdf_page,
  source_printed_page = funding$numerator_source_printed_page,
  notes = paste(funding$numerator_account_notes,
    "Missing component amounts mean not reported, not zero. Totals are published funding statistics, not certified realized receipts or expenditures.")
)]
detail[university_id == "purdue", notes := paste(notes,
  "The original table's footnote e identifies the extra 2,000 as proceeds of farm; corrected from the earlier state/other classification on 2026-10-23.")]
detail[university_id == "lsu", notes := paste(notes,
  "State Experiment Station No. 2: 5,000 federal allocation, 2,000 state appropriation (b), and 1,400 fertilizer tax (g); not the Louisiana-wide station network.")]
detail[, currency := "USD"]
detail[, price_basis := "nominal"]
detail_output <- file.path(folder, "station_founding_decade_finances_annual_funding.csv")
fwrite(detail, detail_output, quote = TRUE, na = "", eol = "\n", bom = TRUE)
reduced <- x[, .(
  university = universities$university_name[match(university_id, universities$university_id)],
  station = station_name,
  year = financial_year,
  operational_anchor_year,
  founding_decade,
  numerator_reported_usd = numerator_reported_total_usd,
  numerator_type = numerator_measure,
  numerator_scope = numerator_account_scope,
  denominator_reported_usd = denominator_reported_total_usd,
  denominator_type = denominator_measure,
  denominator_scope = denominator_account_scope,
  share_pct,
  status = publication_status,
  notes = exclusion_or_missing_reason
)]
stopifnot(nrow(reduced) == nrow(x), ncol(reduced) == 14L,
          identical(reduced$operational_anchor_year, x$operational_anchor_year),
          identical(reduced$founding_decade, x$founding_decade),
          identical(reduced$numerator_reported_usd, x$numerator_reported_total_usd),
          identical(reduced$denominator_reported_usd, x$denominator_reported_total_usd),
          identical(reduced$share_pct, x$share_pct))
# Match by account identity and measure; never attach funding components to an
# expenditure account, another year, or an individual branch of a network.
key <- match(x$numerator_account_id, detail$account_id)
key[x$numerator_measure != "reported_annual_funding" | is.na(x$numerator_measure)] <- NA_integer_
added <- c("federal_funding_reported_usd", "state_appropriation_reported_usd",
           "fertilizer_tax_reported_usd", "farm_proceeds_reported_usd",
           "nonfederal_funding_reported_usd", "funding_coverage_status", "funding_period",
           "source_url", "source_pdf_page", "source_printed_page")
for (field in added) set(reduced, j = paste0(if (startsWith(field, "source_")) "funding_" else "", field), value = detail[[field]][key])
stopifnot(sum(!is.na(reduced$federal_funding_reported_usd)) == 9L,
          sum(!is.na(detail$nonfederal_funding_reported_usd)) == 3L,
          detail[university_id == "purdue", farm_proceeds_reported_usd] == 2000,
          is.na(detail[university_id == "purdue", state_appropriation_reported_usd]),
          detail[university_id == "lsu", state_appropriation_reported_usd] == 2000,
          detail[university_id == "lsu", fertilizer_tax_reported_usd] == 1400,
          detail[university_id == "tennessee", fertilizer_tax_reported_usd] == 800)
output <- file.path(folder, "station_founding_decade_finances_annual_key_columns.csv")
fwrite(reduced, output, quote = TRUE, na = "", eol = "\n", bom = TRUE)
definitions <- c(
  station_id = "Stable station ID preserved from the 1910 inventory.",
  university_id = "Stable university ID preserved from the financial audit.",
  university = "Institution name from the university summary.", station = "Station name from the annual audit.",
  year = "Financial reporting label; not the founding year. All nine funding observations are 1889.",
  operational_anchor_year = "Existing evidence-based first-operation anchor; uncertainty retained in the full station scope.",
  founding_decade = "Decade of the operational anchor; not inferred from funding year.",
  within_founding_decade = "Whether financial reporting year is within the station's founding decade; Cornell's 1889 benchmark is outside.",
  account_id = "Unique funding account; connects to the full accounts and reported-lines audits.",
  federal_funding_reported_usd = "Federal funding as printed in the 1889 table; LSU is a station allocation, not the state-wide grant.",
  state_appropriation_reported_usd = "Separately reported state appropriation, footnote b. Missing is not zero.",
  fertilizer_tax_reported_usd = "Separately reported fertilizer-tax funding, footnote g. Missing is not zero.",
  farm_proceeds_reported_usd = "Separately reported proceeds of farm, footnote e. Missing is not zero.",
  nonfederal_funding_reported_usd = "Sum of reported nonfederal components; missing when no additional component was reported, not a certified zero.",
  total_funding_reported_usd = "Original reported total, unchanged; already includes reported additional funding. Never add components to this total again.",
  funding_coverage_status = "additional_funding_reported or no_additional_funding_reported. Neither certifies complete coverage of all station resources.",
  nonfederal_to_federal_pct = "100 times reported nonfederal components divided by reported federal funding; missing when additional funding was not reported.",
  funding_period = "Source funding period label; table was compiled before fiscal-year close.",
  funding_period_end = "Source identifies the fiscal year ending 1889-06-30; not proof of realized receipts.",
  source_url = "URL of original 1889 USDA OES Bulletin 1.", source_pdf_page = "One-based PDF page.",
  source_printed_page = "Printed source page; 22 or 23.", notes = "Coverage limitations and source classification corrections.",
  currency = "USD.", price_basis = "Nominal historical dollars; no inflation adjustment."
)
stopifnot(all(names(detail) %in% names(definitions)))
dictionary <- data.table(field = names(detail), definition = unname(definitions[names(detail)]))
fwrite(dictionary, file.path(folder, "station_founding_decade_finances_annual_funding_field_dictionary.csv"), quote = TRUE, na = "", eol = "\n")
writeLines(c(
  "# Annual station funding: sourced component supplement", "",
  "Nine station funding observations from USDA Office of Experiment Stations Bulletin 1 (1889), printed pages 22-23 / PDF pages 26-27. This supplement decomposes the existing annual funding accounts; it does not add a second grant or replace funding statistics with realized receipts.",
  "All amounts and footnotes were visually rechecked against the unchanged original scan on 2026-10-23. Purdue's extra 2,000 is farm proceeds (e), correcting the earlier state/other label. LSU's extra 3,400 is split into 2,000 state appropriation (b) and 1,400 fertilizer tax (g). Tennessee's 800 is fertilizer tax (g).", "",
  "## Missing amounts and coverage",
  "Blank category amounts mean not separately reported, never confirmed zero. Six observations have no additional funding reported; three have additional funding. The printed table invites stations to supply further information, so it does not establish absence of university support or unreported income. No land, buildings, staff time, or other in-kind contributions are monetized or imputed.",
  "The existing totals already include all printed components: Purdue 17,000; LSU State Station No. 2 at Baton Rouge 8,400; Tennessee 15,800; other covered stations 15,000 each. Reconciliation is to these published totals, not an assertion of exhaustive funding coverage. No 1910 amount is substituted for 1889.",
  "The LSU row is a station-level federal allocation of 5,000, not Louisiana's full grant. Cornell's 1889 observation remains a later benchmark outside its 1870s founding decade. Other historical branches are not given new allocations from a main station account.", "",
  "## Outputs",
  "station_founding_decade_finances_annual_funding.csv: nine funding observations with category amounts, reported total, federal comparison, dates and source pages.",
  "station_founding_decade_finances_annual_key_columns.csv: original annual audit rows and numerator/denominator values, with matched funding components appended only to funding observations. Original share_pct remains missing pending fiscal-period and accounting reconciliation.",
  "station_founding_decade_finances_annual_funding_field_dictionary.csv: supplement fields. In the reduced annual file, funding_source_url and funding_source_*_page denote the same provenance fields.", "",
  "## Offline reproduction",
  "Run build_station_founding_decade_finances.R, then export_station_finance_key_columns.R with TALENT_DETS_DATA_DIR explicitly set to the Dropbox project root. Both source paths.R. Inputs are financial_lines_curated.csv and annual_funding_components_curated.csv under Data/intermediary/scientific_facilities/station_finances/; the correction audit is funding_source_corrections_curated.csv.",
  "The exporter checks source-line coverage, component reconciliation, unique accounts, nine funding rows, three observed additional-funding totals, source-specific categories, and preservation of original numerator, denominator and share fields. Rebuilding does not require internet access."
), file.path(folder, "station_founding_decade_finances_annual_funding_README.md"), useBytes = TRUE)
stopifnot(identical(before, tools::md5sum(input)))
cat(sprintf("Saved %d rows and %d columns: %s\n", nrow(reduced), ncol(reduced), output))
cat(sprintf("Saved %d funding observations; %d with additional reported funding: %s\n", nrow(detail), sum(!is.na(detail$nonfederal_funding_reported_usd)), detail_output))
