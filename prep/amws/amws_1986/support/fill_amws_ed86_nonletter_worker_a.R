suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(tibble)
})

root <- file.path(
  "C:/Users", Sys.info()[["user"]], "Globtalent Dropbox",
  "gtl_talent_dets", "Data", "intermediary", "amws",
  "manual_nonletter_birth_city_review_20260714"
)

blank_na <- function(x) {
  x <- as.character(x)
  ifelse(is.na(x) | str_to_upper(str_trim(x)) == "NA", "", x)
}

# Explicit row-wise decisions after inspecting raw_text_adjusted.  State is
# intentionally blank outside the USA/Canada schema used by AMWS.
decisions <- tribble(
  ~pilot_id, ~birth_city_new, ~birth_state_new, ~birth_country_new, ~manual_confidence,
  "003", "Magog", "QC", "Canada", "high",
  "005", "Uniontown", "PA", "USA", "medium",
  "007", "Nagercoil", "", "India", "high",
  "008", "Summit", "NJ", "USA", "high",
  "010", "Tainan", "", "China", "high",
  "012", "L'Anse", "MI", "USA", "medium",
  "016", "Pittston", "PA", "USA", "medium",
  "017", "Coeur d'Alene", "ID", "USA", "medium",
  "018", "Emerson", "MB", "Canada", "high",
  "021", "Trichinopoly", "", "India", "high",
  "022", "Kathlal", "", "India", "high",
  "023", "Dallas", "TX", "USA", "medium",
  "025", "Ponce", "PR", "USA", "high",
  "027", "Estcourt", "", "South Africa", "medium",
  "028", "Gudlavalleru", "", "India", "medium",
  "032", "Nabha", "", "India", "high",
  "033", "Rnghudevapuram", "", "India", "high",
  "036", "Davis", "WV", "USA", "medium",
  "037", "Regina", "SK", "Canada", "high",
  "038", "Pana", "IL", "USA", "medium",
  "040", "Coimbatore", "", "India", "high",
  "044", "Leamington", "ON", "Canada", "high",
  "046", "Jullundur", "", "India", "high",
  "047", "Norfolk", "VA", "USA", "high",
  "049", "Burlington Township", "NJ", "USA", "medium",
  "050", "Newcastle", "NB", "Canada", "high",
  "204", "Valleyfield", "QC", "Canada", "high",
  "205", "Kakinada", "", "India", "high",
  "209", "Jhelum", "", "India", "medium",
  "213", "Warsaw", "", "Poland", "medium",
  "216", "Garards Fort", "PA", "USA", "medium",
  "217", "Wapanucka", "OK", "USA", "medium",
  "219", "Sungakkarampatti", "", "India", "medium",
  "220", "Allahabad", "", "India", "high",
  "221", "Bonaparte", "IA", "USA", "high",
  "224", "Calgary", "AB", "Canada", "high",
  "225", "Dayton", "OH", "USA", "medium",
  "226", "Phoenix", "AZ", "USA", "medium",
  "227", "Niagara-on-the-Lake", "ON", "Canada", "high",
  "228", "Wiesbaden", "", "Germany", "medium",
  "229", "Montreal", "QC", "Canada", "high",
  "230", "Welch", "WV", "USA", "medium",
  "231", "Kanpur", "", "India", "high",
  "234", "Ruppura", "", "India", "high",
  "237", "Stevensville", "ON", "Canada", "high",
  "240", "Montreal", "QC", "Canada", "high",
  "241", "Milwaukee", "WI", "USA", "medium",
  "242", "Nagyvarad", "", "Hungary", "medium",
  "244", "Toronto", "ON", "Canada", "high",
  "246", "Charleston", "SC", "USA", "medium",
  "247", "Calcutta", "", "India", "high",
  "248", "Vancouver", "BC", "Canada", "high",
  "250", "Baghdad", "", "Iraq", "high",
  "401", "Brisbane", "", "Australia", "high",
  "402", "Detroit", "MI", "USA", "medium",
  "404", "Summerside", "PE", "Canada", "high",
  "405", "Regina", "SK", "Canada", "high",
  "406", "Hickory", "NC", "USA", "medium",
  "410", "Trivandrum", "", "India", "high",
  "413", "Largs", "", "Australia", "high",
  "416", "Weihwei", "", "China", "medium",
  "419", "Hobbs", "NM", "USA", "high",
  "420", "Fuchow", "", "China", "high",
  "423", "Berhampore", "", "India", "high",
  "424", "Brooklyn", "NY", "USA", "medium",
  "425", "Jallander", "", "India", "high",
  "427", "Montreal", "QC", "Canada", "high",
  "429", "Yarmouth", "NS", "Canada", "high",
  "430", "Bronx", "NY", "USA", "high",
  "431", "Adelaide", "", "Australia", "high",
  "432", "Meerut", "", "India", "high",
  "433", "Waverly", "IA", "USA", "medium",
  "435", "St Peter's Bay", "PE", "Canada", "high",
  "436", "Ste Therese", "QC", "Canada", "high",
  "440", "Milton", "ND", "USA", "high",
  "441", "Toledo", "OH", "USA", "medium",
  "443", "Cardston", "AB", "Canada", "high",
  "446", "Knoxville", "TN", "USA", "medium",
  "448", "Roslyn Heights", "NY", "USA", "high",
  "449", "Toronto", "ON", "Canada", "high",
  "601", "Edmonton", "AB", "Canada", "high",
  "602", "Wilno", "", "Poland", "medium",
  "603", "Corning", "NY", "USA", "high",
  "604", "Toronto", "ON", "Canada", "high",
  "605", "Norwood", "OH", "USA", "medium",
  "606", "Lethbridge", "AB", "Canada", "high",
  "607", "Niagara Falls", "ON", "Canada", "high",
  "609", "Merlin", "ON", "Canada", "high",
  "611", "Karad", "", "India", "high",
  "612", "Spiramadom", "", "India", "high",
  "613", "Karnal", "", "India", "high",
  "615", "Brooklyn", "NY", "USA", "high",
  "619", "Wheatland", "IA", "USA", "medium",
  "620", "Nevada City", "CA", "USA", "medium",
  "622", "Guntur", "", "India", "high",
  "623", "Ottawa", "ON", "Canada", "high",
  "630", "Coeur d'Alene", "ID", "USA", "medium",
  "636", "Wallaroo", "", "Australia", "high",
  "638", "Dallas", "TX", "USA", "medium",
  "639", "Toledo", "OH", "USA", "medium",
  "640", "Wijnegem", "", "Belgium", "medium",
  "641", "Dauphin", "MB", "Canada", "high",
  "642", "Cranbrook", "BC", "Canada", "high",
  "644", "Montreal", "QC", "Canada", "high",
  "645", "Philadelphia", "PA", "USA", "medium",
  "801", "Austin", "TX", "USA", "high",
  "802", "Belle Fourche", "SD", "USA", "medium",
  "803", "Cadomin", "AB", "Canada", "high",
  "805", "Viipuri", "", "Finland", "medium",
  "806", "Rosetown", "SK", "Canada", "high",
  "808", "Sioux Falls", "SD", "USA", "high",
  "810", "Wilmington", "DE", "USA", "medium",
  "819", "Lehighton", "PA", "USA", "medium",
  "824", "Centralia", "IL", "USA", "high",
  "826", "Washington", "DC", "USA", "medium",
  "828", "Khamar", "", "India", "high",
  "830", "Little Neck", "NY", "USA", "high",
  "833", "Dean", "", "Australia", "high",
  "836", "Motihari", "", "India", "high",
  "837", "Burlington", "ON", "Canada", "high",
  "839", "Muzaffarpur", "", "India", "medium",
  "842", "Taegu", "", "Korea", "medium",
  "845", "Dayton", "OH", "USA", "medium"
)

no_change_ids <- c("024", "031", "048", "218", "442", "633", "648",
                   "825", "838", "846")

if (anyDuplicated(decisions$pilot_id) ||
    length(intersect(decisions$pilot_id, no_change_ids))) {
  stop("Worker A decision keys are duplicated or overlap no_change keys.")
}

for (batch_id in c("01", "05", "09", "13", "17")) {
  file_name <- paste0("amws_ed86_bad_ocr_batch_", batch_id, ".csv")
  input_file <- file.path(root, "in", file_name)
  output_file <- file.path(root, "out", file_name)
  x <- read_csv(input_file, col_types = cols(.default = col_character()),
                show_col_types = FALSE) |>
    mutate(across(everything(), blank_na))

  x <- x |>
    mutate(
      birth_city_new = "",
      birth_state_new = "",
      birth_country_new = "",
      location_inference_basis = "not_inferred",
      location_inference_note = "",
      manual_action = "review_unclear",
      manual_confidence = "low",
      manual_note = paste0(
        "raw_text_adjusted is too corrupted or incomplete to support a ",
        "reliable city/state/country recovery."
      ),
      agent_id = "worker_a"
    )

  local_decisions <- decisions |> filter(pilot_id %in% x$pilot_id)
  idx <- match(local_decisions$pilot_id, x$pilot_id)
  x$birth_city_new[idx] <- local_decisions$birth_city_new
  x$birth_state_new[idx] <- local_decisions$birth_state_new
  x$birth_country_new[idx] <- local_decisions$birth_country_new
  x$manual_action[idx] <- "correct"
  x$manual_confidence[idx] <- local_decisions$manual_confidence
  is_high <- local_decisions$manual_confidence == "high"
  x$location_inference_basis[idx] <- ifelse(is_high, "ocr_explicit", "ocr_fragment")
  x$location_inference_note[idx] <- ifelse(
    is_high,
    "City and location suffix are explicit in raw_text_adjusted.",
    "OCR fragment and location suffix uniquely support the normalized location."
  )
  x$manual_note[idx] <- ifelse(
    is_high,
    "Corrected city/state/country directly from the visible birth-location text.",
    "Normalized an OCR-damaged but uniquely identifiable birth location."
  )

  nc_idx <- which(x$pilot_id %in% no_change_ids)
  x$birth_city_new[nc_idx] <- x$birth_city_old[nc_idx]
  x$birth_state_new[nc_idx] <- x$birth_state_old[nc_idx]
  x$birth_country_new[nc_idx] <- x$birth_country_old[nc_idx]
  x$location_inference_basis[nc_idx] <- "not_inferred"
  x$location_inference_note[nc_idx] <- "Existing location is already correct."
  x$manual_action[nc_idx] <- "no_change"
  x$manual_confidence[nc_idx] <- "high"
  x$manual_note[nc_idx] <- "Legitimate place-name punctuation; no correction needed."

  write_excel_csv(x, output_file, na = "")
  cat(batch_id, paste(names(table(x$manual_action)), table(x$manual_action),
                       collapse = "; "), "\n")
}
