###############################################################################
# Build a harmonized UK historical urban-unit population and inventor panel,
# 1801-1961.
#
# Target geography:
# - 1921 Urban Districts, Municipal Boroughs, and County Boroughs.
# - A synthetic GREATER_LONDON unit replaces the City of London / metropolitan
#   London fragments. It is defined as the union of 1921 Nomis/ONS boundaries
#   with at least 50% overlap with the BBCE/ArcGIS 1911 Greater London parish
#   reconstruction. Those 1921 units are removed from the target set to avoid
#   overlapping target geometries.
#
# Sources:
# - Law-Robson-Bennett settlement population, 1801-1911.
# - Nomis historical census CR03 district population, 1921-1961.
# - Laouan et al. cross-verified Wikipedia people database for inventor/scientist
#   birth outcomes.
#
# Matching rules:
# - Law-Robson settlements are assigned by point-in-polygon only.
# - Nomis districts are assigned to the fixed target geography by polygon
#   intersection.
# - When a Nomis source district overlaps multiple target units, allocate by
#   intersection area weighted by the target unit's 1921 population density.
#
# Run from the repository root:
#   Rscript prep/world_fairs_panel/03_build_uk_historical_urban_unit_population_1801_1961.R
###############################################################################

rm(list = ls()); gc()

suppressPackageStartupMessages({
  library(data.table)
  library(readxl)
  library(sf)
})

sf_use_s2(FALSE)

###############################################################################
# Paths and constants
###############################################################################

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_arg) > 0L) {
  script_path <- normalizePath(
    sub("^--file=", "", script_arg[[1L]]),
    winslash = "/",
    mustWork = TRUE
  )
  repo_root <- normalizePath(
    file.path(dirname(script_path), "..", ".."),
    winslash = "/",
    mustWork = TRUE
  )
} else {
  repo_root <- normalizePath(
    Sys.getenv("GTL_REPO", unset = getwd()),
    winslash = "/",
    mustWork = TRUE
  )
}

source(file.path(repo_root, "paths.R"))
suppressPackageStartupMessages(source(file.path(repo_root, "prep", "stem_labels.R")))

if (!dir.exists(TALENT_DETS_DATA_DIR)) {
  user_data_dir <- file.path(
    "C:/Users", Sys.info()[["user"]], "Globtalent Dropbox", "gtl_talent_dets"
  )
  if (dir.exists(user_data_dir)) {
    TALENT_DETS_DATA_DIR <- normalizePath(
      user_data_dir, winslash = "/", mustWork = TRUE
    )
    DATA_INPUT <- file.path(TALENT_DETS_DATA_DIR, "input")
    DATA_OUTPUT <- file.path(TALENT_DETS_DATA_DIR, "output")
  }
}

gbr_dir <- file.path(DATA_INPUT, "worlds_fairs", "city_census", "GBR")
DATA_PROCESSED <- file.path(TALENT_DETS_DATA_DIR, "Data", "processed")
dir.create(DATA_PROCESSED, recursive = TRUE, showWarnings = FALSE)
nomis_raw_dir <- file.path(gbr_dir, "raw", "nomis_historical_census")
boundary_gpkg <- file.path(
  gbr_dir, "raw", "historical_boundaries",
  "uk_historical_districts_1921_1961.gpkg"
)
lau_gpkg <- file.path(
  TALENT_DETS_DATA_DIR, "raw", "gisco", "lau",
  "LAU_RG_01M_2019_4326.gpkg"
)
greater_london_1911_crosswalk_file <- file.path(
  gbr_dir, "raw", "arcgis_english_admin_boundaries_1911",
  "greater_london_1911_to_nomis_1921_crosswalk.csv"
)
law_panel_file <- file.path(
  gbr_dir, "city_population_law_robson_bennett_1801_1911_geocoded.csv"
)
scientists_file <- file.path(DATA_INPUT, "cross-verified-database.csv")
benchmark_1911_file <- file.path(
  gbr_dir, "nomis_urban_units_1911_1921_benchmark.csv"
)
london_population_sources_file <- file.path(
  TALENT_DETS_DATA_DIR, "output",
  "world_fairs_london_population_sources_1871_1961.csv"
)
london_raw_file <- file.path(gbr_dir, "raw", "population_1801_to_2021.xlsx")

observed_file <- file.path(
  gbr_dir,
  "uk_historical_urban_units_population_census_1801_1961.csv"
)
allocation_file <- file.path(
  gbr_dir,
  "uk_historical_urban_units_population_allocation_audit.csv"
)
manual_harmonization_file <- file.path(
  gbr_dir,
  "uk_historical_urban_units_manual_harmonization_audit.csv"
)
quality_file <- file.path(
  gbr_dir,
  "uk_historical_urban_units_population_quality_summary.csv"
)
transition_file <- file.path(
  gbr_dir,
  "uk_historical_urban_units_population_1911_1921_transition_audit.csv"
)
target_file <- file.path(
  gbr_dir,
  "uk_historical_urban_units_1921_target_units.csv"
)
law_match_audit_file <- file.path(
  gbr_dir,
  "uk_historical_urban_units_law_robson_match_audit.csv"
)
nomis_unmatched_file <- file.path(
  gbr_dir,
  "uk_historical_urban_units_nomis_sources_outside_target_geography.csv"
)
inventor_panel_file <- file.path(
  DATA_PROCESSED,
  "uk_historical_urban_units_inventor_panel_1801_1960_census_population.csv"
)
inventor_qc_file <- file.path(
  DATA_PROCESSED,
  "uk_historical_urban_units_inventor_panel_1801_1960_qc.csv"
)
inventor_unmatched_file <- file.path(
  DATA_OUTPUT,
  "uk_historical_urban_units_inventor_unmatched_people.csv"
)

required_files <- c(
  law_panel_file, scientists_file, benchmark_1911_file, boundary_gpkg,
  lau_gpkg, greater_london_1911_crosswalk_file
)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0L) {
  stop("Missing required files:\n", paste(missing_files, collapse = "\n"))
}

census_years_law <- seq(1801L, 1911L, by = 10L)
census_years_nomis <- c(1921L, 1931L, 1951L, 1961L)
census_years <- c(census_years_law, census_years_nomis)
annual_years <- 1801L:1961L
inventor_panel_years <- 1801L:1960L

target_types <- c("Urban District", "Municipal Borough", "County Borough")
london_origin_types <- c(
  "Metropolitan Borough", "County Corporate", "London County Corporate"
)
origin_types <- c(target_types, london_origin_types)
county_parent_types <- c(
  "Administrative County",
  "Administrative County (excluding County Boroughs)",
  "County"
)
population_columns <- c(
  "1921" = "2c3_0003",
  "1931" = "3c3_0003",
  "1951" = "5c3_0003",
  "1961" = "6c3_0003"
)

london_city_id <- 1491L
greater_london_id <- "GBR_HIST_URBAN_GREATER_LONDON"
greater_london_name <- "Greater London"
greater_london_boundary_source <- paste(
  "Nomis/ONS 1921 districts with >=50% overlap with",
  "BBCE/ArcGIS Greater London 1911 parishes"
)
greater_london_1911_allocation_method <- "nomis_1911_greater_london_50pct_overlap"
greater_london_1911_source_label <- paste(
  "Nomis 1911 benchmark aggregate for 1921 districts with >=50%",
  "overlap with BBCE/ArcGIS Greater London 1911 parishes"
)

manual_harmonization_groups <- data.table(
  group_id = c(
    "newcastle_gateshead", "newcastle_gateshead",
    "manchester_salford", "manchester_salford",
    "liverpool_birkenhead", "liverpool_birkenhead",
    "sale_ashton_upon_mersey", "sale_ashton_upon_mersey",
    "altrincham_bowdon_hale", "altrincham_bowdon_hale",
    "altrincham_bowdon_hale"
  ),
  confidence = c(
    "high", "high",
    "high", "high",
    "high", "high",
    "high", "high",
    "medium", "medium", "medium"
  ),
  primary_target_unit_id = c(
    "GBR_HIST_URBAN_H06201154", "GBR_HIST_URBAN_H06201154",
    "GBR_HIST_URBAN_H06200794", "GBR_HIST_URBAN_H06200794",
    "GBR_HIST_URBAN_H06200793", "GBR_HIST_URBAN_H06200793",
    "GBR_HIST_URBAN_H07200306", "GBR_HIST_URBAN_H07200306",
    "GBR_HIST_URBAN_H07200274", "GBR_HIST_URBAN_H07200274",
    "GBR_HIST_URBAN_H07200274"
  ),
  primary_target_unit_name = c(
    "Newcastle upon Tyne, City and County of",
    "Newcastle upon Tyne, City and County of",
    "Manchester, City of", "Manchester, City of",
    "Liverpool, City of", "Liverpool, City of",
    "Sale", "Sale",
    "Altrincham", "Altrincham", "Altrincham"
  ),
  member_target_unit_id = c(
    "GBR_HIST_URBAN_H06201154", "GBR_HIST_URBAN_H06200512",
    "GBR_HIST_URBAN_H06200794", "GBR_HIST_URBAN_H06200799",
    "GBR_HIST_URBAN_H06200793", "GBR_HIST_URBAN_H06200268",
    "GBR_HIST_URBAN_H07200306", "GBR_HIST_URBAN_H07200275",
    "GBR_HIST_URBAN_H07200274", "GBR_HIST_URBAN_H07200277",
    "GBR_HIST_URBAN_H07200287"
  ),
  member_target_unit_name = c(
    "Newcastle upon Tyne, City and County of", "Gateshead",
    "Manchester, City of", "Salford",
    "Liverpool, City of", "Birkenhead",
    "Sale", "Ashton-upon-Mersey",
    "Altrincham", "Bowdon", "Hale"
  ),
  member_role = c(
    "primary", "component",
    "primary", "component",
    "primary", "component",
    "primary", "component",
    "primary", "component", "component"
  ),
  rationale = c(
    rep("Law-Robson reports Newcastle & Gateshead as a combined settlement; post-1921 Nomis separates the two adjacent urban authorities.", 2L),
    rep("Law-Robson reports Manchester & Salford as a combined settlement; post-1921 Nomis separates the two adjacent urban authorities.", 2L),
    rep("Law-Robson reports Liverpool & Birkenhead as a combined settlement; post-1921 Nomis separates the two adjacent Mersey urban authorities.", 2L),
    rep("Nomis 1911 Sale plus Ashton-upon-Mersey reproduces Law-Robson Sale almost exactly and the places are adjacent.", 2L),
    rep("Altrincham, Bowdon, and Hale are contiguous associated urban places; their combined Nomis 1911 population is materially closer to Law-Robson Altrincham than Altrincham alone.", 3L)
  )
)

###############################################################################
# Helpers
###############################################################################

normalize_text <- function(x) {
  x <- ifelse(is.na(x), "", as.character(x))
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- toupper(trimws(x))
  x <- gsub("&", " AND ", x, fixed = TRUE)
  x <- gsub("[^A-Z0-9]+", " ", x)
  gsub("[[:space:]]+", " ", trimws(x))
}

canonical_name <- function(x) {
  x <- normalize_text(x)
  x <- sub(" COUNTY CORPORATE$", "", x)
  x <- sub("^THE ", "", x)
  x <- sub("^CITY OF ", "", x)
  x <- sub(" CITY AND COUNTY OF$", "", x)
  x <- sub(" CITY AND COUNT OF$", "", x)
  x <- sub(" CITY OF$", "", x)
  x <- sub(" BOROUGH OF$", "", x)
  x <- sub(" URBAN$", "", x)
  x <- sub(" COUNTY OF A TOWN$", "", x)
  x <- sub(" ROYAL BOROUGH$", "", x)
  x <- gsub("\\bST[.]? ", "SAINT ", x)
  aliases <- c(
    "ABER CARN" = "ABERCARN",
    "ABERDAR" = "ABERDARE",
    "BARKING TOWN" = "BARKING",
    "BERWICK UPON TWEED" = "BERWICK ON TWEED",
    "BETWS Y COED" = "BETTWS Y COED",
    "BISHOPS CASTLE" = "BISHOP S CASTLE",
    "BISHOPS STORTFORD" = "BISHOP S STORTFORD",
    "BROADSTAIRS AND SAINT PETERS" = "BROADSTAIRS AND SAINT PETER S",
    "BURRYPORT" = "BURRY PORT",
    "CAERNARFON" = "CARNARVON",
    "CITY OF LONDON" = "LONDON",
    "CONNAHS QUAY" = "CONNAH S QUAY",
    "CRICIETH" = "CRICCIETH",
    "CWM BRAN" = "CWMBRAN",
    "GELLI GAER" = "GELLIGAER",
    "GREASBOROUGH" = "GREASBROUGH",
    "KINGS LYNN" = "KING S LYNN",
    "KINGSTON ON THAMES" = "KINGSTON UPON THAMES",
    "LLANDUDNO CUM EGLWYS RHOS" = "LLANDUDNO",
    "LLANYMDDYFRI" = "LLANDOVERY",
    "MAES TEG" = "MAESTEG",
    "MERTHYR TUDFUL" = "MERTHYR TYDFIL",
    "NANT Y GLO AND BLAINA" = "NANTYGLO AND BLAINA",
    "PENMAEN MAWR" = "PENMAENMAWR",
    "PONT Y PRIDD" = "PONTYPRIDD",
    "PORTH CAWL" = "PORTHCAWL",
    "RHUTHUN" = "RUTHIN",
    "ROSS ON WYE" = "ROSS",
    "SALISBURY OR NEW SARUM" = "SALISBURY",
    "STRATFORD ON AVON" = "STRATFORD UPON AVON",
    "WIGSTON MAGNA" = "WIGSTON",
    "BRECKNOCK" = "BRECON",
    "NEW WINDSOR" = "WINDSOR",
    "ROYAL TUNBRIDGE WELLS" = "TUNBRIDGE WELLS",
    "Y BALA" = "BALA",
    "Y BARRI" = "BARRY"
  )
  hit <- match(x, names(aliases))
  x[!is.na(hit)] <- unname(aliases[hit[!is.na(hit)]])
  x
}

canonical_county <- function(x) {
  x <- normalize_text(x)
  x <- sub("^COUNTY OF ", "", x)
  x <- gsub("[()]", "", x)
  aliases <- c(
    "CAERNARVONSHIRE" = "CARNARVONSHIRE",
    "DEVONSHIRE" = "DEVON",
    "DORSETSHIRE" = "DORSET",
    "GLAMORGANSHIRE" = "GLAMORGAN",
    "RUTLANDSHIRE" = "RUTLAND",
    "SOMERSETSHIRE" = "SOMERSET",
    "SOUTHAMPTON" = "HAMPSHIRE",
    "WESTMORELAND" = "WESTMORLAND"
  )
  hit <- match(x, names(aliases))
  x[!is.na(hit)] <- unname(aliases[hit[!is.na(hit)]])
  x
}

normalized_edit_distance <- function(a, b) {
  denominator <- pmax(nchar(a), nchar(b), 1L)
  as.numeric(adist(a, b)) / denominator
}

derive_1951_id <- function(metadata_id, area_type) {
  metadata_id <- as.character(metadata_id)
  prefix <- fcase(
    area_type == "Civil Parish", "H04",
    area_type %chin% c("Ward", "Ward/Parish intersection"), "H05",
    area_type == "County Borough", "H06",
    area_type %chin% c(
      "Metropolitan Borough", "Municipal Borough", "Urban District",
      "Rural District", "County Corporate"
    ), "H07",
    area_type == "District/New Town intersection", "H24",
    area_type == "Newtown/Parish intersection", "H29",
    area_type == "Newtown/Ward/Parish intersection", "H30",
    default = NA_character_
  )
  paste0(prefix, substr(metadata_id, 1L, 1L), substr(metadata_id, 3L, nchar(metadata_id)))
}

safe_ratio <- function(numerator, denominator) {
  fifelse(is.na(denominator) | denominator == 0, NA_real_, numerator / denominator)
}

first_nonmissing_char <- function(x) {
  x <- x[!is.na(x) & nzchar(x)]
  if (length(x) == 0L) NA_character_ else x[[1L]]
}

interp_no_extrapolate <- function(year, population, years_out) {
  ok <- !is.na(year) & !is.na(population)
  year <- as.integer(year[ok])
  population <- as.numeric(population[ok])
  if (length(unique(year)) < 2L) {
    return(rep(NA_real_, length(years_out)))
  }
  approx(
    x = year,
    y = population,
    xout = years_out,
    method = "linear",
    rule = 1,
    ties = sum
  )$y
}

validate_manual_harmonization_groups <- function(groups, target_ids) {
  missing_members <- setdiff(groups$member_target_unit_id, target_ids)
  missing_primaries <- setdiff(groups$primary_target_unit_id, target_ids)
  if (length(missing_members) > 0L || length(missing_primaries) > 0L) {
    stop(
      "Manual harmonization contains target IDs not present in target geography:\n",
      paste(unique(c(missing_members, missing_primaries)), collapse = "\n")
    )
  }
  bad_primary_count <- groups[, .(
    n_primaries = uniqueN(primary_target_unit_id),
    n_primary_rows = sum(member_role == "primary")
  ), by = group_id][n_primaries != 1L | n_primary_rows != 1L]
  if (nrow(bad_primary_count) > 0L) {
    stop(
      "Manual harmonization groups must have exactly one primary row:\n",
      paste(bad_primary_count$group_id, collapse = "\n")
    )
  }
  primary_not_member <- groups[, .(
    primary_in_members = unique(primary_target_unit_id) %chin% member_target_unit_id
  ), by = group_id][primary_in_members == FALSE]
  if (nrow(primary_not_member) > 0L) {
    stop(
      "Manual harmonization primary units must also be listed as members:\n",
      paste(primary_not_member$group_id, collapse = "\n")
    )
  }
  invisible(TRUE)
}

collapse_unique <- function(x, sep = " | ") {
  x <- sort(unique(x[!is.na(x) & nzchar(as.character(x))]))
  if (length(x) == 0L) NA_character_ else paste(x, collapse = sep)
}

append_unique_label <- function(x, label, sep = " | ") {
  x <- collapse_unique(c(x, label), sep = sep)
  if (is.na(x)) label else x
}

apply_manual_population_harmonization <- function(observed, groups, law_years) {
  out <- copy(observed)
  original <- copy(observed)
  audit_rows <- vector("list", length(unique(groups$group_id)) * length(unique(out$census_year)))
  audit_i <- 0L

  for (gid in unique(groups$group_id)) {
    group <- groups[group_id == gid]
    primary_id <- unique(group$primary_target_unit_id)
    confidence <- unique(group$confidence)
    rationale <- unique(group$rationale)
    member_ids <- group$member_target_unit_id
    component_ids <- group[member_role == "component", member_target_unit_id]

    for (yr in sort(unique(out$census_year))) {
      member_original <- original[target_unit_id %chin% member_ids & census_year == yr]
      primary_original <- original[target_unit_id == primary_id & census_year == yr]

      if (yr %in% law_years) {
        new_population <- primary_original$population_observed[[1L]]
        source_members <- primary_id
        action_primary <- "primary_kept_law_robson_composite"
      } else {
        nonmissing_members <- member_original[!is.na(population_observed)]
        if (nrow(nonmissing_members) > 0L) {
          new_population <- sum(nonmissing_members$population_observed, na.rm = TRUE)
          source_members <- nonmissing_members$target_unit_id
        } else {
          new_population <- NA_real_
          source_members <- character()
        }
        action_primary <- "primary_manual_harmonized"
      }

      source_obs <- member_original[target_unit_id %chin% source_members]
      if (nrow(source_obs) == 0L) {
        source_obs <- primary_original
      }

      out[target_unit_id == primary_id & census_year == yr, `:=`(
        population = new_population,
        population_observed = new_population,
        population_available = !is.na(new_population),
        population_quality = paste0(
          "manual_harmonized_population_", confidence, "_confidence"
        ),
        n_source_units = if (nrow(source_obs) == 0L) NA_integer_ else sum(
          source_obs$n_source_units, na.rm = TRUE
        ),
        n_source_allocations = if (nrow(source_obs) == 0L) NA_integer_ else sum(
          source_obs$n_source_allocations, na.rm = TRUE
        ),
        share_population_density_weighted = NA_real_,
        allocation_method = append_unique_label(
          source_obs$allocation_method,
          paste0("manual_harmonization_", confidence, "_confidence")
        ),
        population_source = collapse_unique(source_obs$population_source),
        any_match_needs_review = any(source_obs$any_match_needs_review, na.rm = TRUE)
      )]

      if (length(component_ids) > 0L) {
        out[
          target_unit_id %chin% component_ids & census_year == yr,
          `:=`(
            population = NA_real_,
            population_observed = NA_real_,
            population_available = FALSE,
            population_quality = paste0(
              "manual_harmonization_component_merged_into_", primary_id
            ),
            n_source_units = NA_integer_,
            n_source_allocations = NA_integer_,
            share_population_density_weighted = NA_real_,
            allocation_method = paste0(
              "manual_harmonization_component_merged_into_", primary_id
            ),
            population_source = NA_character_,
            any_match_needs_review = FALSE
          )
        ]
      }

      member_adjusted <- out[target_unit_id %chin% member_ids & census_year == yr]
      audit_i <- audit_i + 1L
      audit_rows[[audit_i]] <- merge(
        group,
        member_original[, .(
          member_target_unit_id = target_unit_id,
          census_year,
          original_population = population_observed,
          original_population_quality = population_quality,
          original_population_source = population_source
        )],
        by = "member_target_unit_id",
        all.x = TRUE,
        sort = FALSE
      )[
        member_adjusted[, .(
          member_target_unit_id = target_unit_id,
          adjusted_population = population_observed,
          adjusted_population_quality = population_quality,
          adjusted_population_source = population_source
        )],
        on = "member_target_unit_id"
      ][, `:=`(
        census_year = yr,
        action = fifelse(member_role == "primary", action_primary, "component_suppressed"),
        source_member_ids = paste(source_members, collapse = ";")
      )]
    }
  }

  list(
    observed = out,
    audit = rbindlist(audit_rows[seq_len(audit_i)], use.names = TRUE, fill = TRUE)
  )
}

apply_manual_inventor_harmonization <- function(inventors, groups) {
  member_map <- groups[, .(
    member_target_unit_id,
    primary_target_unit_id
  )]
  out <- merge(
    copy(inventors),
    member_map,
    by.x = "target_unit_id",
    by.y = "member_target_unit_id",
    all.x = TRUE,
    sort = FALSE
  )
  out[!is.na(primary_target_unit_id), target_unit_id := primary_target_unit_id]
  out[, primary_target_unit_id := NULL]
  out[, .(
    n_inventors = sum(n_inventors, na.rm = TRUE),
    n_stem = sum(n_stem, na.rm = TRUE),
    n_nonstem = sum(n_nonstem, na.rm = TRUE)
  ), by = .(target_unit_id, year)]
}

read_greater_london_1911 <- function() {
  if (file.exists(london_population_sources_file)) {
    x <- fread(london_population_sources_file)
    hit <- x[
      year == 1911L &
        source_id == "census" &
        grepl("Greater London", source, fixed = TRUE)
    ]
    if (nrow(hit) > 0L && is.finite(hit$population[[1L]])) {
      return(as.numeric(hit$population[[1L]]))
    }
  }
  if (file.exists(london_raw_file)) {
    raw <- as.data.table(read_excel(london_raw_file, sheet = "data"))
    if ("area" %chin% names(raw) && "1911" %chin% names(raw)) {
      hit <- raw[area == "Greater London"]
      if (nrow(hit) > 0L) {
        return(as.numeric(hit[["1911"]][[1L]]))
      }
    }
  }
  7162000
}

read_nomis_year <- function(year) {
  year_dir <- file.path(nomis_raw_dir, as.character(year))
  metadata_file <- file.path(year_dir, sprintf("%s_metadata.xlsx", year))
  values_file <- list.files(
    file.path(year_dir, "extracted"),
    pattern = sprintf("%s_cr03_values[.]csv$", year),
    recursive = TRUE,
    full.names = TRUE
  )
  values_file <- values_file[!grepl("__MACOSX", values_file, fixed = TRUE)]
  if (length(values_file) != 1L || !file.exists(metadata_file)) {
    stop("Missing Nomis values or metadata for ", year)
  }

  values <- fread(values_file, na.strings = c("", "NA", ".."))
  pop_column <- unname(population_columns[as.character(year)])
  if (!pop_column %chin% names(values)) {
    stop("Missing population column for ", year, ": ", pop_column)
  }
  values_small <- values[, .(
    value_id = as.character(area_id),
    value_type = as.character(area_type),
    population = suppressWarnings(as.numeric(get(pop_column))),
    population_1911_comparison = if (year == 1921L && "2c3_0002" %chin% names(values)) {
      suppressWarnings(as.numeric(get("2c3_0002")))
    } else {
      NA_real_
    }
  )]

  areas <- as.data.table(read_excel(
    metadata_file,
    sheet = sprintf("%s_areas", year),
    col_types = "text"
  ))
  relationships <- as.data.table(read_excel(
    metadata_file,
    sheet = sprintf("%s_area_relationships", year),
    col_types = "text"
  ))

  areas[, metadata_id := as.character(area_id)]
  areas[, value_id := if (year == 1951L) {
    derive_1951_id(metadata_id, area_type)
  } else {
    metadata_id
  }]
  areas <- merge(areas, values_small, by = "value_id", all.x = TRUE, sort = FALSE)

  parents <- relationships[
    area_type_1 %chin% county_parent_types & area_type_2 %chin% origin_types,
    .(
      metadata_id = as.character(area_id_2),
      source_county = as.character(area_1),
      source_county_type = as.character(area_type_1)
    )
  ]
  parents[, priority := match(source_county_type, county_parent_types)]
  setorder(parents, metadata_id, priority, source_county)
  parents <- parents[, .SD[1L], by = metadata_id]

  districts <- merge(
    areas[area_type %chin% origin_types, .(
      source_area_id = value_id,
      metadata_id,
      source_area_name = as.character(area),
      source_area_type = as.character(area_type),
      population,
      population_1911_comparison
    )],
    parents[, .(metadata_id, source_county, source_county_type)],
    by = "metadata_id",
    all.x = TRUE,
    sort = FALSE
  )
  districts <- districts[!is.na(population)]
  districts[, `:=`(
    census_year = year,
    source_name_canonical = canonical_name(source_area_name),
    source_county_canonical = canonical_county(source_county)
  )]
  districts
}

###############################################################################
# Target geography
###############################################################################

cat("Building fixed 1921 target geography...\n")

districts_1921 <- st_read(boundary_gpkg, layer = "districts_1921", quiet = TRUE)
districts_1921 <- st_make_valid(st_transform(districts_1921, 27700))

# GISCO LAUs are only used later to keep UK birth points before assigning them
# to the historical target polygons. They no longer define Greater London.
lau <- st_read(lau_gpkg, quiet = TRUE)
lau <- st_make_valid(st_transform(lau, 27700))

benchmark_1911 <- fread(benchmark_1911_file)
setnames(
  benchmark_1911,
  old = c(
    "source_area_id", "source_area_name", "source_area_type",
    "nomis_population_1911", "nomis_population_1921"
  ),
  new = c(
    "target_boundary_id", "benchmark_area_name", "benchmark_area_type",
    "target_population_1911", "target_population_1921"
  )
)

greater_london_1911_crosswalk <- fread(greater_london_1911_crosswalk_file)
required_london_crosswalk_cols <- c(
  "nomis_1921_id", "in_greater_london_1911_main"
)
if (!all(required_london_crosswalk_cols %chin% names(greater_london_1911_crosswalk))) {
  stop(
    "Greater London 1911 crosswalk is missing required columns: ",
    paste(
      setdiff(required_london_crosswalk_cols, names(greater_london_1911_crosswalk)),
      collapse = ", "
    )
  )
}
greater_london_1921_ids <- greater_london_1911_crosswalk[
  in_greater_london_1911_main == TRUE,
  unique(as.character(nomis_1921_id))
]
if (!length(greater_london_1921_ids)) {
  stop("No 1921 Nomis/ONS boundaries selected for Greater London.")
}
missing_london_boundaries <- setdiff(
  greater_london_1921_ids,
  as.character(districts_1921$boundary_id)
)
if (length(missing_london_boundaries)) {
  stop(
    "Greater London crosswalk boundaries missing from districts_1921: ",
    paste(missing_london_boundaries, collapse = ", ")
  )
}

greater_london_1921_boundaries <- districts_1921[
  as.character(districts_1921$boundary_id) %chin% greater_london_1921_ids,
]
greater_london_1911_population <- benchmark_1911[
  target_boundary_id %chin% greater_london_1921_ids,
  sum(target_population_1911, na.rm = TRUE)
]
greater_london_1921_population <- benchmark_1911[
  target_boundary_id %chin% greater_london_1921_ids,
  sum(target_population_1921, na.rm = TRUE)
]
if (!is.finite(greater_london_1911_population) ||
    greater_london_1911_population <= 0) {
  stop("Could not compute positive Nomis 1911 population for Greater London.")
}
if (!is.finite(greater_london_1921_population) ||
    greater_london_1921_population <= 0) {
  stop("Could not compute positive Nomis 1921 population for Greater London.")
}
missing_london_population_ids <- setdiff(
  greater_london_1921_ids,
  benchmark_1911[
    !is.na(target_population_1911) & !is.na(target_population_1921),
    target_boundary_id
  ]
)
if (length(missing_london_population_ids)) {
  stop(
    "Greater London selected boundaries missing Nomis 1911/1921 benchmark population: ",
    paste(missing_london_population_ids, collapse = ", ")
  )
}

greater_london_geom <- st_union(greater_london_1921_boundaries)
greater_london_sf <- st_sf(
  target_unit_id = greater_london_id,
  target_unit_name = greater_london_name,
  target_area_type = "Greater London",
  target_boundary_id = greater_london_id,
  target_boundary_source = greater_london_boundary_source,
  geometry = st_sfc(greater_london_geom, crs = 27700)
)

base_targets <- districts_1921[districts_1921$boundary_type %in% target_types, ]
london_overlap_rows <- which(
  as.character(base_targets$boundary_id) %chin% greater_london_1921_ids
)
if (length(london_overlap_rows)) {
  base_targets <- base_targets[-london_overlap_rows, ]
}

base_targets_sf <- st_sf(
  target_unit_id = paste0("GBR_HIST_URBAN_", base_targets$boundary_id),
  target_unit_name = base_targets$boundary_name,
  target_area_type = base_targets$boundary_type,
  target_boundary_id = base_targets$boundary_id,
  target_boundary_source = base_targets$boundary_source,
  geometry = st_geometry(base_targets)
)

targets_sf <- rbind(base_targets_sf, greater_london_sf)
targets_sf <- st_make_valid(targets_sf)
targets_sf$target_area_m2 <- as.numeric(st_area(targets_sf))

target_dt <- as.data.table(st_drop_geometry(targets_sf))
target_dt[, target_area_sqkm := target_area_m2 / 1e6]
target_dt[, target_centroid_x := st_coordinates(st_point_on_surface(targets_sf))[, 1]]
target_dt[, target_centroid_y := st_coordinates(st_point_on_surface(targets_sf))[, 2]]
centroids_ll <- st_transform(st_point_on_surface(targets_sf), 4326)
target_dt[, longitude := st_coordinates(centroids_ll)[, 1]]
target_dt[, latitude := st_coordinates(centroids_ll)[, 2]]

target_dt <- merge(
  target_dt,
  benchmark_1911[, .(
    target_boundary_id,
    benchmark_area_name,
    benchmark_area_type,
    target_population_1911,
    target_population_1921
  )],
  by = "target_boundary_id",
  all.x = TRUE,
  sort = FALSE
)
target_dt[
  target_unit_id == greater_london_id,
  `:=`(
    benchmark_area_name = greater_london_name,
    benchmark_area_type = "Nomis 1911 >=50% overlap aggregate",
    target_population_1911 = greater_london_1911_population,
    target_population_1921 = greater_london_1921_population
  )
]
if (target_dt[is.na(target_population_1911) | is.na(target_population_1921), .N] > 0L) {
  missing_targets <- target_dt[
    is.na(target_population_1911) | is.na(target_population_1921),
    target_unit_name
  ]
  stop(
    "Target units missing 1911/1921 density population:\n",
    paste(missing_targets, collapse = "\n")
  )
}
target_dt[, target_density_1911_per_m2 := target_population_1911 / target_area_m2]
target_dt[, target_density_1921_per_m2 := target_population_1921 / target_area_m2]

targets_sf <- merge(
  targets_sf,
  target_dt[, .(
    target_unit_id,
    target_area_sqkm,
    longitude,
    latitude,
    target_population_1911,
    target_population_1921,
    target_density_1911_per_m2,
    target_density_1921_per_m2
  )],
  by = "target_unit_id",
  all.x = TRUE,
  sort = FALSE
)
validate_manual_harmonization_groups(
  manual_harmonization_groups,
  target_dt$target_unit_id
)

cat("Target units: ", nrow(target_dt), "\n", sep = "")
cat("1921 target units removed because of Greater London >=50% overlap: ",
    length(london_overlap_rows), "\n", sep = "")
cat("Greater London Nomis 1911 population: ",
    greater_london_1911_population, "\n", sep = "")
cat("Greater London Nomis 1921 population: ",
    greater_london_1921_population, "\n", sep = "")

###############################################################################
# Law-Robson allocation
###############################################################################

cat("Assigning Law-Robson settlements to target units...\n")

law <- fread(law_panel_file)
law <- law[
  census_year %in% census_years_law &
    population_available == TRUE &
    !is.na(population) &
    !is.na(easting) &
    !is.na(northing)
]
law[, law_row_id := .I]
law_points <- st_as_sf(
  law,
  coords = c("easting", "northing"),
  crs = 27700,
  remove = FALSE
)

law_target_idx <- st_intersects(law_points, targets_sf, sparse = TRUE)
law_candidates <- rbindlist(lapply(seq_along(law_target_idx), function(i) {
  hits <- law_target_idx[[i]]
  if (length(hits) == 0L) {
    return(data.table(law_row_id = i, target_sf_row = NA_integer_))
  }
  data.table(law_row_id = i, target_sf_row = hits)
}))
law_candidates[, spatial_candidate_count := .N, by = law_row_id]

target_lookup <- target_dt[, .(
  target_sf_row = .I,
  target_unit_id,
  target_unit_name,
  target_area_type,
  target_boundary_id,
  target_density_1911_per_m2
)]
law_candidates <- target_lookup[law_candidates, on = "target_sf_row"]
setorder(
  law_candidates,
  law_row_id,
  -target_density_1911_per_m2,
  target_unit_name
)
law_selected <- law_candidates[, .SD[1L], by = law_row_id]
law_assigned <- merge(
  law_selected,
  law,
  by = "law_row_id",
  all.y = TRUE,
  sort = FALSE
)
law_assigned[, original_target_unit_id := target_unit_id]

law_assigned[
  city_id == london_city_id,
  `:=`(
    target_unit_id = greater_london_id,
    target_unit_name = greater_london_name,
    target_area_type = "Greater London",
    target_boundary_id = greater_london_id,
    spatial_candidate_count = 1L
  )
]
law_assigned[
  city_id != london_city_id & target_unit_id == greater_london_id,
  `:=`(
    target_unit_id = NA_character_,
    target_unit_name = NA_character_,
    target_area_type = NA_character_,
    target_boundary_id = NA_character_
  )
]

law_assigned[, allocation_status := fcase(
  city_id == london_city_id, "assigned_london_to_greater_london",
  city_id != london_city_id & original_target_unit_id == greater_london_id,
  "excluded_non_london_point_inside_greater_london",
  is.na(target_unit_id), "unmatched_no_target_intersection",
  spatial_candidate_count > 1L, "assigned_ambiguous_spatial_candidate",
  default = "assigned_point_in_target"
)]

law_alloc <- law_assigned[!is.na(target_unit_id), .(
  source = "Law-Robson-Bennett Urban Population Database",
  census_year,
  source_area_id = as.character(city_id),
  source_area_name = town_name,
  source_area_type = "Law-Robson settlement",
  source_population = as.numeric(population),
  source_population_1911_comparison = NA_real_,
  source_longitude = longitude,
  source_latitude = latitude,
  source_easting = easting,
  source_northing = northing,
  target_unit_id,
  target_unit_name,
  target_area_type,
  target_boundary_id,
  allocation_method = fcase(
    city_id == london_city_id, "law_london_to_greater_london",
    spatial_candidate_count > 1L, "law_point_intersects_multiple_targets_density_selected",
    default = "law_point_in_target"
  ),
  intersection_area_m2 = NA_real_,
  source_area_m2 = NA_real_,
  raw_area_share = 1,
  density_weight = NA_real_,
  allocation_share = 1,
  allocated_population = as.numeric(population),
  match_needs_review = spatial_candidate_count > 1L
)]
law_alloc <- law_alloc[!(
  census_year == 1911L &
    source_area_id == as.character(london_city_id) &
    target_unit_id == greater_london_id
)]

greater_london_1911_alloc <- data.table(
  source = greater_london_1911_source_label,
  census_year = 1911L,
  source_area_id = greater_london_id,
  source_area_name = greater_london_name,
  source_area_type = "Nomis 1911 benchmark aggregate",
  source_population = as.numeric(greater_london_1911_population),
  source_population_1911_comparison = as.numeric(greater_london_1911_population),
  source_longitude = NA_real_,
  source_latitude = NA_real_,
  source_easting = NA_real_,
  source_northing = NA_real_,
  target_unit_id = greater_london_id,
  target_unit_name = greater_london_name,
  target_area_type = "Greater London",
  target_boundary_id = greater_london_id,
  allocation_method = greater_london_1911_allocation_method,
  intersection_area_m2 = as.numeric(st_area(greater_london_sf)),
  source_area_m2 = as.numeric(st_area(greater_london_sf)),
  raw_area_share = 1,
  density_weight = NA_real_,
  allocation_share = 1,
  allocated_population = as.numeric(greater_london_1911_population),
  match_needs_review = FALSE
)

law_match_audit <- law_assigned[, .(
  census_year,
  city_id,
  town_name,
  standard_name,
  historic_county,
  population,
  longitude,
  latitude,
  easting,
  northing,
  target_unit_id,
  target_unit_name,
  target_area_type,
  original_target_unit_id,
  allocation_status,
  spatial_candidate_count
)]

###############################################################################
# Nomis population and polygon links
###############################################################################

cat("Reading Nomis CR03 population tables...\n")
nomis <- rbindlist(lapply(census_years_nomis, read_nomis_year), fill = TRUE)
expected_nomis <- data.table(
  census_year = census_years_nomis,
  expected = c(1154L, 1147L, 993L, 993L)
)
nomis_counts <- merge(nomis[, .N, by = census_year], expected_nomis, by = "census_year")
if (nomis_counts[, any(N != expected)]) {
  stop("Unexpected Nomis district count:\n", paste(capture.output(print(nomis_counts)), collapse = "\n"))
}

cat("Linking Nomis districts to historical polygons...\n")
boundary_list <- setNames(lapply(census_years_nomis, function(year) {
  x <- st_read(boundary_gpkg, layer = paste0("districts_", year), quiet = TRUE)
  x <- st_make_valid(st_transform(x, 27700))
  x[x$boundary_type %in% origin_types, ]
}), as.character(census_years_nomis))

direct_links <- rbindlist(lapply(c(1921L, 1961L), function(year) {
  b <- as.data.table(st_drop_geometry(boundary_list[[as.character(year)]]))
  n <- nomis[census_year == year]
  b[, boundary_id_lookup := boundary_id]
  out <- merge(
    n,
    b[, .(
      boundary_id_lookup,
      boundary_id,
      boundary_name,
      boundary_type,
      boundary_source
    )],
    by.x = "source_area_id",
    by.y = "boundary_id_lookup",
    all.x = TRUE,
    sort = FALSE
  )
  if (out[is.na(boundary_name), .N] > 0L) {
    stop("Official Nomis-to-boundary links missing for ", year)
  }
  out[, `:=`(
    boundary_link_method = "official_boundary_code",
    boundary_name_distance = 0,
    boundary_type_agrees = source_area_type == boundary_type,
    boundary_link_needs_review = FALSE
  )]
  out
}), fill = TRUE)

anchor_sf <- lapply(c(1921L, 1961L), function(year) {
  links <- direct_links[census_year == year, .(
    boundary_id,
    anchor_county = source_county_canonical
  )]
  x <- merge(
    boundary_list[[as.character(year)]],
    links,
    by = "boundary_id",
    all.x = TRUE,
    sort = FALSE
  )
  x[, c("boundary_id", "anchor_county")]
})
names(anchor_sf) <- c("1921", "1961")

link_historical_year <- function(year) {
  boundaries <- boundary_list[[as.character(year)]]
  boundary_points <- suppressWarnings(st_point_on_surface(boundaries))
  anchor_1921 <- st_join(
    boundary_points,
    anchor_sf[["1921"]],
    join = st_within,
    left = TRUE,
    largest = TRUE
  )$anchor_county
  anchor_1961 <- st_join(
    boundary_points,
    anchor_sf[["1961"]],
    join = st_within,
    left = TRUE,
    largest = TRUE
  )$anchor_county

  b <- as.data.table(st_drop_geometry(boundaries))
  b[, `:=`(
    boundary_row = .I,
    boundary_name_canonical = canonical_name(boundary_name),
    anchor_county_1921 = anchor_1921,
    anchor_county_1961 = anchor_1961
  )]
  n <- copy(nomis[census_year == year])
  n[, source_row := .I]

  candidates <- CJ(source_row = n$source_row, boundary_row = b$boundary_row)
  candidates <- merge(
    candidates,
    n[, .(
      source_row,
      source_area_name,
      source_area_type,
      source_name_canonical,
      source_county_canonical
    )],
    by = "source_row"
  )
  candidates <- merge(
    candidates,
    b[, .(
      boundary_row,
      boundary_name,
      boundary_type,
      boundary_name_canonical,
      anchor_county_1921,
      anchor_county_1961
    )],
    by = "boundary_row"
  )
  candidates[, name_distance := mapply(
    normalized_edit_distance,
    source_name_canonical,
    boundary_name_canonical
  )]
  candidates[, type_penalty := fifelse(source_area_type == boundary_type, 0, 0.18)]
  candidates[, county_agrees :=
    nzchar(source_county_canonical) &
      source_county_canonical %chin% c(anchor_county_1921, anchor_county_1961),
  by = .(source_row, boundary_row)]
  candidates[, county_penalty := fifelse(county_agrees, 0, 0.08)]
  candidates[, score := name_distance + type_penalty + county_penalty]

  setorder(candidates, score, name_distance, type_penalty, boundary_row)
  used_source <- logical(nrow(n))
  used_boundary <- logical(nrow(b))
  chosen <- vector("list", nrow(n))
  chosen_n <- 0L
  for (i in seq_len(nrow(candidates))) {
    s <- candidates$source_row[[i]]
    p <- candidates$boundary_row[[i]]
    if (!used_source[[s]] && !used_boundary[[p]]) {
      chosen_n <- chosen_n + 1L
      chosen[[chosen_n]] <- candidates[i]
      used_source[[s]] <- TRUE
      used_boundary[[p]] <- TRUE
      if (all(used_source)) break
    }
  }
  chosen <- rbindlist(chosen[seq_len(chosen_n)])
  if (nrow(chosen) != nrow(n)) {
    stop("Failed one-to-one Nomis boundary assignment for ", year)
  }

  links <- merge(
    n,
    chosen[, .(
      source_row,
      boundary_row,
      boundary_name_distance = name_distance,
      boundary_type_agrees = type_penalty == 0,
      county_agrees,
      boundary_score = score
    )],
    by = "source_row",
    all.x = TRUE,
    sort = FALSE
  )
  links <- merge(
    links,
    b[, .(
      boundary_row,
      boundary_id,
      boundary_name,
      boundary_type,
      boundary_source,
      anchor_county_1921,
      anchor_county_1961
    )],
    by = "boundary_row",
    all.x = TRUE,
    sort = FALSE
  )
  links[, `:=`(
    boundary_link_method = "name_type_county_spatial_assignment",
    boundary_link_needs_review =
      boundary_name_distance > 0.25 | boundary_score > 0.35
  )]
  links[, c("source_row", "boundary_row") := NULL]
  links
}

historical_links <- rbindlist(lapply(c(1931L, 1951L), link_historical_year), fill = TRUE)
nomis_links <- rbindlist(list(direct_links, historical_links), fill = TRUE)
if (nrow(nomis_links) != nrow(nomis) ||
    nomis_links[, anyDuplicated(paste(census_year, source_area_id))] > 0L ||
    nomis_links[, anyDuplicated(paste(census_year, boundary_id))] > 0L) {
  stop("Nomis-to-boundary crosswalk is not one-to-one.")
}

###############################################################################
# Nomis allocation to fixed historical urban units
###############################################################################

cat("Allocating Nomis districts to fixed target geography...\n")

nomis_alloc_list <- list()
nomis_unmatched_list <- list()
for (year in census_years_nomis) {
  cat("  ", year, "\n", sep = "")
  links <- nomis_links[census_year == year]
  link_payload <- links[, setdiff(
    names(links),
    c("boundary_name", "boundary_type", "boundary_source")
  ), with = FALSE]
  polygons <- merge(
    boundary_list[[as.character(year)]],
    link_payload,
    by = "boundary_id",
    all.x = FALSE,
    sort = FALSE
  )
  polygons$source_area_m2 <- as.numeric(st_area(polygons))

  intersections <- suppressWarnings(st_intersection(
    polygons[, c(
      "boundary_id", "boundary_name", "boundary_type", "boundary_source",
      "source_area_id", "source_area_name", "source_area_type",
      "source_county", "source_county_type", "population",
      "population_1911_comparison", "boundary_link_method",
      "boundary_name_distance", "boundary_type_agrees",
      "boundary_link_needs_review", "source_area_m2"
    )],
    targets_sf[, c(
      "target_unit_id", "target_unit_name", "target_area_type",
      "target_boundary_id", "target_population_1911",
      "target_population_1921", "target_density_1911_per_m2",
      "target_density_1921_per_m2"
    )]
  ))
  if (nrow(intersections) == 0L) {
    stop("No Nomis intersections for ", year)
  }
  intersections$intersection_area_m2 <- as.numeric(st_area(intersections))
  allocation <- as.data.table(st_drop_geometry(intersections))
  allocation <- allocation[intersection_area_m2 > 1]
  allocation[, raw_area_share := intersection_area_m2 / source_area_m2]
  allocation[, density_weight :=
    intersection_area_m2 * target_density_1921_per_m2]
  allocation[
    !is.finite(density_weight) | density_weight < 0,
    density_weight := NA_real_
  ]
  allocation[, total_density_weight := sum(density_weight, na.rm = TRUE),
             by = source_area_id]
  allocation[, total_intersection_area := sum(intersection_area_m2, na.rm = TRUE),
             by = source_area_id]
  allocation[, allocation_share := fifelse(
    total_density_weight > 0,
    density_weight / total_density_weight,
    intersection_area_m2 / total_intersection_area
  )]
  allocation[, allocated_population := population * allocation_share]
  allocation[, `:=`(
    census_year = year,
    source = "Nomis historical census CR03",
    source_population = as.numeric(population),
    source_population_1911_comparison = population_1911_comparison,
    allocation_method = fifelse(
      total_density_weight > 0,
      "nomis_polygon_intersection_1921_density_weighted",
      "nomis_polygon_intersection_area_weighted_fallback"
    ),
    match_needs_review = boundary_link_needs_review |
      raw_area_share < 0.98 |
      .N > 1L
  ), by = source_area_id]
  allocation[, c(
    "population", "population_1911_comparison", "total_density_weight",
    "total_intersection_area"
  ) := NULL]

  matched_sources <- unique(allocation$source_area_id)
  missing_sources <- links[!source_area_id %chin% matched_sources, .(
    census_year,
    source_area_id,
    source_area_name,
    source_area_type,
    population,
    boundary_id,
    boundary_name,
    boundary_type,
    boundary_link_method
  )]
  if (nrow(missing_sources) > 0L) {
    missing_sources[, missing_reason := "source_polygon_did_not_intersect_target_geography"]
  }

  conservation <- allocation[, .(
    allocated = sum(allocated_population),
    source = first(source_population)
  ), by = source_area_id]
  if (conservation[, any(abs(allocated - source) > 1e-6 * pmax(source, 1))]) {
    stop("Nomis allocation does not conserve source populations for ", year)
  }

  nomis_alloc_list[[as.character(year)]] <- allocation
  nomis_unmatched_list[[as.character(year)]] <- missing_sources
}
nomis_alloc <- rbindlist(nomis_alloc_list, fill = TRUE)
nomis_unmatched <- rbindlist(nomis_unmatched_list, fill = TRUE)

###############################################################################
# Final panels and audits
###############################################################################

cat("Building final observed panel and audits...\n")

allocation_columns <- c(
  "source", "census_year", "source_area_id", "source_area_name",
  "source_area_type", "source_population", "source_population_1911_comparison",
  "source_county", "source_county_type", "source_longitude", "source_latitude",
  "source_easting", "source_northing", "boundary_id", "boundary_name",
  "boundary_type", "boundary_source", "boundary_link_method",
  "boundary_name_distance", "boundary_type_agrees",
  "boundary_link_needs_review", "target_unit_id", "target_unit_name",
  "target_area_type", "target_boundary_id", "allocation_method",
  "intersection_area_m2", "source_area_m2", "raw_area_share", "density_weight",
  "allocation_share", "allocated_population", "match_needs_review"
)
for (column in setdiff(allocation_columns, names(law_alloc))) {
  law_alloc[, (column) := NA]
}
for (column in setdiff(allocation_columns, names(greater_london_1911_alloc))) {
  greater_london_1911_alloc[, (column) := NA]
}
for (column in setdiff(allocation_columns, names(nomis_alloc))) {
  nomis_alloc[, (column) := NA]
}
allocation_audit <- rbindlist(list(
  law_alloc[, ..allocation_columns],
  greater_london_1911_alloc[, ..allocation_columns],
  nomis_alloc[, ..allocation_columns]
), use.names = TRUE, fill = TRUE)
setorder(allocation_audit, census_year, source, target_unit_id, source_area_id)

observed_alloc <- allocation_audit[!is.na(allocated_population)]
observed_agg <- observed_alloc[, .(
  population_observed = sum(allocated_population),
  n_source_units = uniqueN(source_area_id),
  n_source_allocations = .N,
  share_population_density_weighted = sum(
    allocated_population[
      allocation_method == "nomis_polygon_intersection_1921_density_weighted"
    ],
    na.rm = TRUE
  ) / sum(allocated_population),
  allocation_method = paste(sort(unique(allocation_method)), collapse = " | "),
  population_source = paste(sort(unique(source)), collapse = " | "),
  any_match_needs_review = any(match_needs_review, na.rm = TRUE)
), by = .(target_unit_id, census_year)]

grid <- CJ(
  target_unit_id = target_dt$target_unit_id,
  census_year = census_years
)
observed <- merge(grid, target_dt, by = "target_unit_id", all.x = TRUE, sort = FALSE)
observed <- merge(
  observed,
  observed_agg,
  by = c("target_unit_id", "census_year"),
  all.x = TRUE,
  sort = FALSE
)
observed[, `:=`(
  population = population_observed,
  population_available = !is.na(population_observed),
  source_panel = "uk_historical_urban_units_population_census_1801_1961",
  population_quality = fcase(
    is.na(population_observed), "missing_no_source_population",
    target_unit_id == greater_london_id &
      census_year == 1911L &
      allocation_method == greater_london_1911_allocation_method,
    "observed_greater_london_nomis_1911_50pct_overlap",
    target_unit_id == greater_london_id, "observed_greater_london_special_definition",
    grepl("density_weighted", allocation_method, fixed = TRUE), "observed_nomis_1921_density_weighted",
    default = "observed_direct_spatial_assignment"
  )
)]
setcolorder(observed, c(
  "target_unit_id", "target_unit_name", "target_area_type",
  "target_boundary_id", "census_year", "longitude", "latitude",
  "population", "population_observed", "population_available",
  "population_quality", "n_source_units", "n_source_allocations",
  "share_population_density_weighted", "allocation_method",
  "population_source", "any_match_needs_review", "source_panel"
))
setorder(observed, target_unit_name, census_year)

manual_population_result <- apply_manual_population_harmonization(
  observed,
  manual_harmonization_groups,
  census_years_law
)
observed <- manual_population_result$observed
manual_harmonization_audit <- manual_population_result$audit
setorder(observed, target_unit_name, census_year)

###############################################################################
# Annual population and inventor/scientist outcomes
###############################################################################

cat("Building annual population panel and inventor outcomes...\n")

annual <- merge(
  CJ(target_unit_id = target_dt$target_unit_id, year = annual_years),
  target_dt,
  by = "target_unit_id",
  all.x = TRUE,
  sort = FALSE
)
annual <- merge(
  annual,
  observed[, .(
    target_unit_id,
    year = census_year,
    population_observed,
    n_source_units,
    n_source_allocations,
    share_population_density_weighted,
    allocation_method,
    population_source,
    population_quality
  )],
  by = c("target_unit_id", "year"),
  all.x = TRUE,
  sort = FALSE
)
annual[, population := interp_no_extrapolate(
  year = year[!is.na(population_observed)],
  population = population_observed[!is.na(population_observed)],
  years_out = year
), by = target_unit_id]
annual[, `:=`(
  population_interpolated = is.na(population_observed) & !is.na(population),
  population_available = !is.na(population)
)]
annual[
  population_interpolated == TRUE & is.na(population_quality),
  population_quality := "interpolated_between_census_years"
]
annual[
  is.na(population_quality),
  population_quality := "missing_no_source_population"
]

people_cols <- c(
  "wikidata_code", "name", "birth", "death", "bplo1", "bpla1",
  "citizenship_1_b", "level1_main_occ", "level2_main_occ",
  "level3_main_occ", "level3_all_occ"
)
people <- fread(scientists_file, select = people_cols, na.strings = c("", "NA"))
people <- as.data.table(add_stem_dummy(people))
people <- people[
  level1_main_occ == "Discovery/Science" &
    birth %between% c(1801L, 1960L) &
    !is.na(bplo1) &
    !is.na(bpla1)
]
people[, `:=`(
  person_row_id = .I,
  birth_year = as.integer(birth)
)]

people_points <- st_as_sf(
  people,
  coords = c("bplo1", "bpla1"),
  crs = 4326,
  remove = FALSE
)
people_points <- st_transform(people_points, 27700)

uk_lau_sf <- lau[lau$CNTR_CODE == "UK", c("LAU_ID")]
uk_lau_sf <- st_make_valid(uk_lau_sf)
people_uk_probe <- st_join(
  people_points[, c("person_row_id")],
  uk_lau_sf,
  join = st_within,
  left = TRUE
)
people_uk_probe_dt <- as.data.table(st_drop_geometry(people_uk_probe))
people_uk_ids <- unique(people_uk_probe_dt[!is.na(LAU_ID), person_row_id])
people_uk_points <- people_points[people_points$person_row_id %in% people_uk_ids, ]

people_joined <- st_join(
  people_uk_points,
  targets_sf[, c("target_unit_id", "target_unit_name", "target_area_type")],
  join = st_within,
  left = TRUE
)
people_dt <- as.data.table(st_drop_geometry(people_joined))
setorder(people_dt, person_row_id, target_unit_id)
people_dt <- people_dt[, .SD[1L], by = person_row_id]

people_unmatched <- people_dt[is.na(target_unit_id)]
people_unmatched[, `:=`(
  inventor_match_method = "unmatched_no_historical_urban_unit_polygon",
  inventor_match_distance_m = NA_real_
)]

people_matched <- people_dt[!is.na(target_unit_id)]
inventors <- people_matched[, .(
  n_inventors = .N,
  n_stem = sum(stem == 1L, na.rm = TRUE),
  n_nonstem = sum(stem != 1L | is.na(stem), na.rm = TRUE)
), by = .(target_unit_id, year = birth_year)]
inventors <- apply_manual_inventor_harmonization(
  inventors,
  manual_harmonization_groups
)

inventor_panel <- merge(
  annual[year %in% inventor_panel_years],
  inventors,
  by = c("target_unit_id", "year"),
  all.x = TRUE,
  sort = FALSE
)
inventor_panel[is.na(n_inventors), `:=`(
  n_inventors = 0L,
  n_stem = 0L,
  n_nonstem = 0L
)]
inventor_panel[, `:=`(
  unit_type = "uk_historical_urban_unit",
  unit_id = target_unit_id,
  GEOID = NA_character_,
  lau_id = NA_character_,
  city_geonameid = NA_integer_,
  place_name = target_unit_name,
  place_name_ascii = target_unit_name,
  country = "United Kingdom",
  iso3 = "GBR",
  lat = latitude,
  lon = longitude,
  any_inventor = as.integer(n_inventors > 0),
  any_stem = as.integer(n_stem > 0),
  log1p_n_inventors = log1p(n_inventors),
  log1p_n_stem = log1p(n_stem),
  inventors_per_100k_pop = fifelse(
    !is.na(population) & population > 0,
    1e5 * n_inventors / population,
    NA_real_
  ),
  stem_per_100k_pop = fifelse(
    !is.na(population) & population > 0,
    1e5 * n_stem / population,
    NA_real_
  ),
  inventors_per_1000_pop = fifelse(
    !is.na(population) & population > 0,
    1000 * n_inventors / population,
    NA_real_
  ),
  stem_per_1000_pop = fifelse(
    !is.na(population) & population > 0,
    1000 * n_stem / population,
    NA_real_
  ),
  population_original = population_observed,
  population_source = "UK historical urban census harmonized to 1921 urban units",
  population_interp_status = population_quality,
  match_status = "matched",
  match_method = "birth_point_within_historical_urban_unit",
  match_distance_km = NA_real_,
  match_needs_review = FALSE,
  source_panel = "uk_historical_urban_units_inventor_panel_1801_1960_census_population"
)]

inventor_columns <- c(
  "unit_type", "unit_id", "GEOID", "lau_id", "city_geonameid",
  "target_unit_id", "target_area_type", "target_boundary_id",
  "place_name", "place_name_ascii", "country", "iso3", "lat", "lon",
  "year", "n_inventors", "n_stem", "n_nonstem", "any_inventor",
  "any_stem", "log1p_n_inventors", "log1p_n_stem", "population",
  "population_original", "population_source", "population_interp_status",
  "inventors_per_100k_pop", "stem_per_100k_pop", "inventors_per_1000_pop",
  "stem_per_1000_pop", "match_status", "match_method", "match_distance_km",
  "match_needs_review", "source_panel", "population_interpolated",
  "population_available", "n_source_units", "n_source_allocations",
  "share_population_density_weighted", "allocation_method"
)
inventor_panel <- inventor_panel[, ..inventor_columns]
setorder(inventor_panel, target_unit_id, year)

inventor_qc <- rbindlist(list(
  data.table(metric = "panel_rows", value = nrow(inventor_panel)),
  data.table(metric = "target_units", value = uniqueN(inventor_panel$target_unit_id)),
  data.table(metric = "matched_uk_people", value = nrow(people_matched)),
  data.table(metric = "unmatched_uk_people", value = nrow(people_unmatched)),
  data.table(metric = "total_inventors", value = sum(inventor_panel$n_inventors)),
  data.table(metric = "total_stem", value = sum(inventor_panel$n_stem)),
  data.table(
    metric = "greater_london_inventors",
    value = inventor_panel[target_unit_id == greater_london_id, sum(n_inventors)]
  ),
  data.table(
    metric = "rows_missing_population",
    value = inventor_panel[is.na(population), .N]
  )
), use.names = TRUE)

source_totals <- allocation_audit[!is.na(allocated_population), .(
  source_population_total = sum(source_population * allocation_share, na.rm = TRUE),
  allocated_population_total = sum(allocated_population, na.rm = TRUE),
  n_source_units = uniqueN(source_area_id),
  n_target_units_receiving_population = uniqueN(target_unit_id),
  n_allocations = .N,
  max_conservation_error = abs(
    sum(allocated_population, na.rm = TRUE) -
      sum(source_population * allocation_share, na.rm = TRUE)
  ),
  share_population_density_weighted = sum(
    allocated_population[
      allocation_method == "nomis_polygon_intersection_1921_density_weighted"
    ],
    na.rm = TRUE
  ) / sum(allocated_population, na.rm = TRUE)
), by = .(source, census_year)]
source_totals[, section := "source_year"]

coverage <- observed[, .(
  source_population_total = sum(population_observed, na.rm = TRUE),
  allocated_population_total = sum(population_observed, na.rm = TRUE),
  n_source_units = sum(!is.na(population_observed)),
  n_target_units_receiving_population = sum(!is.na(population_observed)),
  n_allocations = sum(n_source_allocations, na.rm = TRUE),
  max_conservation_error = NA_real_,
  share_population_density_weighted = if (all(is.na(share_population_density_weighted))) {
    NA_real_
  } else {
    weighted.mean(
      share_population_density_weighted,
      population_observed,
      na.rm = TRUE
    )
  },
  source = first_nonmissing_char(population_source)
), by = census_year]
coverage[, section := "final_panel_coverage"]

boundary_quality <- nomis_links[, .(
  source_population_total = sum(population, na.rm = TRUE),
  allocated_population_total = NA_real_,
  n_source_units = .N,
  n_target_units_receiving_population = NA_integer_,
  n_allocations = sum(boundary_link_needs_review, na.rm = TRUE),
  max_conservation_error = max(boundary_name_distance, na.rm = TRUE),
  share_population_density_weighted = NA_real_
), by = census_year]
boundary_quality[, `:=`(
  section = "nomis_boundary_link_quality",
  source = "Nomis historical census CR03"
)]

unmatched_quality <- nomis_unmatched[, .(
  source_population_total = sum(population, na.rm = TRUE),
  allocated_population_total = NA_real_,
  n_source_units = .N,
  n_target_units_receiving_population = NA_integer_,
  n_allocations = .N,
  max_conservation_error = NA_real_,
  share_population_density_weighted = NA_real_
), by = census_year]
unmatched_quality[, `:=`(
  section = "nomis_sources_outside_target_geography",
  source = "Nomis historical census CR03"
)]

law_quality <- law_match_audit[, .(
  source_population_total = sum(population, na.rm = TRUE),
  allocated_population_total = NA_real_,
  n_source_units = .N,
  n_target_units_receiving_population = uniqueN(target_unit_id, na.rm = TRUE),
  n_allocations = sum(allocation_status != "assigned_point_in_target"),
  max_conservation_error = NA_real_,
  share_population_density_weighted = NA_real_
), by = .(census_year, source = allocation_status)]
law_quality[, section := "law_robson_match_status"]

manual_quality <- manual_harmonization_audit[
  action %chin% c(
    "primary_manual_harmonized",
    "primary_kept_law_robson_composite"
  ),
  .(
    source_population_total = sum(original_population, na.rm = TRUE),
    allocated_population_total = sum(adjusted_population, na.rm = TRUE),
    n_source_units = uniqueN(member_target_unit_id),
    n_target_units_receiving_population = uniqueN(primary_target_unit_id),
    n_allocations = .N,
    max_conservation_error = NA_real_,
    share_population_density_weighted = NA_real_
  ),
  by = .(
    census_year,
    source = paste0("manual_harmonization_", confidence, "_confidence")
  )
]
manual_quality[, section := "manual_harmonization"]

quality <- rbindlist(list(
  source_totals,
  coverage,
  boundary_quality,
  unmatched_quality,
  law_quality,
  manual_quality
), fill = TRUE)
setorder(quality, section, census_year, source)

transition <- merge(
  observed[census_year == 1911L, .(
    target_unit_id,
    target_unit_name,
    target_area_type,
    population_1911 = population_observed,
    quality_1911 = population_quality
  )],
  observed[census_year == 1921L, .(
    target_unit_id,
    population_1921 = population_observed,
    quality_1921 = population_quality
  )],
  by = "target_unit_id",
  all = TRUE
)
transition[, `:=`(
  growth_ratio_1911_1921 = safe_ratio(population_1921, population_1911),
  growth_pct_1911_1921 = 100 * (safe_ratio(population_1921, population_1911) - 1),
  abs_growth_pct_1911_1921 = abs(100 * (safe_ratio(population_1921, population_1911) - 1))
)]
setorder(transition, -abs_growth_pct_1911_1921, target_unit_name)

target_export <- copy(target_dt)
setorder(target_export, target_area_type, target_unit_name)

if (observed[, anyDuplicated(paste(target_unit_id, census_year))] > 0L) {
  stop("Observed panel has duplicate target_unit_id-census_year keys.")
}
if (nrow(annual) != nrow(target_dt) * length(annual_years) ||
    annual[, anyDuplicated(paste(target_unit_id, year))] > 0L) {
  stop("Annual panel is not a complete unique target_unit_id-year skeleton.")
}
if (nrow(inventor_panel) != nrow(target_dt) * length(inventor_panel_years) ||
    inventor_panel[, anyDuplicated(paste(target_unit_id, year))] > 0L) {
  stop("Inventor panel is not a complete unique target_unit_id-year skeleton.")
}
if (observed[!is.na(population), any(population < 0)] ||
    annual[!is.na(population), any(population < 0)]) {
  stop("Negative population found in final panels.")
}
if (sum(inventor_panel$n_inventors) != nrow(people_matched)) {
  stop("Inventor totals do not match the matched person-level records.")
}

cat("Writing outputs...\n")
fwrite(observed, observed_file)
fwrite(allocation_audit, allocation_file)
fwrite(manual_harmonization_audit, manual_harmonization_file)
fwrite(quality, quality_file)
fwrite(transition, transition_file)
fwrite(target_export, target_file)
fwrite(law_match_audit, law_match_audit_file)
fwrite(nomis_unmatched, nomis_unmatched_file)
fwrite(inventor_panel, inventor_panel_file)
fwrite(inventor_qc, inventor_qc_file)
fwrite(people_unmatched, inventor_unmatched_file)

cat("\nDone.\n")
cat("Observed panel: ", observed_file, "\n", sep = "")
cat("Allocation audit: ", allocation_file, "\n", sep = "")
cat("Manual harmonization audit: ", manual_harmonization_file, "\n", sep = "")
cat("Quality summary: ", quality_file, "\n", sep = "")
cat("1911-1921 transition audit: ", transition_file, "\n", sep = "")
cat("Target units: ", target_file, "\n", sep = "")
cat("Law-Robson match audit: ", law_match_audit_file, "\n", sep = "")
cat("Nomis outside-target sources: ", nomis_unmatched_file, "\n", sep = "")
cat("Inventor panel: ", inventor_panel_file, "\n", sep = "")
cat("Inventor QC: ", inventor_qc_file, "\n", sep = "")
cat("Inventor unmatched people: ", inventor_unmatched_file, "\n", sep = "")

cat("\nCoverage by census year:\n")
print(observed[, .(
  target_units_with_population = sum(population_available),
  total_population = sum(population, na.rm = TRUE)
), by = census_year][order(census_year)])

cat("\nNomis source units outside target geography:\n")
if (nrow(nomis_unmatched) == 0L) {
  cat("None\n")
} else {
  print(nomis_unmatched[, .(
    units = .N,
    population = sum(population, na.rm = TRUE)
  ), by = census_year][order(census_year)])
}

cat("\nInventor outcome QC:\n")
print(inventor_qc)
