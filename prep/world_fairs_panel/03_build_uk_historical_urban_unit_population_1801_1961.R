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
# - Caprettini-Voth 1801 parish occupation shares and 1801-1831 parish
#   population densities, allocated from 1851 parish polygons to the fixed 1921
#   target geography.
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
  library(haven)
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
caprettini_voth_dir <- file.path(
  TALENT_DETS_DATA_DIR, "Data", "raw", "world_fairs", "data_c_voth"
)
occupation_shares_file <- file.path(caprettini_voth_dir, "swing-cross.dta")
population_panel_file <- file.path(caprettini_voth_dir, "swing-panel.dta")
occupation_parishes_file <- file.path(caprettini_voth_dir, "Parishes1851.shp")

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
occupation_unit_file <- file.path(
  DATA_PROCESSED,
  "uk_historical_urban_units_1801_occupation_shares.csv"
)
occupation_crosswalk_file <- file.path(
  DATA_PROCESSED,
  "uk_historical_urban_units_1801_occupation_share_crosswalk.csv"
)
occupation_qc_file <- file.path(
  DATA_PROCESSED,
  "uk_historical_urban_units_1801_occupation_shares_qc.csv"
)
swing_population_audit_file <- file.path(
  DATA_PROCESSED,
  "uk_historical_urban_units_swing_population_1801_1831_audit.csv"
)

required_files <- c(
  law_panel_file, scientists_file, benchmark_1911_file, boundary_gpkg,
  lau_gpkg, greater_london_1911_crosswalk_file, occupation_shares_file,
  population_panel_file, occupation_parishes_file
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
swing_period_year <- c("1" = 1801L, "3" = 1811L, "4" = 1821L, "5" = 1831L)
swing_population_years <- unname(swing_period_year)
swing_min_geometry_coverage <- 0.95
swing_max_growth_factor_per_decade <- 5

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

screen_swing_population_growth <- function(year, population, eligible,
                                           factor_per_decade = 5) {
  year <- as.integer(year)
  population <- as.numeric(population)
  eligible <- as.logical(eligible)
  accepted <- rep(FALSE, length(year))
  outlier <- rep(FALSE, length(year))
  previous <- NA_integer_

  for (i in order(year)) {
    if (!isTRUE(eligible[[i]]) || !is.finite(population[[i]]) ||
        population[[i]] <= 0) {
      next
    }
    if (is.na(previous)) {
      accepted[[i]] <- TRUE
      previous <- i
      next
    }
    decades <- (year[[i]] - year[[previous]]) / 10
    allowed_factor <- factor_per_decade ^ decades
    ratio <- population[[i]] / population[[previous]]
    if (!is.finite(ratio) || ratio > allowed_factor ||
        ratio < 1 / allowed_factor) {
      outlier[[i]] <- TRUE
    } else {
      accepted[[i]] <- TRUE
      previous <- i
    }
  }

  list(accepted = accepted, outlier = outlier)
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
# Caprettini-Voth 1801 occupation shares
###############################################################################

cat("Allocating 1801 parish occupation shares to target units...\n")

occupation_share_cols_raw <- c("agri_share", "trade_share", "other_share")
occupation_source_cols <- c(
  "GAZ_CNTY", "PARISH", "PARISH_ID", "AREA_m2", "LATo_WGS84",
  "LOGo_WGS84", "LOGx_BNG", "LATx_BNG", "density",
  occupation_share_cols_raw
)
occupation_source <- as.data.table(read_dta(occupation_shares_file))
missing_occupation_cols <- setdiff(occupation_source_cols, names(occupation_source))
if (length(missing_occupation_cols)) {
  stop(
    "Caprettini-Voth occupation data are missing required columns: ",
    paste(missing_occupation_cols, collapse = ", ")
  )
}
occupation_source <- occupation_source[, ..occupation_source_cols]
occupation_source[, PARISH_ID := as.integer(PARISH_ID)]

if (occupation_source[, anyNA(PARISH_ID)] ||
    occupation_source[, anyDuplicated(PARISH_ID)] > 0L) {
  stop("Caprettini-Voth PARISH_ID must be complete and unique.")
}

occupation_share_missing_count <- occupation_source[, rowSums(is.na(.SD)),
                                                     .SDcols = occupation_share_cols_raw]
if (any(!occupation_share_missing_count %in% c(0L, 3L))) {
  stop("The three 1801 occupation shares must be jointly observed or jointly missing.")
}
occupation_source[, shares_complete := occupation_share_missing_count == 0L]
if (occupation_source[
      shares_complete == TRUE,
      any(unlist(.SD, use.names = FALSE) < 0 |
          unlist(.SD, use.names = FALSE) > 1)
    , .SDcols = occupation_share_cols_raw]) {
  stop("Observed 1801 occupation shares must lie in [0, 1].")
}
occupation_source[, occupation_share_sum :=
  agri_share + trade_share + other_share]
if (occupation_source[
      shares_complete == TRUE,
      any(abs(occupation_share_sum - 1) > 1e-6)
    ]) {
  stop("Observed 1801 occupation shares do not sum to one.")
}
occupation_source[, population_1801_implied := density * AREA_m2 / 1e6]
if (occupation_source[
      !is.na(population_1801_implied),
      any(!is.finite(population_1801_implied) | population_1801_implied < 0)
    ]) {
  stop("Invalid implied 1801 parish population in Caprettini-Voth data.")
}

occupation_parishes_sf <- st_read(occupation_parishes_file, quiet = TRUE)
if (!"PARISH_ID" %in% names(occupation_parishes_sf)) {
  stop("Parishes1851.shp is missing PARISH_ID.")
}
occupation_parishes_sf$PARISH_ID <- as.integer(occupation_parishes_sf$PARISH_ID)
occupation_parishes_sf <- occupation_parishes_sf[
  !is.na(occupation_parishes_sf$PARISH_ID) &
    occupation_parishes_sf$PARISH_ID > 0L,
]
if (anyDuplicated(occupation_parishes_sf$PARISH_ID) > 0L) {
  stop("Parishes1851.shp contains duplicate positive PARISH_ID values.")
}

missing_parish_shapes <- setdiff(
  occupation_source$PARISH_ID,
  occupation_parishes_sf$PARISH_ID
)
extra_parish_shapes <- setdiff(
  occupation_parishes_sf$PARISH_ID,
  occupation_source$PARISH_ID
)
if (length(missing_parish_shapes) || length(extra_parish_shapes)) {
  stop(
    "Caprettini-Voth data and parish polygons do not match one-to-one. ",
    "Missing shapes: ", length(missing_parish_shapes),
    "; extra shapes: ", length(extra_parish_shapes), "."
  )
}

occupation_parishes_sf <- occupation_parishes_sf[, "PARISH_ID"]
occupation_parishes_sf <- st_make_valid(st_transform(occupation_parishes_sf, 27700))
occupation_parishes_sf$parish_geometry_area_m2 <- as.numeric(
  st_area(occupation_parishes_sf)
)
if (any(!is.finite(occupation_parishes_sf$parish_geometry_area_m2) |
        occupation_parishes_sf$parish_geometry_area_m2 <= 0)) {
  stop("Caprettini-Voth parish polygons must have positive finite area.")
}
occupation_parishes_sf <- merge(
  occupation_parishes_sf,
  occupation_source,
  by = "PARISH_ID",
  all.x = TRUE,
  sort = FALSE
)

# The fixed target polygons have a few sub-square-metre topology slivers. Audit
# them and fail only if an overlap exceeds 0.001% of the smaller target.
target_overlap_index <- st_overlaps(targets_sf)
target_overlap_pairs <- rbindlist(lapply(
  seq_along(target_overlap_index),
  function(i) {
    hits <- target_overlap_index[[i]]
    hits <- hits[hits > i]
    if (!length(hits)) return(NULL)
    data.table(target_row_1 = i, target_row_2 = hits)
  }
))
target_overlap_max_relative <- 0
if (nrow(target_overlap_pairs)) {
  target_overlap_pairs[, overlap_area_m2 := vapply(
    seq_len(.N),
    function(i) {
      suppressWarnings(as.numeric(st_area(st_intersection(
        targets_sf[target_row_1[[i]], ],
        targets_sf[target_row_2[[i]], ]
      ))))
    },
    numeric(1L)
  )]
  target_overlap_pairs[, smaller_target_area_m2 := pmin(
    targets_sf$target_area_m2[target_row_1],
    targets_sf$target_area_m2[target_row_2]
  )]
  target_overlap_pairs[, relative_overlap :=
    overlap_area_m2 / smaller_target_area_m2]
  target_overlap_max_relative <- max(
    target_overlap_pairs$relative_overlap,
    na.rm = TRUE
  )
}
if (!is.finite(target_overlap_max_relative) ||
    target_overlap_max_relative > 1e-5) {
  stop(
    "Target geometry overlap exceeds tolerance: ",
    signif(target_overlap_max_relative, 6L)
  )
}

occupation_intersections_sf <- suppressWarnings(st_intersection(
  targets_sf[, c("target_unit_id", "target_unit_name", "target_area_type")],
  occupation_parishes_sf[, c(
    "PARISH_ID", "GAZ_CNTY", "PARISH", "AREA_m2", "density",
    "population_1801_implied", "shares_complete", occupation_share_cols_raw,
    "parish_geometry_area_m2"
  )]
))
occupation_crosswalk <- as.data.table(st_drop_geometry(occupation_intersections_sf))
occupation_crosswalk[, intersection_area_m2 := as.numeric(
  st_area(occupation_intersections_sf)
)]
occupation_crosswalk <- occupation_crosswalk[
  is.finite(intersection_area_m2) & intersection_area_m2 > 0
]
occupation_crosswalk[, parish_overlap_share :=
  intersection_area_m2 / parish_geometry_area_m2]
if (occupation_crosswalk[
      , any(!is.finite(parish_overlap_share) |
            parish_overlap_share < 0 | parish_overlap_share > 1 + 1e-6)
    ]) {
  stop("Invalid parish overlap share in the occupation crosswalk.")
}
occupation_crosswalk[, allocated_population_1801 :=
  population_1801_implied * parish_overlap_share]
occupation_crosswalk[, allocated_source_area_m2 :=
  AREA_m2 * parish_overlap_share]
if (occupation_crosswalk[
      , any(!is.finite(allocated_source_area_m2) |
            allocated_source_area_m2 <= 0)
    ]) {
  stop("Invalid allocated source area in the demographic crosswalk.")
}

occupation_crosswalk_counts <- occupation_crosswalk[, .(
  n_intersecting_parishes = uniqueN(PARISH_ID),
  n_complete_parishes = uniqueN(PARISH_ID[shares_complete == TRUE])
), by = target_unit_id]
occupation_crosswalk_population <- occupation_crosswalk[
  is.finite(allocated_population_1801) & allocated_population_1801 > 0,
  .(
    occupation_share_population_total_1801 = sum(allocated_population_1801),
    occupation_share_population_covered_1801 = sum(
      allocated_population_1801[shares_complete == TRUE]
    )
  ),
  by = target_unit_id
]
occupation_crosswalk_observed <- occupation_crosswalk[
  shares_complete == TRUE &
    is.finite(allocated_population_1801) & allocated_population_1801 > 0,
  .(
    agri_share_1801 = weighted.mean(agri_share, allocated_population_1801),
    trade_share_1801 = weighted.mean(trade_share, allocated_population_1801),
    other_share_1801 = weighted.mean(other_share, allocated_population_1801)
  ),
  by = target_unit_id
]
population_density_area <- occupation_crosswalk[, .(
  population_density_area_total_1801 = sum(allocated_source_area_m2),
  population_density_area_covered_1801 = sum(
    allocated_source_area_m2[is.finite(density)]
  )
), by = target_unit_id]
population_density_observed <- occupation_crosswalk[
  is.finite(density) & is.finite(allocated_population_1801),
  .(
    population_density_1801 = sum(allocated_population_1801) /
      (sum(allocated_source_area_m2) / 1e6),
    population_density_1801_area_weighted_check = weighted.mean(
      density,
      allocated_source_area_m2
    )
  ),
  by = target_unit_id
]

occupation_unit <- Reduce(
  function(x, y) merge(x, y, by = "target_unit_id", all.x = TRUE, sort = FALSE),
  list(
    target_dt[, .(
      target_unit_id, target_unit_name, target_area_type, target_boundary_id
    )],
    occupation_crosswalk_counts,
    occupation_crosswalk_population,
    occupation_crosswalk_observed,
    population_density_area,
    population_density_observed
  )
)
occupation_unit[is.na(n_intersecting_parishes), n_intersecting_parishes := 0L]
occupation_unit[is.na(n_complete_parishes), n_complete_parishes := 0L]
occupation_unit[, occupation_share_coverage_1801 := fifelse(
  !is.na(occupation_share_population_total_1801) &
    occupation_share_population_total_1801 > 0,
  occupation_share_population_covered_1801 /
    occupation_share_population_total_1801,
  NA_real_
)]
occupation_unit[, population_density_area_coverage_1801 := fifelse(
  !is.na(population_density_area_total_1801) &
    population_density_area_total_1801 > 0,
  population_density_area_covered_1801 /
    population_density_area_total_1801,
  NA_real_
)]
occupation_unit[, population_implied_1801 :=
  occupation_share_population_total_1801]
occupation_unit[, `:=`(
  occupation_share_source =
    "Caprettini and Voth (2020), 1801 census occupation shares and density",
  occupation_share_crosswalk_method =
    "1851 parish polygon intersection weighted by implied 1801 population"
)]

occupation_panel_cols <- c(
  "population_implied_1801",
  "agri_share_1801", "trade_share_1801", "other_share_1801",
  "occupation_share_coverage_1801", "population_density_1801",
  "population_density_area_coverage_1801"
)
if (nrow(occupation_unit) != nrow(target_dt) ||
    occupation_unit[, anyDuplicated(target_unit_id)] > 0L) {
  stop("Occupation-share output must contain one row per target unit.")
}
if (occupation_unit[
      !is.na(agri_share_1801),
      any(abs(agri_share_1801 + trade_share_1801 + other_share_1801 - 1) > 1e-6)
    ]) {
  stop("Aggregated 1801 occupation shares do not sum to one.")
}
if (occupation_unit[
      !is.na(occupation_share_coverage_1801),
      any(occupation_share_coverage_1801 < 0 |
          occupation_share_coverage_1801 > 1 + 1e-6)
    ]) {
  stop("Occupation-share population coverage must lie in [0, 1].")
}
if (occupation_unit[
      !is.na(population_implied_1801),
      any(!is.finite(population_implied_1801) | population_implied_1801 <= 0)
    ]) {
  stop("Implied 1801 target-unit population must be finite and positive.")
}
if (occupation_unit[
      !is.na(population_density_1801),
      any(!is.finite(population_density_1801) | population_density_1801 < 0)
    ]) {
  stop("Aggregated 1801 population density must be finite and non-negative.")
}
if (occupation_unit[
      !is.na(population_density_area_coverage_1801),
      any(population_density_area_coverage_1801 < 0 |
          population_density_area_coverage_1801 > 1 + 1e-6)
    ]) {
  stop("Population-density area coverage must lie in [0, 1].")
}
if (occupation_unit[
      !is.na(population_density_1801),
      any(abs(
        population_density_1801 - population_density_1801_area_weighted_check
      ) > 1e-8)
    ]) {
  stop("Population-density aggregation identity failed.")
}

# Compare the chosen polygon overlay with the provided within-parish BNG points.
valid_bng_centroid <- occupation_source[
  is.finite(LOGx_BNG) & is.finite(LATx_BNG) &
    LOGx_BNG != 0 & LATx_BNG != 0
]
occupation_centroid_points <- st_as_sf(
  valid_bng_centroid,
  coords = c("LOGx_BNG", "LATx_BNG"),
  crs = 27700,
  remove = FALSE
)
occupation_centroid_hits <- st_intersects(occupation_centroid_points, targets_sf)
occupation_centroid_match <- data.table(
  PARISH_ID = occupation_centroid_points$PARISH_ID,
  target_row = vapply(
    occupation_centroid_hits,
    function(hit) if (length(hit)) hit[[1L]] else NA_integer_,
    integer(1L)
  ),
  n_target_hits = lengths(occupation_centroid_hits),
  shares_complete = occupation_centroid_points$shares_complete,
  population_1801_implied = occupation_centroid_points$population_1801_implied
)

occupation_qc <- data.table(
  metric = c(
    "source_parishes", "source_parishes_complete_shares",
    "target_units", "target_units_with_occupation_shares",
    "target_units_missing_occupation_shares",
    "target_units_coverage_ge_0_99", "crosswalk_rows",
    "centroid_points_matched_to_target", "centroid_target_units_with_shares",
    "source_parishes_with_population_density",
    "target_units_with_population_density",
    "target_units_missing_population_density",
    "target_units_density_area_coverage_ge_0_95",
    "target_units_density_area_coverage_ge_0_99",
    "max_population_density_identity_deviation",
    "max_aggregated_share_sum_deviation",
    "target_overlap_pairs", "target_overlap_max_relative"
  ),
  value = c(
    nrow(occupation_source),
    occupation_source[shares_complete == TRUE, .N],
    nrow(occupation_unit),
    occupation_unit[!is.na(agri_share_1801), .N],
    occupation_unit[is.na(agri_share_1801), .N],
    occupation_unit[occupation_share_coverage_1801 >= 0.99, .N],
    nrow(occupation_crosswalk),
    occupation_centroid_match[!is.na(target_row), .N],
    occupation_centroid_match[
      !is.na(target_row) & shares_complete == TRUE &
        is.finite(population_1801_implied) & population_1801_implied > 0,
      uniqueN(target_row)
    ],
    occupation_source[is.finite(density), .N],
    occupation_unit[!is.na(population_density_1801), .N],
    occupation_unit[is.na(population_density_1801), .N],
    occupation_unit[population_density_area_coverage_1801 >= 0.95, .N],
    occupation_unit[population_density_area_coverage_1801 >= 0.99, .N],
    occupation_unit[
      !is.na(population_density_1801),
      max(abs(
        population_density_1801 - population_density_1801_area_weighted_check
      ))
    ],
    occupation_unit[
      !is.na(agri_share_1801),
      max(abs(agri_share_1801 + trade_share_1801 + other_share_1801 - 1))
    ],
    nrow(target_overlap_pairs),
    target_overlap_max_relative
  )
)

target_dt <- merge(
  target_dt,
  occupation_unit[, c("target_unit_id", occupation_panel_cols), with = FALSE],
  by = "target_unit_id",
  all.x = TRUE,
  sort = FALSE
)

cat(
  "Occupation shares available for target units: ",
  occupation_unit[!is.na(agri_share_1801), .N],
  "/", nrow(occupation_unit), "\n",
  sep = ""
)
cat(
  "Population density available for target units: ",
  occupation_unit[!is.na(population_density_1801), .N],
  "/", nrow(occupation_unit), "\n",
  sep = ""
)

###############################################################################
# Caprettini-Voth 1801-1831 population knots
###############################################################################

cat("Allocating 1801-1831 Swing population densities to target units...\n")

swing_panel <- as.data.table(read_dta(population_panel_file))
swing_required_cols <- c("period", "PARISH_ID", "density")
swing_missing_cols <- setdiff(swing_required_cols, names(swing_panel))
if (length(swing_missing_cols)) {
  stop(
    "Caprettini-Voth panel is missing required columns: ",
    paste(swing_missing_cols, collapse = ", ")
  )
}
swing_panel <- swing_panel[, ..swing_required_cols]
swing_panel[, `:=`(
  period = as.integer(period),
  PARISH_ID = as.integer(PARISH_ID),
  density = as.numeric(density)
)]
if (swing_panel[!is.na(PARISH_ID), anyDuplicated(paste(PARISH_ID, period))] > 0L) {
  stop("swing-panel.dta contains duplicate PARISH_ID-period keys.")
}

swing_period_map <- data.table(
  period = as.integer(names(swing_period_year)),
  census_year = as.integer(unname(swing_period_year))
)
if (!all(swing_period_map$period %in% unique(swing_panel$period))) {
  stop("swing-panel.dta is missing one or more required census periods.")
}

# Periods 1 and 2 intentionally repeat the 1801 baseline. Validate that fact,
# and validate that period 1 is the same density field used by swing-cross.dta.
swing_p12 <- dcast(
  swing_panel[period %in% c(1L, 2L) & !is.na(PARISH_ID)],
  PARISH_ID ~ period,
  value.var = "density"
)
if (!all(c("1", "2") %chin% names(swing_p12)) ||
    swing_p12[
      is.finite(get("1")) & is.finite(get("2")),
      any(abs(get("1") - get("2")) > 1e-8)
    ]) {
  stop("Swing periods 1 and 2 do not contain identical 1801 densities.")
}
swing_p1_check <- merge(
  swing_panel[period == 1L, .(PARISH_ID, density_panel = density)],
  occupation_source[, .(PARISH_ID, density_cross = density)],
  by = "PARISH_ID",
  all = FALSE
)
if (swing_p1_check[
      is.finite(density_panel) & is.finite(density_cross),
      any(abs(density_panel - density_cross) > 1e-8)
    ]) {
  stop("Swing period 1 does not match the 1801 density in swing-cross.dta.")
}

swing_crosswalk <- occupation_crosswalk[, .(
  period = swing_period_map$period,
  census_year = swing_period_map$census_year,
  intersection_area_m2,
  allocated_source_area_m2
), by = .(
  target_unit_id, target_unit_name, target_area_type, PARISH_ID
)]
swing_crosswalk <- merge(
  swing_crosswalk,
  swing_panel[period %in% swing_period_map$period],
  by = c("PARISH_ID", "period"),
  all.x = TRUE,
  sort = FALSE
)
swing_crosswalk[, valid_density := is.finite(density) & density > 0]
swing_crosswalk[, allocated_population := fifelse(
  valid_density,
  density * allocated_source_area_m2 / 1e6,
  NA_real_
)]

swing_population_raw <- swing_crosswalk[, .(
  population_swing_implied_raw = if (any(valid_density)) {
    sum(allocated_population, na.rm = TRUE)
  } else {
    NA_real_
  },
  swing_intersection_area_m2_raw = sum(intersection_area_m2, na.rm = TRUE),
  swing_valid_density_area_m2_raw = sum(
    intersection_area_m2[valid_density], na.rm = TRUE
  ),
  swing_n_intersecting_parishes_raw = uniqueN(PARISH_ID),
  swing_n_valid_density_parishes_raw = uniqueN(PARISH_ID[valid_density])
), by = .(target_unit_id, census_year, period)]

swing_population_knots <- merge(
  CJ(
    target_unit_id = target_dt$target_unit_id,
    census_year = swing_population_years
  ),
  swing_population_raw,
  by = c("target_unit_id", "census_year"),
  all.x = TRUE,
  sort = FALSE
)
swing_population_knots <- merge(
  swing_population_knots,
  target_dt[, .(
    target_unit_id, target_unit_name, target_area_type, target_boundary_id,
    target_area_m2
  )],
  by = "target_unit_id",
  all.x = TRUE,
  sort = FALSE
)
swing_population_knots[, `:=`(
  population_swing_implied = population_swing_implied_raw,
  swing_target_area_m2 = target_area_m2,
  swing_intersection_area_m2 = swing_intersection_area_m2_raw,
  swing_valid_density_area_m2 = swing_valid_density_area_m2_raw,
  swing_n_intersecting_parishes = swing_n_intersecting_parishes_raw,
  swing_n_valid_density_parishes = swing_n_valid_density_parishes_raw,
  swing_manual_harmonization = "none"
)]

# Match the population geography used for the Law-Robson manual composites:
# sum all component polygons into the primary unit and suppress components.
swing_population_original <- copy(swing_population_knots)
for (gid in unique(manual_harmonization_groups$group_id)) {
  group <- manual_harmonization_groups[group_id == gid]
  primary_id <- unique(group$primary_target_unit_id)
  member_ids <- group$member_target_unit_id
  component_ids <- group[member_role == "component", member_target_unit_id]

  for (yr in swing_population_years) {
    member_rows <- swing_population_original[
      target_unit_id %chin% member_ids & census_year == yr
    ]
    aggregate_population <- if (
      member_rows[is.finite(population_swing_implied_raw), .N] > 0L
    ) {
      member_rows[, sum(population_swing_implied_raw, na.rm = TRUE)]
    } else {
      NA_real_
    }
    swing_population_knots[
      target_unit_id == primary_id & census_year == yr,
      `:=`(
        population_swing_implied = aggregate_population,
        swing_target_area_m2 = sum(member_rows$target_area_m2, na.rm = TRUE),
        swing_intersection_area_m2 = sum(
          member_rows$swing_intersection_area_m2_raw, na.rm = TRUE
        ),
        swing_valid_density_area_m2 = sum(
          member_rows$swing_valid_density_area_m2_raw, na.rm = TRUE
        ),
        swing_n_intersecting_parishes = sum(
          member_rows$swing_n_intersecting_parishes_raw, na.rm = TRUE
        ),
        swing_n_valid_density_parishes = sum(
          member_rows$swing_n_valid_density_parishes_raw, na.rm = TRUE
        ),
        swing_manual_harmonization = paste0(
          "manual_composite_", gid
        )
      )
    ]
    if (length(component_ids)) {
      swing_population_knots[
        target_unit_id %chin% component_ids & census_year == yr,
        `:=`(
          population_swing_implied = NA_real_,
          swing_manual_harmonization = paste0(
            "component_merged_into_", primary_id
          )
        )
      ]
    }
  }
}

swing_population_knots[, `:=`(
  population_swing_geometry_coverage = swing_intersection_area_m2 /
    swing_target_area_m2,
  population_swing_density_coverage = swing_valid_density_area_m2 /
    swing_target_area_m2
)]
swing_population_knots[, population_swing_eligible_pre_growth :=
  is.finite(population_swing_implied) & population_swing_implied > 0 &
  is.finite(population_swing_geometry_coverage) &
    population_swing_geometry_coverage >= swing_min_geometry_coverage &
  is.finite(population_swing_density_coverage) &
    population_swing_density_coverage >= swing_min_geometry_coverage &
  !grepl("^component_merged_into_", swing_manual_harmonization)]
setorder(swing_population_knots, target_unit_id, census_year)
swing_population_knots[, c(
  "population_swing_usable", "population_swing_growth_outlier"
) := {
  screened <- screen_swing_population_growth(
    census_year,
    population_swing_implied,
    population_swing_eligible_pre_growth,
    factor_per_decade = swing_max_growth_factor_per_decade
  )
  list(screened$accepted, screened$outlier)
}, by = target_unit_id]
swing_population_knots[, population_swing_exclusion_reason := fcase(
  grepl("^component_merged_into_", swing_manual_harmonization),
    swing_manual_harmonization,
  !is.finite(population_swing_implied) | population_swing_implied <= 0,
    "missing_or_nonpositive_implied_population",
  !is.finite(population_swing_geometry_coverage) |
    population_swing_geometry_coverage < swing_min_geometry_coverage,
    "target_geometry_coverage_below_0.95",
  !is.finite(population_swing_density_coverage) |
    population_swing_density_coverage < swing_min_geometry_coverage,
    "valid_density_coverage_below_0.95",
  population_swing_growth_outlier, "growth_outlier_gt_factor_5_per_decade",
  population_swing_usable, "usable",
  default = "not_usable"
)]

if (swing_population_knots[
      !is.na(population_swing_geometry_coverage),
      any(population_swing_geometry_coverage < 0 |
          population_swing_geometry_coverage > 1 + 1e-5)
    ] || swing_population_knots[
      !is.na(population_swing_density_coverage),
      any(population_swing_density_coverage < 0 |
          population_swing_density_coverage > 1 + 1e-5)
    ]) {
  stop("Invalid Swing target-geometry coverage.")
}

cat("Usable Swing population knots by year:\n")
print(swing_population_knots[, .(
  usable = sum(population_swing_usable),
  growth_outliers = sum(population_swing_growth_outlier),
  geometry_or_density_incomplete = sum(
    population_swing_exclusion_reason %chin% c(
      "target_geometry_coverage_below_0.95",
      "valid_density_coverage_below_0.95"
    )
  )
), by = census_year][order(census_year)])

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

swing_merge_cols <- c(
  "target_unit_id", "census_year", "period",
  "population_swing_implied_raw", "population_swing_implied",
  "population_swing_geometry_coverage", "population_swing_density_coverage",
  "population_swing_eligible_pre_growth", "population_swing_growth_outlier",
  "population_swing_usable", "population_swing_exclusion_reason",
  "swing_manual_harmonization", "swing_n_intersecting_parishes",
  "swing_n_valid_density_parishes"
)
observed <- merge(
  observed,
  swing_population_knots[, ..swing_merge_cols],
  by = c("target_unit_id", "census_year"),
  all.x = TRUE,
  sort = FALSE
)
setnames(observed, "period", "population_swing_period")
observed[, population_swing_used :=
  is.na(population_observed) & population_swing_usable == TRUE]
observed[population_swing_used == TRUE, `:=`(
  population = population_swing_implied,
  population_available = TRUE,
  population_quality = "swing_implied_parish_density_knot",
  n_source_units = as.integer(swing_n_valid_density_parishes),
  n_source_allocations = as.integer(swing_n_intersecting_parishes),
  share_population_density_weighted = NA_real_,
  allocation_method = "caprettini_voth_parish_density_area_allocation",
  population_source = "Caprettini and Voth (2020), British census parish density",
  any_match_needs_review = population_swing_growth_outlier == TRUE
)]
observed[, population_available := !is.na(population)]
setorder(observed, target_unit_name, census_year)

swing_population_audit <- observed[
  census_year %in% swing_population_years,
  .(
    target_unit_id, target_unit_name, target_area_type, target_boundary_id,
    census_year, population_swing_period, population_observed,
    population_swing_implied_raw, population_swing_implied,
    population_swing_geometry_coverage, population_swing_density_coverage,
    population_swing_eligible_pre_growth, population_swing_growth_outlier,
    population_swing_usable, population_swing_used,
    population_swing_exclusion_reason, swing_manual_harmonization,
    swing_n_intersecting_parishes, swing_n_valid_density_parishes,
    observed_to_swing_ratio = safe_ratio(
      population_observed, population_swing_implied
    ),
    final_population_knot = population,
    final_population_quality = population_quality,
    final_population_source = population_source
  )
]
setorder(swing_population_audit, census_year, target_area_type, target_unit_name)

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
    population_knot = population,
    population_observed,
    population_swing_implied,
    population_swing_used,
    population_swing_geometry_coverage,
    population_swing_density_coverage,
    population_swing_growth_outlier,
    population_swing_exclusion_reason,
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
  year = year[!is.na(population_knot)],
  population = population_knot[!is.na(population_knot)],
  years_out = year
), by = target_unit_id]
annual[, `:=`(
  population_knot_available = !is.na(population_knot),
  population_interpolated = is.na(population_observed) & !is.na(population),
  population_available = !is.na(population)
)]
annual[
  population_interpolated == TRUE & is.na(population_knot),
  `:=`(
    population_quality = "interpolated_between_observed_or_swing_knots",
    population_source = "Linear interpolation between observed or Swing population knots"
  )
]
annual[
  is.na(population_quality),
  population_quality := "missing_no_source_population"
]
annual[is.na(population_swing_used), population_swing_used := FALSE]

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
  "share_population_density_weighted", "allocation_method",
  "population_knot", "population_knot_available",
  "population_swing_implied", "population_swing_used",
  "population_swing_geometry_coverage", "population_swing_density_coverage",
  "population_swing_growth_outlier", "population_swing_exclusion_reason",
  "population_implied_1801",
  "agri_share_1801", "trade_share_1801", "other_share_1801",
  "occupation_share_coverage_1801", "population_density_1801",
  "population_density_area_coverage_1801"
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
  ),
  data.table(
    metric = "swing_population_knots_used",
    value = inventor_panel[population_swing_used == TRUE, .N]
  ),
  data.table(
    metric = "target_units_using_swing_population",
    value = inventor_panel[
      population_swing_used == TRUE, uniqueN(target_unit_id)
    ]
  ),
  data.table(
    metric = "swing_population_growth_outliers",
    value = swing_population_audit[population_swing_growth_outlier == TRUE, .N]
  ),
  data.table(
    metric = "swing_population_knots_incomplete_coverage",
    value = swing_population_audit[
      population_swing_exclusion_reason %chin% c(
        "target_geometry_coverage_below_0.95",
        "valid_density_coverage_below_0.95"
      ), .N
    ]
  ),
  data.table(
    metric = "target_units_with_occupation_shares_1801",
    value = inventor_panel[
      !is.na(agri_share_1801), uniqueN(target_unit_id)
    ]
  ),
  data.table(
    metric = "target_units_missing_occupation_shares_1801",
    value = inventor_panel[
      is.na(agri_share_1801), uniqueN(target_unit_id)
    ]
  ),
  data.table(
    metric = "target_units_with_population_implied_1801",
    value = inventor_panel[
      !is.na(population_implied_1801), uniqueN(target_unit_id)
    ]
  ),
  data.table(
    metric = "target_units_with_population_density_1801",
    value = inventor_panel[
      !is.na(population_density_1801), uniqueN(target_unit_id)
    ]
  ),
  data.table(
    metric = "target_units_missing_population_density_1801",
    value = inventor_panel[
      is.na(population_density_1801), uniqueN(target_unit_id)
    ]
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
if (observed[
      !is.na(population_observed),
      any(abs(population - population_observed) > 1e-8)
    ]) {
  stop("A Swing population knot overwrote an observed population.")
}
if (observed[
      population_swing_used == TRUE,
      any(!is.na(population_observed) | population_swing_usable != TRUE)
    ]) {
  stop("Invalid Swing population-knot precedence.")
}
if (observed[
      population_swing_growth_outlier == TRUE,
      any(population_swing_used == TRUE)
    ]) {
  stop("A growth-outlier Swing knot was used in the final population panel.")
}
annual_knot_check <- annual[
  !is.na(population_knot),
  abs(population - population_knot)
]
if (length(annual_knot_check) && any(annual_knot_check > 1e-8)) {
  stop("Annual population does not reproduce an observed or Swing knot.")
}
if (sum(inventor_panel$n_inventors) != nrow(people_matched)) {
  stop("Inventor totals do not match the matched person-level records.")
}
occupation_static_check <- inventor_panel[, lapply(.SD, uniqueN),
                                          by = target_unit_id,
                                          .SDcols = occupation_panel_cols]
if (occupation_static_check[
      , any(unlist(.SD, use.names = FALSE) != 1L),
      .SDcols = occupation_panel_cols
    ]) {
  stop("The 1801 demographic columns are not time invariant by target unit.")
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
fwrite(occupation_unit, occupation_unit_file)
fwrite(occupation_crosswalk, occupation_crosswalk_file)
fwrite(occupation_qc, occupation_qc_file)
fwrite(swing_population_audit, swing_population_audit_file)

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
cat("Occupation shares: ", occupation_unit_file, "\n", sep = "")
cat("Occupation-share crosswalk: ", occupation_crosswalk_file, "\n", sep = "")
cat("Occupation-share QC: ", occupation_qc_file, "\n", sep = "")
cat("Swing population audit: ", swing_population_audit_file, "\n", sep = "")

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
