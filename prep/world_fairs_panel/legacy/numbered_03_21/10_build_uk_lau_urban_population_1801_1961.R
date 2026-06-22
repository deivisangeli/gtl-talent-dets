###############################################################################
# Build a harmonized England and Wales urban-population panel on fixed
# GISCO 2019 LAU boundaries, 1801-1961.
#
# Law-Robson-Bennett settlements are assigned by point-in-polygon for
# 1801-1911. All Nomis urban districts are allocated by polygon intersection
# for 1921, 1931, 1951, and 1961. The resulting measure is covered urban
# population, not total resident population of the modern LAU.
#
# Run from the repository root:
#   Rscript prep/world_fairs_panel/10_build_uk_lau_urban_population_1801_1961.R
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
law_panel_file <- file.path(
  gbr_dir, "city_population_law_robson_bennett_1801_1911_geocoded.csv"
)
scientists_unmatched_file <- file.path(
  DATA_OUTPUT, "discovery_science_lau_year_europe_unmatched.csv"
)

observed_file <- file.path(
  gbr_dir, "uk_lau_urban_population_census_1801_1961.csv"
)
annual_file <- file.path(
  gbr_dir, "uk_lau_urban_population_census_1801_1961_annual.csv"
)
allocation_file <- file.path(
  gbr_dir, "uk_lau_urban_population_allocation_audit.csv"
)
quality_file <- file.path(
  gbr_dir, "uk_lau_urban_population_quality_summary.csv"
)
transition_file <- file.path(
  gbr_dir, "uk_lau_population_1911_1921_transition_audit.csv"
)
inventor_panel_file <- file.path(
  DATA_PROCESSED, "uk_lau_inventor_panel_1801_1960_census_population.csv"
)
inventor_unmatched_file <- file.path(
  DATA_OUTPUT, "uk_lau_inventor_panel_1801_1960_unmatched_people.csv"
)

required_files <- c(
  law_panel_file, boundary_gpkg, lau_gpkg, scientists_unmatched_file
)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0L) {
  stop("Missing required files:\n", paste(missing_files, collapse = "\n"))
}

census_years_law <- seq(1801L, 1911L, by = 10L)
census_years_nomis <- c(1921L, 1931L, 1951L, 1961L)
census_years <- c(census_years_law, census_years_nomis)
annual_years <- 1801L:1961L

urban_types <- c(
  "Urban District", "Municipal Borough", "County Borough",
  "Metropolitan Borough", "County Corporate", "London County Corporate"
)

population_columns <- c(
  "1921" = "2c3_0003", "1931" = "3c3_0003",
  "1951" = "5c3_0003", "1961" = "6c3_0003"
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
    "BARKING" = "BARKING",
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
    "BARKING TOWN" = "BARKING",
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

first_nonmissing_char <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(x)]
  if (length(x) == 0L) NA_character_ else x[[1L]]
}

interp_no_extrapolate <- function(year, population, years_out) {
  ok <- !is.na(year) & !is.na(population)
  year <- as.integer(year[ok])
  population <- as.numeric(population[ok])
  if (length(unique(year)) < 2L) return(rep(NA_real_, length(years_out)))
  approx(
    x = year,
    y = population,
    xout = years_out,
    method = "linear",
    rule = 1,
    ties = sum
  )$y
}

###############################################################################
# Fixed GISCO 2019 LAUs
###############################################################################

cat("Reading GISCO 2019 LAUs...\n")
lau <- st_read(lau_gpkg, quiet = TRUE)
lau <- lau[
  lau$CNTR_CODE == "UK" & substr(lau$LAU_ID, 1L, 1L) %in% c("E", "W"),
  c("LAU_ID", "LAU_NAME")
]
if (nrow(lau) != 348L || anyDuplicated(lau$LAU_ID)) {
  stop("Expected 348 unique England and Wales GISCO 2019 LAUs.")
}
lau <- st_make_valid(st_transform(lau, 27700))

lau_points <- suppressWarnings(st_point_on_surface(lau))
lau_coords <- st_coordinates(st_transform(lau_points, 4326))
lau_meta <- as.data.table(st_drop_geometry(lau))[, .(
  lau_id = LAU_ID,
  lau_name = LAU_NAME
)]
lau_meta[, `:=`(
  longitude = lau_coords[, 1L],
  latitude = lau_coords[, 2L]
)]

###############################################################################
# Law-Robson allocation, 1801-1911
###############################################################################

cat("Allocating Law-Robson settlements to LAUs...\n")
law <- fread(law_panel_file, na.strings = c("", "NA"))
if (uniqueN(law$city_id) != 934L) stop("Expected 934 Law-Robson city IDs.")

law_city <- unique(law[, .(
  city_id,
  source_name = town_name,
  source_type = "Law-Robson urban settlement",
  easting,
  northing,
  longitude,
  latitude,
  geocode_status,
  geocode_method
)], by = "city_id")

located <- law_city[!is.na(easting) & !is.na(northing)]
law_points <- st_as_sf(
  located,
  coords = c("easting", "northing"),
  crs = 27700,
  remove = FALSE
)
law_joined <- st_join(
  law_points,
  lau[, c("LAU_ID", "LAU_NAME")],
  join = st_within,
  left = TRUE
)

outside <- which(is.na(law_joined$LAU_ID))
law_joined$distance_to_lau_m <- 0
law_joined$allocation_method <- "point_within_lau"
if (length(outside) > 0L) {
  nearest <- st_nearest_feature(law_joined[outside, ], lau)
  distance <- as.numeric(st_distance(
    law_joined[outside, ], lau[nearest, ], by_element = TRUE
  ))
  if (any(distance > 500)) {
    stop("Law-Robson points outside LAUs exceed the 500 metre fallback.")
  }
  law_joined$LAU_ID[outside] <- lau$LAU_ID[nearest]
  law_joined$LAU_NAME[outside] <- lau$LAU_NAME[nearest]
  law_joined$distance_to_lau_m[outside] <- distance
  law_joined$allocation_method[outside] <- "nearest_lau_within_500m"
}

law_crosswalk <- as.data.table(st_drop_geometry(law_joined))[, .(
  city_id,
  source_name,
  source_type,
  lau_id = LAU_ID,
  lau_name = LAU_NAME,
  allocation_method,
  distance_to_lau_m
)]

# Medway Towns is the only Law-Robson composite without a settlement point.
medway <- law_city[city_id == 1154L]
if (nrow(medway) != 1L) stop("Expected Law-Robson city_id 1154 for Medway Towns.")
law_crosswalk <- rbind(
  law_crosswalk,
  data.table(
    city_id = 1154L,
    source_name = medway$source_name,
    source_type = medway$source_type,
    lau_id = "E06000035",
    lau_name = "Medway",
    allocation_method = "reviewed_composite_to_lau",
    distance_to_lau_m = NA_real_
  ),
  use.names = TRUE
)
if (nrow(law_crosswalk) != 934L || anyDuplicated(law_crosswalk$city_id)) {
  stop("Law-Robson LAU crosswalk does not cover 934 unique cities.")
}

law_alloc <- merge(
  law[, .(
    city_id,
    census_year = as.integer(census_year),
    source_population = as.numeric(population),
    population_available = as.logical(population_available)
  )],
  law_crosswalk,
  by = "city_id",
  all.x = TRUE,
  sort = FALSE
)
law_alloc[, `:=`(
  source = "Law-Robson-Bennett Urban Population Database",
  source_area_id = paste0("LRB_", city_id),
  source_area_name = source_name,
  source_area_type = source_type,
  boundary_id = NA_character_,
  boundary_name = NA_character_,
  preferred_lower_unit_type = NA_character_,
  exact_lower_decomposition = NA,
  intersection_area_m2 = NA_real_,
  source_area_m2 = NA_real_,
  raw_area_share = 1,
  allocation_share = 1,
  allocated_population = source_population
)]

###############################################################################
# Nomis populations, exact lower-unit accounting, and district boundaries
###############################################################################

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
  pop_column <- population_columns[[as.character(year)]]
  values_small <- values[, .(
    value_id = as.character(area_id),
    value_name = as.character(area),
    value_type = as.character(area_type),
    population = suppressWarnings(as.numeric(get(pop_column))),
    population_1911_comparison = if (year == 1921L) {
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

  county_parent_types <- c(
    "Administrative County (excluding County Boroughs)",
    "Administrative County",
    "Administrative County with any County Boroughs",
    "Administrative County with associated County Boroughs",
    "Ancient County",
    "City and County of York"
  )
  parents <- relationships[
    area_type_1 %chin% county_parent_types & area_type_2 %chin% urban_types,
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
    areas[area_type %chin% urban_types, .(
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
  if (districts[is.na(population), .N] > 0L) {
    stop("Missing Nomis district populations for ", year)
  }

  child <- areas[, .(
    child_metadata_id = metadata_id,
    child_population = population
  )]
  rel <- relationships[
    area_type_1 %chin% urban_types,
    .(
      metadata_id = as.character(area_id_1),
      child_metadata_id = as.character(area_id_2),
      child_type = as.character(area_type_2)
    )
  ]
  rel <- merge(rel, child, by = "child_metadata_id", all.x = TRUE, sort = FALSE)
  lower_types <- c(
    "Enumeration District", "Civil Parish", "Parish", "Ward"
  )
  lower_sums <- rel[
    child_type %chin% lower_types & !is.na(child_population),
    .(
      child_population_sum = sum(child_population),
      n_lower_units = .N
    ),
    by = .(metadata_id, child_type)
  ]
  lower_sums <- merge(
    lower_sums,
    districts[, .(metadata_id, district_population = population)],
    by = "metadata_id",
    all.x = TRUE
  )
  lower_sums[, exact := abs(child_population_sum - district_population) < 0.5]
  lower_sums <- lower_sums[exact == TRUE]
  lower_sums[, priority := match(
    child_type, c("Enumeration District", "Civil Parish", "Parish", "Ward")
  )]
  setorder(lower_sums, metadata_id, priority)
  lower_sums <- lower_sums[, .SD[1L], by = metadata_id]

  districts <- merge(
    districts,
    lower_sums[, .(
      metadata_id,
      preferred_lower_unit_type = child_type,
      n_lower_units,
      exact_lower_decomposition = exact
    )],
    by = "metadata_id",
    all.x = TRUE,
    sort = FALSE
  )
  districts[is.na(exact_lower_decomposition), exact_lower_decomposition := FALSE]
  districts[, `:=`(
    census_year = year,
    source_name_canonical = canonical_name(source_area_name),
    source_county_canonical = canonical_county(source_county)
  )]
  districts
}

cat("Reading all Nomis urban districts...\n")
nomis <- rbindlist(lapply(census_years_nomis, read_nomis_year), fill = TRUE)
expected_nomis <- data.table(
  census_year = census_years_nomis,
  expected = c(1154L, 1147L, 993L, 993L)
)
nomis_counts <- nomis[, .N, by = census_year]
nomis_counts <- merge(nomis_counts, expected_nomis, by = "census_year")
if (nomis_counts[, any(N != expected)]) stop("Unexpected Nomis urban district count.")

cat("Linking Nomis districts to historical polygons...\n")
boundary_list <- setNames(lapply(census_years_nomis, function(year) {
  x <- st_read(
    boundary_gpkg,
    layer = paste0("districts_", year),
    quiet = TRUE
  )
  x <- st_make_valid(st_transform(x, 27700))
  x[x$boundary_type %in% urban_types, ]
}), as.character(census_years_nomis))

# Direct official identifiers are available for 1921 and 1961.
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

# Build county anchors from the official-code years. A point-on-surface lookup
# supplies historical county context for disambiguating repeated district names.
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
    by = .(source_row, boundary_row)
  ]
  candidates[, county_penalty := fifelse(county_agrees, 0, 0.08)]
  candidates[, score := name_distance + type_penalty + county_penalty]

  # Greedy one-to-one assignment, beginning with the strongest name/county
  # evidence. Counts differ by one in 1931, so the assignment is source-led.
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
# Area-weighted Nomis allocation to fixed LAUs
###############################################################################

nomis_alloc_list <- list()
for (year in census_years_nomis) {
  cat("Allocating Nomis districts for ", year, "...\n", sep = "")
  links <- nomis_links[census_year == year]
  link_payload <- links[, setdiff(
    names(links),
    c("boundary_name", "boundary_type", "boundary_source")
  ), with = FALSE]
  polygons <- merge(
    boundary_list[[as.character(year)]],
    link_payload,
    by = "boundary_id",
    all.y = TRUE,
    sort = FALSE
  )
  if (nrow(polygons) != nrow(links)) stop("Missing linked polygons for ", year)
  polygons$source_area_m2 <- as.numeric(st_area(polygons))

  intersections <- suppressWarnings(st_intersection(
    polygons[, c(
      "boundary_id", "boundary_name", "boundary_type", "boundary_source",
      "source_area_id", "source_area_name", "source_area_type", "population",
      "population_1911_comparison",
      "preferred_lower_unit_type", "exact_lower_decomposition", "n_lower_units",
      "boundary_link_method", "boundary_name_distance", "boundary_type_agrees",
      "boundary_link_needs_review", "source_area_m2"
    )],
    lau[, c("LAU_ID", "LAU_NAME")]
  ))
  intersections$intersection_area_m2 <- as.numeric(st_area(intersections))
  allocation <- as.data.table(st_drop_geometry(intersections))
  allocation[, raw_area_share := intersection_area_m2 / source_area_m2]
  allocation[, allocation_share := raw_area_share / sum(raw_area_share),
             by = source_area_id]
  allocation[, allocated_population := population * allocation_share]
  allocation[, `:=`(
    census_year = year,
    source = "Nomis historical census CR03",
    source_population = population,
    source_population_1911_comparison = population_1911_comparison,
    lau_id = LAU_ID,
    lau_name = LAU_NAME,
    allocation_method = "district_area_weighted",
    distance_to_lau_m = NA_real_
  )]
  allocation[, c("LAU_ID", "LAU_NAME", "population", "population_1911_comparison") := NULL]

  missing_districts <- setdiff(links$source_area_id, allocation$source_area_id)
  if (length(missing_districts) > 0L) {
    stop("Nomis districts do not intersect any LAU for ", year, ": ",
         paste(missing_districts, collapse = ", "))
  }
  conservation <- allocation[, .(
    allocated = sum(allocated_population),
    source = first(source_population)
  ), by = source_area_id]
  if (conservation[, any(abs(allocated - source) > 1e-6 * pmax(source, 1))]) {
    stop("District population is not conserved for ", year)
  }
  nomis_alloc_list[[as.character(year)]] <- allocation
}
nomis_alloc <- rbindlist(nomis_alloc_list, fill = TRUE)

# Law-Robson's London row is a metropolitan aggregate, not the population of
# the modern City of London LAU. Allocate it across the 33 Greater London LAUs
# using their shares of the 1911 comparison population published in the 1921
# Nomis CR03 table. Other Law-Robson points inside Greater London are excluded
# from the main series to avoid double counting the metropolitan aggregate.
london_weights <- nomis_alloc[
  census_year == 1921L & grepl("^E090000", lau_id),
  .(weight_population = sum(
    source_population_1911_comparison * allocation_share,
    na.rm = TRUE
  )),
  by = .(lau_id, lau_name)
]
if (nrow(london_weights) != 33L || london_weights[, any(weight_population <= 0)]) {
  stop("Expected positive 1911 Nomis comparison weights for 33 Greater London LAUs.")
}
london_weights[, allocation_share_london :=
  weight_population / sum(weight_population)]

london_source <- law_alloc[city_id == 1491L]
if (nrow(london_source) != length(census_years_law)) {
  stop("Expected one Law-Robson London row per census year.")
}
london_source[, join_key := 1L]
london_weights[, join_key := 1L]
london_alloc <- merge(
  london_source[, setdiff(names(london_source), c(
    "lau_id", "lau_name", "allocation_method", "raw_area_share",
    "allocation_share", "allocated_population"
  )), with = FALSE],
  london_weights[, .(join_key, lau_id, lau_name, allocation_share_london)],
  by = "join_key",
  allow.cartesian = TRUE
)
london_alloc[, `:=`(
  allocation_method = "greater_london_1911_nomis_weights",
  raw_area_share = NA_real_,
  allocation_share = allocation_share_london,
  allocated_population = source_population * allocation_share_london
)]
london_alloc[, c("join_key", "allocation_share_london") := NULL]
law_alloc <- rbindlist(list(
  law_alloc[
    city_id != 1491L &
      !(census_year %in% census_years_law & grepl("^E090000", lau_id))
  ],
  london_alloc
), use.names = TRUE, fill = TRUE)

allocation_columns <- c(
  "source", "census_year", "source_area_id", "source_area_name",
  "source_area_type", "source_population", "source_population_1911_comparison",
  "boundary_id", "boundary_name",
  "boundary_type", "boundary_source", "preferred_lower_unit_type",
  "exact_lower_decomposition", "n_lower_units", "boundary_link_method",
  "boundary_name_distance", "boundary_type_agrees", "boundary_link_needs_review",
  "lau_id", "lau_name", "allocation_method", "distance_to_lau_m",
  "intersection_area_m2", "source_area_m2", "raw_area_share",
  "allocation_share", "allocated_population"
)
for (column in setdiff(allocation_columns, names(law_alloc))) {
  law_alloc[, (column) := NA]
}
allocation_audit <- rbindlist(list(
  law_alloc[, ..allocation_columns],
  nomis_alloc[, ..allocation_columns]
), use.names = TRUE, fill = TRUE)
setorder(allocation_audit, census_year, source, source_area_id, lau_id)

###############################################################################
# Observed and annual LAU population panels
###############################################################################

cat("Building observed and interpolated LAU panels...\n")
observed_alloc <- allocation_audit[!is.na(allocated_population)]
observed_agg <- observed_alloc[, .(
  population_observed = sum(allocated_population),
  n_source_units = uniqueN(source_area_id),
  n_source_allocations = .N,
  share_area_weighted = sum(
    allocated_population[allocation_method == "district_area_weighted"],
    na.rm = TRUE
  ) / sum(allocated_population),
  allocation_method = paste(sort(unique(allocation_method)), collapse = " | "),
  population_source = paste(sort(unique(source)), collapse = " | ")
), by = .(lau_id, lau_name, census_year)]
observed_agg[!is.finite(share_area_weighted), share_area_weighted := NA_real_]

observed <- merge(
  CJ(lau_id = lau_meta$lau_id, census_year = census_years, unique = TRUE),
  lau_meta,
  by = "lau_id",
  all.x = TRUE
)
observed <- merge(
  observed,
  observed_agg,
  by = c("lau_id", "lau_name", "census_year"),
  all.x = TRUE,
  sort = FALSE
)
observed[, `:=`(
  population = population_observed,
  population_interpolated = FALSE,
  population_available = !is.na(population_observed),
  population_quality = fcase(
    is.na(population_observed), "missing_no_covered_urban_unit",
    grepl("greater_london_1911_nomis_weights", allocation_method),
    "observed_london_composite_1911_nomis_weights",
    share_area_weighted == 1, "observed_district_area_weighted",
    share_area_weighted == 0, "observed_settlement_point_aggregate",
    default = "observed_mixed_allocation"
  ),
  source_panel = "uk_lau_urban_population_census_1801_1961"
)]
setorder(observed, lau_id, census_year)

annual <- merge(
  CJ(lau_id = lau_meta$lau_id, year = annual_years, unique = TRUE),
  lau_meta,
  by = "lau_id",
  all.x = TRUE
)
annual <- merge(
  annual,
  observed[, .(
    lau_id,
    year = census_year,
    population_observed,
    n_source_units,
    n_source_allocations,
    share_area_weighted,
    allocation_method,
    population_source,
    observed_quality = population_quality
  )],
  by = c("lau_id", "year"),
  all.x = TRUE,
  sort = FALSE
)
annual[, population := interp_no_extrapolate(
  year = year[!is.na(population_observed)],
  population = population_observed[!is.na(population_observed)],
  years_out = year
), by = lau_id]
annual[, `:=`(
  population_interpolated = is.na(population_observed) & !is.na(population),
  population_available = !is.na(population),
  population_quality = fcase(
    !is.na(population_observed), observed_quality,
    !is.na(population), "linear_interpolation_between_censuses",
    default = "missing_no_extrapolation"
  ),
  source_panel = "uk_lau_urban_population_census_1801_1961_annual"
)]
setorder(annual, lau_id, year)

###############################################################################
# Quality summaries and the 1911-1921 transition
###############################################################################

source_totals <- allocation_audit[!is.na(allocated_population), .(
  source_population_total = sum(source_population * allocation_share),
  allocated_population_total = sum(allocated_population),
  n_source_units = uniqueN(source_area_id),
  n_laus_receiving_population = uniqueN(lau_id),
  n_allocations = .N,
  share_population_area_weighted = sum(
    allocated_population[allocation_method == "district_area_weighted"],
    na.rm = TRUE
  ) / sum(allocated_population),
  max_conservation_error = abs(
    sum(allocated_population) - sum(source_population * allocation_share)
  )
), by = .(source, census_year)]
source_totals[, section := "source_year"]

coverage <- observed[, .(
  source_population_total = sum(population_observed, na.rm = TRUE),
  allocated_population_total = sum(population_observed, na.rm = TRUE),
  n_source_units = sum(n_source_units, na.rm = TRUE),
  n_laus_receiving_population = sum(!is.na(population_observed)),
  n_allocations = sum(n_source_allocations, na.rm = TRUE),
  share_population_area_weighted = if (all(is.na(share_area_weighted))) {
    NA_real_
  } else {
    weighted.mean(
      share_area_weighted,
      w = fifelse(is.na(population_observed), 0, population_observed),
      na.rm = TRUE
    )
  },
  max_conservation_error = 0
), by = census_year]
coverage[, `:=`(
  section = "lau_coverage",
  source = "all_sources"
)]

boundary_quality <- nomis_links[, .(
  source_population_total = sum(population),
  allocated_population_total = NA_real_,
  n_source_units = .N,
  n_laus_receiving_population = NA_integer_,
  n_allocations = sum(boundary_link_needs_review, na.rm = TRUE),
  share_population_area_weighted = mean(exact_lower_decomposition),
  max_conservation_error = max(boundary_name_distance, na.rm = TRUE)
), by = census_year]
boundary_quality[, `:=`(
  section = "nomis_boundary_link_quality",
  source = "Nomis historical census CR03"
)]

quality <- rbindlist(list(source_totals, coverage, boundary_quality), fill = TRUE)
setorder(quality, section, census_year, source)

transition <- dcast(
  observed[census_year %in% c(1911L, 1921L), .(
    lau_id, lau_name, census_year, population_observed
  )],
  lau_id + lau_name ~ census_year,
  value.var = "population_observed"
)
setnames(transition, c("1911", "1921"), c("population_1911", "population_1921"))
transition[, `:=`(
  population_change = population_1921 - population_1911,
  population_ratio = population_1921 / population_1911,
  population_change_pct = 100 * (population_1921 / population_1911 - 1)
)]
transition[, transition_status := fcase(
  is.na(population_1911) & is.na(population_1921), "missing_both_years",
  is.na(population_1911), "missing_1911_law_robson",
  is.na(population_1921), "missing_1921_nomis",
  population_1911 == 0, "zero_1911_population",
  abs(population_change_pct) > 100, "large_change_over_100pct",
  default = "observed_both_years"
)]
transition[, absolute_change_pct := abs(population_change_pct)]
transition <- transition[order(is.na(absolute_change_pct), -absolute_change_pct)]

###############################################################################
# Alternative England and Wales inventor panel
###############################################################################

cat("Building the alternative UK LAU inventor panel...\n")
people <- fread(scientists_unmatched_file, na.strings = c("", "NA"))
people <- people[
  birth_iso3 == "GBR" & birth >= 1801L & birth <= 1960L &
    !is.na(bplo1) & !is.na(bpla1)
]
people_points <- st_as_sf(
  people,
  coords = c("bplo1", "bpla1"),
  crs = 4326,
  remove = FALSE
)
people_points <- st_transform(people_points, 27700)
people_joined <- st_join(
  people_points,
  lau[, c("LAU_ID", "LAU_NAME")],
  join = st_within,
  left = TRUE
)

unmatched_index <- which(is.na(people_joined$LAU_ID))
people_joined$inventor_match_method <- "point_within_lau"
people_joined$inventor_match_distance_m <- 0
if (length(unmatched_index) > 0L) {
  nearest <- st_nearest_feature(people_joined[unmatched_index, ], lau)
  distance <- as.numeric(st_distance(
    people_joined[unmatched_index, ], lau[nearest, ], by_element = TRUE
  ))
  accept <- distance <= 500
  rows <- unmatched_index[accept]
  people_joined$LAU_ID[rows] <- lau$LAU_ID[nearest[accept]]
  people_joined$LAU_NAME[rows] <- lau$LAU_NAME[nearest[accept]]
  people_joined$inventor_match_method[rows] <- "nearest_lau_within_500m"
  people_joined$inventor_match_distance_m[rows] <- distance[accept]
}

people_dt <- as.data.table(st_drop_geometry(people_joined))
people_unmatched <- people_dt[is.na(LAU_ID)]
people_unmatched[, `:=`(
  inventor_match_method = "unmatched_no_lau_polygon_within_500m",
  inventor_match_distance_m = NA_real_
)]
people_matched <- people_dt[!is.na(LAU_ID)]
inventors <- people_matched[, .(
  n_inventors = .N,
  n_stem = sum(stem == 1L, na.rm = TRUE),
  n_nonstem = sum(stem != 1L | is.na(stem), na.rm = TRUE)
), by = .(lau_id = LAU_ID, year = as.integer(birth))]

inventor_panel <- merge(
  annual[year <= 1960L],
  inventors,
  by = c("lau_id", "year"),
  all.x = TRUE,
  sort = FALSE
)
inventor_panel[is.na(n_inventors), `:=`(
  n_inventors = 0L,
  n_stem = 0L,
  n_nonstem = 0L
)]
inventor_panel[, `:=`(
  unit_type = "europe_lau",
  unit_id = paste0("GISCO_LAU_", lau_id),
  GEOID = NA_character_,
  city_geonameid = NA_integer_,
  place_name = lau_name,
  place_name_ascii = lau_name,
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
  population_source = "UK urban census harmonized to GISCO 2019 LAUs",
  population_interp_status = population_quality,
  match_status = "matched",
  match_method = "birth_point_within_gisco_2019_lau",
  match_distance_km = NA_real_,
  match_needs_review = FALSE,
  source_panel = "uk_lau_inventor_panel_1801_1960_census_population"
)]

inventor_columns <- c(
  "unit_type", "unit_id", "GEOID", "lau_id", "city_geonameid",
  "place_name", "place_name_ascii", "country", "iso3", "lat", "lon",
  "year", "n_inventors", "n_stem", "n_nonstem", "any_inventor",
  "any_stem", "log1p_n_inventors", "log1p_n_stem", "population",
  "population_original", "population_source", "population_interp_status",
  "inventors_per_100k_pop", "stem_per_100k_pop", "inventors_per_1000_pop",
  "stem_per_1000_pop", "match_status", "match_method", "match_distance_km",
  "match_needs_review", "source_panel", "population_interpolated",
  "n_source_units", "share_area_weighted"
)
inventor_panel <- inventor_panel[, ..inventor_columns]
setorder(inventor_panel, lau_id, year)

###############################################################################
# Final validation and writes
###############################################################################

if (nrow(observed) != 348L * length(census_years) ||
    observed[, anyDuplicated(paste(lau_id, census_year))] > 0L) {
  stop("Observed LAU panel is not a complete unique LAU-census-year skeleton.")
}
if (nrow(annual) != 348L * length(annual_years) ||
    annual[, anyDuplicated(paste(lau_id, year))] > 0L) {
  stop("Annual LAU panel is not a complete unique LAU-year skeleton.")
}
if (nrow(inventor_panel) != 348L * length(1801L:1960L) ||
    inventor_panel[, anyDuplicated(paste(lau_id, year))] > 0L) {
  stop("Alternative inventor panel is not a complete unique LAU-year skeleton.")
}
if (observed[!is.na(population), any(population < 0)] ||
    annual[!is.na(population), any(population < 0)]) {
  stop("Negative population found in final panels.")
}

fwrite(observed, observed_file)
fwrite(annual, annual_file)
fwrite(allocation_audit, allocation_file)
fwrite(quality, quality_file)
fwrite(transition, transition_file)
fwrite(inventor_panel, inventor_panel_file)
fwrite(people_unmatched, inventor_unmatched_file)

cat("\nCompleted UK LAU urban-population harmonization.\n")
cat("Observed panel: ", observed_file, "\n", sep = "")
cat("Annual panel: ", annual_file, "\n", sep = "")
cat("Allocation audit: ", allocation_file, "\n", sep = "")
cat("Alternative inventor panel: ", inventor_panel_file, "\n", sep = "")
cat("\nCoverage by census year:\n")
print(observed[, .(
  laus_with_population = sum(!is.na(population_observed)),
  total_population = sum(population_observed, na.rm = TRUE)
), by = census_year])
cat("\nNomis boundary links requiring review:\n")
print(nomis_links[, .(
  districts = .N,
  requiring_review = sum(boundary_link_needs_review, na.rm = TRUE),
  max_name_distance = max(boundary_name_distance, na.rm = TRUE)
), by = census_year])
