###############################################################################
# Prep per-school JSONL inputs for the enrollment-research subagent dispatch.
# Each batch = ONE school. Agent fills year10/year20/year30 seats.
#
# Schools to research: all 13 high-access strict + all 57 low-access (private
# tuition, secondary, in-frame, founded <= 1910). Total 70 schools.
#
# Outputs:
#   prep/output/enrollment_research/in/<key>.jsonl    (one input per school)
#   prep/output/enrollment_research/out/<key>.jsonl   (agent writes here)
###############################################################################
suppressPackageStartupMessages({library(data.table); library(jsonlite)})
source("../paths.R")

s <- fread(file.path(SCHOOLS_OUTPUT, "elite_high_schools_core_1800_1930.csv"))
existing <- fread(file.path(SCHOOLS_OUTPUT, "elite_high_schools_enrollment.tsv"))

# 13 high-access strict
hi <- s[crit_high_access_strict == "yes",
        .(school, state_abbr, founding_year_used, city, county_name,
          group = "high_access")]
# 57 low-access private-tuition in frame, founded <= 1910
la <- s[access_model_historical_prelim == "private_tuition_dominant" &
        crit_secondary_school == "yes" &
        crit_in_frame_1800_1940 == "yes" &
        founding_year_used <= 1910,
        .(school, state_abbr, founding_year_used, city, county_name,
          group = "low_access")]
schools <- rbind(hi, la)
schools <- unique(schools, by = c("school","state_abbr"))   # safety
cat("schools to research:", nrow(schools), " (",
    sum(schools$group == "high_access"), "high-access +",
    sum(schools$group == "low_access"), "low-access )\n")

# Merge in any existing year10_point so the agent can use it as a hint
schools <- merge(schools,
                 existing[, .(school, state_abbr,
                              prior_year10_point = year10_point,
                              prior_year10_year  = year10_year,
                              prior_circa1900_point = circa1900_point,
                              prior_note = note)],
                 by = c("school","state_abbr"), all.x = TRUE)

out_root <- file.path(SCHOOLS_OUTPUT, "enrollment_research")
dir.create(file.path(out_root, "in"),  recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(out_root, "out"), recursive = TRUE, showWarnings = FALSE)

# Build a stable key per school for filename
schools[, key := tolower(paste0(state_abbr, "_",
                                 gsub("[^A-Za-z0-9]+", "_", school)))]
schools[, key := substr(key, 1, 80)]
fwrite(schools, file.path(out_root, "schools_to_research.csv"))

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a)) b else a

for (i in seq_len(nrow(schools))) {
  row <- schools[i]
  obj <- list(
    school          = unbox(row$school),
    state_abbr      = unbox(row$state_abbr),
    city            = unbox(row$city %||% ""),
    county          = unbox(row$county_name %||% ""),
    founding_year   = unbox(as.integer(row$founding_year_used)),
    group           = unbox(row$group),
    prior_year10    = unbox(if (is.na(row$prior_year10_point)) "" else as.integer(row$prior_year10_point)),
    prior_year10_yr = unbox(if (is.na(row$prior_year10_year))  "" else as.integer(row$prior_year10_year)),
    prior_circa1900 = unbox(if (is.na(row$prior_circa1900_point)) "" else as.integer(row$prior_circa1900_point)),
    prior_note      = unbox(row$prior_note %||% "")
  )
  writeLines(toJSON(obj, auto_unbox = FALSE),
             file.path(out_root, "in", paste0(row$key, ".jsonl")),
             useBytes = TRUE)
}
cat("wrote", nrow(schools), "input files to", file.path(out_root, "in"), "\n")
