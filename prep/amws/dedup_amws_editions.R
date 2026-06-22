###############################################################################
# Cross-edition dedup for AMWS 1906 + 1938 + 1955.
#
# Reads each edition's geocoded_final + the corresponding cleaned/raw file to
# recover (last_name, first_initial, birth_year). Builds a person key
# (norm_last + first_init + birth_year + state), drops repeats across editions
# (keep earliest), and writes a combined geocoded file with edition tag.
#
# Outputs:
#   output/amws_combined_us_geocoded.csv
#   output/amws_combined_dedup_report.txt
###############################################################################
suppressPackageStartupMessages({
  library(data.table); library(readxl); library(haven); library(readr)
})
script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_arg)) {
  script_path <- normalizePath(sub("^--file=", "", script_arg[1]), winslash = "/", mustWork = TRUE)
  repo_root <- normalizePath(file.path(dirname(script_path), "..", ".."), winslash = "/", mustWork = TRUE)
} else {
  repo_root <- normalizePath(Sys.getenv("GTL_REPO", unset = getwd()), winslash = "/", mustWork = TRUE)
}
source(file.path(repo_root, "paths.R"))

out_root <- AMWS_OUTPUT
in_root  <- AMWS_INPUT

norm_last <- function(x) {
  x <- iconv(x, to = "ASCII//TRANSLIT", sub = "")
  x <- tolower(trimws(x))
  x <- gsub("[^a-z]", "", x)
  x
}
first_init <- function(x) {
  x <- iconv(x, to = "ASCII//TRANSLIT", sub = "")
  x <- toupper(trimws(x))
  substr(gsub("[^A-Z]", "", x), 1, 1)
}

# ---- 1906: name from raw xlsx (col 2 = amsname), birth_year from cleaned ----
raw06 <- as.data.table(read_excel(file.path(in_root, "amws_1906.xlsx"),
                                  sheet = 1, col_names = FALSE))
setnames(raw06, paste0("c", 1:10))
raw06[, lineid := as.integer(c1)]
raw06[, amsname := c2]
# Split "LAST, FIRST..." -> last/first
raw06[, last  := trimws(sub(",.*", "", amsname))]
raw06[, first := trimws(sub("^[^,]*,\\s*", "", amsname))]
names06 <- raw06[, .(lineid, last06 = last, first06 = first)]

cl06 <- fread(file.path(out_root, "amws_1906_cleaned.csv"))[, .(lineid, birth_year)]
g06  <- fread(file.path(out_root, "amws_1906_us_geocoded_final.csv"))
e06 <- merge(g06, cl06, by = "lineid", all.x = TRUE)
e06 <- merge(e06, names06, by = "lineid", all.x = TRUE)
e06[, edition := 1906L]
e06[, last := last06][, first := first06][, c("last06","first06") := NULL]

# ---- 1938: AMSname from cleaned, birth_year from cleaned -------------------
cl38 <- fread(file.path(out_root, "amws_1938_cleaned.csv"))[, .(lineid, AMSname, birth_year)]
cl38[, last  := trimws(sub(",.*", "", AMSname))]
cl38[, first := trimws(sub("^[^,]*,\\s*", "", AMSname))]
g38  <- fread(file.path(out_root, "amws_1938_us_geocoded_final.csv"))
e38 <- merge(g38, cl38[, .(lineid, birth_year, last, first)], by = "lineid", all.x = TRUE)
e38[, edition := 1938L]

# ---- 1955: names and canonical birth_year from split.csv -------------------
sp55 <- fread(file.path(out_root, "amws_1955_split.csv"))[, .(
  lineid, last, first, birth_year
)]
g55 <- fread(file.path(out_root, "amws_1955_us_geocoded_final.csv"))
e55 <- merge(g55, sp55, by = "lineid", all.x = TRUE)
e55[, edition := 1955L]

# ---- Stack and build person key --------------------------------------------
keep_cols <- c("edition","lineid","last","first","birth_year",
               "city","state","geoid","county_name","lat","lon",
               "match_source","flag","birthplace_orig")
for (dt in list(e06, e38, e55)) {
  miss <- setdiff(keep_cols, names(dt))
  if (length(miss)) for (m in miss) dt[, (m) := NA]
}
all <- rbindlist(list(e06[, ..keep_cols], e38[, ..keep_cols], e55[, ..keep_cols]),
                 use.names = TRUE)
all[, norm_last := norm_last(last)]
all[, finit     := first_init(first)]

# Person key: norm_last + finit + birth_year + state.
# Tolerance: birth_year ±1 — implement by rounding to nearest 2-year bucket
# AND linking adjacent buckets (try both rounded down and rounded up).
all[, key_lo := paste(norm_last, finit, birth_year,     state, sep = "|")]
all[, key_hi := paste(norm_last, finit, birth_year + 1, state, sep = "|")]

setorder(all, edition, lineid)
# Mark first-seen by either key
seen <- new.env(hash = TRUE)
keep <- logical(nrow(all))
for (i in seq_len(nrow(all))) {
  if (is.na(all$norm_last[i]) || all$norm_last[i] == "" ||
      is.na(all$birth_year[i]) || is.na(all$state[i])) {
    keep[i] <- TRUE; next                    # missing key -> keep, can't match
  }
  k1 <- all$key_lo[i]
  k2 <- all$key_hi[i]
  k3 <- paste(all$norm_last[i], all$finit[i], all$birth_year[i] - 1,
              all$state[i], sep = "|")
  if (exists(k1, envir = seen) || exists(k2, envir = seen) ||
      exists(k3, envir = seen)) {
    keep[i] <- FALSE
  } else {
    keep[i] <- TRUE
    assign(k1, TRUE, envir = seen)
  }
}
all[, kept := keep]

# ---- Report ----------------------------------------------------------------
n_ed <- all[, .N, by = edition][order(edition)]
n_kept <- all[kept == TRUE, .N, by = edition][order(edition)]
report <- c(
  sprintf("AMWS cross-edition dedup (1906 + 1938 + 1955)"),
  sprintf("  Person key: norm_last + first_initial + birth_year(±1) + state"),
  sprintf("  Missing key -> always kept"),
  "",
  sprintf("  Rows per edition (pre-dedup):"),
  sprintf("    1906: %d", n_ed[edition == 1906, N]),
  sprintf("    1938: %d", n_ed[edition == 1938, N]),
  sprintf("    1955: %d", n_ed[edition == 1955, N]),
  sprintf("    TOTAL pre-dedup:  %d", nrow(all)),
  "",
  sprintf("  Rows per edition (post-dedup, kept = earliest appearance):"),
  sprintf("    1906: %d", n_kept[edition == 1906, N]),
  sprintf("    1938: %d", n_kept[edition == 1938, N]),
  sprintf("    1955: %d", n_kept[edition == 1955, N]),
  sprintf("    TOTAL post-dedup: %d", sum(all$kept)),
  sprintf("    Dropped as duplicates: %d", sum(!all$kept))
)
writeLines(report, file.path(out_root, "amws_combined_dedup_report.txt"))
cat(paste(report, collapse = "\n"), "\n")

# ---- Write combined --------------------------------------------------------
fwrite(all, file.path(out_root, "amws_combined_us_geocoded.csv"))
cat("\nwrote", file.path(out_root, "amws_combined_us_geocoded.csv"), "\n")
