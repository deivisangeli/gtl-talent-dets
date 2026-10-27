# Build the 1910 couple and man-level files for the professor-fertility analysis.
#
# Two ways of counting a professor's children, kept side by side:
#
#   1. Mother-reported, wife-linked. CHBORN (ever born) and CHSURV (surviving)
#      were asked only of ever-married women, so a male professor's count is his
#      wife's, reached through SPLOC. These count children wherever they live.
#   2. Father-reported, direct. Persons whose POPLOC points at the man. Needs no
#      wife and covers widowers, but sees only co-resident children, so the
#      headline counts are restricted to children under 5 and under 10.
#
# Decoding (IPUMS codes are raw in the Parquet):
#   chborn  = CHBORN - 1 for codes 01-96; NA for 00 (N/A) and 97-99 (unknown,
#             illegible, missing)
#   chsurv  = CHSURV - 1 for codes 01+;   NA for 00
#   durmarr = DURMARR for codes 00-97;    NA for 98-99 (99 is also never married)
#
# Outputs (OCC_FERTILITY_OUT_DIR, default <Dropbox>/output/occupational_fertility):
#   couples_1910.parquet           one row per reciprocal SPLOC couple
#   men_own_children_1910.parquet  one row per man aged 20-64
#     Both key on STATEFIP x SERIAL x PERNUM; HISTID is left out to keep them
#     small in Dropbox and can be joined back from the source Parquet.
#   female_professors_1910.csv     every woman with OCC1950 010-029
#   linkage_diagnostics.csv        per group x MARST: link and household counts
#   occupation_groups.csv          the OCC1950 -> group map used
#   build_manifest.json
#
# Run from any working directory:
#   Rscript prep/occupational_fertility/build_professor_fertility.R
# Inputs: IPUMS_1910_FERTILITY_PARQUET_DIR (default D:/ipums_1910_fullcount_fertility).

suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
})

repo_root <- local({
  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  candidate <- if (length(file_arg)) {
    dirname(dirname(normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/")))
  } else {
    Sys.getenv("GTL_REPO", unset = getwd())
  }
  while (!file.exists(file.path(candidate, "paths.R"))) {
    parent <- dirname(candidate)
    if (identical(parent, candidate)) stop("Could not locate the repo root (no paths.R found)")
    candidate <- parent
  }
  normalizePath(candidate, winslash = "/")
})

IN_DIR <- Sys.getenv("IPUMS_1910_FERTILITY_PARQUET_DIR",
                     unset = "D:/ipums_1910_fullcount_fertility")
OUT_DIR <- Sys.getenv("OCC_FERTILITY_OUT_DIR", unset = "")
if (!nzchar(OUT_DIR)) {
  data_root <- local({
    env <- new.env()
    suppressWarnings(try(sys.source(file.path(repo_root, "paths.R"), envir = env), silent = TRUE))
    candidates <- c(
      get0("TALENT_DETS_DATA_DIR", envir = env, ifnotfound = ""),
      Sys.getenv("DET_DIR"),
      file.path("C:/Users", Sys.info()[["user"]], "Globtalent Dropbox/gtl_talent_dets")
    )
    hit <- candidates[nzchar(candidates) & dir.exists(candidates)]
    if (!length(hit)) stop("Could not locate the Dropbox data root. Set TALENT_DETS_DATA_DIR or DET_DIR.")
    normalizePath(hit[1], winslash = "/", mustWork = TRUE)
  })
  OUT_DIR <- file.path(data_root, "output/occupational_fertility")
}
THREADS <- as.integer(Sys.getenv("OCC_FERTILITY_THREADS", unset = "12"))

source(file.path(repo_root, "prep/occupational_fertility/occupation_groups.R"))

say <- function(...) message(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "  ", ...)
stop_if <- function(condition, ...) if (isTRUE(condition)) stop(..., call. = FALSE)
sql_path <- function(path) gsub("\\\\", "/", path)
q <- function(con, sql) DBI::dbGetQuery(con, sql)
x <- function(con, sql) invisible(DBI::dbExecute(con, sql))

# DuckDB identifiers are case-insensitive, so the decoded columns cannot be
# called chborn/chsurv/durmarr next to the raw CHBORN/CHSURV/DURMARR: a
# reference to the decoded name would silently read the raw code. They carry a
# dec_ prefix in the working table and get their plain names on output.
decode_sql <- paste(
  "SELECT *,",
  "CASE WHEN CHBORN BETWEEN 1 AND 96 THEN CHBORN - 1 END AS dec_chborn,",
  "CASE WHEN CHSURV >= 1 THEN CHSURV - 1 END AS dec_chsurv,",
  "CASE WHEN DURMARR BETWEEN 0 AND 97 THEN DURMARR END AS dec_durmarr,",
  "(BPL BETWEEN 1 AND 99) AS us_born,",
  group_case(), "AS occ_group",
  "FROM src")

main <- function() {
  started <- Sys.time()
  glob <- sql_path(file.path(IN_DIR, "STATEFIP=*", "*.parquet"))
  stop_if(!length(Sys.glob(glob)), "No Parquet files under ", IN_DIR)
  dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  x(con, paste0("SET threads = ", THREADS))
  x(con, "SET memory_limit = '20GB'")
  x(con, "SET preserve_insertion_order = false")
  spill <- file.path(tempdir(), "occ_fertility_spill")
  dir.create(spill, showWarnings = FALSE)
  x(con, sprintf("SET temp_directory = '%s'", sql_path(spill)))

  x(con, sprintf(paste(
    "CREATE VIEW src AS SELECT CAST(STATEFIP AS INTEGER) AS STATEFIP, * EXCLUDE (STATEFIP)",
    "FROM read_parquet('%s', hive_partitioning = true)"), glob))
  say("Decoding persons")
  x(con, paste("CREATE TABLE p AS", decode_sql))
  n_persons <- q(con, "SELECT COUNT(*) AS n FROM p")$n
  say("Persons: ", format(n_persons, big.mark = ","))
  unmapped <- q(con, "SELECT COUNT(*) AS n FROM p WHERE occ_group IS NULL")$n
  stop_if(unmapped > 0, unmapped, " persons have an OCC1950 code outside every group")

  # ---- Way 2: own children via the father and mother pointers -------------- #
  say("Counting own children by POPLOC and MOMLOC")
  own_children <- function(pointer) paste(
    "SELECT STATEFIP, SERIAL,", pointer, "AS PERNUM,",
    "COUNT(*) AS own_children_all,",
    "COUNT(*) FILTER (WHERE AGE <= 4) AS own_children_u5,",
    "COUNT(*) FILTER (WHERE AGE <= 9) AS own_children_u10,",
    "COUNT(*) FILTER (WHERE STEPPOP = 0) AS own_children_all_bio,",
    "COUNT(*) FILTER (WHERE AGE <= 4 AND STEPPOP = 0) AS own_children_u5_bio,",
    "COUNT(*) FILTER (WHERE AGE <= 9 AND STEPPOP = 0) AS own_children_u10_bio",
    "FROM p WHERE", pointer, "> 0 GROUP BY ALL")
  x(con, paste("CREATE TABLE kids_father AS", own_children("POPLOC")))
  # STEPPOP describes the father, so the mother side keeps only the totals.
  x(con, paste(
    "CREATE TABLE kids_mother AS SELECT STATEFIP, SERIAL, MOMLOC AS PERNUM,",
    "COUNT(*) AS own_children_all, COUNT(*) FILTER (WHERE AGE <= 4) AS own_children_u5,",
    "COUNT(*) FILTER (WHERE AGE <= 9) AS own_children_u10",
    "FROM p WHERE MOMLOC > 0 GROUP BY ALL"))

  kid_cols <- c("own_children_all", "own_children_u5", "own_children_u10",
                "own_children_all_bio", "own_children_u5_bio", "own_children_u10_bio")
  zero_fill <- function(alias, cols, prefix = "") paste0(
    "COALESCE(", alias, ".", cols, ", 0) AS ", prefix, cols, collapse = ", ")

  say("Building the man-level file")
  x(con, paste(
    "CREATE TABLE men AS SELECT",
    "m.STATEFIP, m.SERIAL, m.PERNUM, m.COUNTYICP, m.URBAN, m.GQ,",
    "m.RELATE, m.RELATED, m.AGE, m.MARST, m.dec_durmarr AS durmarr, m.SPLOC, m.NCHILD,",
    "m.RACE, m.BPL, m.us_born, m.OCC1950, m.occ_group,",
    "(m.GQ IN (1, 2)) AS in_household,", zero_fill("k", kid_cols),
    "FROM p m LEFT JOIN kids_father k USING (STATEFIP, SERIAL, PERNUM)",
    "WHERE m.SEX = 1 AND m.AGE BETWEEN 20 AND 64"))

  # ---- Way 1: SPLOC couples ------------------------------------------------- #
  say("Linking spouses through SPLOC")
  link <- q(con, paste(
    "SELECT COUNT(*) AS husbands_with_sploc,",
    "COUNT(w.PERNUM) AS spouse_found,",
    "COUNT(*) FILTER (WHERE w.SPLOC = h.PERNUM) AS reciprocal,",
    "COUNT(*) FILTER (WHERE w.SEX = 2) AS spouse_female,",
    "COUNT(*) FILTER (WHERE h.MARST <> 1) AS husband_not_marst1",
    "FROM p h LEFT JOIN p w ON w.STATEFIP = h.STATEFIP AND w.SERIAL = h.SERIAL",
    "AND w.PERNUM = h.SPLOC WHERE h.SEX = 1 AND h.SPLOC > 0"))
  marst1_no_sploc <- q(con,
    "SELECT COUNT(*) AS n FROM p WHERE SEX = 1 AND MARST = 1 AND SPLOC = 0")$n
  say("Husbands with SPLOC: ", format(link$husbands_with_sploc, big.mark = ","),
      "; reciprocal ", format(link$reciprocal, big.mark = ","),
      "; MARST=1 without SPLOC ", format(marst1_no_sploc, big.mark = ","))
  stop_if(link$spouse_found != link$husbands_with_sploc,
          "SPLOC points at a PERNUM that is not in the household")

  x(con, paste(
    "CREATE TABLE couples AS SELECT",
    "h.STATEFIP, h.SERIAL, h.COUNTYICP, h.URBAN, h.GQ,",
    "h.PERNUM AS h_pernum, h.AGE AS h_age, h.MARST AS h_marst,",
    "h.RELATE AS h_relate, h.RELATED AS h_related, h.RACE AS h_race, h.BPL AS h_bpl,",
    "h.us_born AS h_us_born, h.OCC1950 AS h_occ1950, h.occ_group,",
    "w.PERNUM AS w_pernum, w.AGE AS w_age, w.MARST AS w_marst,",
    "w.RACE AS w_race, w.BPL AS w_bpl, w.us_born AS w_us_born, w.OCC1950 AS w_occ1950,",
    "w.CHBORN AS w_chborn_raw, w.CHSURV AS w_chsurv_raw,",
    "w.dec_chborn AS chborn, w.dec_chsurv AS chsurv, w.dec_durmarr AS durmarr,",
    "h.dec_durmarr AS h_durmarr,",
    "w.AGE - w.dec_durmarr AS w_age_at_marriage, h.AGE - w.dec_durmarr AS h_age_at_marriage,",
    "(w.dec_chsurv > w.dec_chborn) AS flag_surv_gt_born,",
    "h.SPRULE_HIST,",
    zero_fill("kf", kid_cols, "h_"), ",",
    "COALESCE(km.own_children_all, 0) AS w_own_children_all,",
    "COALESCE(km.own_children_u5, 0) AS w_own_children_u5,",
    "COALESCE(km.own_children_u10, 0) AS w_own_children_u10",
    "FROM p h JOIN p w ON w.STATEFIP = h.STATEFIP AND w.SERIAL = h.SERIAL",
    "AND w.PERNUM = h.SPLOC AND w.SPLOC = h.PERNUM AND w.SEX = 2",
    "LEFT JOIN kids_father kf ON kf.STATEFIP = h.STATEFIP AND kf.SERIAL = h.SERIAL",
    "AND kf.PERNUM = h.PERNUM",
    "LEFT JOIN kids_mother km ON km.STATEFIP = w.STATEFIP AND km.SERIAL = w.SERIAL",
    "AND km.PERNUM = w.PERNUM",
    "WHERE h.SEX = 1 AND h.SPLOC > 0"))
  dup <- q(con, paste(
    "SELECT (SELECT COUNT(*) FROM couples) - (SELECT COUNT(*) FROM",
    "(SELECT DISTINCT STATEFIP, SERIAL, w_pernum FROM couples)) AS n"))$n
  stop_if(dup != 0, dup, " wives are linked to more than one husband")
  n_couples <- q(con, "SELECT COUNT(*) AS n FROM couples")$n
  say("Couples: ", format(n_couples, big.mark = ","))

  # ---- Diagnostics ---------------------------------------------------------- #
  say("Writing diagnostics")
  diagnostics <- q(con, paste(
    "SELECT m.occ_group, m.MARST, COUNT(*) AS men,",
    "COUNT(*) FILTER (WHERE m.SPLOC > 0) AS with_sploc,",
    "COUNT(c.h_pernum) AS linked_couples,",
    "COUNT(*) FILTER (WHERE c.chsurv IS NOT NULL) AS wife_chsurv_known,",
    "COUNT(*) FILTER (WHERE c.chborn IS NOT NULL) AS wife_chborn_known,",
    "COUNT(*) FILTER (WHERE m.RELATE = 1) AS head,",
    "COUNT(*) FILTER (WHERE m.RELATED IN (1201, 1202, 1203, 1204, 1241)) AS boarder_lodger,",
    "COUNT(*) FILTER (WHERE m.GQ IN (3, 4)) AS group_quarters,",
    "COUNT(*) FILTER (WHERE c.h_pernum IS NOT NULL AND m.RELATE <> 1) AS linked_not_head",
    "FROM p m LEFT JOIN couples c ON c.STATEFIP = m.STATEFIP AND c.SERIAL = m.SERIAL",
    "AND c.h_pernum = m.PERNUM WHERE m.SEX = 1 GROUP BY ALL ORDER BY ALL"))
  utils::write.csv(diagnostics, file.path(OUT_DIR, "linkage_diagnostics.csv"),
                   row.names = FALSE)
  utils::write.csv(OCC_GROUPS, file.path(OUT_DIR, "occupation_groups.csv"), row.names = FALSE)

  nchild_check <- q(con, paste(
    "SELECT COUNT(*) AS men, COUNT(*) FILTER (WHERE LEAST(own_children_all, 9) <> NCHILD)",
    "AS nchild_mismatch FROM men"))
  fert_check <- q(con, paste(
    "SELECT COUNT(*) FILTER (WHERE flag_surv_gt_born) AS surv_gt_born,",
    "COUNT(*) FILTER (WHERE chsurv IS NULL) AS wife_chsurv_na,",
    "COUNT(*) FILTER (WHERE chborn IS NULL) AS wife_chborn_na,",
    "COUNT(*) FILTER (WHERE durmarr IS NULL) AS wife_durmarr_na FROM couples"))

  female_prof <- q(con, paste(
    "SELECT STATEFIP, SERIAL, PERNUM, HISTID, COUNTYICP, URBAN, GQ, RELATE, RELATED,",
    "AGE, MARST, dec_durmarr AS durmarr, RACE, BPL, us_born, OCC1950, occ_group,",
    "CHBORN AS chborn_raw, CHSURV AS chsurv_raw, dec_chborn AS chborn, dec_chsurv AS chsurv,",
    "NCHILD FROM p WHERE SEX = 2 AND OCC1950 BETWEEN 10 AND 29",
    "ORDER BY STATEFIP, SERIAL, PERNUM"))
  utils::write.csv(female_prof, file.path(OUT_DIR, "female_professors_1910.csv"),
                   row.names = FALSE, na = "")

  say("Writing Parquet outputs to ", OUT_DIR)
  for (name in c("couples", "men")) {
    target <- file.path(OUT_DIR, paste0(name, if (name == "men") "_own_children" else "",
                                        "_1910.parquet"))
    if (file.exists(target)) file.remove(target)
    x(con, sprintf("COPY %s TO '%s' (FORMAT PARQUET, COMPRESSION ZSTD)", name,
                   sql_path(target)))
  }

  manifest <- list(
    completed_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    input = sql_path(normalizePath(IN_DIR, winslash = "/")),
    persons = n_persons,
    couples = n_couples,
    men_20_64 = nchild_check$men,
    female_professors = nrow(female_prof),
    sploc_link = as.list(link),
    married_spouse_present_without_sploc = marst1_no_sploc,
    nchild_mismatch_capped9 = nchild_check$nchild_mismatch,
    couples_fertility = as.list(fert_check),
    elapsed_minutes = round(as.numeric(difftime(Sys.time(), started, units = "mins")), 2))
  jsonlite::write_json(manifest, file.path(OUT_DIR, "build_manifest.json"),
                       pretty = TRUE, auto_unbox = TRUE, na = "null")
  say("Done: ", format(n_couples, big.mark = ","), " couples, ",
      format(nchild_check$men, big.mark = ","), " men aged 20-64, NCHILD mismatches ",
      nchild_check$nchild_mismatch)
}

main()
