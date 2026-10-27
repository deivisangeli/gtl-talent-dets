# Shared loading of American Men of Science 1956 from Kim & Moser's
# replication package (Econometrica, ECMA MS-22741), for
#   analysis/occupational_fertility/professor_fertility_mos1956.R
#   prep/occupational_fertility/build_professor_birth_schedule.R
#
# Only two CSVs are extracted from the package (the cleaned MoS 1956 file and
# the career-title dictionary), with SHA-256 provenance; none of its code is
# run. A professor is anyone who ever held a faculty rank (FACULTY below);
# blank `child` counts as 0, as Kim & Moser treat blanks as non-parents.
# Callers must define `say()` and `stop_if()`.

MOS1956_MEMBERS <- c(mos = "ECMA_MS-22741_Replication/01_Data/02_Clean/01_MoS 1956.csv",
                     titles = "ECMA_MS-22741_Replication/01_Data/03_Dictionaries/01_Career Titles.csv")
MOS1956_FACULTY <- c("Assistant Professor", "Associate Professor", "Professor",
                     "Medical Assistant Professor", "Medical Associate Professor", "Medical Professor",
                     "Department Head", "Medical Department Head", "Dean", "Professor Emeritus")
MOS1956_N_CAREER <- 60L
# The file as profiled on 2026-10-26: a different file fails here, not downstream.
MOS1956_EXPECTED_ROWS <- 82094L
MOS1956_EXPECTED_CHILD <- c(blank = 24121L, `1` = 13555L, `2` = 24018L, `3` = 13641L, `4` = 4949L)

mos1956_raw_dir <- function(data_root) {
  Sys.getenv("MOS1956_RAW_DIR", unset = file.path(data_root, "raw/kim_moser_mos1956"))
}

# Extract the two CSVs once, recording provenance.
mos1956_extract <- function(data_root,
                            zip = Sys.getenv("MOS1956_REPLICATION_ZIP",
                                             unset = "D:/ECMA_MS-22741_Replication.zip")) {
  raw_dir <- mos1956_raw_dir(data_root)
  local_path <- function(member) file.path(raw_dir, basename(member))
  if (all(file.exists(vapply(MOS1956_MEMBERS, local_path, "")))) return(invisible(raw_dir))
  stop_if(!file.exists(zip), "Replication package not found: ", zip)
  say("Extracting ", length(MOS1956_MEMBERS), " files from ", zip)
  dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
  utils::unzip(zip, files = MOS1956_MEMBERS, exdir = raw_dir, junkpaths = TRUE)
  jsonlite::write_json(list(
    extracted_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    zip = list(path = normalizePath(zip, winslash = "/"), bytes = unname(file.size(zip)),
               sha256 = digest::digest(file = zip, algo = "sha256")),
    files = lapply(MOS1956_MEMBERS, function(m) list(
      member = m, bytes = unname(file.size(local_path(m))),
      sha256 = digest::digest(file = local_path(m), algo = "sha256"))),
    source = paste("Kim & Moser, Women in Science: Lessons from the Baby Boom (Econometrica),",
                   "replication package ECMA MS-22741")),
    file.path(raw_dir, "provenance.json"), pretty = TRUE, auto_unbox = TRUE)
  invisible(raw_dir)
}

# One row per scientist with professor status, first faculty year, children and
# marriage, after validating the file; plus the faculty-rank counts.
mos1956_load <- function(data_root) {
  raw_dir <- mos1956_extract(data_root)
  local_path <- function(member) file.path(raw_dir, basename(member))
  career_cols <- c(paste0("careertitle", seq_len(MOS1956_N_CAREER)),
                   paste0("careerstartyear", seq_len(MOS1956_N_CAREER)))
  mos <- data.table::fread(local_path(MOS1956_MEMBERS[["mos"]]), encoding = "UTF-8", showProgress = FALSE,
                           select = c("unique_id", "birthyear", "yearofmarriage", "child", "gender",
                                      "field_visher", "academic", career_cols),
                           colClasses = list(character = paste0("careertitle", seq_len(MOS1956_N_CAREER))))
  titles <- data.table::fread(local_path(MOS1956_MEMBERS[["titles"]]), encoding = "UTF-8",
                              showProgress = FALSE, colClasses = "character")
  stop_if(nrow(mos) != MOS1956_EXPECTED_ROWS,
          "Expected ", MOS1956_EXPECTED_ROWS, " scientists; found ", nrow(mos))
  stop_if(data.table::uniqueN(mos$unique_id) != nrow(mos), "unique_id is not unique")
  stop_if(any(!is.na(mos$child) & (mos$child < 1 | mos$child > 16 | mos$child %% 1 != 0)),
          "child holds values outside whole numbers 1-16")
  child_counts <- c(blank = sum(is.na(mos$child)),
                    vapply(1:4, function(k) sum(mos$child == k, na.rm = TRUE), 1L))
  names(child_counts) <- names(MOS1956_EXPECTED_CHILD)
  stop_if(!identical(child_counts, MOS1956_EXPECTED_CHILD),
          "child distribution differs from the profiled file: ",
          paste(names(child_counts), child_counts, collapse = "; "))
  say(nrow(mos), " scientists read; child distribution as profiled")

  long <- data.table::melt(mos[, c("unique_id", career_cols), with = FALSE], id.vars = "unique_id",
                           measure.vars = data.table::patterns(title = "^careertitle",
                                                               start = "^careerstartyear"))
  long <- long[!is.na(title) & nzchar(title)]
  long[, clean := titles$careertitle_clean[match(title, titles$careertitle)]]
  spells <- long[clean %in% MOS1956_FACULTY,
                 .(professor_start = suppressWarnings(min(start, na.rm = TRUE))), by = unique_id]
  spells[!is.finite(professor_start), professor_start := NA_real_]
  rank_counts <- long[clean %in% MOS1956_FACULTY, .(entries = .N, scientists = data.table::uniqueN(unique_id)),
                      by = clean][order(-scientists)]

  d <- merge(mos[, .(unique_id, birthyear, yearofmarriage, child, gender, field_visher, academic)],
             spells, by = "unique_id", all.x = TRUE)
  d[, `:=`(professor = unique_id %in% spells$unique_id,
           children = data.table::fifelse(is.na(child), 0, child),
           married = !is.na(yearofmarriage),
           decade = as.integer(birthyear %/% 10 * 10))]
  d[, professor_by40 := professor & !is.na(professor_start) & professor_start <= birthyear + 40]
  stop_if(d[professor == TRUE, any(academic != 1)],
          "A professor is not academic in Kim & Moser's flag")
  list(d = d, rank_counts = rank_counts)
}
