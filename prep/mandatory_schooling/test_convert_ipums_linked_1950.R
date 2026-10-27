# Small end-to-end fixture for the DDI-driven conversion and birthplace rules.

suppressPackageStartupMessages(library(xml2))

script <- file.path("prep", "mandatory_schooling", "convert_ipums_linked_1950.R")
ddi <- Sys.getenv("IPUMS_LINKED_1950_DDI", unset = "D:/usa_00003_ddi.xml")
stopifnot(file.exists(script), file.exists(ddi))
doc <- xml2::read_xml(ddi)
vars <- xml2::xml_find_all(doc, ".//*[local-name()='dataDscr']/*[local-name()='var']")
columns <- xml2::xml_attr(vars, "name")
stopifnot(length(columns) == 60L)

root <- file.path(tempdir(), "ipums_linked_1950_fixture")
dir.create(root, showWarnings = FALSE)
source_path <- file.path(root, "fixture.csv.gz")
output_path <- file.path(root, "parquet")
rows <- as.data.frame(matrix("", nrow = 13L, ncol = length(columns)),
                      stringsAsFactors = FALSE)
names(rows) <- columns
rows$YEAR <- c("1900", "1910", "1920", "1930", "1940", "1950", "1950",
               "1950", "1920", "1950", "1950", "1930", "1950")
rows$HISTID <- paste0("record-", seq_len(nrow(rows)))
rows$HIK <- c("person-a", "", "person-b", "person-a", "person-c",
              "person-a", "", "person-c", "person-d", "person-d",
              "person-d", "person-e", "person-e")
rows$BPL <- c("001", "004", "150", "001", "012", "006", "036", "013",
              "005", "012", "013", "001", "150")
rows$OCC1950 <- c("999", "061", "", "062", "068", "007", "069", "063",
                  "061", "063", "067", "999", "")
rows$PRESGL <- c("40.7", rep("", 12L))
rows$ERSCOR50 <- c("999.9", rep("", 12L))
rows$EDSCOR50 <- c("3.2", rep("", 12L))
connection <- gzfile(source_path, "wt")
utils::write.csv(rows, connection, row.names = FALSE, na = "")
close(connection)

Sys.setenv(IPUMS_LINKED_1950_SOURCE = source_path,
           IPUMS_LINKED_1950_DDI = ddi,
           IPUMS_LINKED_1950_PARQUET_DIR = output_path)
source(script, local = TRUE)

suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
})
con <- DBI::dbConnect(duckdb::duckdb())
on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
glob <- gsub("\\\\", "/", file.path(output_path, "birth_bpl=*", "*.parquet"))
actual <- DBI::dbGetQuery(con, sprintf(
  "SELECT HISTID, YEAR, BPL, birth_bpl, scientist_occ1950, PRESGL FROM read_parquet('%s', hive_partitioning=true) ORDER BY HISTID",
  glob))
stopifnot(nrow(actual) == 13L)
by_id <- setNames(seq_len(nrow(actual)), actual$HISTID)
stopifnot(actual$birth_bpl[by_id[["record-1"]]] == "006")
stopifnot(actual$birth_bpl[by_id[["record-2"]]] == "unknown")
stopifnot(actual$birth_bpl[by_id[["record-3"]]] == "unknown")
stopifnot(actual$birth_bpl[by_id[["record-4"]]] == "006")
stopifnot(actual$birth_bpl[by_id[["record-5"]]] == "013")
stopifnot(actual$birth_bpl[by_id[["record-6"]]] == "006")
stopifnot(actual$birth_bpl[by_id[["record-7"]]] == "036")
stopifnot(actual$birth_bpl[by_id[["record-8"]]] == "013")
stopifnot(actual$birth_bpl[by_id[["record-9"]]] == "unknown")
stopifnot(actual$birth_bpl[by_id[["record-10"]]] == "012")
stopifnot(actual$birth_bpl[by_id[["record-11"]]] == "013")
stopifnot(actual$birth_bpl[by_id[["record-12"]]] == "150")
stopifnot(actual$birth_bpl[by_id[["record-13"]]] == "150")
stopifnot(identical(actual$scientist_occ1950[by_id[["record-1"]]], FALSE))
stopifnot(identical(actual$scientist_occ1950[by_id[["record-2"]]], TRUE))
stopifnot(is.na(actual$scientist_occ1950[by_id[["record-3"]]]))
stopifnot(actual$PRESGL[by_id[["record-1"]]] == 40.7)
stopifnot(file.exists(file.path(output_path, "dictionary.md")),
          file.exists(file.path(output_path, "variable_dictionary.csv")),
          file.exists(file.path(output_path, "value_labels.csv")),
          file.exists(file.path(output_path, "_manifest.json")))
message("linked 1950 converter fixture passed")
