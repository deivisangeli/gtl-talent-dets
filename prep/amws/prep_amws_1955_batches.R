suppressPackageStartupMessages({
  library(haven)
  library(readr)
})
script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_arg)) {
  script_path <- normalizePath(sub("^--file=", "", script_arg[1]), winslash = "/", mustWork = TRUE)
  repo_root <- normalizePath(file.path(dirname(script_path), "..", ".."), winslash = "/", mustWork = TRUE)
} else {
  repo_root <- normalizePath(Sys.getenv("GTL_REPO", unset = getwd()), winslash = "/", mustWork = TRUE)
}
source(file.path(repo_root, "paths.R"))

f <- file.path(AMWS_INPUT, "amws_1955.dta")
out_dir <- file.path(AMWS_OUTPUT, "amws_1955_batches")
dir.create(file.path(out_dir, "in"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(out_dir, "out"), recursive = TRUE, showWarnings = FALSE)

d <- read_dta(f)
for (nm in names(d)) {
  if (is.character(d[[nm]])) {
    d[[nm]] <- iconv(d[[nm]], from = "latin1", to = "UTF-8", sub = "?")
  }
}

bp <- gsub("[\t\r\n]+", " ", d$birthplace)
raw <- data.frame(lineid = d$lineid, birthplace_orig = bp, stringsAsFactors = FALSE)

write_tsv(raw, file.path(out_dir, "amws_1955_raw.tsv"))

batch_size <- 25
n <- nrow(raw)
n_batches <- ceiling(n / batch_size)

for (b in seq_len(n_batches)) {
  rng <- ((b - 1) * batch_size + 1):min(b * batch_size, n)
  lines <- paste0(raw$lineid[rng], "\t", raw$birthplace_orig[rng])
  fp <- file.path(out_dir, "in", sprintf("%05d.tsv", b))
  writeLines(c("lineid\tbirthplace_orig", lines), fp, useBytes = TRUE)
}

cat("wrote", n, "rows in", n_batches, "batches to", file.path(out_dir, "in"), "\n")
