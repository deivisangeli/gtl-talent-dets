args <- commandArgs(trailingOnly = TRUE)
script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
repo_root <- if (length(script_arg)) {
  wrapper_path <- normalizePath(sub("^--file=", "", script_arg[1]), winslash = "/", mustWork = TRUE)
  normalizePath(file.path(dirname(wrapper_path), "..", ".."), winslash = "/", mustWork = TRUE)
} else {
  normalizePath(Sys.getenv("GTL_REPO", unset = getwd()), winslash = "/", mustWork = TRUE)
}

target <- file.path(repo_root, "prep", "amws", "amws_1986", "support",
                    "apply_amws_ed16_remaining_regex_rule_corrections.R")
status <- system2("Rscript", c(shQuote(target), args))
if (!identical(status, 0L)) quit(save = "no", status = as.integer(status))
