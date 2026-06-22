script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_arg)) {
  script_path <- normalizePath(sub("^--file=", "", script_arg[1]), winslash = "/", mustWork = TRUE)
  repo_root <- normalizePath(file.path(dirname(script_path), "..", ".."), winslash = "/", mustWork = TRUE)
} else {
  repo_root <- normalizePath(Sys.getenv("GTL_REPO", unset = getwd()), winslash = "/", mustWork = TRUE)
}
source(file.path(repo_root, "prep", "amws", "state_alias.R"))
test <- c("N. Y", "N.Y", "N. Y.", "Mass", "Ohio", "Ohi", "N", "ny", "NY")
for (t in test) cat(sprintf("'%s' -> %s\n", t, normalize_state(t)))
cat("\nSTATE_ALIAS['n. y']:", STATE_ALIAS["n. y"], "\n")
cat("Names containing 'n. y':\n"); print(grep("^n\\.? ?y", names(STATE_ALIAS), value=TRUE))
