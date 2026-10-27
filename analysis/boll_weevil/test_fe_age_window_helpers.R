suppressPackageStartupMessages(library(data.table))
arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
root <- normalizePath(file.path(dirname(sub("^--file=", "", arg[1])), "../.."), winslash = "/")
source(file.path(root, "analysis/boll_weevil/fe_age_window_helpers.R"))
x <- bwa_indicators(4:30)
stopifnot(ncol(x) == 15L, all(rowSums(x)[1:15] == 1L), all(rowSums(x)[16:27] == 0L))
stopifnot(x$age_14[11] == 1L, x$age_13[11] == 0L,
          inherits(try(bwa_indicators(c(3,31)), silent = TRUE), "try-error"))
L <- bwa_contrast_matrix()
stopifnot(all(abs(rowSums(L)[1:5] - 1) < 1e-12), abs(sum(L[6,])) < 1e-12,
          L["14-18","age_13"] == 0, all(L["14-18",paste0("age_",14:18)] == .2),
          all(L[6,] == L["14-18",] - L["4-9",]))
beta <- setNames(rep(1,15), colnames(L))
V <- matrix(1,15,15,dimnames=list(names(beta),names(beta)))
r <- bwa_inference(beta,V)
# Perfectly correlated annual effects must retain variance one when averaged;
# the difference of identically correlated averages has zero variance.
stopifnot(max(abs(r$table$se[1:5] - 1)) < 1e-12, r$table$se[6] < 1e-7,
          abs(r$covariance["13-18","16-18"] - 1) < 1e-12)
p <- c(.04,.001,.2,.05,.01,.8)
stopifnot(max(abs(p.adjust(p,"holm") - c(.16,.006,.4,.16,.05,.8))) < 1e-12)
V <- diag(15);dimnames(V) <- list(names(beta),names(beta))
r <- bwa_inference(setNames(4:18,names(beta)),V)
stopifnot(abs(r$table[contrast=="14-18",estimate] - 16) < 1e-12,
          abs(r$table[contrast=="14-18_minus_4-9",estimate] - 9.5) < 1e-12,
          abs(r$covariance["13-18","16-18"] - 1/6) < 1e-12,
          all(r$table$p_holm >= r$table$p_value))
cat("PASS: age boundaries, older reference, equal-age contrasts, overlapping covariance, differences, and six-test Holm adjustment.\n")
