## R/shared-helpers.R is the same, byte for byte, in illumex and illume.
## This compares git's record of the file (line endings as stored, whatever a
## checkout does to them) in this repository with the other package's, each
## at the ref given, and says which lines differ when they do.
##
## Run from illumex's root:
##   Rscript dev/check_shared_helpers.R <illume repository> [this ref] [illume ref]
## The refs default to origin/main in both.
a <- commandArgs(TRUE)
if (!length(a)) stop("give the path to the illume repository", call. = FALSE)
other <- a[1]
refs <- c(if (length(a) >= 2L) a[2] else "origin/main", if (length(a) >= 3L) a[3] else "origin/main")
f <- "R/shared-helpers.R"
git <- function(repo, ...) suppressWarnings(system2("git", c("-C", shQuote(repo), ...),
                                                    stdout = TRUE, stderr = FALSE))
blob <- c(illumex = git(".", "rev-parse", paste0(refs[1], ":", f)),
          illume = git(other, "rev-parse", paste0(refs[2], ":", f)))
cat(sprintf("%-8s %-12s %s\n", names(blob), refs, blob), sep = "")
if (length(blob) < 2L || any(!nzchar(blob)))
  stop("the file is missing at one of the refs", call. = FALSE)
if (identical(blob[[1]], blob[[2]])) {
  cat("identical\n")
} else {
  x <- git(".", "show", paste0(refs[1], ":", f)); y <- git(other, "show", paste0(refs[2], ":", f))
  d <- which(x[seq_len(min(length(x), length(y)))] != y[seq_len(min(length(x), length(y)))])
  cat("DIFFERENT: ", length(x), " and ", length(y), " lines; first differing lines: ",
      paste(utils::head(d, 10), collapse = ", "), "\n", sep = "")
  quit(status = 1)
}
