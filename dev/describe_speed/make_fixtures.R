## Records what the describe functions return for the calls in
## tests/testthat/helper-describe-speed.R, so that test-describe-speed.R can
## hold a faster grouping to exactly the same results and messages.
##
## Run once, from the package root, on the commit BEFORE the change it guards,
## with a clean working tree apart from the helper and this script:
##   Rscript dev/describe_speed/make_fixtures.R <commit>
## where <commit> is that commit's hash; it is stored with the record, and the
## script stops if the package code differs from it. Record it again if the
## approved wording of a message or the n_inf column changes.
args <- commandArgs(TRUE)
if (length(args) < 1L) stop("give the commit the record is made on")
commit <- args[1]
changed <- system2("git", c("diff", "--name-only", commit, "--", "R", "DESCRIPTION", "NAMESPACE"),
                   stdout = TRUE)
if (length(changed))
  stop("the package code differs from ", commit, ": ", paste(changed, collapse = ", "))
suppressMessages(pkgload::load_all(".", quiet = TRUE))
source(file.path("tests", "testthat", "helper-describe-speed.R"))
d <- describe_speed_data()
record <- lapply(describe_speed_calls, describe_speed_run, d = d)
attr(record, "recorded_on") <- commit
attr(record, "illumex") <- as.character(utils::packageVersion("illumex"))
attr(record, "r") <- paste(R.version$major, R.version$minor, sep = ".")
out <- file.path("tests", "testthat", "fixtures", "describe_speed.rds")
saveRDS(record, out, version = 3)
cat("recorded", length(record), "calls on", commit, "to", out, "\n")
