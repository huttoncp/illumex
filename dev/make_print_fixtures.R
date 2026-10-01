## Record the printed output of every case in
## tests/testthat/helper-print-cases.R, from the code as it stands, into
## tests/testthat/fixtures/print/<case>.txt. Run before a change that must
## not alter what prints (results gaining a class; tables keeping full
## precision), and never after it: test-print-unchanged.R holds the changed
## code to these texts.
##
## Run from the package root:
##   Rscript dev/make_print_fixtures.R
suppressMessages(pkgload::load_all(".", quiet = TRUE, helpers = FALSE))
source(file.path("tests", "testthat", "helper-print-cases.R"))
out <- file.path("tests", "testthat", "fixtures", "print")
dir.create(out, showWarnings = FALSE, recursive = TRUE)
commit <- system2("git", c("rev-parse", "--short", "HEAD"), stdout = TRUE)
cases <- print_cases()
changed <- character(0)
for (nm in names(cases)) {
  txt <- print_text(cases[[nm]])
  f <- file.path(out, paste0(nm, ".txt"))
  before <- if (file.exists(f)) readLines(f, warn = FALSE, encoding = "UTF-8")
  if (!identical(before, txt)) changed <- c(changed, basename(f))
  writeLines(txt, f, useBytes = TRUE)
  cat(sprintf("%-26s %4d lines%s\n", nm, length(txt),
              if (basename(f) %in% changed) "  CHANGED" else ""))
}
## The README is the history of every deliberate re-recording, so it is
## appended to, never rewritten: one line naming the files whose text
## changed, to which the reason is added by hand. Nothing is added when no
## text changed.
if (length(changed)) {
  cat(paste0("re-recorded at ", commit, " by dev/make_print_fixtures.R, deliberately: ",
             paste(changed, collapse = ", "), " (why: add it here)\n"),
      file = file.path(out, "README"), append = TRUE)
  cat("appended to README:", paste(changed, collapse = ", "), "\n")
}
