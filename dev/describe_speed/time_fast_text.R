## What fast = TRUE saves on one text column, where it leaves out the count of
## case variants and orders tied top counts by code point (request 46, its
## rulings of 16 Nov 2026). ilm_describe() at its default and with fast =
## TRUE, alternating within each repeat.
##
##   Rscript dev/describe_speed/time_fast_text.R <package tree> <rows> <reps> <out.csv>
##
## Three columns of made-up text: all distinct, mixed case ("all distinct");
## 1,000 distinct values repeated ("repeated"); and 50,000 distinct values
## in which about one in three has a case or spacing variant ("variants").
## Time is elapsed seconds; memory is what R allocated during the call
## (bench::bench_memory), not its peak.
args <- commandArgs(TRUE)
tree <- args[1]; n <- as.integer(args[2]); reps <- as.integer(args[3]); out <- args[4]
suppressMessages(pkgload::load_all(tree, quiet = TRUE))
set.seed(4603)
word <- function(k) vapply(seq_len(k), function(i)
  paste(sample(c(letters, LETTERS), 8, TRUE), collapse = ""), "")
base <- word(50000L)
var <- base
pick <- sample(length(base), length(base) %/% 3)
var[pick] <- ifelse(seq_along(pick) %% 2 == 0, toupper(base[pick]), paste0(base[pick], " "))
d <- data.frame(
  all_distinct = paste0(sample(c("a", "B", "_"), n, TRUE), seq_len(n)),
  repeated = sample(word(1000L), n, TRUE),
  variants = sample(c(base, var), n, TRUE),
  stringsAsFactors = FALSE)
rows <- list()
for (v in names(d)) {
  for (fast in c(FALSE, TRUE)) invisible(ilm_describe(d, v, fast = fast))   # warm up
  for (i in seq_len(reps)) for (fast in c(FALSE, TRUE)) {
    f <- function() ilm_describe(d, v, fast = fast)
    gc(FALSE)
    m <- as.numeric(bench::bench_memory(f())$mem_alloc)
    t <- as.numeric(bench::bench_time(f())[["real"]])
    rows[[length(rows) + 1L]] <- data.frame(column = v, fast = fast, rep = i, rows = n,
                                            seconds = t, mb = m / 2^20)
  }
}
r <- do.call(rbind, rows)
utils::write.csv(r, out, row.names = FALSE)
cat("rows", n, "|", reps, "repeats | R", as.character(getRversion()), "|",
    format(Sys.time(), "%d %b %Y %H:%M:%S"), "\n")
for (v in names(d)) {
  a <- r[r$column == v & !r$fast, ]; b <- r[r$column == v & r$fast, ]
  ta <- stats::median(a$seconds); tb <- stats::median(b$seconds)
  ma <- stats::median(a$mb); mb <- stats::median(b$mb)
  cat(sprintf("%-13s default %6.3f s %8.1f MB | fast %6.3f s %8.1f MB | time %+6.1f%%  memory %+6.1f%%\n",
              v, ta, ma, tb, mb, 100 * (tb / ta - 1), 100 * (mb / ma - 1)))
}
