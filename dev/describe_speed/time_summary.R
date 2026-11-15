## Medians of time_describe.R's rows, before against after, by setting.
##   Rscript dev/describe_speed/time_summary.R <out.csv>
r <- utils::read.csv(commandArgs(TRUE)[1])
cat(sprintf("rows %s, %d repeats of each version\n", paste(unique(r$rows), collapse = ","),
            max(table(r$label, r$setting))))
for (s in unique(r$setting)) {
  b <- r[r$setting == s & r$label == "before", ]; a <- r[r$setting == s & r$label == "after", ]
  cat(sprintf("%-14s before %6.3f s %8.1f MB | after %6.3f s %8.1f MB | time %+6.1f%%  memory %+6.1f%%\n",
              s, median(b$seconds), median(b$mb), median(a$seconds), median(a$mb),
              100 * (median(a$seconds) / median(b$seconds) - 1),
              100 * (median(a$mb) / median(b$mb) - 1)))
}
