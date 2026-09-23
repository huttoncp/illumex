## Generate a grouped pkgdown reference index from the actual Rd topics, so
## that no topic can be left out of the index by hand.
## Run from the package root.

rd <- list.files("man", pattern = "[.]Rd$")
topic <- sub("[.]Rd$", "", rd)
## pkgdown indexes by the Rd \name, which is what the filename encodes except
## where roxygen escaped a character; read the name to be certain
nm <- vapply(file.path("man", rd), function(f) {
  x <- readLines(f, warn = FALSE)
  i <- grep("^\\\\name\\{", x)[1]
  if (is.na(i)) NA_character_ else sub("^\\\\name\\{(.*)\\}.*$", "\\1", x[i])
}, character(1))
topic <- ifelse(is.na(nm), topic, nm)
topic <- setdiff(topic, "illumex-package")

## ---- groups, in the order they should appear -------------------------------
## Each entry: title, a one-line description, a predicate over topic names,
## and optionally the topics to list first, in that order -- the front door
## of a group before its parts. Whatever else matches follows alphabetically.
##
## Edit THIS file, not _pkgdown.yml: the yml is regenerated from it, and a
## hand edit there is lost the next time an export is added.
grp <- list(
  list("Describing data",
       "Descriptive statistics, counts, and the things that are wrong with a data frame before any model sees it.",
       function(x) (grepl("^ilm_(describe|counts|dupes|copies|frame_issues|gauss_check)", x) &&
                    !grepl("_na", x)),
       c("ilm_describe_all", "ilm_describe", "ilm_frame_issues", "ilm_gauss_check")),

  list("Cleaning",
       "Tidying a data frame and recoding values that were entered wrongly.",
       function(x) grepl("^ilm_(wash_df|recode_errors|translate)", x)),

  list("Uncertainty without a model",
       "Bootstrap intervals for a statistic, and for differences between groups.",
       function(x) grepl("^ilm_(boot_|plot_boot_diff)", x)),

  list("Outliers and anomalies",
       "A value extreme for its own column, against a row implausible as a combination.",
       function(x) grepl("^ilm_(outliers|anomal)", x) || x == "ilm_plot_anomaly",
       c("ilm_anomaly", "ilm_anomalous", "ilm_plot_anomaly")),

  list("Structure: profile, cluster, reduce",
       "ilm_profile() is the front door: it reduces, clusters and describes the groups in one call, and handles mixed columns. The other two are the same pipeline taken a step at a time, for when you want the coordinates or the partition on their own.",
       function(x) grepl("^ilm_(reduce|cluster|profile|glrm|var_contrib)", x) ||
                   grepl("^ilm_plot_(reduce|cluster|profile)", x),
       c("ilm_profile", "ilm_var_contrib", "ilm_cluster", "ilm_reduce",
         "ilm_glrm", "ilm_profile_na", "ilm_cluster_na", "ilm_reduce_na")),

  list("Missing values",
       "What is missing, whether it matters, and which values go missing together. Filling them in and pooling across the imputations is illume's.",
       function(x) x %in% c("ilm_check_missing", "ilm_plot_missing") || grepl("_na$|_na_all$", x),
       c("ilm_check_missing", "ilm_describe_na_all", "ilm_describe_na", "ilm_plot_missing")),

  list("Plots",
       "Built on tinyplot, named for what they show.",
       function(x) grepl("^ilm_(plot|pick_geom|geom_spec)", x),
       c("ilm_plot", "ilm_plot_all", "ilm_pick_geom", "ilm_geom_spec")),

  list("Example data",
       "A grouped data set with a known structure, used throughout the examples.",
       function(x) x == "ilm_sim")
)

assigned <- character(0)
lines <- c("reference:")
for (g in grp) {
  hit <- Filter(function(x) isTRUE(tryCatch(g[[3]](x), error = function(e) FALSE)),
                setdiff(topic, assigned))
  hit <- sort(hit)
  if (length(g) >= 4L) hit <- c(intersect(g[[4]], hit), setdiff(hit, g[[4]]))
  if (!length(hit)) next
  assigned <- c(assigned, hit)
  lines <- c(lines,
             paste0("- title: \"", g[[1]], "\""),   # quoted: titles may hold a colon
             paste0("  desc: >"),
             paste0("    ", g[[2]]),
             "  contents:",
             paste0("  - ", hit))
}

left <- sort(setdiff(topic, assigned))
if (length(left)) {
  lines <- c(lines, "- title: \"Shared parameters and print methods\"",
             "  desc: >",
             "    Documentation shared across functions, and methods you call by printing rather than by name.",
             "  contents:",
             paste0("  - ", left))
}

head <- c(
  "url: https://huttoncp.github.io/illumex/",
  "template:",
  "  bootstrap: 5",
  "  bslib:",
  "    primary: \"#2a6f97\"",
  "",
  "home:",
  "  title: Describe, Clean, Plot and Profile Data Before a Model",
  "",
  "navbar:",
  "  structure:",
  "    left:  [reference, articles, news, illume]",
  "    right: [search, github]",
  "  components:",
  "    illume:",
  "      text: Modelling (illume)",
  "      href: https://huttoncp.github.io/illume/",
  "",
  "articles:",
  "- title: Articles",
  "  navbar: ~",
  "  contents:",
  "  - exploring-data",
  "  - profiling",
  "  - anomaly-detection",
  "")

writeLines(c(head, lines), "_pkgdown.yml")
cat("topics:", length(topic), " assigned:", length(assigned),
    " unassigned:", length(left), "\n")
if (length(left)) cat("UNASSIGNED:", paste(left, collapse = ", "), "\n")
