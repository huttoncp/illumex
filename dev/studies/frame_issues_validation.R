## ilm_frame_issues() against established implementations of each check.
##
##   rank deficiency   stats::alias() on lm(), and caret::findLinearCombos()
##                     on the same model matrix: which variables are exact
##                     combinations of the others
##   nesting, aliasing lme4::isNested(), in both directions
##   Cramer's V        DescTools::CramerV() (no continuity correction)
##
## Each replicate draws a data frame with structure planted at random, so the
## comparison covers data with and without each problem. Agreement is counted
## per replicate; a disagreement is printed with its seed so it can be rerun.
##
## Run from the package root:
##   Rscript dev/studies/frame_issues_validation.R [reps]
pkgload::load_all(".", quiet = TRUE, helpers = FALSE)
for (p in c("caret", "lme4", "DescTools"))
  if (!requireNamespace(p, quietly = TRUE)) stop("this study needs ", p)
args <- commandArgs(TRUE)
reps <- if (length(args) >= 1L) as.integer(args[1]) else 200L

## ---- rank deficiency ----------------------------------------------------------
## numeric columns and one factor; 0 to 2 planted dependencies, each a sum of
## numeric columns or a number fixed by the factor's levels
rank_case <- function(seed) {
  set.seed(seed)
  n <- sample(c(40L, 200L, 1000L), 1L)
  d <- as.data.frame(matrix(rnorm(n * 4), n, 4, dimnames = list(NULL, paste0("x", 1:4))))
  d$g <- sample(letters[1:4], n, TRUE)
  planted <- character(0)
  if (runif(1) < 0.5) {
    w <- sample(1:4, 2); d$s <- d[[w[1]]] * runif(1, 0.5, 2) - d[[w[2]]]
    planted <- c(planted, "s")
  }
  if (runif(1) < 0.5) {
    d$k <- setNames(runif(4, 0, 10), letters[1:4])[d$g]
    planted <- c(planted, "k")
  }
  d$z <- rnorm(n)
  d
}
rank_ours <- function(d) {
  r <- ilm_frame_issues(d)
  sort(sub(" ~.*", "", r$columns[r$issue == "rank_deficient"]))
}
rank_alias <- function(d) {
  y <- rnorm(nrow(d))
  al <- stats::alias(stats::lm(y ~ ., data = d))$Complete
  if (is.null(al)) return(character(0))
  mm <- stats::model.matrix(~ ., data = d)
  own <- names(d)[attr(mm, "assign")[match(rownames(al), colnames(mm))]]
  sort(unique(own))
}
rank_caret <- function(d) {
  mm <- stats::model.matrix(~ ., data = d)
  rm <- caret::findLinearCombos(mm)$remove
  if (!length(rm)) return(character(0))
  sort(unique(names(d)[attr(mm, "assign")[rm]]))
}

## ---- nesting and aliasing -----------------------------------------------------
cat_case <- function(seed) {
  set.seed(seed)
  n <- sample(c(30L, 120L, 600L), 1L)
  la <- sample(2:6, 1L)
  a <- sample(paste0("a", seq_len(la)), n, TRUE)
  kind <- sample(c("nested", "aliased", "independent"), 1L)
  b <- switch(kind,
    nested = paste0(a, "-", sample(1:sample(2:4, 1L), n, TRUE)),
    aliased = paste0("code_", match(a, sort(unique(a)))),
    independent = sample(paste0("b", seq_len(sample(2:6, 1L))), n, TRUE))
  data.frame(a = a, b = b, stringsAsFactors = FALSE)
}
cat_ours <- function(d) {
  r <- ilm_frame_issues(d)
  if ("aliased_factors" %in% r$issue) "aliased"
  else if ("nested" %in% r$issue) "nested" else "neither"
}
cat_lme4 <- function(d) {
  fa <- factor(d$a); fb <- factor(d$b)
  ab <- lme4::isNested(fa, fb); ba <- lme4::isNested(fb, fa)
  if (ab && ba) "aliased" else if (ab || ba) "nested" else "neither"
}
## levels seen twice or more, as the singleton guard requires of the finer one
guard_ok <- function(d) {
  fine <- if (length(unique(d$a)) >= length(unique(d$b))) d$a else d$b
  mean(table(fine) >= 2L) >= 0.5
}

## ---- run -----------------------------------------------------------------------
res_rank <- t(vapply(seq_len(reps), function(s) {
  d <- rank_case(s)
  o <- rank_ours(d); a <- rank_alias(d); c <- rank_caret(d)
  c(alias = identical(o, a), caret = identical(o, c), any = length(a) > 0)
}, c(alias = TRUE, caret = TRUE, any = TRUE)))
bad <- which(!res_rank[, "alias"] | !res_rank[, "caret"])

truth <- character(reps)
res_cat <- vapply(seq_len(reps), function(s) {
  d <- cat_case(s)
  truth[s] <<- cat_lme4(d)
  identical(cat_ours(d), if (guard_ok(d)) cat_lme4(d) else
    if (cat_lme4(d) == "aliased") "aliased" else "neither")
}, TRUE)

vdiff <- vapply(seq_len(reps), function(s) {
  set.seed(s)
  n <- sample(c(30L, 200L, 1000L), 1L)
  a <- sample(1:sample(2:6, 1L), n, TRUE)
  b <- if (runif(1) < 0.5) (a + sample(0:1, n, TRUE, prob = c(0.9, 0.1))) else
    sample(1:sample(2:5, 1L), n, TRUE)
  abs(ilm_cramer_v(a, b) - DescTools::CramerV(a, b, correct = FALSE))
}, 0)

cat(sprintf("rank deficiency, %d data frames, %d with a dependency:\n", reps,
            sum(res_rank[, "any"])))
cat(sprintf("  same variables as stats::alias():          %d of %d\n",
            sum(res_rank[, "alias"]), reps))
cat(sprintf("  same variables as caret::findLinearCombos: %d of %d\n",
            sum(res_rank[, "caret"]), reps))
if (length(bad)) cat("  disagreements at seeds:", head(bad, 20), "\n")
cat(sprintf("nesting and aliasing, %d pairs: same verdict as lme4::isNested(): %d of %d\n",
            reps, sum(res_cat), reps))
tt <- table(truth)
cat("  lme4's verdicts:", paste(names(tt), tt, collapse = ", "), "\n")
if (any(!res_cat)) cat("  disagreements at seeds:", head(which(!res_cat), 20), "\n")
cat(sprintf("Cramer's V, %d pairs: largest difference from DescTools::CramerV(): %.2e\n",
            reps, max(vdiff)))
