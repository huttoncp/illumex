## How many dimensions to keep before clustering: a fixed five, or as many as
## parallel analysis finds (item 327). Registered before any run.
##
## Why. ilm_profile() and ilm_reduce() keep ndim = 5 by default, and
## ilm_cluster() chooses k by the gap statistic on those dimensions. On mixed
## data with few columns the dimensions past the first few carry one
## categorical column's own levels, and the gap statistic splits the groups
## along them. Measured before this study, on 300 rows drawn from 3 groups (4
## numeric columns and a 2-level factor): ndim = 2 chose k = 3 under every
## seed and B (25 or 100); ndim = 3 chose 10, 4 and 4 across seeds, the curve
## creeping up (0.962, 0.992, 0.993 at k = 3, 4, 5); the 4 numeric columns
## alone chose 3. Across three such designs and three seeds, ndim = 5 chose 5
## to 10 clusters (ARI 0.37 to 0.55) and ndim = 2 chose 3 in all 18 runs (ARI
## 0.77 to 1.00). A comment in R/ilm_cluster.R already records the symptom:
## on mixed data every k-selector tried over-shot.
##
## Question. Does choosing ndim by parallel analysis recover the number of
## clusters better than ndim = 5, without doing worse anywhere the current
## default works -- the cluster-options study's designs included?
##
## Design, fixed before any run.
##   Arms, each ilm_reduce() (FAMD, the default) then ilm_cluster() with k
##   chosen by the gap statistic (gap_method "firstSEmax", k_max = 10,
##   B = 50, seed = the replicate's seed):
##     fixed5  ndim = 5, today's default, capped at the dimensions the data
##             have.
##     pa      ndim by Horn's parallel analysis on the reduction itself: each
##             ORIGINAL column is permuted independently (a factor keeps its
##             levels and their counts), the permuted data are reduced the
##             same way, and the observed eigenvalues are set against the
##             95th percentile of 30 permutations'. Counting from the first,
##             dimensions are kept until the first that does not beat its
##             percentile; at least 1, at most min(5, the dimensions the data
##             have).
##     oracle  ndim = the dimension of the space the cluster centres span
##             (k - 1 for the 3-group designs, 3 for the 4-cluster ones).
##             A reference for what is possible, not a candidate; the
##             continuum has none, and no oracle row.
##   GLRM is left out: its penalty search costs 37 s on 300 rows, and the arms
##   differ in ndim, not in the reduction.
##   Data, 9 conditions, 20 replicates each, seed = 1000 * condition +
##   replicate, truth known:
##     1 mixed_few        300 rows: age, income, spend, visits (numeric) and
##                        a 2-level work factor, 3 groups
##     2 mixed_few_noise  the same plus a 4-level factor unrelated to them
##     3 mixed_counts     300 rows: age, income, two Poisson counts and a
##                        3-level work factor, 3 groups (the profiling
##                        vignette's design)
##     4 mixed_many       1,000 rows: 12 numeric and 4 factors (2 to 5
##                        levels) from 2 latent dimensions, 3 groups
##     5 numeric_few      300 rows: 5 numeric, 3 groups
##     6 numeric_many     1,000 rows: 20 numeric, 10 from 3 latent
##                        dimensions and 10 pure noise, 4 groups
##     7 separated        1,000 rows, the cluster-options study's design (a):
##                        5 numeric, 4 clusters 6 apart, sd 1
##     8 overlapping      its design (b): centres 2.5 apart
##     9 continuum        its design (c): correlated normal, rho 0.6^|i-j|,
##                        no clusters (truth k = 1)
##   The options study's designs run at 1,000 rows, not 10,000: the gap
##   statistic's cost grows as the square of the rows.
##   Outcomes per replicate and arm: ndim, k, whether k is the truth, ARI
##   against the true groups (the continuum has none: there, whether k = 1),
##   seconds.
##   Decision rule, fixed now. pa replaces fixed5 as the default if (i) in at
##   least two of the four mixed conditions its mean ARI exceeds fixed5's by
##   at least 0.05, the paired difference more than two paired standard
##   errors; (ii) in no condition is its mean ARI below fixed5's by more than
##   0.02; and (iii) on the continuum its share of k = 1 is not below
##   fixed5's by more than 0.05. Otherwise ndim = 5 stays and the help and the
##   profiling vignette say how to choose it. Every mean is reported with its
##   Monte Carlo standard error, beside the oracle's. The results are
##   reported as they are, whichever way the rule falls; nothing changes in
##   the package until they are reviewed.
##   Cost: piloted at the largest size before the estimate is registered (an
##   addendum below, before the full run); the run stops and reports at twice
##   that estimate.
##
## Addendum, after the first pilot and before any full run (2026-10-06).
##   Why: in the first pilot (d429d97, replicates 1 and 2 of mixed_many and
##   numeric_many), every arm, the oracle included, chose k = 1: the
##   clusters as first drawn (a triangle of side 3; centres 4 apart through
##   random loadings) could not be found at all, so those conditions could
##   not separate the arms. None of that pilot's rows is kept.
##   Change, the only one: mixed_many's triangle has side 6, and
##   numeric_many's centres are 6 apart, each signal column loading 1 on
##   its own latent dimension in turn. The other seven designs, the arms,
##   the outcomes and the decision rule are unchanged.
##   Gate, before the full run: a re-pilot on fresh replicates 101 and 102,
##   from a clean tree at this commit. A redesigned condition passes when,
##   in each of the 2 replicates, the oracle chooses the true k (3 for
##   mixed_many, 4 for numeric_many) with ARI at least 0.8. The arms'
##   results play no part. If either condition fails, the study stops
##   there and the numbers are reported: no second round of changes, and
##   no run without that condition, until that is reviewed.
##   Cost, registered now: the first pilot measured the largest size (12
##   clusterings at 1,000 rows in 221 s, 18 s each), and the redesign
##   changes no size. The full run is 2 hours on one core (the five
##   1,000-row conditions about 90 minutes, the four 300-row ones about
##   20), and stops and reports at 4 hours.
##
## Run from the package root, from a clean tree at the registration commit:
##   Rscript dev/studies/ndim_choice.R smoke|pilot|full <out.csv>
## "smoke" checks the code on every condition at one replicate with B = 5
## and 5 permutations; its output is not a result and is not kept.
## "pilot" runs conditions 4 and 6 (the 1,000-row mixed and numeric designs)
## for replicates 101 and 102, outside the full run's 1 to 20, so the full
## run's data stay unseen; "full" runs everything. A row is written as each
## replicate finishes, so a stopped run keeps what it did.

suppressMessages(pkgload::load_all(".", quiet = TRUE, helpers = FALSE))
if (!requireNamespace("cluster", quietly = TRUE)) stop("this study needs cluster")
a <- commandArgs(TRUE)
MODE <- if (length(a)) a[1] else "pilot"
OUT <- if (length(a) >= 2L) a[2] else file.path("dev", "studies", "results", "ndim_choice.csv")
SMOKE <- MODE == "smoke"
B_GAP <- if (SMOKE) 5L else 50L; K_MAX <- 10L; N_PERM <- if (SMOKE) 5L else 30L; REPS <- 20L

q <- function(e) suppressWarnings(suppressMessages(e))

## ---- data -----------------------------------------------------------------

three_groups <- function(n) sample(1:3, n, TRUE, prob = c(0.4, 0.35, 0.25))

gen <- list(
  mixed_few = function(n = 300L) {
    g <- three_groups(n)
    list(g = g, oracle = 2L, d = data.frame(
      age = round(c(31, 47, 71)[g] + stats::rnorm(n, 0, 5)),
      income = round(c(36, 74, 30)[g] + stats::rnorm(n, 0, 7), 1),
      spend = round(c(12, 30, 18)[g] + stats::rnorm(n, 0, 4), 1),
      visits = round(c(2, 3, 7)[g] + stats::rnorm(n, 0, 1), 1),
      work = factor(ifelse(stats::runif(n) < c(0.95, 0.97, 0.12)[g], "employed", "retired"))))
  },
  mixed_few_noise = function(n = 300L) {
    x <- gen$mixed_few(n)
    x$d$region <- factor(sample(c("north", "south", "east", "west"), n, TRUE))
    x
  },
  mixed_counts = function(n = 300L) {
    g <- three_groups(n)
    list(g = g, oracle = 2L, d = data.frame(
      age = round(c(31, 47, 71)[g] + stats::rnorm(n, 0, 6)),
      income = round(c(38, 72, 31)[g] + stats::rnorm(n, 0, 9), 1),
      household = 1 + stats::rpois(n, c(1.2, 2.4, 0.4)[g]),
      visits = stats::rpois(n, c(2, 3, 6)[g]),
      work = factor(ifelse(stats::runif(n) < c(0.9, 0.95, 0.15)[g], "employed",
                           ifelse(g == 3, "retired", "studying")))))
  },
  mixed_many = function(n = 1000L) {
    g <- three_groups(n)
    ctr <- rbind(c(0, 0), c(6, 0), c(3, 5.2))            # an equilateral triangle, side 6 (addendum)
    L <- ctr[g, ] + matrix(stats::rnorm(2 * n), n, 2)
    W <- matrix(stats::rnorm(2 * 12), 2, 12)
    num <- L %*% W + matrix(stats::rnorm(12 * n), n, 12)
    colnames(num) <- paste0("x", 1:12)
    cut_k <- function(v, k) factor(cut(v, stats::quantile(v, seq(0, 1, length.out = k + 1L)),
                                       include.lowest = TRUE, labels = letters[seq_len(k)]))
    fac <- lapply(2:5, function(k) cut_k(L %*% stats::rnorm(2) + stats::rnorm(n), k))
    names(fac) <- paste0("f", 1:4)
    list(g = g, oracle = 2L, d = data.frame(num, fac))
  },
  numeric_few = function(n = 300L) {
    x <- gen$mixed_few(n)
    x$d$work <- NULL
    x$d$tenure <- round(c(3, 12, 30)[x$g] + stats::rnorm(n, 0, 3), 1)
    x
  },
  numeric_many = function(n = 1000L) {
    g <- sample.int(4L, n, replace = TRUE)
    ctr <- rbind(c(0, 0, 0), c(6, 0, 0), c(0, 6, 0), c(0, 0, 6))   # 6 apart (addendum)
    L <- ctr[g, ] + matrix(stats::rnorm(3 * n), n, 3)
    ## each signal column loads 1 on its own latent dimension, in turn (addendum)
    sig <- L[, rep(1:3, length.out = 10)] + matrix(stats::rnorm(10 * n), n, 10)
    noise <- matrix(stats::rnorm(10 * n), n, 10)
    d <- as.data.frame(cbind(sig, noise)); names(d) <- c(paste0("s", 1:10), paste0("z", 1:10))
    list(g = g, oracle = 3L, d = d)
  },
  separated = function(n = 1000L) options_design("separated", n),
  overlapping = function(n = 1000L) options_design("overlapping", n),
  continuum = function(n = 1000L) options_design("continuum", n))

## the cluster-options study's make_data(), its seed left to the caller
options_design <- function(kind, n) {
  p <- 5L
  if (kind == "continuum") {
    R <- 0.6 ^ abs(outer(1:p, 1:p, "-"))
    X <- matrix(stats::rnorm(n * p), n, p) %*% chol(R)
    return(list(g = rep(1L, n), oracle = NA_integer_, d = as.data.frame(X)))
  }
  gapc <- if (kind == "separated") 6 else 2.5
  centres <- rbind(c(0, 0, 0, 0, 0), c(gapc, 0, 0, 0, 0), c(0, gapc, 0, 0, 0), c(0, 0, gapc, 0, 0))
  g <- sample.int(4L, n, replace = TRUE)
  list(g = g, oracle = 3L, d = as.data.frame(centres[g, ] + matrix(stats::rnorm(n * p), n, p)))
}

CONDS <- names(gen)
TRUTH <- c(mixed_few = 3L, mixed_few_noise = 3L, mixed_counts = 3L, mixed_many = 3L,
           numeric_few = 3L, numeric_many = 4L, separated = 4L, overlapping = 4L, continuum = 1L)

## ---- arms -----------------------------------------------------------------

n_dims <- function(d) nrow(q(ilm_reduce(d, ndim = 1L))$eig)

pa_ndim <- function(d, cap) {
  obs <- q(ilm_reduce(d, ndim = 1L))$eig$eigenvalue
  perm <- vapply(seq_len(N_PERM), function(b) {
    e <- q(ilm_reduce(as.data.frame(lapply(d, sample)), ndim = 1L))$eig$eigenvalue
    e[seq_along(obs)]
  }, numeric(length(obs)))
  thr <- apply(perm, 1L, stats::quantile, 0.95, na.rm = TRUE)
  beat <- obs > thr
  k <- if (all(beat)) length(beat) else which(!beat)[1L] - 1L
  max(1L, min(k, cap))
}

ari <- function(a, b) {
  t <- table(a, b); n <- sum(t)
  s <- sum(choose(t, 2)); sa <- sum(choose(rowSums(t), 2)); sb <- sum(choose(colSums(t), 2))
  e <- sa * sb / choose(n, 2); m <- (sa + sb) / 2
  if (m == e) return(1)
  (s - e) / (m - e)
}

one_arm <- function(d, g, ndim, seed) {
  t <- system.time({
    r <- q(ilm_reduce(d, ndim = ndim))
    cl <- q(ilm_cluster(r, k_max = K_MAX, B = B_GAP, seed = seed))
  })[["elapsed"]]
  list(k = cl$k, ari = if (length(unique(g)) > 1L) ari(g, cl$ind_cluster$cluster) else NA_real_,
       seconds = t)
}

write_row <- function(row) utils::write.table(row, OUT, sep = ",", append = file.exists(OUT),
                                              col.names = !file.exists(OUT), row.names = FALSE)

## ---- run ------------------------------------------------------------------

plan <- switch(MODE,
  smoke = expand.grid(rep = 1L, cond = CONDS, stringsAsFactors = FALSE),
  pilot = expand.grid(rep = 101:102, cond = c("mixed_many", "numeric_many"), stringsAsFactors = FALSE),
  full = expand.grid(rep = seq_len(REPS), cond = CONDS, stringsAsFactors = FALSE),
  stop("mode is smoke, pilot or full", call. = FALSE))
cat(sprintf("ndim_choice %s: %d replicates, to %s; started %s\n", MODE, nrow(plan), OUT,
            format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
for (i in seq_len(nrow(plan))) {
  cond <- plan$cond[i]; rep <- plan$rep[i]
  seed <- 1000L * match(cond, CONDS) + rep
  set.seed(seed)
  x <- gen[[cond]]()
  cap <- min(5L, n_dims(x$d))
  t_pa <- system.time(nd_pa <- pa_ndim(x$d, cap))[["elapsed"]]
  arms <- list(fixed5 = cap, pa = nd_pa,
               oracle = if (is.na(x$oracle)) NA_integer_ else min(x$oracle, cap))
  for (arm in names(arms)) {
    if (is.na(arms[[arm]])) next
    res <- one_arm(x$d, x$g, arms[[arm]], seed)
    write_row(data.frame(cond = cond, rep = rep, arm = arm, ndim = arms[[arm]], k = res$k,
                         truth = TRUTH[[cond]], k_ok = res$k == TRUTH[[cond]], ari = res$ari,
                         seconds = round(res$seconds + if (arm == "pa") t_pa else 0, 2)))
  }
  cat(sprintf("%s %-16s rep %2d  ndim fixed5 %d pa %d  done %s\n", format(Sys.time(), "%H:%M:%S"),
              cond, rep, arms$fixed5, arms$pa, format(Sys.time(), "%H:%M:%S")))
}
cat("finished", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
