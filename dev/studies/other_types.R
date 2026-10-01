## Column types other than dates in a clustering: what each does to recovery
## of two known clusters, through ilm_reduce() + k-means (k = 2), as
## ilm_profile() would run them. 400 rows; x1 and x2 separate the clusters by
## 1.2 sd, and each variant adds a column or replaces one with another type
## carrying the same information. Scored by the adjusted Rand index.
##
## Nothing in the package acts on these yet; this is the evidence for which
## types are worth handling. Run from the package root:
##   Rscript dev/studies/other_types.R [reps] [outfile.csv]
suppressMessages(pkgload::load_all(".", quiet = TRUE, helpers = FALSE))

ari <- function(a, b) {
  tab <- table(a, b); n <- sum(tab)
  s <- sum(choose(tab, 2)); sa <- sum(choose(rowSums(tab), 2))
  sb <- sum(choose(colSums(tab), 2)); e <- sa * sb / choose(n, 2)
  (s - e) / ((sa + sb) / 2 - e)
}
clus <- function(d) {
  r <- suppressMessages(suppressWarnings(ilm_reduce(d, ndim = 5)))
  co <- as.matrix(r$ind_coord[setdiff(names(r$ind_coord), "row_id")])
  stats::kmeans(co, 2, nstart = 25)$cluster
}

variant <- function(v, seed, n = 400) {
  set.seed(seed)
  cl <- rep(1:2, each = n / 2); s <- cl == 2
  x1 <- rnorm(n, 1.2 * s); x2 <- rnorm(n, 1.2 * s)
  d <- data.frame(x1 = x1, x2 = x2)
  switch(v,
    base = NULL,
    normal_noise = d$z <- rnorm(n),
    lognormal_noise = d$z <- exp(rnorm(n, 0, 1.5)),
    id = d$id <- sprintf("id%04d", seq_len(n)),
    highcard = d$g <- factor(sample(1:100, n, TRUE)),
    skew_raw = d$x1 <- exp(1.5 * x1),
    skew_log = d$x1 <- log(exp(1.5 * x1)),
    ordinal_factor = d$x2 <- factor(cut(x2, c(-Inf, -0.5, 0.2, 0.9, 1.6, Inf), labels = FALSE),
                                    ordered = TRUE),
    ordinal_number = d$x2 <- as.integer(cut(x2, c(-Inf, -0.5, 0.2, 0.9, 1.6, Inf), labels = FALSE)),
    angle_raw = , angle_sincos = {
      ## a direction: one cluster around north, which wraps, one around south
      a <- (ifelse(s, 180, 0) + rnorm(n, 0, 40)) %% 360
      d$x2 <- NULL
      if (v == "angle_raw") d$deg <- a
      else { d$deg_sin <- sin(a * pi / 180); d$deg_cos <- cos(a * pi / 180) }
    },
    comp_raw = , comp_clr = {
      ## shares of a whole (spend by category), the clusters differing in mix
      g <- cbind(rnorm(n, 0.8 * s, 0.6), rnorm(n, 0, 0.6), rnorm(n, -0.8 * s, 0.6))
      p <- exp(g) / rowSums(exp(g))
      d$x2 <- NULL
      if (v == "comp_raw") { d$p1 <- p[, 1]; d$p2 <- p[, 2]; d$p3 <- p[, 3] }
      else { lp <- log(p) - rowMeans(log(p)); d$c1 <- lp[, 1]; d$c2 <- lp[, 2]; d$c3 <- lp[, 3] }
    })
  list(d = d, cl = cl)
}

vs <- c("base", "normal_noise", "lognormal_noise", "id", "highcard", "skew_raw", "skew_log",
        "ordinal_factor", "ordinal_number", "angle_raw", "angle_sincos", "comp_raw", "comp_clr")
args <- commandArgs(TRUE)
R <- if (length(args) >= 1L) as.integer(args[1]) else 20L
res <- do.call(rbind, lapply(vs, function(v) do.call(rbind, lapply(seq_len(R), function(r) {
  g <- variant(v, 1000 * r + 3)
  lab <- tryCatch(clus(g$d), error = function(e) rep(1L, nrow(g$d)))
  data.frame(variant = v, rep = r, ari = ari(lab, g$cl))
}))))
if (length(args) >= 2L) utils::write.csv(res, args[2], row.names = FALSE)
m <- tapply(res$ari, res$variant, mean)[vs]
se <- tapply(res$ari, res$variant, function(a) sd(a) / sqrt(length(a)))[vs]
print(data.frame(variant = vs, ari = round(m, 2), se = round(se, 3)), row.names = FALSE)
