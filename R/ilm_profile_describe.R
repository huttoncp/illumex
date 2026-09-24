## ---------------------------------------------------------------------------
## Saying what each cluster is, variable by variable.
##
## The point of profiling is to learn what each subgroup IS -- "older,
## retired, small households" against "young students who rent" -- so a
## cluster is described by the original variables that set it apart, in their
## own units. Each cluster is compared with all rows on every variable the
## clustering used, and the comparisons are ranked by a v-test, Lebart's
## statistic as in FactoMineR's catdes(): how far the cluster's mean (for a
## number) or share (for a category) sits from everyone's, in standard errors
## of a subset of that size drawn without replacement.
##
## It used to go through the dimensions instead: find those a cluster sits
## far along, and name the variables that load on them. That names which
## variables a position is made of without saying which way the cluster lies
## on them, and on a sociodemographic sample with three known subgroups --
## families, retirees, students, which the clustering recovered almost
## exactly -- the families and the retirees got word for word the same
## sentence, "dim 2 (employment, children); dim 1 (employment, high age)".
##
## A variable is named when its v-test clears the threshold AND the
## difference is big enough to matter: 0.2 standard deviations for a number,
## 10 percentage points for a share. The second condition is not decoration.
## On 1,200 rows a region that was pure noise by construction cleared 2.37 in
## one cluster and -2.47 in another -- differences of 3 and 6 points -- and
## the v-test alone would have told the reader the retirees live less often
## in the centre.
##
## None of it is a p-value. The clusters were found from these variables, so
## a variable the clustering used will differ between the clusters it helped
## make; the v-test ranks the differences, it does not certify them.
## ---------------------------------------------------------------------------

## Numbers as a reader writes them: whole numbers stay whole, large ones get a
## thousands separator, the rest three significant figures.
#' @keywords internal
#' @noRd
ilm_fmt_num <- function(v) {
  if (all(abs(v - round(v)) < 1e-9, na.rm = TRUE))
    return(formatC(round(v), format = "d", big.mark = ","))
  formatC(signif(v, 3), format = "fg", digits = 3, big.mark = ",")
}

## The v-test of the rows in `m` against all rows, for a mean and for a share.
#' @keywords internal
#' @noRd
ilm_vtest_mean <- function(x, m) {
  ok <- !is.na(x); x <- x[ok]; m <- m[ok]
  n <- length(x); nc <- sum(m)
  s2 <- mean((x - mean(x))^2)
  if (nc < 2L || nc == n || !is.finite(s2) || s2 == 0) return(NA_real_)
  (mean(x[m]) - mean(x)) / sqrt(s2 / nc * (n - nc) / (n - 1))
}

#' @keywords internal
#' @noRd
ilm_vtest_share <- function(hit, m) {
  ok <- !is.na(hit); hit <- hit[ok]; m <- m[ok]
  n <- length(hit); nc <- sum(m); P <- mean(hit)
  if (nc < 2L || nc == n || P %in% c(0, 1)) return(NA_real_)
  (sum(hit[m]) - nc * P) / sqrt(nc * P * (1 - P) * (n - nc) / (n - 1))
}

## "the middle half 54,400 to 90,700, against 33,300 to 78,000 overall"
#' @keywords internal
#' @noRd
ilm_middle_half <- function(num, m, fmt) {
  q <- function(z) stats::quantile(z, c(0.25, 0.75), type = 1, na.rm = TRUE,
                                   names = FALSE)
  if (!any(m & !is.na(num))) return(NA_character_)
  qc <- q(num[m]); qa <- q(num)
  sprintf("the middle half %s to %s, against %s to %s overall",
          fmt(qc[1]), fmt(qc[2]), fmt(qa[1]), fmt(qa[2]))
}

## "only 6% of them, against 44% overall"
#' @keywords internal
#' @noRd
ilm_share_phrase <- function(pc, pa) {
  k <- round(100 * pc)
  sprintf("%s, against %d%% overall",
          if (!k) "none of them" else if (pc < pa) sprintf("only %d%% of them", k)
          else sprintf("%d%% of them", k),
          round(100 * pa))
}

## One row per cluster and variable -- and per aspect of a date the reduction
## used, its time line and any cycle -- with the v-test, the size of the
## difference on the variable's own scale (`kind` says which: standard
## deviations for a number, a difference of shares for a category), whether
## it may be named at all (`eligible`), and the words for it. `missing`
## describes present/missing markers: "income is missing for 92% of them".
#' @keywords internal
#' @noRd
ilm_profile_characterize <- function(data, cl, map = NULL, missing = FALSE) {
  cls <- sort(unique(cl[!is.na(cl)]))
  rows <- list()
  add <- function(...) rows[[length(rows) + 1L]] <<-
    data.frame(..., stringsAsFactors = FALSE)
  for (v in names(data)) {
    x <- data[[v]]
    if (ilm_is_time(x)) {
      num <- ilm_time_number(x); fmt <- ilm_time_formatter(x)
      dur <- inherits(x, "difftime")
      sdv <- stats::sd(num, na.rm = TRUE)
      cyc <- setdiff(unique(unname(map[[v]])), c("elapsed", "duration"))
      for (cc in cls) {
        m <- !is.na(cl) & cl == cc
        z <- ilm_vtest_mean(num, m)
        up <- isTRUE(z > 0)
        add(cluster = cc, variable = v,
            aspect = if (dur) "length" else "time line", kind = "number",
            v = z, size = abs(mean(num[m], na.rm = TRUE) - mean(num, na.rm = TRUE)) / sdv,
            eligible = TRUE,
            description = sprintf("%s is %s: %s", v,
                                  if (dur) { if (up) "longer" else "shorter" }
                                  else if (up) "later" else "earlier",
                                  ilm_middle_half(num, m, fmt)))
        ## where in a cycle the cluster stands out, for each cycle the
        ## reduction used
        for (a in cyc) {
          arc <- ilm_time_arc(x, m, a)
          if (is.null(arc)) next
          add(cluster = cc, variable = v, aspect = ilm_time_words[[a]],
              kind = "share", v = ilm_vtest_share(arc$hit, m),
              size = abs(arc$share - arc$share_all), eligible = arc$stands_out,
              description = sprintf("%s falls %s for %s", v, arc$lab,
                                    ilm_share_phrase(arc$share, arc$share_all)))
        }
      }
    } else if (is.numeric(x)) {
      sdv <- stats::sd(x, na.rm = TRUE)
      for (cc in cls) {
        m <- !is.na(cl) & cl == cc
        z <- ilm_vtest_mean(x, m)
        add(cluster = cc, variable = v, aspect = "", kind = "number", v = z,
            size = abs(mean(x[m], na.rm = TRUE) - mean(x, na.rm = TRUE)) / sdv,
            eligible = TRUE,
            description = sprintf("%s is %s: %s", v,
                                  if (isTRUE(z > 0)) "higher" else "lower",
                                  ilm_middle_half(x, m, ilm_fmt_num)))
      }
    } else if (is.factor(x) || is.character(x) || is.logical(x)) {
      f <- factor(x)
      for (cc in cls) {
        m <- !is.na(cl) & cl == cc
        ## the value whose share in the cluster sits furthest from its share
        ## among everyone speaks for the variable
        best <- NULL
        for (l in levels(f)) {
          hit <- f == l
          z <- ilm_vtest_share(hit, m)
          if (is.na(z) || (!is.null(best) && abs(z) <= abs(best$z))) next
          best <- list(z = z, l = l, pc = mean(hit[m], na.rm = TRUE),
                       pa = mean(hit, na.rm = TRUE))
        }
        if (is.null(best)) next
        ## a TRUE/FALSE variable is said the TRUE way round
        if (is.logical(x) && best$l == "FALSE") {
          best$pc <- 1 - best$pc; best$pa <- 1 - best$pa; best$z <- -best$z
        }
        what <- if (missing) "is missing" else if (is.logical(x)) "is TRUE"
                else sprintf("is '%s'", best$l)
        add(cluster = cc, variable = v, aspect = "", kind = "share", v = best$z,
            size = abs(best$pc - best$pa), eligible = TRUE,
            description = sprintf("%s %s for %s", v, what,
                                  ilm_share_phrase(best$pc, best$pa)))
      }
    }
  }
  if (!length(rows))
    return(data.frame(cluster = integer(0), variable = character(0),
                      aspect = character(0), kind = character(0),
                      v = numeric(0), size = numeric(0), eligible = logical(0),
                      description = character(0), stringsAsFactors = FALSE))
  out <- do.call(rbind, rows)
  out <- out[order(out$cluster, -abs(out$v)), , drop = FALSE]
  rownames(out) <- NULL
  out
}

## Which rows of the table are worth a reader's attention: a v-test past the
## threshold and a difference past its floor.
#' @keywords internal
#' @noRd
ilm_profile_distinctive <- function(tab, vtest_threshold) {
  floor <- ifelse(tab$kind == "number", 0.2, 0.1)
  !is.na(tab$v) & abs(tab$v) >= vtest_threshold & !is.na(tab$size) &
    tab$size >= floor & tab$eligible
}

## Every value of every categorical variable, per cluster: how many of the
## cluster's members have it, their share, and its share among all rows. For
## markers of missingness the values are "missing" and "present".
#' @keywords internal
#' @noRd
ilm_profile_frequencies <- function(data, cl, missing = FALSE) {
  cls <- sort(unique(cl[!is.na(cl)]))
  out <- list()
  for (v in names(data)) {
    x <- data[[v]]
    if (!(is.factor(x) || is.character(x) || is.logical(x))) next
    f <- if (missing) factor(ifelse(x, "missing", "present"),
                             levels = c("missing", "present"))
         else factor(x)
    ta <- table(f, useNA = "ifany")
    for (cc in cls) {
      m <- !is.na(cl) & cl == cc
      tb <- table(f[m], useNA = "ifany")
      tb <- tb[names(ta)]; tb[is.na(tb)] <- 0L
      out[[length(out) + 1L]] <- data.frame(
        cluster = cc, variable = v,
        value = ifelse(is.na(names(ta)), "(missing)", names(ta)),
        n = as.integer(tb), share = round(as.numeric(tb) / sum(m), 3),
        share_all = round(as.numeric(ta) / length(f), 3),
        stringsAsFactors = FALSE)
    }
  }
  if (!length(out)) return(NULL)
  res <- do.call(rbind, out)
  res <- res[order(res$cluster, match(res$variable, names(data)), -res$n), ,
             drop = FALSE]
  rownames(res) <- NULL
  res
}

## "a; b; c; and d"
#' @keywords internal
#' @noRd
ilm_join_and <- function(x, sep = "; ") {
  if (length(x) < 2L) return(x)
  if (length(x) == 2L) return(paste(x, collapse = if (sep == "; ") "; and " else " and "))
  paste0(paste(x[-length(x)], collapse = sep), sep, "and ", x[length(x)])
}

## A paragraph per cluster: its size, the variables that set it apart in
## order of strength, and what the clustering says about its members.
#' @keywords internal
#' @noRd
ilm_profile_prose <- function(tab, cluster_res, top_n_vars, vtest_threshold) {
  ct <- cluster_res$clusters
  ind <- cluster_res$ind_cluster
  dist <- ilm_profile_distinctive(tab, vtest_threshold)
  vapply(ct$cluster, function(cc) {
    r <- tab[dist & tab$cluster == cc, , drop = FALSE]
    r <- r[order(-abs(r$v)), , drop = FALSE]
    shown <- r[seq_len(min(top_n_vars, nrow(r))), , drop = FALSE]
    more <- length(setdiff(unique(r$variable), unique(shown$variable)))
    si <- ct[ct$cluster == cc, , drop = FALSE]
    head_txt <- sprintf("Cluster %d holds %s rows, %.1f%% of the data (%s).",
                        cc, ilm_fmt_num(si$size), si$pct,
                        as.character(si$stability))
    body <- if (!nrow(shown))
      "Nothing sets it clearly apart from the rest."
    else paste0("What sets it apart: ", ilm_join_and(shown$description), ".")
    if (more > 0L)
      body <- paste(body, sprintf("Less strongly, %d more variable%s set%s it apart as well.",
                                  more, if (more == 1L) "" else "s",
                                  if (more == 1L) "s" else ""))
    notes <- character()
    if (isTRUE(si$anomalous))
      notes <- c(notes, sprintf(
        "It is a small cluster, %.1f%% of observations: possibly a real minority pattern, possibly a data problem, but worth looking at either way.",
        si$pct))
    n_amb <- sum(ind$cluster == cc & ind$is_ambiguous)
    if (n_amb > 0)
      ## "N of its members" is plural whatever N is; only the verb agrees
      notes <- c(notes, sprintf(
        "%d of its members sit%s close enough to another cluster to be uncertain.",
        n_amb, if (n_amb == 1L) "s" else ""))
    paste(c(head_txt, body, notes), collapse = " ")
  }, "")
}

## The variables that set no cluster apart, which the paragraphs leave out:
## named when there are a few, counted when the data are wide.
#' @keywords internal
#' @noRd
ilm_profile_closing <- function(not_distinctive, n_vars) {
  k <- length(not_distinctive)
  if (!k) return(NULL)
  if (k <= 5L)
    return(paste0("Not distinctive in any cluster: ",
                  ilm_join_and(not_distinctive, sep = ", "), "."))
  sprintf("Not distinctive in any cluster: %d of the %d variables.", k, n_vars)
}

## The original columns a reduction was built from, dates as themselves
## rather than as the numbers they became.
#' @keywords internal
#' @noRd
ilm_profile_cols <- function(rr, data) {
  enc <- unlist(lapply(rr$time, names), use.names = FALSE)
  intersect(names(data), unique(c(setdiff(rr$cols, enc), names(rr$time))))
}

## Everything a profile says about its clusters, from the rows it clustered.
#' @keywords internal
#' @noRd
ilm_profile_describe <- function(dsub, cl, rr, cluster_res, top_n_vars,
                                 vtest_threshold, missing = FALSE) {
  tab <- ilm_profile_characterize(dsub, cl, rr$time, missing = missing)
  dist <- ilm_profile_distinctive(tab, vtest_threshold)
  not_dist <- setdiff(names(dsub), unique(tab$variable[dist]))
  attr(not_dist, "of") <- ncol(dsub)
  list(characterization = tab,
       frequencies = ilm_profile_frequencies(dsub, cl, missing = missing),
       summary = ilm_profile_prose(tab, cluster_res, top_n_vars, vtest_threshold),
       not_distinctive = not_dist)
}

## ilm_describe_all() of the profiled variables, by cluster: the full
## descriptive statistics behind the paragraphs.
#' @keywords internal
#' @noRd
ilm_profile_by_cluster <- function(data, cl) {
  nm <- "cluster"
  while (nm %in% names(data)) nm <- paste0(nm, "_")
  d <- data
  d[[nm]] <- factor(cl)
  tryCatch(suppressMessages(ilm_describe_all(d, by = nm)),
           error = function(e) NULL)
}
