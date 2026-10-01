# The four kinds of data dev/studies/famd_own.R measured ilm_famd() on, built
# the same way by dev/studies/make_pcamix_fixtures.R (which records what
# PCAmixdata::PCAmix() gives for them) and by test-famd.R (which holds
# ilm_famd() to that record).
famd_cases <- function() {
  d <- ilm_sim()
  q <- d[c("score", "income", "visits", "claims")]
  ql <- data.frame(grp = d$grp, site = factor(d$site))
  qn <- q; qn$income[c(3, 50, 99)] <- NA
  qln <- ql; qln$grp[c(5, 60)] <- NA
  list(mixed = list(quanti = q, quali = ql, ndim = 5L),
       numeric_only = list(quanti = q, quali = NULL, ndim = 3L),
       categorical_only = list(quanti = NULL,
                               quali = data.frame(ql, flag = factor(d$flag)), ndim = 5L),
       mixed_with_gaps = list(quanti = qn, quali = qln, ndim = 5L))
}
