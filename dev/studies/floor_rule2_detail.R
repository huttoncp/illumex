## Commit hashes in this file are from illumex's history before its public
## restart on 2026-10-01; they are not in this repository.
## Detail tables for floor-rule study 2 (58fabe8): detection on the rating,
## count and 0-100 kinds, and every non-zero false-floor rate, for the arms
## that matter
options(width = 250)
d <- utils::read.csv(commandArgs(TRUE)[1], stringsAsFactors = FALSE)
d$lab <- ifelse(is.na(d$par), d$kind, paste(d$kind, d$par))
A <- c("G0_R0", "G0_A3", "G0_C001", "G1_A3", "G1_C01", "G1_C001")
tr <- d[!is.na(d$bound), ]
f <- function(s, a) mean(ifelse(s$bound == "min", s[[paste0("min_", a)]], s[[paste0("max_", a)]]))
t <- do.call(rbind, lapply(split(tr, list(tr$lab, tr$size), drop = TRUE), function(s)
  data.frame(data = s$lab[1], n = s$size[1],
             at_bound = round(mean(ifelse(s$bound == "min", s$c_min, s$c_max) / s$n), 3),
             next_value = round(mean(ifelse(s$bound == "min", s$c_next_min, s$c_next_max) / s$n), 3),
             t(vapply(A, function(a) round(f(s, a), 2), 0)))))
t <- t[order(t$data, t$n), ]
cat("Detection (all replicates), n >= 300\n"); print(t[t$n >= 300, ], row.names = FALSE)
nl <- d[is.na(d$bound) & d$kind != "panel_id", ]
e <- function(s, a) mean(s[[paste0("min_", a)]] | s[[paste0("max_", a)]])
u <- do.call(rbind, lapply(split(nl, list(nl$lab, nl$size), drop = TRUE), function(s)
  data.frame(data = s$lab[1], n = s$size[1], p_min = round(mean(s$c_min / s$n), 3),
             p_next = round(mean(s$c_next_min / s$n), 3),
             t(vapply(A, function(a) round(e(s, a), 3), 0)))))
u <- u[order(u$data, u$n), ]
cat("\nFalse floors, rows where any of these arms fires\n")
print(u[apply(u[, A], 1, max) > 0, ], row.names = FALSE)
