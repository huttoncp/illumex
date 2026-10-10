## ---------------------------------------------------------------------------
## A data frame's fingerprint, and secrets kept out of the record.
##
## A remedies table belongs to the data it was listed for, and applying it to
## other data -- or to the same data re-sorted, where a remedy naming rows
## would hit the wrong ones -- must be refused. So the fingerprint depends on
## every value and on its POSITION, row and column, and two offsetting edits
## must not cancel.
##
## It is computed in base R, exactly, with no hashing package: every value is
## reduced to integers (a number to the two 32-bit words of its bit pattern, a
## string to a polynomial hash of its characters, computed once per distinct
## value), each integer is weighted by its row position under two independent
## weightings modulo two primes below 2^20, and the column sums are weighted
## by column position. Every product is below 2^40 and every term is reduced
## before it is summed, so the sums are exact in double precision. It is not
## cryptographic; it tells one table's data from another's, which is all it is
## for. Row names are left out: they are labels, not data.
## ---------------------------------------------------------------------------

ilm_id_p <- c(1048573, 1048571)

## a string's hash, mod p: sum over its characters of code * 131^position
ilm_id_str <- function(s, p) {
  u <- unique(s)
  h <- vapply(u, function(x) {
    if (is.na(x)) return(p - 1)
    cp <- utf8ToInt(enc2utf8(x))
    if (!length(cp)) return(p - 2)
    pw <- numeric(length(cp)); pw[1L] <- 1
    for (k in seq_along(cp)[-1L]) pw[k] <- (pw[k - 1L] * 131) %% p
    sum((cp %% p) * pw %% p) %% p
  }, 0, USE.NAMES = FALSE)
  h[match(s, u)]
}

## one column as a matrix of integers in [0, p), one row per data row
ilm_id_words <- function(x, p) {
  n <- length(x)
  if (is.factor(x)) return(matrix(ilm_id_str(as.character(x), p), n))
  if (is.character(x)) return(matrix(ilm_id_str(x, p), n))
  if (is.logical(x) || is.integer(x)) {
    v <- as.numeric(x) %% p
    v[is.na(x)] <- p - 3
    return(matrix(v, n))
  }
  if (is.complex(x)) return(cbind(ilm_id_words(Re(x), p), ilm_id_words(Im(x), p)))
  if (is.numeric(x)) {
    w <- readBin(writeBin(as.double(x), raw(), endian = "little"), "integer",
                 n = 2L * n, size = 4L, endian = "little")
    return(matrix(as.numeric(w) %% p, n, 2L, byrow = TRUE))
  }
  ## anything else -- a list column -- by its printed form
  matrix(ilm_id_str(vapply(x, function(e) paste(deparse(e), collapse = " "), ""), p), n)
}

#' A data frame's fingerprint
#'
#' A short string that identifies a data frame's contents: its size, its
#' column names and classes, and every value in its place. It changes when
#' any value changes, when rows or columns are put in another order, and when
#' two edits would offset each other in a sum; row names do not enter it.
#' [ilm_apply_remedy()] uses it to refuse a remedy listed for other data, and
#' the cleaning log records it before and after every remedy.
#'
#' It is computed exactly in base R and is the same on every platform and in
#' every session. It is not a cryptographic hash.
#'
#' @param data A data frame.
#' @return A single string, `"<rows>x<columns>:<hash>"`.
#' @examples
#' d <- data.frame(a = 1:3, b = c("x", "y", "z"))
#' ilm_data_id(d)
#' ilm_data_id(d[3:1, ])
#' @export
ilm_data_id <- function(data) {
  if (!is.data.frame(data))
    stop("`data` must be a data frame; it is ", class(data)[1], call. = FALSE)
  n <- nrow(data); m <- ncol(data)
  i <- seq_len(n)
  h <- vapply(seq_along(ilm_id_p), function(k) {
    p <- ilm_id_p[k]
    ## two independent position weightings: the row number itself, and a
    ## scrambled one (a different multiplier and offset per prime)
    w <- if (k == 1L) (i %% p) + 1 else ((i * 40503) %% p + 977) %% p
    hc <- vapply(seq_len(m), function(j) {
      W <- ilm_id_words(data[[j]], p)
      s <- 0
      for (c in seq_len(ncol(W))) {
        wc <- (w * (c * 2 + 1)) %% p
        s <- (s + sum((W[, c] * wc) %% p)) %% p
      }
      s
    }, 0)
    cw <- if (k == 1L) (seq_len(m) * 7919 + 1) %% p else (seq_len(m) * 104729 + 31) %% p
    meta <- ilm_id_str(paste(names(data), vapply(data, function(x) paste(class(x), collapse = "/"), ""),
                             sep = "\t"), p)
    (sum((hc * cw) %% p) + sum((meta * ((cw + 17) %% p)) %% p)) %% p
  }, 0)
  sprintf("%dx%d:%05x%05x", n, m, as.integer(h[1L]), as.integer(h[2L]))
}

#' Keep a secret out of the record
#'
#' A remedy argument that must not be written down -- the key a column is
#' pseudonymised with -- is given as `ilm_secret("ENV_VAR")`. Its value is
#' read from that environment variable when the remedy is made, and only the
#' call `ilm_secret("ENV_VAR")` appears in the remedy's `change`, the cleaning
#' log and the cleaning script, whose comment names the variable to set.
#'
#' If the variable is not set, an interactive session asks for the value; a
#' non-interactive one stops and names the variable.
#'
#' @param var The environment variable's name.
#' @return The secret's value, a string.
#' @examples
#' Sys.setenv(MY_PSEUDO_KEY = "not a real key")
#' nchar(ilm_secret("MY_PSEUDO_KEY"))
#' Sys.unsetenv("MY_PSEUDO_KEY")
#' @export
ilm_secret <- function(var) {
  if (!is.character(var) || length(var) != 1L || !nzchar(var))
    stop("`var` must name one environment variable.", call. = FALSE)
  v <- Sys.getenv(var, unset = NA_character_)
  if (is.na(v) || !nzchar(v)) {
    if (!interactive())
      stop("the secret this remedy needs is read from the environment variable ",
           var, ", which is not set. Set it (Sys.setenv(", var, " = ...)) and ",
           "run the step again.", call. = FALSE)
    v <- readline(sprintf("Value for %s (it is not stored): ", var))
  }
  v
}

## the environment variables a call reads secrets from
ilm_secret_vars <- function(call) {
  out <- character(0)
  walk <- function(e) {
    if (is.call(e)) {
      if (identical(e[[1L]], as.name("ilm_secret")) && length(e) >= 2L && is.character(e[[2L]]))
        out <<- c(out, e[[2L]])
      for (k in as.list(e)[-1L]) walk(k)
    }
  }
  walk(call)
  unique(out)
}
