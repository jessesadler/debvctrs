## Checks ##


# lsd and bases checks ----------------------------------------------------

lsd_check <- function(l, s, d) {
  # Check that l, s, and d are numeric
  if (!is.numeric(l)) {
    stop(call. = FALSE, "`l` must be a numeric vector")
  }
  if (!is.numeric(s)) {
    stop(call. = FALSE, "`s` must be a numeric vector")
  }
  if (!is.numeric(d)) {
    stop(call. = FALSE, "`d` must be a numeric vector")
  }

  # Check that l, s, and d are same length, length 1, or length 0
  # Including length zero to correspond with what vctrs does
  lengths <- purrr::map_int(list(l, s, d), length)
  non_scalar <- lengths[lengths != 1 & lengths != 0]
  if (length(unique(non_scalar)) > 1L) {
    stop(call. = FALSE, "`l`, `s`, and `d` must be vectors of equal length or length 1")
  }
}

# From integer docs and SO: https://stackoverflow.com/a/4562291
is_natural <- function(x, tol = .Machine$double.eps^0.5) {
  x > tol & abs(x - round(x)) < tol
}

bases_check <- function(bases) {
  if (!is.numeric(bases) | vctrs::vec_size(bases) != 2L | is.null(bases)) {
    stop(call. = FALSE, "`bases` must be a numeric vector of length 2.")
  }
  if (any(rlang::are_na(bases))) {
    stop(call. = FALSE, "`bases` cannot be `NA`.")
  }
  if (!all(is_natural(bases))) {
    stop(call. = FALSE, "`bases` must be natural numbers greater than zero.")
  }
}
