## Checks ##


# lsd checks --------------------------------------------------------------

# Informative error messages for l, s, and d values in deb_lsd
lsd_check <- function(l, s, d) {
  # Check that l, s, and d are numeric
  if (!all(rlang::are_na(l))) {
    if (!is.numeric(l)) {
      stop(call. = FALSE, "`l` must be a numeric vector.")
    }
  }

  if (!all(rlang::are_na(s))) {
    if (!is.numeric(s)) {
      stop(call. = FALSE, "`s` must be a numeric vector.")
    }
  }

  if (!all(rlang::are_na(d))) {
    if (!is.numeric(d)) {
      stop(call. = FALSE, "`d` must be a numeric vector.")
    }
  }

  # Check that l, s, and d are same length, length 1, or all length 0
  lengths <- c(vec_size(l), vec_size(s), vec_size(d))

  # Must be either all zero length or no zero length
  if (sum(lengths) == 1L || sum(lengths) == 2L) {
    stop(call. = FALSE,
         paste0("`l`, `s`, and `d` must all have values. ",
                "You may have forgotten a value or need to use 0."))
  }

  # Must be only one length other than scalar
  non_scalar <- lengths[lengths != 1L]
  if (length(unique(non_scalar)) > 1L) {
    stop(call. = FALSE,
         "`l`, `s`, and `d` must be vectors of equal length or length 1.")
  }
}


# bases check -------------------------------------------------------------

# Check that bases are natural number: whole number greater than 0
# From integer docs and SO: https://stackoverflow.com/a/4562291
is_natural <- function(x, tol = .Machine$double.eps^0.5) {
  x > tol & abs(x - round(x)) < tol
}

# Informative error messages for bases attribute
bases_check <- function(bases) {
  if (!is.numeric(bases) || vctrs::vec_size(bases) != 2L || is.null(bases)) {
    stop(call. = FALSE, "`bases` must be a numeric vector of length 2.")
  }
  if (any(rlang::are_na(bases))) {
    stop(call. = FALSE, "`bases` cannot be `NA`.")
  }
  if (!all(is_natural(bases))) {
    stop(call. = FALSE, "`bases` must be natural numbers greater than zero.")
  }
}

# Bases equivalent --------------------------------------------------------

# Check that bases are equal for two deb-style objects
# Vectors with different bases cannot be combined.
bases_equal <- function(x, y) {
  if (!identical(deb_bases(x), deb_bases(y))) {
    stop(call. = FALSE,
         paste0("`bases` attributes must be equal to combine <deb_lsd> ",
                "or <deb_decimal> objects."))
  }
}
