## Checks ##


# lsd and bases checks ----------------------------------------------------

lsd_check <- function(l, s, d) {
  # Check that l, s, and d are numeric
  if (!rlang::is_na(l)) {
    if (!is.numeric(l)) {
      stop(call. = FALSE, "`l` must be a numeric vector")
    }
  }

  if (!rlang::is_na(s)) {
    if (!is.numeric(s)) {
      stop(call. = FALSE, "`s` must be a numeric vector")
    }
  }

  if (!rlang::is_na(d)) {
    if (!is.numeric(d)) {
      stop(call. = FALSE, "`d` must be a numeric vector")
    }
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


# Bases assert ------------------------------------------------------------

bases_assert <- function(bases) {
  bases <- rlang::set_names(bases, NULL) # vec_assert has error if named
  vctrs::vec_assert(bases, ptype = integer(), size = 2)
  rlang::set_names(bases, c("s", "d"))
}

# Bases equivalent --------------------------------------------------------

# Check that bases are equal for two lsd objects
bases_equal <- function(x, y) {
  if (!identical(deb_bases(x), deb_bases(y))) {
    stop(call. = FALSE, "`bases` attributes must be equal to combine <deb_lsd> or <deb_decimal> objects.")
  }
}


# Units equivalent --------------------------------------------------------

unit_equal <- function(x, y) {
  if (!identical(attr(x, "unit"), attr(y, "unit"))) {
    stop(call. = FALSE, "`unit` attributes must be equal to combine <deb_decimal> objects.")
  }
}
