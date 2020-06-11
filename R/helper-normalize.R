## Normalize values in deb_lsd vectors ##

# Normalization is the process of converting a set of compound units to a
# standard form consistent with the bases for each unit in a manner similar to
# "carrying over" digits in decimal arithmetic. Normalization is central to
# integrating non-decimal currencies into R.

# 1. Convert l and s units to whole numbers -----------------------------

# Function to help deal with floating point problems
should_be_int <- function(x, tol = .Machine$double.eps^0.5) {
  abs(x - round(x)) < tol
}

# Deal with any decimals in l and s units
decimal_check <- function(lsd) {
  l <- field(lsd, "l")
  s <- field(lsd, "s")
  d <- field(lsd, "d")

  field(lsd, "l") <- trunc(l)
  temp_s <- s + (l - trunc(l)) * deb_bases(lsd)[[1]]
  field(lsd, "s") <- trunc(temp_s)
  field(lsd, "d") <- d + (temp_s - trunc(temp_s)) * deb_bases(lsd)[[2]]

  # Deal with floating point problems potentially introduced by the above
  field(lsd, "d") <- if_else(should_be_int(field(lsd, "d")),
                             round(field(lsd, "d")),
                             field(lsd, "d"))

  lsd
}


# 2. Normalization --------------------------------------------------------

# Different parts of the normalization function

# a) Is lsd value positive or negative?
is_negative <- function(x) {
  field(x, "l") + field(x, "s") /
    deb_bases(x)[[1]] + field(x, "d") / prod(deb_bases(x)) < 0
}

# b) Normalization for positive lsd values
lsd_normalize <- function(lsd) {
  l <- field(lsd, "l")
  s <- field(lsd, "s")
  d <- field(lsd, "d")
  bases <- deb_bases(lsd)

  field(lsd, "l") <- l + ((s + d %/% bases[[2]]) %/% bases[[1]])
  field(lsd, "s") <- (s + d %/% bases[[2]]) %% bases[[1]]
  field(lsd, "d") <- d %% bases[[2]]

  lsd
}

# c) Normalization for negative lsd values
lsd_normalize_neg <- function(lsd) {
  l <- -field(lsd, "l")
  s <- -field(lsd, "s")
  d <- -field(lsd, "d")
  bases <- deb_bases(lsd)

  field(lsd, "l") <- l + ((s + d %/% bases[[2]]) %/% bases[[1]])
  field(lsd, "s") <- (s + d %/% bases[[2]]) %% bases[[1]]
  field(lsd, "d") <- d %% bases[[2]]

  -lsd
}


# 3. deb_normalize methods ------------------------------------------------

#' Normalize pounds, shillings, and pence
#'
#' @param x Either an object of class `deb_lsd` or a numeric vector of
#'   length 3 representing the values to be normalized.
#' @param bases Used only if `x` is a numeric vector. A Numeric vector of
#'   length 2 used to specify the bases for the shillings or s and pence or
#'   d units. Default is `c(20, 12)`, which conforms to the most widely used
#'   system of 1 pound = 20 shillings and 1 shilling = 12 pence.
#' @param ... Arguments passed on to further methods.
#'
#' @name normalize
NULL

# Generic
#' @rdname normalize
#' @export
deb_normalize <- function(x, ...) {
  UseMethod("deb_normalize")
}

# a) Default
#' @rdname normalize
#' @export
deb_normalize.default <- function(x, ...) {
  stop(call. = FALSE,
       "`x` must be a <deb_lsd> vector or a numeric vector of length 3.")
}

# b) deb_lsd
#' @rdname normalize
#' @export
deb_normalize.deb_lsd <- function(x, ...) {
  decimals <- decimal_check(x)
  if_else(is_negative(x),
          lsd_normalize_neg(decimals),
          lsd_normalize(decimals))
}

# c) numeric: provides a quick way to normalize a single value
#' @rdname normalize
#' @export
deb_normalize.numeric <- function(x, bases = c(20, 12), ...) {
  if (vec_size(x) != 3L) {
    stop(call. = FALSE, "`x` must be a numeric vector of length 3.")
  }

  lsd <- deb_lsd(x[[1]], x[[2]], x[[3]], bases)
  deb_normalize(lsd)
}
