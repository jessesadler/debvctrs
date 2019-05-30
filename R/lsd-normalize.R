## Normalize lsd values ##

# To help deal with floating point problems in lsd_decimal
should_be_int <- function(x, tol = .Machine$double.eps^0.5) {
  abs(x - round(x)) < tol
}

decimal_check <- function(lsd) {
  l <- vctrs::field(lsd, "l")
  s <- vctrs::field(lsd, "s")
  d <- vctrs::field(lsd, "d")

  vctrs::field(lsd, "l") <- trunc(l)
  temp_s <- s + (l - trunc(l)) * deb_bases(lsd)[[1]]
  vctrs::field(lsd, "s") <- trunc(temp_s)
  vctrs::field(lsd, "d") <- d + (temp_s - trunc(temp_s)) * deb_bases(lsd)[[2]]
  # Deal with floating point problems potentially introduced by the above
  vctrs::field(lsd, "d") <- dplyr::if_else(should_be_int(vctrs::field(lsd, "d")),
                                           round(vctrs::field(lsd, "d")),
                                           vctrs::field(lsd, "d"))

  lsd
}

is_negative <- function(x) {
  vctrs::field(x, "l") + vctrs::field(x, "s") /
    deb_bases(x)[[1]] + vctrs::field(x, "d") / prod(deb_bases(x)) < 0
}

lsd_normalize <- function(lsd) {
  l <- vctrs::field(lsd, "l")
  s <- vctrs::field(lsd, "s")
  d <- vctrs::field(lsd, "d")
  bases <- deb_bases(lsd)

  vctrs::field(lsd, "l") <- l + ((s + d %/% bases[[2]]) %/% bases[[1]])
  vctrs::field(lsd, "s") <- (s + d %/% bases[[2]]) %% bases[[1]]
  vctrs::field(lsd, "d") <- d %% bases[[2]]

  lsd
}

lsd_normalize_neg <- function(lsd) {
  l <- -vctrs::field(lsd, "l")
  s <- -vctrs::field(lsd, "s")
  d <- -vctrs::field(lsd, "d")
  bases <- deb_bases(lsd)

  vctrs::field(lsd, "l") <- l + ((s + d %/% bases[[2]]) %/% bases[[1]])
  vctrs::field(lsd, "s") <- (s + d %/% bases[[2]]) %% bases[[1]]
  vctrs::field(lsd, "d") <- d %% bases[[2]]

  -lsd
}


# deb_normalize methods ---------------------------------------------------

#' Normalize pounds, shillings, and pence
#'
#' Normalize pounds, shillings, and pence values to given bases of solidus
#' and denarius units.
#'
#' @param x Either an object of class `deb_lsd` or a numeric vector of
#'   length 3 representing the values to be normalized.
#' @param bases Used only if `x` is a numeric vector. A Numeric vector of
#'   length 2 used to specify the bases for the shillings or s and pence or
#'   d units. Default is `c(20, 12)`, which conforms to the most widely used
#'   system of 1 pound = 20 shillings and 1 shilling = 12 pence.
#' @param ... Arguments passed on to further methods.
#'
#' @return Returns an object of class `deb_lsd` with normalized solidus and
#'   denarius units.
#' @examples
#'
#' # Normalize a deb_lsd object
#' x <- deb_lsd(12, 93, 78)
#' y <- deb_lsd(12, 93, 78, bases = c(60, 16))
#' deb_normalize(x)
#' deb_normalize(y)
#'
#' # Normalize a numeric vector of length 3
#' deb_normalize(c(12, 93, 78), bases = c(20, 12))
#' deb_normalize(c(12, 93, 78), bases = c(60, 16))
#'
#' @name normalize
NULL

#' @rdname normalize
#' @export
deb_normalize <- function(x, ...) {
  UseMethod("deb_normalize")
}

#' @rdname normalize
#' @export
deb_normalize.default <- function(x, ...) {
  stop(call. = FALSE,
       "`x` must be a <deb_lsd> vector or a numeric vector of length 3.")
}

#' @rdname normalize
#' @export
deb_normalize.deb_lsd <- function(x, ...) {
  decimals <- decimal_check(x)
  dplyr::if_else(is_negative(x),
                 lsd_normalize_neg(decimals),
                 lsd_normalize(decimals))
}

#' @rdname normalize
#' @export
deb_normalize.numeric <- function(x, bases = c(20, 12), ...) {
  if (vctrs::vec_size(x) != 3L) {
    stop(call. = FALSE, "`x` must be a numeric vector of length 3.")
  }

  lsd <- deb_lsd(x[[1]], x[[2]], x[[3]], bases)
  deb_normalize(lsd)
}
