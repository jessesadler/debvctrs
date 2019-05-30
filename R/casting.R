## Casting for deb_lsd and deb_decimal ##

# deb_lsd -----------------------------------------------------------------

# Boilerplate

#' @rdname vctrs-compat
#' @method vec_cast deb_lsd
#' @export
#' @export vec_cast.deb_lsd
vec_cast.deb_lsd <- function(x, to) UseMethod("vec_cast.deb_lsd")

#' @method vec_cast.deb_lsd default
#' @export
vec_cast.deb_lsd.default <- function(x, to) vctrs::stop_incompatible_cast(x, to)

#' @method vec_cast.deb_lsd logical
#' @export
vec_cast.deb_lsd.logical <- function(x, to) vctrs::vec_unspecified_cast(x, to)

# deb_lsd to deb_lsd

#' @method vec_cast.deb_lsd deb_lsd
#' @export
vec_cast.deb_lsd.deb_lsd <- function(x, to) {
  bases_equal(x, to)
  x
}

# deb_lsd to double

#' @method vec_cast.deb_lsd double
#' @export
vec_cast.double.deb_lsd <- function(x, to) {
  l <- vctrs::field(x, "l")
  s <- vctrs::field(x, "s")
  d <- vctrs::field(x, "d")
  bases <- deb_bases(x)

  l + s / bases[[1]] + d / prod(bases)
}

# double to deb_lsd

#' @method vec_cast.double deb_lsd
#' @export
vec_cast.deb_lsd.double <- function(x, to) {
  lsd <- deb_lsd(x, 0, 0, bases = deb_bases(to))
  deb_normalize(lsd)
}

# integer to deb_lsd

#' @method vec_cast.deb_lsd integer
#' @export
vec_cast.deb_lsd.integer <- function(x, to) {
  deb_lsd(x, 0, 0, bases = deb_bases(to))
}

# deb_decimal -------------------------------------------------------------

# Boilerplate

#' @rdname vctrs-compat
#' @method vec_cast deb_decimal
#' @export
#' @export vec_cast.deb_decimal
vec_cast.deb_decimal <- function(x, to) UseMethod("vec_cast.deb_decimal")

#' @method vec_cast.deb_decimal default
#' @export
vec_cast.deb_decimal.default <- function(x, to) vctrs::stop_incompatible_cast(x, to)

#' @method vec_cast.deb_decimal logical
#' @export
vec_cast.deb_decimal.logical <- function(x, to) vctrs::vec_unspecified_cast(x, to)

# deb_decimal to deb_decimal

#' @method vec_cast.deb_decimal deb_decimal
#' @export
vec_cast.deb_decimal.deb_decimal <- function(x, to) {
  bases_equal(x, to)
  unit_equal(x, to)
  x
}

# double to deb_decimal and back

#' @method vec_cast.deb_decimal double
#' @export
vec_cast.deb_decimal.double  <- function(x, to) {
  deb_decimal(x, unit = deb_unit(to), bases = deb_bases(to))
}

#' @method vec_cast.double deb_decimal
#' @export
vec_cast.double.deb_decimal  <- function(x, to) vctrs::vec_data(x)

# integer to deb_decimal

#' @method vec_cast.deb_decimal integer
#' @export
vec_cast.deb_decimal.integer  <- function(x, to) {
  deb_decimal(x, unit = deb_unit(to), bases = deb_bases(to))
}

# deb_decimal to deb_lsd --------------------------------------------------

decimal_to_lsd <- function(x) {
  bases <- deb_bases(x)
  unit <- deb_unit(x)

  if (unit == "l") {
    lsd <- deb_lsd(x, 0, 0, bases = bases)
  } else if (unit == "s") {
    lsd <- deb_lsd(0, x, 0, bases = bases)
  } else if (unit == "d") {
    lsd <- deb_lsd(0, 0, x, bases = bases)
  }
  deb_normalize(lsd)
}

#' @method vec_cast.deb_decimal deb_lsd
#' @export
vec_cast.deb_lsd.deb_decimal <- function(x, to) {
  bases_equal(x, to)

  decimal_to_lsd(x)
}

# deb_lsd to deb_decimal --------------------------------------------------

lsd_to_decimal <- function(x, to) {
  l <- vctrs::field(x, "l")
  s <- vctrs::field(x, "s")
  d <- vctrs::field(x, "d")
  bases <- deb_bases(x)
  unit <- deb_unit(to)

  if (unit == "l") {
    decimalized <- l + s / bases[[1]] + d / prod(bases)
  } else if (unit == "s") {
    decimalized <- l * bases[[1]] + s + d / bases[[2]]
  } else if (unit == "d") {
    decimalized <- l * prod(bases) + s * bases[[2]] + d
  }
  new_decimal(x = decimalized,
              unit = unit,
              bases = bases)
}

#' @method vec_cast.deb_lsd deb_decimal
#' @export
vec_cast.deb_decimal.deb_lsd <- function(x, to) {
  bases_equal(x, to)

  lsd_to_decimal(x, to)
}


# deb_lsd casting methods -------------------------------------------------

#' Cast to deb_lsd
#'
#' Cast `x` to a `deb_lsd` vector.
#'
#' @param x An object to coerce to `deb_lsd`.
#' @param ... Arguments passed on to further methods.
#' @param bases Numeric vector of length 2 used to specify the bases for the
#'   solidus or s and denarius or d units. Default is `c(20, 12)`, which
#'   conforms to the most widely used system of 1 pound = 20 shillings and
#'   1 shilling = 12 pence.
#'
#' @return A `deb_lsd` object.
#'
#' @examples
#'
#' # Cast a deb_decimal object to deb_lsd
#' x <- c(5.825, 3.25, 22/3)
#' d1 <- deb_decimal(x)
#' deb_as_lsd(d1)
#'
#' # Bases are automatically applied when
#' # casting from deb_decimal to deb_lsd
#' d2 <- deb_decimal(x, bases = c(60, 16))
#' deb_as_lsd(d2)
#'
#' # Cast a numeric vector to deb_lsd
#' deb_as_lsd(x)
#'
#' # Use the bases argument to apply non-default bases
#' deb_as_lsd(x, bases = c(60, 16))

#' @rdname cast-lsd
#' @export
deb_as_lsd  <- function(x, ...) {
  UseMethod("deb_as_lsd")
}

#' @rdname cast-lsd
#' @export
deb_as_lsd.default <- function(x, ...) {
  vctrs::stop_incompatible_cast(x, deb_lsd())
}

#' @rdname cast-lsd
#' @export
deb_as_lsd.deb_lsd <- function(x, ...) x

#' @rdname cast-lsd
#' @export
deb_as_lsd.deb_decimal <- function(x, ...) {
  decimal_to_lsd(x)
}

#' @rdname cast-lsd
#' @export
deb_as_lsd.numeric <- function(x, bases = c(20, 12), ...) {
  vctrs::vec_cast(x, to = deb_lsd(bases = bases))
}

#' @rdname cast-lsd
#' @export
deb_as_lsd.logical <- function(x, bases = c(20, 12), ...) {
  vctrs::vec_cast(x, to = deb_lsd(bases = bases))
}

# deb_decimal casting methods ---------------------------------------------

#' Cast to deb_decimal
#'
#' Cast `x` to a `deb_decimal` vector.
#'
#' @param x An object to coerce to `deb_decimal`.
#' @param ... Arguments passed on to further methods.
#' @param unit A character vector of length one indicating the unit for the
#'   decimalized values, either `"l"` (libra, the default), `"s"` (solidus),
#'   or `"d"` (denarius).
#' @param bases Numeric vector of length 2 used to specify the bases for the
#'   solidus or s and denarius or d units. Default is `c(20, 12)`, which
#'   conforms to the most widely used system of 1 pound = 20 shillings and
#'   1 shilling = 12 pence.
#'
#' @return A `deb_decimal` object.
#'
#' @examples
#'
#' # Cast a deb_lsd object to deb_decimal
#' x <- deb_lsd(l = c(5, 3, 7),
#'              s = c(16, 5, 6),
#'              d = c(6, 0, 8))
#' deb_as_decimal(x)
#'
#' # Bases are automatically applied when
#' # casting from deb_lsd to deb_decimal
#' x2 <- deb_lsd(l = c(5, 3, 7),
#'               s = c(16, 5, 6),
#'               d = c(6, 0, 8),
#'               bases = c(60, 16))
#' deb_as_decimal(x2)
#'
#' # Cast a numeric vector to deb_decimal
#' y <- c(5.825, 3.25, 22/3)
#' deb_as_decimal(y)
#'
#' # Use the unit and bases arguments to specify
#' # the unit and apply non-default bases
#' deb_as_decimal(y, unit = "s", bases = c(60, 16))

#' @rdname cast-decimal
#' @export

#' @rdname cast-decimal
#' @export
deb_as_decimal <- function(x, ...) {
  UseMethod("deb_as_decimal")
}

#' @rdname cast-decimal
#' @export
deb_as_decimal.default <- function(x, ...) {
  vctrs::stop_incompatible_cast(x, deb_decimal())
}

#' @rdname cast-decimal
#' @export
deb_as_decimal.deb_decimal <- function(x, ...) x

#' @rdname cast-decimal
#' @export
deb_as_decimal.deb_lsd <- function(x, unit = c("l", "s", "d"), ...) {
  unit <- rlang::arg_match(unit)
  lsd_to_decimal(x, to = deb_decimal(unit = unit))
}

#' @rdname cast-decimal
#' @export
deb_as_decimal.numeric <- function(x,
                                   unit = c("l", "s", "d"),
                                   bases = c(20, 12), ...) {
  vctrs::vec_cast(x, to = deb_decimal(unit = unit, bases = bases))
}

#' @rdname cast-decimal
#' @export
deb_as_decimal.logical <- function(x,
                                   unit = c("l", "s", "d"),
                                   bases = c(20, 12), ...) {
  vctrs::vec_cast(x, to = deb_decimal(unit = unit, bases = bases))
}
