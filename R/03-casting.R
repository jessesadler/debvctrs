## Casting for deb_decimal and deb_lsd ##


# A) deb_decimal ----------------------------------------------------------

# 1. Casting boilerplate --------------------------------------------------

#' @rdname vctrs-compat
#' @method vec_cast deb_decimal
#' @export
#' @export vec_cast.deb_decimal
vec_cast.deb_decimal <- function(x, to, ...) UseMethod("vec_cast.deb_decimal")

#' @method vec_cast.deb_decimal default
#' @export
vec_cast.deb_decimal.default <- function(x, to, ...) {
  vctrs::vec_default_cast(x, to)
}


# 2. deb_decimal to deb_decimal -------------------------------------------

# Must check to ensure bases are equal.
# This ensures two deb_decimal vectors are compatible wherever casting is used.
# Conversion between units enables deb_decimal() vectors with different units
# to be compared and combined.

#' @method vec_cast.deb_decimal deb_decimal
#' @export
vec_cast.deb_decimal.deb_decimal <- function(x, to, ...) {
  bases_equal(x, to)

  from_unit <- deb_unit(x)
  to_unit <- deb_unit(to)

  if (from_unit == to_unit) {
    return(x)
  }

  bases <- deb_bases(x)

  # Change value of x from the from_unit to to_unit
  if (from_unit == "l" && to_unit == "s") {
    converted <- x * bases[[1]]
  } else if (from_unit == "l" && to_unit == "d") {
    converted <- x * prod(bases)
  } else if (from_unit == "s" && to_unit == "d") {
    converted <- x * bases[[2]]
  } else if (from_unit == "s" && to_unit == "l") {
    converted <- x / bases[[1]]
  } else if (from_unit == "d" && to_unit == "l") {
    converted <- x / prod(bases)
  } else if (from_unit == "d" && to_unit == "s") {
    converted <- x / bases[[2]]
  }

  # Change unit attribute to reflect value change from above
  attr(converted, "unit") <- to_unit

  converted
}


# 3. Cast to and from compatible types ------------------------------------

# deb_decimal to double
# Get to underlying data

#' @method vec_cast.double deb_decimal
#' @export
vec_cast.double.deb_decimal  <- function(x, to, ...) vctrs::vec_data(x)

# double to deb_decimal
# Enables comparison between deb_decimal() and double()

#' @method vec_cast.deb_decimal double
#' @export
vec_cast.deb_decimal.double  <- function(x, to, ...) {
  deb_decimal(x, unit = deb_unit(to), bases = deb_bases(to))
}

# integer to deb_decimal
# Enables comparison between deb_decimal() and integer()

#' @method vec_cast.deb_decimal integer
#' @export
vec_cast.deb_decimal.integer  <- function(x, to, ...) {
  deb_decimal(x, unit = deb_unit(to), bases = deb_bases(to))
}

# deb_decimal to character
# Simple print method

#' @method vec_cast.character deb_decimal
#' @export
vec_cast.character.deb_decimal <- function(x, to, ...) {
  as.character(vctrs::vec_data(x))
}


# B) deb_lsd --------------------------------------------------------------

# 1. Casting boilerplate --------------------------------------------------

#' @rdname vctrs-compat
#' @method vec_cast deb_lsd
#' @export
#' @export vec_cast.deb_lsd
vec_cast.deb_lsd <- function(x, to, ...) UseMethod("vec_cast.deb_lsd")

#' @method vec_cast.deb_lsd default
#' @export
vec_cast.deb_lsd.default <- function(x, to, ...) vctrs::vec_default_cast(x, to)


# 2. deb_lsd to deb_lsd ---------------------------------------------------

# Must check to ensure bases are equal.
# This ensures two deb_lsd() vectors are compatible wherever casting is used.

#' @method vec_cast.deb_lsd deb_lsd
#' @export
vec_cast.deb_lsd.deb_lsd <- function(x, to, ...) {
  bases_equal(x, to)
  x
}

# 3. Cast to and from compatible types ------------------------------------

# deb_lsd to double
# Provides a way out of deb_lsd to base vector

#' @method vec_cast.double deb_lsd
#' @export
vec_cast.double.deb_lsd <- function(x, to, ...) {
  l <- vctrs::field(x, "l")
  s <- vctrs::field(x, "s")
  d <- vctrs::field(x, "d")
  bases <- deb_bases(x)

  l + s / bases[[1]] + d / prod(bases)
}

# double to deb_lsd
# Enables comparison between deb_lsd() and double()

#' @method vec_cast.deb_lsd double
#' @export
vec_cast.deb_lsd.double <- function(x, to, ...) {
  lsd <- deb_lsd(x, 0, 0, bases = deb_bases(to))
  deb_normalize(lsd)
}

# integer to deb_lsd
# Enables comparison between deb_lsd() and integer()

#' @method vec_cast.deb_lsd integer
#' @export
vec_cast.deb_lsd.integer <- function(x, to, ...) {
  deb_lsd(x, 0, 0, bases = deb_bases(to))
}

# deb_lsd to character
# Enables View(as.data.frame(deb_lsd))

#' @method vec_cast.character deb_lsd
#' @export
vec_cast.character.deb_lsd <- function(x, to, ...) {
  format(x, ...)
}


# C) Casting between deb_decimal and deb_lsd ------------------------------

# deb_decimal to deb_lsd

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

#' @method vec_cast.deb_lsd deb_decimal
#' @export
vec_cast.deb_lsd.deb_decimal <- function(x, to, ...) {
  bases_equal(x, to)

  decimal_to_lsd(x)
}

# deb_lsd to deb_decimal

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

#' @method vec_cast.deb_decimal deb_lsd
#' @export
vec_cast.deb_decimal.deb_lsd <- function(x, to, ...) {
  bases_equal(x, to)

  lsd_to_decimal(x, to)
}


# 4. Casting function -----------------------------------------------------

# May be able to just create a casting function: as_myclass()
# Due to issues with conversion of attributes, it makes sense to implement a
# generic with methods for the different types that use different arguments.
# For instance, deb_as_decimal() should not be used to convert unit or bases.
# Therefore, these arguments are not listed in the method for deb_decimal.

# A) deb_decimal casting generic and methods ------------------------------

#' Cast to `deb_decimal`
#'
#' Cast `x` to a `deb_decimal` vector.
#'
#' @param x An object to coerce to `deb_decimal`.
#' @param ... Arguments passed on to further methods.
#' @param unit A character vector of length one indicating the unit for the
#'   decimalized values, either `"l"` (pounds, the default), `"s"` (shillings),
#'   or `"d"` (pence).
#' @param bases Numeric vector of length 2 used to specify the bases for the
#'   solidus or s and denarius or d units. Default is `c(20, 12)`, which
#'   conforms to the most widely used system of 1 pound = 20 shillings and
#'   1 shilling = 12 pence.
#'
#' @name cast-decimal
NULL

# Generic function
#' @rdname cast-decimal
#' @export
deb_as_decimal <- function(x, ...) {
  UseMethod("deb_as_decimal")
}

# From deb_decimal
#' @rdname cast-decimal
#' @export
deb_as_decimal.deb_decimal <- function(x, ...) x

# From deb_lsd
#' @rdname cast-decimal
#' @export
deb_as_decimal.deb_lsd <- function(x, unit = c("l", "s", "d"), ...) {
  unit <- rlang::arg_match(unit)
  lsd_to_decimal(x, to = deb_decimal(unit = unit))
}

# From numeric
#' @rdname cast-decimal
#' @export
deb_as_decimal.numeric <- function(x,
                                   unit = c("l", "s", "d"),
                                   bases = c(20, 12), ...) {
  vctrs::vec_cast(x, to = deb_decimal(unit = unit, bases = bases))
}

# From logical (NA)
#' @rdname cast-decimal
#' @export
deb_as_decimal.logical <- function(x,
                                   unit = c("l", "s", "d"),
                                   bases = c(20, 12), ...) {
  vctrs::vec_cast(x, to = deb_decimal(unit = unit, bases = bases))
}


# B) deb_lsd casting generic and methods ----------------------------------

#' Cast to `deb_lsd`
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
#' @name cast-lsd
NULL

# Generic function
#' @rdname cast-lsd
#' @export
deb_as_lsd  <- function(x, ...) {
  UseMethod("deb_as_lsd")
}

# From deb_lsd
#' @rdname cast-lsd
#' @export
deb_as_lsd.deb_lsd <- function(x, ...) x

# From deb_decimal
#' @rdname cast-lsd
#' @export
deb_as_lsd.deb_decimal <- function(x, ...) {
  decimal_to_lsd(x)
}

# From numeric
#' @rdname cast-lsd
#' @export
deb_as_lsd.numeric <- function(x, bases = c(20, 12), ...) {
  vctrs::vec_cast(x, to = deb_lsd(bases = bases))
}

# From logical (NA)
#' @rdname cast-lsd
#' @export
deb_as_lsd.logical <- function(x, bases = c(20, 12), ...) {
  vctrs::vec_cast(x, to = deb_lsd(bases = bases))
}
