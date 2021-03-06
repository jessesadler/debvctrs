## Convert the bases and units of decimal and lsd vectors ##

# A) Convert bases --------------------------------------------------------

#' Convert bases of `deb_lsd` and `deb_decimal` vectors
#'
#' `deb_convert_bases()` is the only way to change the bases of the solidus
#' and denarius units associated with `deb_lsd` or `deb_lsd`types.
#'
#' @param x An object of class `deb_lsd` or `deb_decimal`.
#' @param to Numeric vector of length 2, representing the bases for the
#'   solidus and denarius units to be converted to.
#'
#' @name convert-bases
NULL

# Generic
#' @rdname convert-bases
#' @export
deb_convert_bases <- function(x, to) {
  UseMethod("deb_convert_bases")
}

# Default
deb_convert_bases.default <- function(x, to) {
  stop(call. = FALSE,
       "`x` must be a <deb_lsd> or <deb_decimal> vector.")
}

# 1. deb_lsd()
#' @rdname convert-bases
#' @export
deb_convert_bases.deb_lsd <- function(x, to) {
  bases_check(to)

  from <- deb_bases(x)
  to <- vec_cast(to, to = integer())

  if (identical(from, to)) {
    return(x)
  }

  # Conversion arithmetic
  temp_s <- field(x, "s") * to[[1]] / from[[1]]
  field(x, "s") <- trunc(temp_s)
  field(x, "d") <- (temp_s - trunc(temp_s)) * to[[2]] +
    field(x, "d") * prod(to) / prod(from)
  attr(x, "bases") <- to

  deb_normalize(x)
}

# 2. deb_decimal()
#' @rdname convert-bases
#' @export
deb_convert_bases.deb_decimal <- function(x, to) {
  bases_check(to)

  from <- deb_bases(x)
  to <- vec_cast(to, to = integer())

  # Conversion arithmetic
  if (deb_unit(x) == "l") {
    converted <- x
  } else if (deb_unit(x) == "s") {
    converted <- x * to[[1]] / from[[1]]
  } else if (deb_unit(x) == "d") {
    converted <- x * prod(to) / prod(from)
  }

  attr(converted, "bases") <- to

  converted
}

# B) Convert units --------------------------------------------------------

#' Convert the unit of `deb_decimal` vectors
#'
#' @param x An object of class `deb_decimal`.
#' @param to A character vector of length one indicating the unit to be
#'   converted to. Choice of `"l"` (pounds, the default), `"s"` (shillings),
#'   or `"d"` (pence).
#' @export

deb_convert_unit <- function(x, to = c("l", "s", "d")) {
  if (!deb_is_decimal(x)) {
    stop(call. = FALSE, "`x` must be a <deb_decimal> object.")
  }
  to_unit <- rlang::arg_match(to)
  # Actual conversion done by vec_cast()
  vec_cast(x, deb_decimal(unit = to_unit, bases = deb_bases(x)))
}
