## bases and unit conversion ##

# Convert bases -----------------------------------------------------------

#' Convert bases of pounds, shillings, and pence values
#'
#' Convert the bases of the solidus and denarius units of `deb_lsd` or
#' `deb_decimal` objects.
#'
#' `deb_convert_bases()` is the only way to change the bases of the solidus
#' and denarius units associated with objects of class `deb_lsd` or `deb_lsd`.
#'
#' @param x An object of class `deb_lsd` or `deb_decimal`.
#' @param to Numeric vector of length 2 representing the bases for the
#'   solidus and denarius units to be converted to.
#'
#' @return An object of the same class as `x` with converted `bases` attribute.
#' @examples
#'
#' x <- deb_lsd(5, 3, 8)
#' y <- deb_decimal(8.825)
#'
#' deb_convert_bases(x, to = c(60, 16))
#' deb_convert_bases(y, to = c(60, 16))
#'
#' @name convert-bases
NULL

#' @rdname convert-bases
#' @export
deb_convert_bases <- function(x, to) {
  UseMethod("deb_convert_bases")
}

deb_convert_bases.default <- function(x, to) {
  stop(call. = FALSE,
       "`x` must be a <deb_lsd>  or <deb_decimal> vector.")
}

#' @rdname convert-bases
#' @export
deb_convert_bases.deb_lsd <- function(x, to) {
  bases_check(to)

  from <- deb_bases(x)
  to <- vctrs::vec_cast(to, to = integer())
  to <- rlang::set_names(to, c("s", "d"))

  if (identical(from, to)) {
    return(x)
  }

  temp_s <- vctrs::field(x, "s") * to[[1]] / from[[1]]
  vctrs::field(x, "s") <- trunc(temp_s)
  vctrs::field(x, "d") <- (temp_s - trunc(temp_s)) * to[[2]] +
    vctrs::field(x, "d") * prod(to) / prod(from)
  attr(x, "bases") <- to

  deb_normalize(x)
}

#' @rdname convert-bases
#' @export
deb_convert_bases.deb_decimal <- function(x, to) {
  bases_check(to)

  from <- deb_bases(x)
  to <- vctrs::vec_cast(to, to = integer())
  to <- rlang::set_names(to, c("s", "d"))

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

# Convert units -----------------------------------------------------------

#' Convert the unit of a `deb_decimal` object
#'
#' Convert the `unit` attribute of a `deb_decimal` object.
#'
#' `deb_convert_unit()` converts the `unit` of a `deb_decimal` object to
#' either `"l"`, `"s"`, or `"d"`. This changes the representation of the
#' object, but the value remains equivalent.
#'
#' @param x An object of class `deb_decimal`.
#' @param to A character vector of length one indicating the unit to be
#'   converted to. Choice of `"l"` (libra, the default), `"s"` (solidus),
#'   or `"d"` (denarius).
#'
#' @return A `deb_decimal` object with a converted unit.
#' @export
#' @examples
#'
#' x <- deb_decimal(c(8.825, 15.125, 3.65))
#' y <- deb_decimal(c(56.45, 106.525, 200.4), unit = "s")
#'
#' deb_convert_unit(x, to = "s")
#' deb_convert_unit(x, to = "d")
#' deb_convert_unit(y, to = "l")
#' deb_convert_unit(y, to = "d")

deb_convert_unit <- function(x, to = c("l", "s", "d")) {
  if (!deb_is_decimal(x)) {
    stop(call. = FALSE, "`x` must be a <deb_decimal> object.")
  }
  to <- rlang::arg_match(to)
  from <- deb_unit(x)
  bases <- deb_bases(x)

  if (from == to) {
    return(x)
  }

  converted <- dplyr::case_when(
    from == "l" & to == "s" ~ x * bases[[1]],
    from == "l" & to == "d" ~ x * prod(bases),
    from == "s" & to == "d" ~ x * bases[[2]],
    from == "s" & to == "l" ~ x / bases[[1]],
    from == "d" & to == "l" ~ x / prod(bases),
    from == "d" & to == "s" ~ x / bases[[2]]
  )

  attr(converted, "unit") <- to

  converted
}
