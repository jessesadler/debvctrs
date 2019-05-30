## Define deb_lsd class ##

# Constructor -------------------------------------------------------------

#' Internal constructor to create deb_lsd type
#'
#' Asserts that `l`, `s` and `d` are of type `double()` and that `bases` is an
#' `integer()` of length 2. Creates the object through `vctrs::new_rcrd()`.
#'
#' @return An object of class `deb_lsd`.
#' @keywords internal

new_lsd <- function(l = double(),
                    s = double(),
                    d = double(),
                    bases = c(20L, 12L)) {

  vctrs::vec_assert(l, ptype = double())
  vctrs::vec_assert(s, ptype = double())
  vctrs::vec_assert(d, ptype = double())

  bases <- bases_assert(bases)

  vctrs::new_rcrd(list(l = l, s = s, d = d),
                  bases = bases,
                  class = "deb_lsd")
}


# Helper ------------------------------------------------------------------

#' A class for pounds, shillings and pence values
#'
#' Create an object of class `deb_lsd` to integrate non-decimal currencies
#' into standardized forms of analysis provided by R.
#'
#' The `deb_lsd` class and the `debkeepr` package use the nomenclature of
#' [l, s, and d](https://en.wikipedia.org/wiki/£sd) to represent pounds,
#' shillings, and pence units. The abbreviations derive from the Latin terms
#' [libra](https://en.wikipedia.org/wiki/French_livre),
#' [solidus](https://en.wikipedia.org/wiki/Solidus_(coin)), and
#' [denarius](https://en.wikipedia.org/wiki/Denarius). In the 8th century a
#' solidus came to represent 12 denarii coins, and 240 denarii were made from
#' one libra or pound of silver. The custom of counting coins in dozens
#' (solidi) and scores of dozens (librae) spread throughout the Carolingian
#' Empire and became engrained in much of Europe. However,
#' [other bases](https://en.wikipedia.org/wiki/Non-decimal_currency) for the
#' solidus and denarius units were also in use. The `bases` attribute makes
#' it possible to specify alternative bases for the solidus and denarius units.
#'
#' The length of `l`, `s`, and `d` must either be all equal or a vector of
#' length 1 can be recycled to the length of the other argument(s). See
#' the [vctrs package](https://vctrs.r-lib.org/articles/type-size.html)
#' for further details on recycling vectors. In addition, `l`, `s`, and `d`
#' must either all have no values—resulting in a vector of length 0—or all
#' possess numeric vectors.
#'
#' The `deb_lsd` class works in concert with the `deb_decimal` class, which
#' represents non-decimal currencies as decimalized values.
#'
#' @param l Numeric vector representing the pounds unit.
#' @param s Numeric vector representing the shillings unit.
#' @param d Numeric vector representing the pence unit.
#' @param bases Numeric vector of length 2 used to specify the bases for the
#'   solidus or s and denarius or d units. Default is `c(20, 12)`, which
#'   conforms to the most widely used system of 1 pound = 20 shillings and
#'   1 shilling = 12 pence.
#'
#' @return Returns an object of class `deb_lsd`.
#' @export
#' @examples
#'
#' deb_lsd(5, 3, 8)
#' deb_lsd(l = c(10, 8, 5),
#'         s = c(6, 13, 8),
#'         d = c(8, 4, 10))
#'
#' # Recycle length 1 vector
#' deb_lsd(l = c(10, 8, 5),
#'         s = c(6, 13, 8),
#'         d = 0)
#'
#' # Set the `bases` of the object
#' deb_lsd(5, 3, 8, bases = c(60, 16))
#' deb_lsd(l = c(10, 28, 5),
#'         s = c(6, 33, 13),
#'         d = c(8, 42, 10),
#'         bases = c(60, 16))
#'
#' # Create a prototype or object of length 0
#' deb_lsd()

deb_lsd <- function(l = double(),
                    s = double(),
                    d = double(),
                    bases = c(20, 12)) {
  # checks
  lsd_check(l, s, d)
  bases_check(bases)

  c(l, s, d) %<-% vctrs::vec_cast_common(l, s, d, .to = double())
  c(l, s, d) %<-% vctrs::vec_recycle_common(l, s, d)

  bases <- vctrs::vec_cast(bases, to = integer())

  new_lsd(l = l, s = s, d = d, bases = bases)
}


# Attribute access --------------------------------------------------------

#' Access the `bases` attribute of a `deb_lsd` object.
#'
#' @keywords internal

deb_bases <- function(x) attr(x, "bases")


# Class check -------------------------------------------------------------

#' Test if an object is of class `deb_lsd`
#'
#' Test if an object is of class `deb_lsd`.
#'
#' @param x An object.
#'
#' @return `TRUE` if object is of class `deb_lsd` and `FALSE` if it is not.
#' @export
#' @examples
#' x <- deb_lsd(5, 3, 8)
#' y <- c(5, 3, 8)
#'
#' deb_is_lsd(x)
#' deb_is_lsd(y)

deb_is_lsd <- function(x) inherits(x, "deb_lsd")


# Format method -----------------------------------------------------------

#' @export
format.deb_lsd <- function(x, ...) {
  l <- round(vctrs::field(x, "l"), 3) # only print 3 decimals
  s <- round(vctrs::field(x, "s"), 3)
  d <- round(vctrs::field(x, "d"), 3)

  out <- paste0(l, ":", s, "s:", d, "d")
  out[is.na(l) | is.na(s) | is.na(d)] <- NA
  out
}

#' @export
obj_print_footer.deb_lsd <- function(x, ...) {
  s <- format(attr(x, "bases")[[1]])
  d <- format(attr(x, "bases")[[2]])
  cat("# Bases: ", s, "s ", d, "d", "\n", sep = "")
}


# Abbreviated name type ---------------------------------------------------

#' @export
vec_ptype_abbr.deb_lsd <- function(x) {
  paste0("lsd[", attr(x, "bases")[[1]], "s:", attr(x, "bases")[[2]], "d]")
}
