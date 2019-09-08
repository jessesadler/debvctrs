## Define deb_lsd class ##

# record-style vector: List of three equal-length double vectors
# bases attribute for bases of shillings and pence units.

# 1. Constructor -------------------------------------------------------------

#' Internal constructor to create deb_lsd type
#'
#' Asserts that `l`, `s` and `d` are of type `double()` and that `bases` is an
#' `integer()` of length 2. Creates the object through `vctrs::new_rcrd()`.
#'
#' @keywords internal

new_lsd <- function(l = double(),
                    s = double(),
                    d = double(),
                    bases = c(20L, 12L)) {

  # Assert l, s, and d are double vectors
  vctrs::vec_assert(l, ptype = double())
  vctrs::vec_assert(s, ptype = double())
  vctrs::vec_assert(d, ptype = double())

  # Assert bases is a vector of length 2
  vctrs::vec_assert(bases, ptype = integer(), size = 2)

  # Create deb_lsd class
  vctrs::new_rcrd(list(l = l, s = s, d = d),
                  bases = bases,
                  class = "deb_lsd")
}


# 2. Helper ------------------------------------------------------------------

#' A class for pounds, shillings and pence values
#'
#' User-facing function to create deb_lsd vector.
#' @param l Numeric vector representing the pounds unit.
#' @param s Numeric vector representing the shillings unit.
#' @param d Numeric vector representing the pence unit.
#' @param bases Numeric vector of length 2 used to specify the bases for the
#'   solidus or s and denarius or d units. Default is `c(20, 12)`, which
#'   conforms to the most widely used system of 1 pound = 20 shillings and
#'   1 shilling = 12 pence.
#' @export

deb_lsd <- function(l = double(),
                    s = double(),
                    d = double(),
                    bases = c(20, 12)) {
  # checks
  lsd_check(l, s, d)
  bases_check(bases)

  # Cast l, s, and d to double() using unpacking assignment from zeallot
  c(l, s, d) %<-% vctrs::vec_cast_common(l, s, d, .to = double())

  # Enforce recycling rules for l, s, and d using unpacking assignment
  # For discussion of recycling rules, see
  # https://vctrs.r-lib.org/articles/type-size.html#size
  c(l, s, d) %<-% vctrs::vec_recycle_common(l, s, d)

  bases <- vctrs::vec_cast(bases, to = integer())

  new_lsd(l = l, s = s, d = d, bases = bases)
}


# 3. Attribute access --------------------------------------------------------

#' Access the `bases` attribute of a `deb_lsd` object.
#'
#' @keywords internal

deb_bases <- function(x) attr(x, "bases")


# 4. Class check -------------------------------------------------------------

#' Test if an object is of class `deb_lsd`
#' @param x An object.
#'
#' @return `TRUE` if object is of class `deb_lsd` and `FALSE` if it is not.
#' @export

deb_is_lsd <- function(x) inherits(x, "deb_lsd")


# 5. Format method -----------------------------------------------------------

#' @export
format.deb_lsd <- function(x, ...) {
  l <- round(vctrs::field(x, "l"), 3) # only print 3 decimals
  s <- round(vctrs::field(x, "s"), 3)
  d <- round(vctrs::field(x, "d"), 3)

  out <- paste0(l, ":", s, "s:", d, "d")
  out[is.na(l) | is.na(s) | is.na(d)] <- NA
  out
}

# Add footer with attribute data
#' @export
obj_print_footer.deb_lsd <- function(x, ...) {
  s <- format(attr(x, "bases")[[1]])
  d <- format(attr(x, "bases")[[2]])
  cat("# Bases: ", s, "s ", d, "d", "\n", sep = "")
}


# 6. Abbreviated name type ---------------------------------------------------

#' @export
vec_ptype_abbr.deb_lsd <- function(x) {
  paste0("lsd[", attr(x, "bases")[[1]], "s:", attr(x, "bases")[[2]], "d]")
}
