## Define deb_lsd class ##

# The deb_lsd class is based on a record-style vector.
# Underneath it is a list of three equal-length double vectors.
# It has a bases attribute for bases of shillings and pence units.
# This enables the use of bases beyond the standard 20s. 12d.

# 1. Constructor ----------------------------------------------------------

#' Internal constructor to create deb_lsd type
#'
#' Asserts that `l`, `s` and `d` are of type `double()` and that `bases` is an
#' `integer()` of length 2. Creates the object through `vctrs::new_rcrd()`.
#'
#' @keywords internal

# Constructor steps overview
# 1. Define arguments
# 2. Checks: Ensure proper types and sizes for arguments
#    a) Assert l, s, and d are double vectors
#    b) Assert bases is a vector of length 2
# 3. Create deb_lsd class

new_lsd <- function(l = double(),
                    s = double(),
                    d = double(),
                    bases = c(20L, 12L)) {

  # 2. Checks
  vctrs::vec_assert(l, ptype = double())
  vctrs::vec_assert(s, ptype = double())
  vctrs::vec_assert(d, ptype = double())
  vctrs::vec_assert(bases, ptype = integer(), size = 2)

  # 3. Create deb_lsd class
  vctrs::new_rcrd(list(l = l, s = s, d = d),
                  bases = bases,
                  class = "deb_lsd")
}


# 2. Helper ---------------------------------------------------------------

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

# Helper steps
# 1. Define function
# 2. Checks: see 01.3-check.R
# 3. Cast to allow compatible types for each argument
#    Cast l, s, and d to double() using unpacking assignment from zeallot
# 4. Enforce recycling rules for l, s, and d using unpacking assignment
#    For discussion of recycling rules, see
#    https://vctrs.r-lib.org/articles/type-size.html#size
# 5. Use new_lsd() to do actual creation of the vector

deb_lsd <- function(l = double(),
                    s = double(),
                    d = double(),
                    bases = c(20, 12)) {

  # 2. Checks
  lsd_check(l, s, d)
  bases_check(bases)

  # 3. Casts for compatible types
  c(l, s, d) %<-% vctrs::vec_cast_common(l, s, d, .to = double())
  bases <- vctrs::vec_cast(bases, to = integer())

  # 4. Enforce recycling rules
  c(l, s, d) %<-% vctrs::vec_recycle_common(l, s, d)

  # 5. Create deb_lsd vector
  new_lsd(l = l, s = s, d = d, bases = bases)
}


# 3. Formally declare S3 class --------------------------------------------

# Must add methods to Imports in DESCRIPTION

#' @importFrom methods setOldClass
setOldClass(c("deb_lsd", "vctrs_rcrd", "vctrs_vctr"))


# 4. Attribute access -----------------------------------------------------

#' Access the `bases` attribute of a `deb_lsd` object.
#'
#' @keywords internal

deb_bases <- function(x) attr(x, "bases")


# 5. Class check ----------------------------------------------------------

#' Test if an object is of class `deb_lsd`
#' @param x An object.
#'
#' @return `TRUE` if object is of class `deb_lsd` and `FALSE` if it is not.
#' @export

deb_is_lsd <- function(x) inherits(x, "deb_lsd")


# 6. Format method --------------------------------------------------------

#' @export
format.deb_lsd <- function(x, ...) {
  l <- round(vctrs::field(x, "l"), 3) # only print 3 decimals
  s <- round(vctrs::field(x, "s"), 3)
  d <- round(vctrs::field(x, "d"), 3)

  out <- paste0(l, ":", s, "s:", d, "d")
  out[is.na(l) | is.na(s) | is.na(d)] <- NA # Format NAs
  out
}

# Add footer with bases attribute data
#' @export
obj_print_footer.deb_lsd <- function(x, ...) {
  s <- format(attr(x, "bases")[[1]])
  d <- format(attr(x, "bases")[[2]])
  cat("# Bases: ", s, "s ", d, "d", "\n", sep = "")
}


# 7. Abbreviated name type ------------------------------------------------
# Used in column labels in tibble and str()

#' @export
vec_ptype_abbr.deb_lsd <- function(x) {
  paste0("lsd[", attr(x, "bases")[[1]], "s:", attr(x, "bases")[[2]], "d]")
}
