## Define deb_decimal class ##

# Based on a double vector
# bases attribute for bases of shillings and pence units.
# unit attribute whether value is pounds (libra), shillings (solidus),
# or pence (denarius).

# 1. Constructor ----------------------------------------------------------

#' Internal constructor to create `deb_decimal` type
#'
#' Asserts that `x` is a `double()`, that `unit` is "l", "s", or "d", and
#' that `bases` is an `integer()` of length 2. Creates the object through
#' `vctrs::new_vctr()`.
#'
#' @keywords internal

# 1. Define arguments
new_decimal <- function(x = double(),
                        unit = c("l", "s", "d"),
                        bases = c(20L, 12L)) {

# 2. Ensure proper types and sizes for arguments
  # Assert x is double()
  vctrs::vec_assert(x, ptype = double())

  # Match unit: also checks that unit is character()
  unit <- rlang::arg_match(unit)

  # Assert bases is a vector of length 2
  vctrs::vec_assert(bases, ptype = integer(), size = 2)

# 3. Create deb_decimal class
  vctrs::new_vctr(x,
                  unit = unit,
                  bases = bases,
                  class = "deb_decimal")
}


# 2. Helper ---------------------------------------------------------------

#' A decimalized class for pounds, shillings, and pence values
#'
#' User-facing function to create deb_decimal vector.
#' @param x A numeric vector representing the decimalized values of a
#'   non-decimal currency.
#' @param unit A character vector of length one indicating the unit for the
#'   decimalized values, either `"l"` (libra, the default), `"s"` (solidus),
#'   or `"d"` (denarius).
#' @inheritParams deb_lsd
#' @export

#1. Define function
deb_decimal <- function(x = double(),
                        unit = c("l", "s", "d"),
                        bases = c(20, 12)) {

# 2. Checks: see 01.3-check.R
  unit <- rlang::arg_match(unit)
  bases_check(bases)

# 3. Cast to allow compatible types for each argument
  x <- vctrs::vec_cast(x, to = double())
  bases <- vctrs::vec_cast(bases, to = integer())

# 4. Use new_decimal() to do actual creation of the vector
  new_decimal(x = x, unit = unit, bases = bases)
}


# 3. Formally declare S3 class --------------------------------------------

# Must add methods to Imports in DESCRIPTION

#' @importFrom methods setOldClass
setOldClass(c("deb_decimal", "vctrs_vctr"))


# 4. Attribute access -----------------------------------------------------

#' Access the unit attribute of a `deb_decimal` object.
#'
#' @keywords internal

deb_unit <- function(x) attr(x, "unit")


# 5. Class check ----------------------------------------------------------

#' Test if an object is of class `deb_decimal`
#' @param x An object.
#'
#' @return `TRUE` if object is of class `deb_decimal` and `FALSE` if it is not.
#' @export

deb_is_decimal <- function(x) inherits(x, "deb_decimal")


# 6. Format method --------------------------------------------------------
# No format.deb_decimal to keep default vector printing

# Add footer with attribute data

# To print full name of unit in footer
unit_word <- function(x) {
  if (attr(x, "unit") == "l") {
    unit <- "pounds"
  } else if (attr(x, "unit") == "s") {
    unit <- "shillings"
  } else {
    unit <- "pence"
  }
  unit
}

#' @export
obj_print_footer.deb_decimal <- function(x, ...) {
  # Use full name of unit
  unit <- unit_word(x)

  s <- format(attr(x, "bases")[[1]])
  d <- format(attr(x, "bases")[[2]])
  cat("# Unit: ", unit, "\n",
      "# Bases: ", s, "s ", d, "d", "\n", sep = "")
}

# 7. Abbreviated name type ------------------------------------------------
# Used in column labels in tibble and str()

#' @export
vec_ptype_abbr.deb_decimal <- function(x) {
  paste0(attr(x, "unit"), "[",
         attr(x, "bases")[[1]], "s:",
         attr(x, "bases")[[2]], "d]")
}
