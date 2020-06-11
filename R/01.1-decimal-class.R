## Define deb_decimal class ##

# The deb_decimal class is based on a double vector.
# It has two attributes:
# 1. bases attribute determines the bases of shillings and pence units.
# 2. unit attribute determines whether the unit of the value is
#    pounds (libra), shillings (solidus), or pence (denarius).

# 1. Constructor ----------------------------------------------------------

#' Internal constructor to create `deb_decimal` type
#'
#' Asserts that `x` is a `double()`, that `unit` is "l", "s", or "d", and
#' that `bases` is an `integer()` of length 2. Creates the object through
#' `new_vctr()`.
#'
#' @keywords internal

# Constructor steps overview
# 1. Define arguments
# 2. Checks: Ensure proper types and sizes for arguments
#    a) Assert x is double()
#    b) Match unit: also checks that unit is character()
#    c) Assert bases is a vector of length 2
# 3. Create deb_decimal class

new_decimal <- function(x = double(),
                        unit = c("l", "s", "d"),
                        bases = c(20L, 12L)) {
  # 2. Checks
  vec_assert(x, ptype = double())
  unit <- rlang::arg_match(unit)
  vec_assert(bases, ptype = integer(), size = 2)

  # 3. Create deb_decimal class
  #    inherit_base_type = TRUE adds double to vector of classes
  new_vctr(.data = x,
           unit = unit,
           bases = bases,
           class = "deb_decimal",
           inherit_base_type = TRUE)
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

# Helper steps
# 1. Define function
# 2. Checks: see 01.3-check.R
# 3. Cast to allow compatible types for each argument
# 4. Use new_decimal() to do actual creation of the vector
deb_decimal <- function(x = double(),
                        unit = c("l", "s", "d"),
                        bases = c(20, 12)) {

  # 2. Checks
  unit <- rlang::arg_match(unit)
  bases_check(bases)

  # 3. Casts for compatible types
  x <- vec_cast(x, to = double())
  bases <- vec_cast(bases, to = integer())

  # 4. Create deb_decimal vector
  new_decimal(x = x, unit = unit, bases = bases)
}


# 3. Formally declare S3 class --------------------------------------------

# Must add methods to Imports in DESCRIPTION
# usethis::use_package("methods")

methods::setOldClass(c("deb_decimal", "vctrs_vctr"))


# 4. Attribute access -----------------------------------------------------

# Note: Access to bases attribute is in 01.2-lsd-class.R

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
# However, we can add a footer with the attribute data

#' Print full name of unit in footer
#'
#' @param x A deb_decimal object.
#' @keywords internal

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
