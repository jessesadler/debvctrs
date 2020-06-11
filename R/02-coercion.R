## Coercion for deb_decimal and deb_lsd prototypes ##

# Create coercion hierarchy: numeric() -> deb_decimal() -> deb_lsd()

# A) deb_decimal ----------------------------------------------------------

# 1. deb_decimal and deb_decimal ------------------------------------------

# Hierarchy: d -> s -> l
unit_hierarchy <- function(x, y) {
  if (identical(deb_unit(x), deb_unit(y))) {
    deb_unit(x)
  } else if (any(c(deb_unit(x), deb_unit(y)) == "l")) {
    "l"
  } else {
    "s"
  }
}

#' @export
vec_ptype2.deb_decimal.deb_decimal <- function(x, y, ...) {
  bases_equal(x, y)
  unit <- unit_hierarchy(x, y)

  new_decimal(bases = deb_bases(x), unit = unit)
}


# 2. Coercion with compatible types ---------------------------------------

# a) double -> deb_decimal

#' @export
vec_ptype2.deb_decimal.double <- function(x, y, ...) x

#' @export
vec_ptype2.double.deb_decimal <- function(x, y, ...) y

# b) integer -> deb_decimal

#' @export
vec_ptype2.deb_decimal.integer <- function(x, y, ...) x

#' @export
vec_ptype2.integer.deb_decimal <- function(x, y, ...) y


# B) deb_lsd --------------------------------------------------------------

# 1. deb_lsd and deb_lsd --------------------------------------------------

#' @export
vec_ptype2.deb_lsd.deb_lsd <- function(x, y, ...) {
  # Ensure that the bases are equal
  bases_equal(x, y)
  new_lsd(bases = deb_bases(x))
}


# 2. Coercion with compatible types ---------------------------------------

# a) double -> deb_lsd

#' @export
vec_ptype2.deb_lsd.double <- function(x, y, ...) x

#' @export
vec_ptype2.double.deb_lsd <- function(x, y, ...) y

# b) integer -> deb_lsd

#' @export
vec_ptype2.deb_lsd.integer <- function(x, y, ...) x

#' @export
vec_ptype2.integer.deb_lsd <- function(x, y, ...) y


# C) Coercion with deb_lsd and deb_decimal --------------------------------

# deb_decimal -> deb_lsd

#' @export
vec_ptype2.deb_lsd.deb_decimal <- function(x, y, ...) x

#' @export
vec_ptype2.deb_decimal.deb_lsd <- function(x, y, ...) y
