## Coercion for deb_lsd and deb_decimal ##

# deb_lsd -----------------------------------------------------------------

# Boilerplate
vec_type2.deb_lsd <- function(x, y) UseMethod("vec_type2.deb_lsd", y)
vec_type2.deb_lsd.default <- function(x, y) vctrs::stop_incompatible_type(x, y)
vec_type2.deb_lsd.vctrs_unspecified <- function(x, y) x

# deb_lsd and deb_lsd
vec_type2.deb_lsd.deb_lsd <- function(x, y) {
  bases_equal(x, y)
  new_lsd(bases = deb_bases(x))
}

# deb_lsd and double
vec_type2.deb_lsd.double <- function(x, y) x
vec_type2.double.deb_lsd <- function(x, y) y

# deb_decimal -------------------------------------------------------------

# Boilerplate
vec_type2.deb_decimal <- function(x, y) UseMethod("vec_type2.deb_decimal", y)
vec_type2.deb_decimal.default <- function(x, y) vctrs::stop_incompatible_type(x, y)
vec_type2.deb_decimal.vctrs_unspecified <- function(x, y) x

# deb_decimal and deb_decimal
vec_type2.deb_decimal.deb_decimal <- function(x, y) {
  bases_equal(x, y)
  unit_equal(x, y)
  new_decimal(bases = deb_bases(x), unit = deb_unit(x))
}

# deb_decimal and double
vec_type2.deb_decimal.double <- function(x, y) x
vec_type2.double.deb_decimal <- function(x, y) y


# deb_lsd and deb_decimal -------------------------------------------------

# deb_lsd and double
vec_type2.deb_lsd.deb_decimal <- function(x, y) x
vec_type2.deb_decimal.deb_lsd <- function(x, y) y
