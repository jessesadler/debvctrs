## Coercion for deb_lsd and deb_decimal ##

# deb_lsd -----------------------------------------------------------------

# Boilerplate

#' @rdname vctrs-compat
#' @method vec_type2 deb_lsd
#' @export
#' @export vec_type2.deb_lsd
vec_type2.deb_lsd <- function(x, y) UseMethod("vec_type2.deb_lsd", y)

#' @method vec_type2.deb_lsd default
#' @export
vec_type2.deb_lsd.default <- function(x, y) vctrs::stop_incompatible_type(x, y)

#' @method vec_type2.deb_lsd vctrs_unspecified
#' @export
vec_type2.deb_lsd.vctrs_unspecified <- function(x, y) x

# deb_lsd and deb_lsd

#' @method vec_type2.deb_lsd deb_lsd
#' @export
vec_type2.deb_lsd.deb_lsd <- function(x, y) {
  bases_equal(x, y)
  new_lsd(bases = deb_bases(x))
}

# deb_lsd and double

#' @method vec_type2.deb_lsd double
#' @export
vec_type2.deb_lsd.double <- function(x, y) x

#' @method vec_type2.double deb_lsd
#' @export
vec_type2.double.deb_lsd <- function(x, y) y

# deb_lsd and integer

#' @method vec_type2.deb_lsd integer
#' @export
vec_type2.deb_lsd.integer <- function(x, y) x

#' @method vec_type2.integer deb_lsd
#' @export
vec_type2.integer.deb_lsd <- function(x, y) y

# deb_decimal -------------------------------------------------------------

# Boilerplate
#' @rdname vctrs-compat
#' @method vec_type2 deb_decimal
#' @export
#' @export vec_type2.deb_decimal
vec_type2.deb_decimal <- function(x, y) UseMethod("vec_type2.deb_decimal", y)

#' @method vec_type2.deb_decimal default
#' @export
vec_type2.deb_decimal.default <- function(x, y) vctrs::stop_incompatible_type(x, y)

#' @method vec_type2.deb_decimal vctrs_unspecified
#' @export
vec_type2.deb_decimal.vctrs_unspecified <- function(x, y) x

# deb_decimal and deb_decimal

#' @method vec_type2.deb_decimal deb_decimal
#' @export
vec_type2.deb_decimal.deb_decimal <- function(x, y) {
  bases_equal(x, y)
  unit_equal(x, y)
  new_decimal(bases = deb_bases(x), unit = deb_unit(x))
}

# deb_decimal and double

#' @method vec_type2.deb_decimal double
#' @export
vec_type2.deb_decimal.double <- function(x, y) x

#' @method vec_type2.double deb_decimal
#' @export
vec_type2.double.deb_decimal <- function(x, y) y

# deb_decimal and integer

#' @method vec_type2.deb_decimal integer
#' @export
vec_type2.deb_decimal.integer <- function(x, y) x

#' @method vec_type2.integer deb_decimal
#' @export
vec_type2.integer.deb_decimal <- function(x, y) y

# deb_lsd and deb_decimal -------------------------------------------------

# deb_lsd and double

#' @method vec_type2.deb_lsd deb_decimal
#' @export
vec_type2.deb_lsd.deb_decimal <- function(x, y) x

#' @method vec_type2.deb_decimal deb_lsd
#' @export
vec_type2.deb_decimal.deb_lsd <- function(x, y) y
