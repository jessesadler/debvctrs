## Casting for deb_lsd and deb_decimal ##


# deb_lsd -----------------------------------------------------------------

# Boilerplate
vec_cast.deb_lsd <- function(x, to) UseMethod("vec_cast.deb_lsd")
vec_cast.deb_lsd.default <- function(x, to) vctrs::stop_incompatible_cast(x, to)
vec_cast.deb_lsd.logical <- function(x, to) vctrs::vec_unspecified_cast(x, to)

# deb_lsd to deb_lsd
vec_cast.deb_lsd.deb_lsd <- function(x, to) {
  bases_equal(x, to)
  x
}

# deb_lsd to double
vec_cast.double.deb_lsd <- function(x, to) {
  l <- vctrs::field(x, "l")
  s <- vctrs::field(x, "s")
  d <- vctrs::field(x, "d")
  bases <- deb_bases(x)

  l + s / bases[[1]] + d / prod(bases)
}

# deb_decimal -------------------------------------------------------------

# Boilerplate
vec_cast.deb_decimal <- function(x, to) UseMethod("vec_cast.deb_decimal")
vec_cast.deb_decimal.default <- function(x, to) vctrs::stop_incompatible_cast(x, to)
vec_cast.deb_decimal.logical <- function(x, to) vctrs::vec_unspecified_cast(x, to)

# deb_decimal to deb_decimal
vec_cast.deb_decimal.deb_decimal <- function(x, to) {
  bases_equal(x, to)
  unit_equal(x, to)
  x
}

# double to deb_decimal and back
vec_cast.deb_decimal.double  <- function(x, to) {
  new_decimal(x,
              unit = deb_unit(to),
              bases = deb_bases(to))
}
vec_cast.double.deb_decimal  <- function(x, to) vctrs::vec_data(x)

# Cast to deb_decimal method
deb_as_decimal <- function(x,
                           unit = c("l", "s", "d"),
                           bases = c(20, 12)) {
  vctrs::vec_cast(x,
                  to = deb_decimal(x = x,
                                   unit = unit,
                                   bases = bases))
}


# deb_lsd to deb_decimal --------------------------------------------------

vec_cast.deb_decimal.deb_lsd <- function(x, to) {
  l <- vctrs::field(x, "l")
  s <- vctrs::field(x, "s")
  d <- vctrs::field(x, "d")
  bases <- deb_bases(x)
  unit <- deb_unit(to)

  if (unit == "l") {
    decimalized <- l + s / bases[[1]] + d / prod(bases)
  } else if (unit == "s") {
    decimalized <- l * bases[[1]] + s + d / bases[[2]]
  } else if (unit == "d") {
    decimalized <- l * prod(bases) + s * bases[[2]] + d
  }
  new_decimal(x = decimalized,
              unit = unit,
              bases = bases)
}
