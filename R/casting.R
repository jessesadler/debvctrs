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

# double to deb_lsd
vec_cast.deb_lsd.double <- function(x, to) {
  lsd <- deb_lsd(x, 0, 0, bases = deb_bases(to))
  deb_normalize(lsd)
}

# integer to deb_lsd
vec_cast.deb_lsd.integer <- function(x, to) {
  deb_lsd(x, 0, 0, bases = deb_bases(to))
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
  deb_decimal(x, unit = deb_unit(to), bases = deb_bases(to))
}
vec_cast.double.deb_decimal  <- function(x, to) vctrs::vec_data(x)

# integer to deb_decimal
vec_cast.deb_decimal.integer  <- function(x, to) {
  deb_decimal(x, unit = deb_unit(to), bases = deb_bases(to))
}

# deb_decimal to deb_lsd --------------------------------------------------

decimal_to_lsd <- function(x) {
  bases <- deb_bases(x)
  unit <- deb_unit(x)

  if (unit == "l") {
    lsd <- deb_lsd(x, 0, 0, bases = bases)
  } else if (unit == "s") {
    lsd <- deb_lsd(0, x, 0, bases = bases)
  } else if (unit == "d") {
    lsd <- deb_lsd(0, 0, x, bases = bases)
  }
  deb_normalize(lsd)
}

vec_cast.deb_lsd.deb_decimal <- function(x, to) {
  bases_equal(x, to)

  decimal_to_lsd(x)
}

# deb_lsd to deb_decimal --------------------------------------------------

lsd_to_decimal <- function(x, to) {
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

vec_cast.deb_decimal.deb_lsd <- function(x, to) {
  bases_equal(x, to)

  lsd_to_decimal(x, to)
}


# deb_lsd casting methods -------------------------------------------------

deb_as_lsd  <- function(x, ...) {
  UseMethod("deb_as_lsd")
}

deb_as_lsd.deb_lsd <- function(x) x

deb_as_lsd.deb_decimal <- function(x) {
  decimal_to_lsd(x)
}

deb_as_lsd.numeric <- function(x, bases = c(20, 12)) {
  vctrs::vec_cast(x, to = deb_lsd(bases = bases))
}

deb_as_lsd.logical <- function(x, bases = c(20, 12)) {
  vctrs::vec_cast(x, to = deb_lsd(bases = bases))
}

# deb_decimal casting methods ---------------------------------------------

deb_as_decimal <- function(x, ...) {
  UseMethod("deb_as_decimal")
}

deb_as_decimal.deb_decimal <- function(x) x

deb_as_decimal.deb_lsd <- function(x, unit = c("l", "s", "d")) {
  unit <- rlang::arg_match(unit)
  lsd_to_decimal(x, to = deb_decimal(unit = unit))
}

deb_as_decimal.numeric <- function(x,
                                   unit = c("l", "s", "d"),
                                   bases = c(20, 12)) {
  vctrs::vec_cast(x, to = deb_decimal(unit = unit, bases = bases))
}

deb_as_decimal.logical <- function(x,
                                   unit = c("l", "s", "d"),
                                   bases = c(20, 12)) {
  vctrs::vec_cast(x, to = deb_decimal(unit = unit, bases = bases))
}
