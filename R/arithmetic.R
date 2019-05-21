## Arithmetic for deb_lsd and deb_decimal ##

# deb_lsd mathematic functions --------------------------------------------

# sum #
lsd_sum <- function(x, ...) {
  ret <- new_lsd(sum(vctrs::field(x, "l")),
                 sum(vctrs::field(x, "s")),
                 sum(vctrs::field(x, "d")),
                 bases = deb_bases(x))

  deb_normalize(ret)
}

# deb_lsd and deb_lsd
vec_math.deb_lsd <- function(fun, x, ...) {
  switch(
    fun,
    sum = lsd_sum(x, ...),
    vctrs::vec_math_base(fun, x, ...)
  )
}

# deb_lsd arithmetic operators --------------------------------------------

## Arithmetic boilerplate ##
vec_arith.deb_lsd <- function(op, x, y) {
  UseMethod("vec_arith.deb_lsd", y)
}
vec_arith.deb_lsd.default <- function(op, x, y) {
  vctrs::stop_incompatible_op(op, x, y)
}

lsd_plus <- function(x, y) {
  ret <- new_lsd(vctrs::field(x, "l") + vctrs::field(y, "l"),
                 vctrs::field(x, "s") + vctrs::field(y, "s"),
                 vctrs::field(x, "d") + vctrs::field(y, "d"),
                 bases = deb_bases(x))

  deb_normalize(ret)
}

lsd_minus <- function(x, y) {
  ret <- new_lsd(vctrs::field(x, "l") - vctrs::field(y, "l"),
                 vctrs::field(x, "s") - vctrs::field(y, "s"),
                 vctrs::field(x, "d") - vctrs::field(y, "d"),
                 bases = deb_bases(x))

  deb_normalize(ret)
}

# deb_lsd and deb_lsd
vec_arith.deb_lsd.deb_lsd <- function(op, x, y) {
  bases_equal(x, y)

  switch(
    op,
    "+" = lsd_plus(x, y),
    "-" = lsd_minus(x, y),
    vctrs::stop_incompatible_op(op, x, y)
  )
}

lsd_multiply <- function(lsd, multiplier) {
  vctrs::field(lsd, "l") <- vctrs::field(lsd, "l") * multiplier
  vctrs::field(lsd, "s") <- vctrs::field(lsd, "s") * multiplier
  vctrs::field(lsd, "d") <- vctrs::field(lsd, "d") * multiplier

  deb_normalize(lsd)
}

lsd_divide <- function(lsd, divisor) {
  vctrs::field(lsd, "l") <- vctrs::field(lsd, "l") * divisor
  vctrs::field(lsd, "s") <- vctrs::field(lsd, "s") * divisor
  vctrs::field(lsd, "d") <- vctrs::field(lsd, "d") * divisor

  deb_normalize(lsd)
}


# deb_lsd and numeric
vec_arith.deb_lsd.numeric <- function(op, x, y) {
  switch(
    op,
    "*" = lsd_multiply(lsd = x, multiplier = y),
    "/" = lsd_divide(lsd = x, divisor = y),
    vctrs::stop_incompatible_op(op, x, y)
  )
}

# numeric and deb_lsd
vec_arith.numeric.deb_lsd <- function(op, x, y) {
  switch(
    op,
    "*" = lsd_multiply(lsd = y, multiplier = x),
    vctrs::stop_incompatible_op(op, x, y)
  )
}

# Unary operators
lsd_negate <- function(lsd) {
  vctrs::field(lsd, "l") <- vctrs::field(lsd, "l") * -1
  vctrs::field(lsd, "s") <- vctrs::field(lsd, "s") * -1
  vctrs::field(lsd, "d") <- vctrs::field(lsd, "d") * -1

  lsd
}

vec_arith.deb_lsd.MISSING <- function(op, x, y) {
  switch(
    op,
    `-` = lsd_negate(x),
    `+` = x,
    vctrs::stop_incompatible_op(op, x, y)
  )
}

# deb_decimal arithmetic operators ----------------------------------------

## Arithmetic boilerplate ##
vec_arith.deb_decimal <- function(op, x, y) {
  UseMethod("vec_arith.deb_decimal", y)
}
vec_arith.deb_decimal.default <- function(op, x, y) {
  vctrs::stop_incompatible_op(op, x, y)
}

# deb_decimal and deb_decimal
vec_arith.deb_decimal.deb_decimal <- function(op, x, y) {
  # Ensure bases and units are equal
  bases_equal(x, y)
  unit_equal(x, y)

  switch(
    op,
    "+" = ,
    "-" = new_decimal(vctrs::vec_arith_base(op, x, y),
                      unit = deb_unit(x),
                      bases = deb_bases(x)),
    vctrs::stop_incompatible_op(op, x, y)
  )
}


# deb_decimal and numeric
vec_arith.deb_decimal.numeric <- function(op, x, y) {
  switch(
    op,
    "+" = ,
    "-" = ,
    "/" = ,
    "*" = ,
    "^" = ,
    "%%" = ,
    "%/%" = new_decimal(vctrs::vec_arith_base(op, x, y),
                        unit = deb_unit(x),
                        bases = deb_bases(x)),
    vctrs::stop_incompatible_op(op, x, y)
  )
}

# numeric and deb_decimal
vec_arith.numeric.deb_decimal <- function(op, x, y) {
  switch(
    op,
    "+" = ,
    "-" = ,
    "*" = new_decimal(vctrs::vec_arith_base(op, x, y),
                        unit = deb_unit(y),
                        bases = deb_bases(y)),
    vctrs::stop_incompatible_op(op, x, y)
  )
}

# Unary operators
vec_arith.deb_decimal.MISSING <- function(op, x, y) {
  switch(
    op,
    `-` = x * -1,
    `+` = x,
    vctrs::stop_incompatible_op(op, x, y)
  )
}
