## Arithmetic for deb_lsd and deb_decimal ##

# deb_lsd mathematic functions --------------------------------------------

# sum
lsd_sum <- function(x, ...) {
  ret <- new_lsd(sum(vctrs::field(x, "l")),
                 sum(vctrs::field(x, "s")),
                 sum(vctrs::field(x, "d")),
                 bases = deb_bases(x))

  deb_normalize(ret)
}


# Rounding ----------------------------------------------------------------
round.deb_lsd <- function(x, digits = 0) {
  vctrs::field(x, "d") <- round(vctrs::field(x, "d"), digits = digits)
  deb_normalize(x)
}

signif.deb_lsd <- function(x, digits = 6) {
  vctrs::field(x, "d") <- signif(vctrs::field(x, "d"), digits = digits)
  deb_normalize(x)
}

lsd_ceiling <- function(x) {
  vctrs::field(x, "d") <- ceiling(vctrs::field(x, "d"))
  deb_normalize(x)
}

lsd_floor <- function(x) {
  vctrs::field(x, "d") <- floor(vctrs::field(x, "d"))
  deb_normalize(x)
}

lsd_trunc <- function(x, ...) {
  vctrs::field(x, "d") <- trunc(vctrs::field(x, "d"))
  deb_normalize(x)
}

# Methods
vec_math.deb_lsd <- function(fun, x, ...) {
  switch(
    fun,
    sum = lsd_sum(x, ...),
    mean = lsd_sum(x) / vctrs::vec_size(x),
    ceiling = lsd_ceiling(x),
    floor = lsd_floor(x),
    trunc = lsd_trunc(x, ...),
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

# Operators with lsd and lsd
lsd_plus <- function(x, y) {
  c(x, y) %<-% vctrs::vec_recycle_common(x, y)

  ret <- new_lsd(vctrs::field(x, "l") + vctrs::field(y, "l"),
                 vctrs::field(x, "s") + vctrs::field(y, "s"),
                 vctrs::field(x, "d") + vctrs::field(y, "d"),
                 bases = deb_bases(x))

  deb_normalize(ret)
}

lsd_minus <- function(x, y) {
  c(x, y) %<-% vctrs::vec_recycle_common(x, y)

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
    "/" = as.double(x) / as.double(y),
    vctrs::stop_incompatible_op(op, x, y)
  )
}

# Operators with numeric
lsd_multiply <- function(x, multiplier) {
  c(x, multiplier) %<-% vctrs::vec_recycle_common(x, multiplier)

  ret <- new_lsd(vctrs::field(x, "l") * multiplier,
                 vctrs::field(x, "s") * multiplier,
                 vctrs::field(x, "d") * multiplier,
                 bases = deb_bases(x))

  deb_normalize(ret)
}

lsd_divide <- function(x, divisor) {
  c(x, divisor) %<-% vctrs::vec_recycle_common(x, divisor)

  ret <- new_lsd(vctrs::field(x, "l") / divisor,
                 vctrs::field(x, "s") / divisor,
                 vctrs::field(x, "d") / divisor,
                 bases = deb_bases(x))

  deb_normalize(ret)
}


# deb_lsd and numeric
vec_arith.deb_lsd.numeric <- function(op, x, y) {
  switch(
    op,
    "*" = lsd_multiply(x, multiplier = y),
    "/" = lsd_divide(x, divisor = y),
    vctrs::stop_incompatible_op(op, x, y)
  )
}

# numeric and deb_lsd
vec_arith.numeric.deb_lsd <- function(op, x, y) {
  switch(
    op,
    "*" = lsd_multiply(y, multiplier = x),
    vctrs::stop_incompatible_op(op, x, y)
  )
}

# Unary operators
lsd_negate <- function(x) {
  vctrs::field(x, "l") <- vctrs::field(x, "l") * -1
  vctrs::field(x, "s") <- vctrs::field(x, "s") * -1
  vctrs::field(x, "d") <- vctrs::field(x, "d") * -1

  x
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
    "/" = vctrs::vec_arith_base(op, x, y),
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


# deb_lsd and deb_decimal arithmetic --------------------------------------

# deb_lsd and deb_decimal
vec_arith.deb_lsd.deb_decimal <- function(op, x, y) {
  switch(
    op,
    "+" = lsd_plus(x, deb_as_lsd(y)),
    "-" = lsd_minus(x, deb_as_lsd(y)),
    "/" = as.double(x) / vctrs::vec_data(y),
    vctrs::stop_incompatible_op(op, x, y)
  )
}

# deb_decimal and deb_lsd
vec_arith.deb_decimal.deb_lsd <- function(op, x, y) {
  switch(
    op,
    "+" = lsd_plus(deb_as_lsd(x), y),
    "-" = lsd_minus(deb_as_lsd(x), y),
    "/" = vctrs::vec_data(x) / as.double(y),
    vctrs::stop_incompatible_op(op, x, y)
  )
}
