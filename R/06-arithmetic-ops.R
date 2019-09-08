## Arithmetic operations for deb_lsd and deb_decimal ##

# A) deb_decimal arithmetic operators -------------------------------------

# 1. Arithmetic boilerplate -----------------------------------------------

#' @rdname vctrs-compat
#' @method vec_arith deb_decimal
#' @export
#' @export vec_arith.deb_decimal
vec_arith.deb_decimal <- function(op, x, y) {
  UseMethod("vec_arith.deb_decimal", y)
}

#' @rdname vctrs-compat
#' @method vec_arith.deb_decimal default
#' @export
vec_arith.deb_decimal.default <- function(op, x, y) {
  vctrs::stop_incompatible_op(op, x, y)
}


# 2. Operators with deb_decimal and deb_decimal ------------------------------

dec_arithmetic <- function(op, x, y) {
  xy <- vctrs::vec_cast_common(x, y)
  vctrs::vec_arith_base(op, xy[[1]], xy[[2]])
}

#' @rdname vctrs-compat
#' @method vec_arith.deb_decimal deb_decimal
#' @export
vec_arith.deb_decimal.deb_decimal <- function(op, x, y) {
  # Ensure bases are equal
  bases_equal(x, y)

  switch(
    op,
    "+" = ,
    "-" = new_decimal(dec_arithmetic(op, x, y),
                      unit = unit_hierarchy(x, y),
                      bases = deb_bases(x)),
    "/" = dec_arithmetic(op, x, y),
    vctrs::stop_incompatible_op(op, x, y)
  )
}


# 3. Operators with deb_decimal and numeric ----------------------------------

#' @rdname vctrs-compat
#' @method vec_arith.deb_decimal numeric
#' @export
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

#' @rdname vctrs-compat
#' @method vec_arith.numeric deb_decimal
#' @export
vec_arith.numeric.deb_decimal <- function(op, x, y) {
  switch(
    op,
    "+" = ,
    "-" = ,
    "*" = ,
    "/" = new_decimal(vctrs::vec_arith_base(op, x, y),
                      unit = deb_unit(y),
                      bases = deb_bases(y)),
    vctrs::stop_incompatible_op(op, x, y)
  )
}


# 4. Unary operators with deb_decimal ----------------------------------------

#' @rdname vctrs-compat
#' @method vec_arith.deb_decimal MISSING
#' @export
vec_arith.deb_decimal.MISSING <- function(op, x, y) {
  switch(
    op,
    `-` = x * -1,
    `+` = x,
    vctrs::stop_incompatible_op(op, x, y)
  )
}

# B) deb_lsd arithmetic operators -----------------------------------------

# 1. Arithmetic boilerplate -----------------------------------------------

#' @rdname vctrs-compat
#' @method vec_arith deb_lsd
#' @export
#' @export vec_arith.deb_lsd
vec_arith.deb_lsd <- function(op, x, y) {
  UseMethod("vec_arith.deb_lsd", y)
}

#' @rdname vctrs-compat
#' @method vec_arith.deb_lsd default
#' @export
vec_arith.deb_lsd.default <- function(op, x, y) {
  vctrs::stop_incompatible_op(op, x, y)
}


# 2. Operators with lsd and lsd ----------------------------------------------

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

#' @rdname vctrs-compat
#' @method vec_arith.deb_lsd deb_lsd
#' @export
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


# 3. Operators with deb_lsd and numeric --------------------------------------

lsd_multiply <- function(x, multiplier) {
  c(x, multiplier) %<-% vctrs::vec_recycle_common(x, multiplier)

  ret <- new_lsd(vctrs::field(x, "l") * multiplier,
                 vctrs::field(x, "s") * multiplier,
                 vctrs::field(x, "d") * multiplier,
                 bases = deb_bases(x))

  deb_normalize(ret)
}

# Divide lsd by numeric
lsd_dividend <- function(x, divisor) {
  c(x, divisor) %<-% vctrs::vec_recycle_common(x, divisor)

  ret <- new_lsd(vctrs::field(x, "l") / divisor,
                 vctrs::field(x, "s") / divisor,
                 vctrs::field(x, "d") / divisor,
                 bases = deb_bases(x))

  deb_normalize(ret)
}

# Divide numeric by lsd
lsd_divisor <- function(dividend, x) {
  c(dividend, x) %<-% vctrs::vec_recycle_common(dividend, x)

  ret <- dividend / deb_as_decimal(x)

  deb_as_lsd(ret)
}

# deb_lsd and numeric

#' @rdname vctrs-compat
#' @method vec_arith.deb_lsd numeric
#' @export
vec_arith.deb_lsd.numeric <- function(op, x, y) {
  switch(
    op,
    "*" = lsd_multiply(x, multiplier = y),
    "/" = lsd_dividend(x, divisor = y),
    vctrs::stop_incompatible_op(op, x, y)
  )
}

# numeric and deb_lsd

#' @rdname vctrs-compat
#' @method vec_arith.numeric deb_lsd
#' @export
vec_arith.numeric.deb_lsd <- function(op, x, y) {
  switch(
    op,
    "*" = lsd_multiply(y, multiplier = x),
    "/" = lsd_divisor(dividend = x, y),
    vctrs::stop_incompatible_op(op, x, y)
  )
}


# 4. Unary operators with deb_lsd --------------------------------------------

lsd_negate <- function(x) {
  vctrs::field(x, "l") <- vctrs::field(x, "l") * -1
  vctrs::field(x, "s") <- vctrs::field(x, "s") * -1
  vctrs::field(x, "d") <- vctrs::field(x, "d") * -1

  x
}

#' @rdname vctrs-compat
#' @method vec_arith.deb_lsd MISSING
#' @export
vec_arith.deb_lsd.MISSING <- function(op, x, y) {
  switch(
    op,
    `-` = lsd_negate(x),
    `+` = x,
    vctrs::stop_incompatible_op(op, x, y)
  )
}


# C) Operators with deb_lsd and deb_decimal -------------------------------

# deb_lsd and deb_decimal

#' @rdname vctrs-compat
#' @method vec_arith.deb_lsd deb_decimal
#' @export
vec_arith.deb_lsd.deb_decimal <- function(op, x, y) {
  bases_equal(x, y)

  switch(
    op,
    "+" = lsd_plus(x, deb_as_lsd(y)),
    "-" = lsd_minus(x, deb_as_lsd(y)),
    "/" = as.double(x) / vctrs::vec_data(y),
    vctrs::stop_incompatible_op(op, x, y)
  )
}

# deb_decimal and deb_lsd

#' @rdname vctrs-compat
#' @method vec_arith.deb_decimal deb_lsd
#' @export
vec_arith.deb_decimal.deb_lsd <- function(op, x, y) {
  bases_equal(x, y)

  switch(
    op,
    "+" = lsd_plus(deb_as_lsd(x), y),
    "-" = lsd_minus(deb_as_lsd(x), y),
    "/" = vctrs::vec_data(x) / as.double(y),
    vctrs::stop_incompatible_op(op, x, y)
  )
}
