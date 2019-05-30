## Arithmetic for deb_lsd and deb_decimal ##

#' Mathematical functions with deb_lsd objects
#'
#' Mathematical functions with pounds, shillings, and pence values as
#' represented by the deb_lsd class.
#'
#' `sum()` and `mean()` return a normalized `deb_lsd` value.
#'
#' Round family of functions only affect the denarius (`d`) unit of a
#' `deb_lsd` object. The value will be normalized if necessary.
#'
#' @param x An object of class `deb_lsd`.
#' @param ... Arguments passed on to further methods.
#' @param digits Integer indicating the number of decimal places (round)
#'   or significant digits (signif) to be used.
#' @param fun Used internally to enable debkeepr to work with vctrs.
#'
#' @return A `deb_lsd` object with normalized values.
#' @examples
#'
#' x <- deb_lsd(l = c(5, 8, 12),
#'              s = c(16, 6, 13),
#'              d = c(6, 11, 0))
#'
#' # Sum and mean with a deb_lsd object
#' # All values are normalized
#' sum(x)
#' mean(x)
#'
#' # Round family on deb_lsd affects the denarius unit
#' x2 <- deb_lsd(5, 12, 5.8365)
#' round(x2)
#' round(x2, digits = 2)
#' signif(x2, digits = 2)
#' ceiling(x2)
#' floor(x2)
#' trunc(x2)
#'
#' # The returned values are normalized whether
#' # they are positive or negative
#' x3 <- deb_lsd(9, 19, 11.825)
#' x4 <- deb_lsd(-9, -19, -11.825)
#' round(x3)
#' round(x3, digits = 1)
#'
#' ceiling(x3)
#' floor(x4)
#'
#' trunc(x3)
#' trunc(x4)
#'
#' @name mathematics
NULL

# deb_lsd mathematic functions --------------------------------------------

# sum
lsd_sum <- function(x, ...) {
  ret <- new_lsd(sum(vctrs::field(x, "l"), ...),
                 sum(vctrs::field(x, "s"), ...),
                 sum(vctrs::field(x, "d"), ...),
                 bases = deb_bases(x))

  deb_normalize(ret)
}

# Rounding ----------------------------------------------------------------

#' @rdname mathematics
#' @export
round.deb_lsd <- function(x, digits = 0) {
  vctrs::field(x, "d") <- round(vctrs::field(x, "d"), digits = digits)
  deb_normalize(x)
}

#' @rdname mathematics
#' @export
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

#' @rdname mathematics
#' @method vec_math deb_lsd
#' @export
#' @export vec_math.deb_lsd
vec_math.deb_lsd <- function(fun, x, ...) {
  switch(
    fun,
    sum = lsd_sum(x, ...),
    # Remove NA from divisor
    mean = lsd_sum(x, ...) / vctrs::vec_size(purrr::discard(x, .p = is.na)),
    ceiling = lsd_ceiling(x),
    floor = lsd_floor(x),
    trunc = lsd_trunc(x, ...)
  )
}


# Arithmetic operators documentation --------------------------------------

#' Arithmetic operations with deb_lsd and deb_decimal
#'
#' Implementation of arithmetic operations for pounds, shillings, and pence
#' values as `deb_lsd` and `deb_decimal` vectors. Available operations are:
#'
#' * `deb_lsd` and `deb_lsd`: `+`, `-`, and `/`. The first two return a
#'   `deb_lsd` vector; the last returns a numeric vector.
#'
#' * `deb_lsd` and `numeric`: `\*` and `/`. Both return a `deb_lsd` vector.
#'
#' * `numeric` and `deb_lsd`: `\*`, returning a `deb_lsd` vector.
#'
#' * `deb_decimal` and `deb_decimal`: `+`, `-`, and `/`. The first two
#'   return a `deb_lsd` vector; the last returns a numeric vector.
#'
#' * `deb_decimal` and `numeric`: `+`, `-`, `/`, `\*`, `^`, `%%`, `%/%`.
#'   All return a `deb_decimal` vector.
#'
#' * `numeric` and `deb_decimal`: `+`, `-`, `\*`. All return a
#'   `deb_decimal` vector.
#'
#' * `deb_lsd` and `deb_decimal`: `+`, `-`, `/`. The first two return a
#'   `deb_lsd` vector; the last returns a numeric vector.
#'
#' * `deb_decimal` and `deb_lsd`: `+`, `-`, `/`. The first two return a
#'   `deb_lsd` vector; the last returns a numeric vector.
#'
#' @param x,y A pair of vectors.
#' @param op Used internally to enable debkeepr to work with vctrs.
#'
#' @examples
#'
#' # Arithmetic with deb_lsd
#' lsd1 <- deb_lsd(5, 16, 6)
#' lsd2 <- deb_lsd(7, 6, 8)
#'
#' lsd1 + lsd2
#' lsd2 - lsd1
#' # Find the ration between two values
#' lsd2 / lsd1
#'
#' # Works with deb_lsd and numeric values
#' lsd2 / 2
#' 2 * lsd1
#'
#' # Arithmetic with deb_decimal
#' dec1 <- deb_decimal(8.45)
#' dec2 <- deb_decimal(4.625)
#'
#' dec1 + dec2
#' dec1 - dec2
#'
#' # Works with deb_decimal and numeric values
#' dec1 + dec2 + 7/3 + 8.25
#' dec1 / 2
#' 2 * dec2
#'
#' # deb_lsd and deb_decimal returns a deb_lsd
#' # except with division
#'
#' lsd1 + dec1
#' dec1 - lsd2
#'
#' # When combing two deb-style vectors, the bases must match
#'
#' \dontrun{
#' lsd1 + deb_lsd(5, 8, 3, bases = c(60, 16))
#' dec1 - deb_decimal(3.25, bases = c(60, 16))
#' }
#'
#' # When combining two `deb_decimal` vectors, the units must match
#'
#' \dontrun{
#' dec1 + deb_decimal(23.25, unit = "s")
#' }
#'
#' @name arithmetic
NULL

# deb_lsd arithmetic operators --------------------------------------------

## Arithmetic boilerplate ##

#' @rdname arithmetic
#' @method vec_arith deb_lsd
#' @export
#' @export vec_arith.deb_lsd
vec_arith.deb_lsd <- function(op, x, y) {
  UseMethod("vec_arith.deb_lsd", y)
}

#' @rdname arithmetic
#' @method vec_arith.deb_lsd default
#' @export
vec_arith.deb_lsd.default <- function(op, x, y) {
  vctrs::stop_incompatible_op(op, x, y)
}


# Operators with lsd and lsd ----------------------------------------------

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

#' @rdname arithmetic
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


# Operators with deb_lsd and numeric --------------------------------------

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

#' @rdname arithmetic
#' @method vec_arith.deb_lsd numeric
#' @export
vec_arith.deb_lsd.numeric <- function(op, x, y) {
  switch(
    op,
    "*" = lsd_multiply(x, multiplier = y),
    "/" = lsd_divide(x, divisor = y),
    vctrs::stop_incompatible_op(op, x, y)
  )
}

# numeric and deb_lsd

#' @rdname arithmetic
#' @method vec_arith.numeric deb_lsd
#' @export
vec_arith.numeric.deb_lsd <- function(op, x, y) {
  switch(
    op,
    "*" = lsd_multiply(y, multiplier = x),
    vctrs::stop_incompatible_op(op, x, y)
  )
}


# Unary operators with deb_lsd --------------------------------------------

lsd_negate <- function(x) {
  vctrs::field(x, "l") <- vctrs::field(x, "l") * -1
  vctrs::field(x, "s") <- vctrs::field(x, "s") * -1
  vctrs::field(x, "d") <- vctrs::field(x, "d") * -1

  x
}

#' @rdname arithmetic
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

# deb_decimal arithmetic operators ----------------------------------------

## Arithmetic boilerplate ##

#' @rdname arithmetic
#' @method vec_arith deb_decimal
#' @export
#' @export vec_arith.deb_decimal
vec_arith.deb_decimal <- function(op, x, y) {
  UseMethod("vec_arith.deb_decimal", y)
}

#' @rdname arithmetic
#' @method vec_arith.deb_decimal default
#' @export
vec_arith.deb_decimal.default <- function(op, x, y) {
  vctrs::stop_incompatible_op(op, x, y)
}


# Operators with deb_decimal and deb_decimal ------------------------------

#' @rdname arithmetic
#' @method vec_arith.deb_decimal deb_decimal
#' @export
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


# Operators with deb_decimal and numeric ----------------------------------

#' @rdname arithmetic
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

#' @rdname arithmetic
#' @method vec_arith.numeric deb_decimal
#' @export
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


# Unary operators with deb_decimal ----------------------------------------

#' @rdname arithmetic
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


# Operators with deb_lsd and deb_decimal ----------------------------------

# deb_lsd and deb_decimal

#' @rdname arithmetic
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

#' @rdname arithmetic
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
