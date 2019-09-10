## Mathematical functions with deb_lsd ##

# deb_decimal gets this for free because it is based on double()

#' Mathematical functions with deb_lsd objects
#'
#' @param x An object of class `deb_lsd`.
#' @param ... `deb_lsd` vectors in `sum()` and arguments passed on to
#'   further methods in `mean()`.
#' @param na.rm Logical. Should missing values (including `NaN``) be removed?
#' @param digits Integer. Indicating the number of decimal places
#'   (`round()`) or significant digits (`signif()`) to be used.
#'
#' @name mathematics
NULL


# 1. Implemented mathematical functions -----------------------------------

# sum ---------------------------------------------------------------------

#' @rdname mathematics
#' @export
sum.deb_lsd <- function(..., na.rm = FALSE) {
  x <- vctrs::vec_c(...)
  # Remove NA if na.rm is TRUE
  if (na.rm == TRUE) {
    x <- x[!is.na(x)]
  }

  ret <- new_lsd(sum(vctrs::field(x, "l"), na.rm = na.rm),
                 sum(vctrs::field(x, "s"), na.rm = na.rm),
                 sum(vctrs::field(x, "d"), na.rm = na.rm),
                 bases = deb_bases(x))

  deb_normalize(ret)
}


# mean --------------------------------------------------------------------

#' @rdname mathematics
#' @export
mean.deb_lsd <- function(x, ..., na.rm = FALSE) {
  if (na.rm == TRUE) {
    x <- x[!is.na(x)]
  }
  sum(x) / vctrs::vec_size(x)
}

#' @export
abs.deb_lsd <- function(x) {
  dec <- deb_as_decimal(x)
  deb_as_lsd(abs(dec))
}


# cumulative functions ----------------------------------------------------

#' @rdname mathematics
#' @export
cumsum.deb_lsd <- function(x) {
  ret <- new_lsd(cumsum(vctrs::field(x, "l")),
                 cumsum(vctrs::field(x, "s")),
                 cumsum(vctrs::field(x, "d")),
                 bases = deb_bases(x))

  deb_normalize(ret)
}

#' @export
cummin.deb_lsd <- function(x) {
  dec <- deb_as_decimal(x)
  deb_as_lsd(cummin(dec))
}

#' @export
cummax.deb_lsd <- function(x) {
  dec <- deb_as_decimal(x)
  deb_as_lsd(cummax(dec))
}


# Rounding ----------------------------------------------------------------

#' @rdname mathematics
#' @export
round.deb_lsd <- function(x, digits = 0) {
  x <- decimal_check(x)
  vctrs::field(x, "d") <- round(vctrs::field(x, "d"), digits = digits)
  deb_normalize(x)
}

#' @rdname mathematics
#' @export
signif.deb_lsd <- function(x, digits = 6) {
  vctrs::field(x, "d") <- signif(vctrs::field(x, "d"), digits = digits)
  deb_normalize(x)
}

#' @rdname mathematics
#' @export
ceiling.deb_lsd <- function(x) {
  x <- decimal_check(x)
  vctrs::field(x, "d") <- ceiling(vctrs::field(x, "d"))
  deb_normalize(x)
}

#' @rdname mathematics
#' @export
floor.deb_lsd <- function(x) {
  x <- decimal_check(x)
  vctrs::field(x, "d") <- floor(vctrs::field(x, "d"))
  deb_normalize(x)
}

#' @rdname mathematics
#' @export
trunc.deb_lsd <- function(x, ...) {
  x <- decimal_check(x)
  vctrs::field(x, "d") <- trunc(vctrs::field(x, "d"))
  deb_normalize(x)
}



# 2. Error message for unimplemented functions ----------------------------

#' @rdname vctrs-compat
#' @method vec_math deb_lsd
#' @export
#' @export vec_math.deb_lsd
vec_math.deb_lsd <- function(.fn, .x, ...) {
  stop(call. = FALSE,
       paste0("`", .fn, ".", class(.x)[[1]], "()` not implemented."))
}
