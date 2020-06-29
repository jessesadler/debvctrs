## Mathematical functions with deb_lsd ##

# deb_decimal() gets all this for free because it is based on double()
# Choose which functions to implement and which to not implement.

#' Mathematical functions with deb_lsd vectors
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


# A) Implemented mathematical functions -----------------------------------

# 1. Sum ------------------------------------------------------------------

#' @rdname mathematics
#' @export
sum.deb_lsd <- function(..., na.rm = FALSE) {
  x <- vec_c(...)
  # Remove NA if na.rm is TRUE
  if (na.rm == TRUE) {
    x <- x[!is.na(x)]
  }
  # Separate sum for each unit in a newly created deb_lsd vector
  ret <- new_lsd(sum(field(x, "l"), na.rm = na.rm),
                 sum(field(x, "s"), na.rm = na.rm),
                 sum(field(x, "d"), na.rm = na.rm),
                 bases = deb_bases(x))
  # Normalize the summed units
  deb_normalize(ret)
}


# 2. Mean -----------------------------------------------------------------

#' @rdname mathematics
#' @export
mean.deb_lsd <- function(x, ..., na.rm = FALSE) {
  if (na.rm == TRUE) {
    x <- x[!is.na(x)]
  }
  sum(x) / vec_size(x)
}

# 3. abs ------------------------------------------------------------------

#' @export
abs.deb_lsd <- function(x) {
  dec <- deb_as_decimal(x)
  deb_as_lsd(abs(dec))
}


# 4. Cumulative functions -------------------------------------------------

#' @rdname mathematics
#' @export
cumsum.deb_lsd <- function(x) {
  # Same approach as sum.deb_lsd
  ret <- new_lsd(cumsum(field(x, "l")),
                 cumsum(field(x, "s")),
                 cumsum(field(x, "d")),
                 bases = deb_bases(x))

  deb_normalize(ret)
}

#' @export
cummin.deb_lsd <- function(x) {
  # Convert to deb_as_decimal and then perform cummin
  dec <- deb_as_decimal(x)
  deb_as_lsd(cummin(dec))
}

#' @export
cummax.deb_lsd <- function(x) {
  # Convert to deb_as_decimal and then perform cummin
  dec <- deb_as_decimal(x)
  deb_as_lsd(cummax(dec))
}


# 5. Rounding -------------------------------------------------------------

#' @rdname mathematics
#' @export
round.deb_lsd <- function(x, digits = 0) {
  # Move any decimals to pence unit
  x <- decimal_check(x)
  # Round the pence unit
  field(x, "d") <- round(field(x, "d"), digits = digits)
  # Normalize the value
  deb_normalize(x)
}

#' @rdname mathematics
#' @export
signif.deb_lsd <- function(x, digits = 6) {
  field(x, "d") <- signif(field(x, "d"), digits = digits)
  deb_normalize(x)
}

#' @rdname mathematics
#' @export
ceiling.deb_lsd <- function(x) {
  x <- decimal_check(x)
  field(x, "d") <- ceiling(field(x, "d"))
  deb_normalize(x)
}

#' @rdname mathematics
#' @export
floor.deb_lsd <- function(x) {
  x <- decimal_check(x)
  field(x, "d") <- floor(field(x, "d"))
  deb_normalize(x)
}

#' @rdname mathematics
#' @export
trunc.deb_lsd <- function(x, ...) {
  x <- decimal_check(x)
  field(x, "d") <- trunc(field(x, "d"))
  deb_normalize(x)
}


# B) Error message for unimplemented functions ----------------------------

#' @export
vec_math.deb_lsd <- function(.fn, .x, ...) {
  stop(call. = FALSE,
       paste0("`", .fn, ".", class(.x)[[1]], "()` not implemented."))
}
