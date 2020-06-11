## Equality and comparison for deb_lsd ##

# deb_decimal() gets both equality and comparison for free
# because it is based on double()

#' Equality and comparison
#' @param x A deb_lsd object.
#' @param ... Arguments passed on to further methods.
#' @name comparison
NULL

# A) deb_lsd equality -----------------------------------------------------

# Normalize x and then create data frame for equality check
# Enables ==, !=, unique(), etc.
# Example of equality: £1 0s. 0d. is equal to £0 20s. 0d.

#' @rdname comparison
#' @export
vec_proxy_equal.deb_lsd <- function(x, ...) {
  x <- deb_normalize(x)
  data.frame(l = field(x, "l"),
             s = field(x, "s"),
             d = field(x, "d"))
}


# B) deb_lsd comparison ---------------------------------------------------

# Convert to double() to do comparison
# Enables <, <=, >=, >, etc.

#' @rdname comparison
#' @export
vec_proxy_compare.deb_lsd <- function(x, ...) {
  field(x, "l") + field(x, "s") /
    deb_bases(x)[[1]] + field(x, "d") / prod(deb_bases(x))
}
