## Equality and comparison for deb_lsd ##

# deb_decimal gets this for free because it is based on double()

# 1. deb_lsd equality --------------------------------------------------------

# Normalize x and then create data.frame for equality check

#' @rdname vctrs-compat
#' @method vec_proxy_equal deb_lsd
#' @export
#' @export vec_proxy_equal.deb_lsd
vec_proxy_equal.deb_lsd <- function(x, ...) {
  x <- deb_normalize(x)
  data.frame(l = vctrs::field(x, "l"),
             s = vctrs::field(x, "s"),
             d = vctrs::field(x, "d"))
}


# 2. deb_lsd comparison ------------------------------------------------------

# Convert to double() to do comparison

#' @rdname vctrs-compat
#' @method vec_proxy_compare deb_lsd
#' @export
#' @export vec_proxy_compare.deb_lsd
vec_proxy_compare.deb_lsd <- function(x, ...) {
  vctrs::field(x, "l") + vctrs::field(x, "s") /
    deb_bases(x)[[1]] + vctrs::field(x, "d") / prod(deb_bases(x))
}
