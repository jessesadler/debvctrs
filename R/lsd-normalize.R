## Normalize lsd values ##

is_negative <- function(x) {
  vctrs::field(x, "l") + vctrs::field(x, "s") /
    deb_bases(x)[[1]] + vctrs::field(x, "d") / prod(deb_bases(x)) < 0
}

decimal_check <- function(lsd) {
  l <- vctrs::field(lsd, "l")
  s <- vctrs::field(lsd, "s")
  d <- vctrs::field(lsd, "d")

  vctrs::field(lsd, "l") <- trunc(l)
  temp_s <- s + (l - trunc(l)) * deb_bases(lsd)[[1]]
  vctrs::field(lsd, "s") <- trunc(temp_s)
  vctrs::field(lsd, "d") <- d + (temp_s - trunc(temp_s)) * deb_bases(lsd)[[2]]

  lsd
}

lsd_normalize <- function(lsd) {
  l <- vctrs::field(lsd, "l")
  s <- vctrs::field(lsd, "s")
  d <- vctrs::field(lsd, "d")
  bases <- deb_bases(lsd)

  vctrs::field(lsd, "l") <- l + ((s + d %/% bases[[2]]) %/% bases[[1]])
  vctrs::field(lsd, "s") <- (s + d %/% bases[[2]]) %% bases[[1]]
  vctrs::field(lsd, "d") <- d %% bases[[2]]

  lsd
}

lsd_normalize_neg <- function(lsd) {
  l <- -vctrs::field(lsd, "l")
  s <- -vctrs::field(lsd, "s")
  d <- -vctrs::field(lsd, "d")
  bases <- deb_bases(lsd)

  vctrs::field(lsd, "l") <- l + ((s + d %/% bases[[2]]) %/% bases[[1]])
  vctrs::field(lsd, "s") <- (s + d %/% bases[[2]]) %% bases[[1]]
  vctrs::field(lsd, "d") <- d %% bases[[2]]

  -lsd
}


# deb_normalize methods ---------------------------------------------------

deb_normalize <- function(x, ...) {
  UseMethod("deb_normalize")
}

deb_normalize.deb_lsd <- function(x) {
  decimals <- decimal_check(x)
  dplyr::if_else(is_negative(x),
                 lsd_normalize_neg(decimals),
                 lsd_normalize(decimals))
}

deb_normalize.numeric <- function(x, bases = c(20, 12)) {
  if (vctrs::vec_size(x) != 3L) {
    stop(call. = FALSE, "`x` must be a numeric vector of length 3.")
  }

  lsd <- deb_lsd(x[[1]], x[[2]], x[[3]], bases)
  deb_normalize(lsd)
}
