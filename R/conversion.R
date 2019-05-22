## bases and unit conversion ##

# Convert bases -----------------------------------------------------------

deb_convert_bases <- function(x, to) {
  UseMethod("deb_convert_bases")
}

deb_convert_bases.deb_lsd <- function(x, to) {
  bases_check(to)

  from <- deb_bases(x)
  to <- vctrs::vec_cast(to, to = integer())
  to <- rlang::set_names(to, c("s", "d"))

  if (identical(from, to)) {
    return(x)
  }

  temp_s <- vctrs::field(x, "s") * to[[1]] / from[[1]]
  vctrs::field(x, "s") <- trunc(temp_s)
  vctrs::field(x, "d") <- (temp_s - trunc(temp_s)) * to[[2]] +
    vctrs::field(x, "d") * prod(to) / prod(from)
  attr(x, "bases") <- to

  x
}

deb_convert_bases.deb_decimal <- function(x, to) {
  bases_check(to)

  from <- deb_bases(x)
  to <- vctrs::vec_cast(to, to = integer())
  to <- rlang::set_names(to, c("s", "d"))

  if (deb_unit(x) == "l") {
    converted <- x
  } else if (deb_unit(x) == "s") {
    converted <- x * to[[1]] / from[[1]]
  } else {
    converted <- x * prod(to) / prod(from)
  }

  attr(converted, "bases") <- to

  converted
}

# Convert units -----------------------------------------------------------

deb_convert_unit <- function(x, to = c("l", "s", "d")) {
  if (!deb_is_decimal(x)) {
    stop(call. = FALSE, "`x` must be a <deb_decimal> object.")
  }
  to <- rlang::arg_match(to)
  from <- deb_unit(x)
  bases <- deb_bases(x)

  if (from == to) {
    return(x)
  }

  converted <- dplyr::case_when(
    from == "l" & to == "s" ~ x * bases[[1]],
    from == "l" & to == "d" ~ x * prod(bases),
    from == "s" & to == "d" ~ x * bases[[2]],
    from == "s" & to == "l" ~ x / bases[[1]],
    from == "d" & to == "l" ~ x / prod(bases),
    from == "d" & to == "s" ~ x / bases[[2]]
  )

  attr(converted, "unit") <- to

  converted
}
