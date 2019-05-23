## Define deb_lsd class ##

# Constructor -------------------------------------------------------------

new_lsd <- function(l = double(),
                    s = double(),
                    d = double(),
                    bases = c(20L, 12L)) {

  vctrs::vec_assert(l, ptype = double())
  vctrs::vec_assert(s, ptype = double())
  vctrs::vec_assert(d, ptype = double())

  bases <- bases_assert(bases)

  vctrs::new_rcrd(list(l = l, s = s, d = d),
                  bases = bases,
                  class = "deb_lsd")
}


# Helper ------------------------------------------------------------------

deb_lsd <- function(l = double(),
                    s = double(),
                    d = double(),
                    bases = c(20, 12)) {
  # checks
  lsd_check(l, s, d)
  bases_check(bases)

  c(l, s, d) %<-% vctrs::vec_cast_common(l, s, d, .to = double())
  c(l, s, d) %<-% vctrs::vec_recycle_common(l, s, d)

  bases <- vctrs::vec_cast(bases, to = integer())

  new_lsd(l = l, s = s, d = d, bases = bases)
}


# Attribute access --------------------------------------------------------
deb_bases <- function(x) attr(x, "bases")


# Class check -------------------------------------------------------------
deb_is_lsd <- function(x) inherits(x, "deb_lsd")


# Format method -----------------------------------------------------------
format.deb_lsd <- function(x, ...) {
  l <- round(vctrs::field(x, "l"), 3) # only print 3 decimals
  s <- round(vctrs::field(x, "s"), 3)
  d <- round(vctrs::field(x, "d"), 3)

  out <- paste0(l, ":", s, "s:", d, "d")
  out[is.na(l) | is.na(s) | is.na(d)] <- NA
  out
}

obj_print_footer.deb_lsd <- function(x, ...) {
  s <- format(attr(x, "bases")[[1]])
  d <- format(attr(x, "bases")[[2]])
  cat("# Bases: ", s, "s ", d, "d", "\n", sep = "")
}


# Abbreviated name type ---------------------------------------------------
vec_ptype_abbr.deb_lsd <- function(x) {
  paste0("lsd[", attr(x, "bases")[[1]], "s:", attr(x, "bases")[[2]], "d]")
}
