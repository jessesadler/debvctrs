## Define deb_decimal class ##

# Constructor -------------------------------------------------------------

new_decimal <- function(x = double(),
                        unit = c("l", "s", "d"),
                        bases = c(20L, 12L)) {
  unit <- rlang::arg_match(unit)

  vctrs::vec_assert(x, ptype = double())
  vctrs::vec_assert(bases, ptype = integer(), size = 2)

  vctrs::new_vctr(x,
                  unit = unit,
                  bases = bases,
                  class = "deb_decimal")
}


# Helper ------------------------------------------------------------------

deb_decimal <- function(x,
                        unit = c("l", "s", "d"),
                        bases = c(20, 12)) {
  unit <- rlang::arg_match(unit)
  bases_check(bases)

  x <- vctrs::vec_cast(x, to = double())
  bases <- vctrs::vec_cast(bases, to = integer())

  new_decimal(x = x, unit = unit, bases = bases)
}


# Attribute access --------------------------------------------------------
deb_unit <- function(x) {
  if (attr(x, "unit") == "l") {
    unit <- "libra"
  } else if (attr(x, "unit") == "s") {
    unit <- "solidus"
  } else {
    unit <- "denarius"
  }
  unit
}


# Class check -------------------------------------------------------------
deb_is_decimal <- function(x) {
  inherits(x, "deb_decimal")
}


# Format method -----------------------------------------------------------
# No format.deb_decimal to keep default vector printing

# Add footer with attribute data
obj_print_footer.deb_decimal <- function(x, ...) {
  # Use full name of units
  unit <- deb_unit(x)

  s <- format(attr(x, "bases")[[1]])
  d <- format(attr(x, "bases")[[2]])
  cat("# Unit: ", unit, "\n",
      "# Bases: ", s, "s ", d, "d", "\n", sep = "")
}

# Abbreviated name type ---------------------------------------------------
vec_ptype_abbr.deb_decimal <- function(x) {
  paste0("dec_", attr(x, "unit"), "[",
         attr(x, "bases")[[1]], "s:", attr(x, "bases")[[2]], "d]")
}
