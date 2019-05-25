## Test checks ##

# lsd_check ---------------------------------------------------------------

test_that("non-numeric is an error", {
  expect_error(lsd_check("hello", 3, 4),
               "`l` must be a numeric vector")
  expect_error(lsd_check(3, "hello", 4),
               "`s` must be a numeric vector")
  expect_error(lsd_check(3, 4, "hello"),
               "`d` must be a numeric vector")
})

test_that("NA scalar is not an error", {
  expect_invisible(lsd_check(NA, 3, 4))
  expect_invisible(lsd_check(3, NA, 4))
  expect_invisible(lsd_check(3, 4, NA))
})

test_that("length of l, s, and d all have values or are all length 0", {
  expect_invisible(lsd_check(double(), double(), double()))
  expect_error(lsd_check(2, double(), double()),
               "`l`, `s`, and `d` must all have values. You may have forgotten a value or need to use 0.")
  expect_error(lsd_check(2, 3, double()),
               "`l`, `s`, and `d` must all have values. You may have forgotten a value or need to use 0.")
})

test_that("length of l, s, and d are same length, length 1, or length 0", {
  # Successful
  expect_invisible(lsd_check(l = 3, s = 4, d = 1))
  expect_invisible(lsd_check(l = c(3, 5, 3),
                             s = c(4, 9, 5),
                             d = c(1, 3, 2)))
  expect_invisible(lsd_check(l = c(3, 5, 3),
                             s = c(4, 9, 5),
                             d = 0))

  # Errors
  expect_error(lsd_check(l = c(3, 5, 3),
                         s = c(4, 9),
                         d = 0),
               "`l`, `s`, and `d` must be vectors of equal length or length 1")
})


# bases_check -------------------------------------------------------------
test_that("bases is numeric vector of length 2", {
  # Successful
  expect_invisible(bases_check(c(20, 12)))
  expect_error(bases_check(NULL),
               "`bases` must be a numeric vector of length 2.")
  expect_error(bases_check(c("hello", "goodbye")),
               "`bases` must be a numeric vector of length 2.")
  expect_error(bases_check(1),
               "`bases` must be a numeric vector of length 2.")
  expect_error(bases_check(c(1, 3, 4)),
               "`bases` must be a numeric vector of length 2.")
})

test_that("bases does not have any missing values", {
  expect_error(bases_check(c(NA, 3)),
               "`bases` cannot be `NA`.")
  expect_error(bases_check(c(3, NA)),
               "`bases` cannot be `NA`.")
})

test_that("bases are natural numbers", {
  expect_error(bases_check(c(-12, -3)),
               "`bases` must be natural numbers greater than zero.")
  expect_error(bases_check(c(20, 0)),
               "`bases` must be natural numbers greater than zero.")
  expect_error(bases_check(c(20.5, 8.23)),
               "`bases` must be natural numbers greater than zero.")
})


# Equivalency -------------------------------------------------------------

test_that("bases assert allows named vector", {
  expect_equal(bases_assert(c(s = 20L, d = 12L)),
               c(s = 20L, d = 12L))
})

test_that("bases tests equivalency", {
  expect_invisible(bases_equal(deb_lsd(2, 3, 4), deb_lsd(3, 2, 1)))
  expect_invisible(bases_equal(deb_lsd(2, 3, 4, bases = c(60, 12)),
                               deb_lsd(3, 2, 1, bases = c(60, 12))))
  expect_invisible(bases_equal(deb_decimal(1.25), deb_decimal(1.25)))
  # Errors
  expect_error(bases_equal(deb_lsd(2, 3, 4), deb_lsd(3, 2, 1, bases = c(60, 12))),
               "`bases` attributes must be equal to combine <deb_lsd> or <deb_decimal> objects.")
  expect_error(bases_equal(deb_lsd(2, 3, 4), deb_lsd(3, 2, 1, bases = c(20, 16))),
               "`bases` attributes must be equal to combine <deb_lsd> or <deb_decimal> objects.")
  expect_error(bases_equal(deb_decimal(1.25), deb_decimal(1.25, bases = c(60, 12))),
               "`bases` attributes must be equal to combine <deb_lsd> or <deb_decimal> objects.")
  expect_error(bases_equal(deb_decimal(1.25), deb_decimal(1.25, bases = c(20, 16))),
               "`bases` attributes must be equal to combine <deb_lsd> or <deb_decimal> objects.")
})

test_that("unit tests equivalency", {
  expect_invisible(unit_equal(deb_decimal(1.25), deb_decimal(1.25)))
  expect_invisible(unit_equal(deb_decimal(1.25, "s"), deb_decimal(1.25, "s")))
  expect_error(unit_equal(deb_decimal(1.25), deb_decimal(1.25, "s")),
               "`unit` attributes must be equal to combine <deb_decimal> objects.")
})
