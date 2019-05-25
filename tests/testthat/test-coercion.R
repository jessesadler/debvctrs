## Test coercion with deb_lsd and deb_decimal ##

x <- deb_lsd(1, 2, 3)
y <- deb_lsd(4, 5, 6)

w <- deb_decimal(c(1.35, 2.5))
z <- deb_decimal(3.25)

# deb_lsd coercion --------------------------------------------------------

test_that("deb_lsd coerces to itself", {
  # Successful
  expect_equal(vctrs::vec_type_common(deb_lsd(), deb_lsd()), deb_lsd())
  expect_equal(length(vctrs::vec_c(x, y)), 2)
  expect_equal(vctrs::vec_c(x, y),
               deb_lsd(c(1, 4), c(2, 5), c(3, 6)))
  expect_equal(length(c(x, y)), 2)
  expect_equal(c(x, y), deb_lsd(c(1, 4), c(2, 5), c(3, 6)))
  # Errors
  expect_error(vctrs::vec_c(x, deb_lsd(1, 2, 3, bases = c(50, 16))),
               "`bases` attributes must be equal to combine <deb_lsd> or <deb_decimal> objects.")
  expect_error(c(x, deb_lsd(1, 2, 3, bases = c(50, 16))),
               "`bases` attributes must be equal to combine <deb_lsd> or <deb_decimal> objects.")
})

test_that("double coerces to deb_lsd", {
  expect_equal(vctrs::vec_type_common(deb_lsd(), double()), deb_lsd())
  expect_equal(length(vctrs::vec_c(x, 1.5)), 2)
  expect_equal(vctrs::vec_c(x, 1.5),
               deb_lsd(c(1, 1), c(2, 10), c(3, 0)))
  expect_equal(vctrs::vec_c(1.5, x),
               deb_lsd(c(1, 1), c(10, 2), c(0, 3)))
  expect_equal(length(c(x, 1.5)), 2)
  expect_equal(c(x, 1.5), deb_lsd(c(1, 1), c(2, 10), c(3, 0)))
})

test_that("integer coerces to deb_lsd", {
  expect_equal(vctrs::vec_type_common(deb_lsd(), integer()), deb_lsd())
  expect_equal(length(vctrs::vec_c(x, 1L)), 2)
  expect_equal(vctrs::vec_c(x, 1L),
               deb_lsd(c(1, 1), c(2, 0), c(3, 0)))
  expect_equal(vctrs::vec_c(1L, x),
               deb_lsd(c(1, 1), c(0, 2), c(0, 3)))
  expect_equal(length(c(x, 1L)), 2)
  expect_equal(c(x, 1L), deb_lsd(c(1, 1), c(2, 0), c(3, 0)))
})

test_that("deb_lsd coercion works with NA", {
  expect_equal(vctrs::vec_c(NA, x), deb_lsd(c(NA, 1), c(NA, 2), c(NA, 3)))
  expect_equal(c(x, NA), deb_lsd(c(1, NA), c(2, NA), c(3, NA)))
})

test_that("incompatible types do not work", {
  expect_error(c(x, "hello"))
  expect_error(c(x, TRUE))
})

# deb_decimal coercion ----------------------------------------------------

test_that("deb_decimal coerces to itself", {
  # Successful
  expect_equal(vctrs::vec_type_common(deb_decimal(), deb_decimal()), deb_decimal())
  expect_equal(length(vctrs::vec_c(w, z)), 3)
  expect_equal(vctrs::vec_c(w, z), deb_decimal(c(1.35, 2.5, 3.25)))
  expect_equal(length(c(w, z)), 3)
  expect_equal(c(w, z), deb_decimal(c(1.35, 2.5, 3.25)))
  # Errors
  expect_error(vctrs::vec_c(w, deb_decimal(4.5, bases = c(50, 16))),
               "`bases` attributes must be equal to combine <deb_lsd> or <deb_decimal> objects.")
  expect_error(c(w, deb_decimal(4.5, bases = c(50, 16))),
               "`bases` attributes must be equal to combine <deb_lsd> or <deb_decimal> objects.")
  expect_error(vctrs::vec_c(w, deb_decimal(4.5, unit = "s")),
               "`unit` attributes must be equal to combine <deb_decimal> objects.")
  expect_error(vctrs::vec_c(w, deb_decimal(4.5, unit = "d")),
               "`unit` attributes must be equal to combine <deb_decimal> objects.")
})

test_that("double coerces to deb_decimal", {
  expect_equal(vctrs::vec_type_common(deb_decimal(), double()), deb_decimal())
  expect_equal(length(vctrs::vec_c(w, 4.5)), 3)
  expect_equal(vctrs::vec_c(w, 4.5), deb_decimal(c(1.35, 2.5, 4.5)))
  expect_equal(vctrs::vec_c(4.5, w), deb_decimal(c(4.5, 1.35, 2.5)))
  expect_equal(length(c(w, 4.5)), 3)
  expect_equal(c(w, 4.5), deb_decimal(c(1.35, 2.5, 4.5)))
})

test_that("integer coerces to deb_decimal", {
  expect_equal(vctrs::vec_type_common(deb_decimal(), integer()), deb_decimal())
  expect_equal(length(vctrs::vec_c(w, 4L)), 3)
  expect_equal(vctrs::vec_c(w, 4L), deb_decimal(c(1.35, 2.5, 4)))
  expect_equal(vctrs::vec_c(4L, w), deb_decimal(c(4, 1.35, 2.5)))
  expect_equal(length(c(w, 4L)), 3)
  expect_equal(c(w, 4L), deb_decimal(c(1.35, 2.5, 4)))
})

test_that("deb_decimal coercion works with NA", {
  expect_equal(vctrs::vec_c(NA, w), deb_decimal(c(NA, 1.35, 2.5)))
  expect_equal(c(w, NA), deb_decimal(c(1.35, 2.5, NA)))
})

test_that("incompatible types do not work", {
  expect_error(c(w, "hello"))
  expect_error(c(w, TRUE))
})


# deb_lsd and deb_decimal coercion ----------------------------------------

test_that("deb_decimal coerces to deb_lsd", {
  # Successful
  expect_equal(vctrs::vec_c(x, z), deb_lsd(c(1, 3), c(2, 5), c(3, 0)))
  expect_equal(c(x, z), deb_lsd(c(1, 3), c(2, 5), c(3, 0)))
  expect_equal(vctrs::vec_c(z, x), deb_lsd(c(3, 1), c(5, 2), c(0, 3)))
  # Errors
  expect_error(vctrs::vec_c(x, deb_decimal(5.5, bases = c(50, 16))),
               "`bases` attributes must be equal to combine <deb_lsd> or <deb_decimal> objects.")
  expect_error(vctrs::vec_c(w, deb_lsd(1, 2, 3, bases = c(50, 16))),
               "`bases` attributes must be equal to combine <deb_lsd> or <deb_decimal> objects.")
})
