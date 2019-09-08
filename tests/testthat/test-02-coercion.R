## Test coercion with deb_lsd and deb_decimal ##

lsd <- deb_lsd(1, 2, 3)
lsd2 <- deb_lsd(4, 5, 6)

dec <- deb_decimal(c(1.35, 2.5))
dec2 <- deb_decimal(3.25)
dec3 <- deb_decimal(c(1.35, 2.5, 3.25))
dec_s <- deb_decimal(c(27, 50), unit = "s")
dec_s2 <- deb_decimal(65, unit = "s")
dec_s3 <- deb_decimal(c(27, 50, 65), unit = "s")
dec_d <- deb_decimal(c(324, 600), unit = "d")
dec_d2 <- deb_decimal(780, unit = "d")
dec_d3 <- deb_decimal(c(324, 600, 780), unit = "d")

bases_error <- paste0("`bases` attributes must be equal to combine ",
                      "<deb_lsd> or <deb_decimal> objects.")

# deb_lsd coercion --------------------------------------------------------

test_that("deb_lsd coerces to itself", {
  # Successful
  expect_equal(vctrs::vec_ptype_common(deb_lsd(), deb_lsd()), deb_lsd())
  expect_equal(length(vctrs::vec_c(lsd, lsd2)), 2)
  expect_equal(vctrs::vec_c(lsd, lsd2),
               deb_lsd(c(1, 4), c(2, 5), c(3, 6)))
  expect_equal(length(c(lsd, lsd2)), 2)
  expect_equal(c(lsd, lsd2), deb_lsd(c(1, 4), c(2, 5), c(3, 6)))
  # Errors
  expect_error(vctrs::vec_c(lsd, deb_lsd(1, 2, 3, bases = c(50, 16))),
               bases_error)
  expect_error(c(lsd, deb_lsd(1, 2, 3, bases = c(50, 16))),
               bases_error)
})

test_that("double coerces to deb_lsd", {
  expect_equal(vctrs::vec_ptype_common(deb_lsd(), double()), deb_lsd())
  expect_equal(length(vctrs::vec_c(lsd, 1.5)), 2)
  expect_equal(vctrs::vec_c(lsd, 1.5),
               deb_lsd(c(1, 1), c(2, 10), c(3, 0)))
  expect_equal(vctrs::vec_c(1.5, lsd),
               deb_lsd(c(1, 1), c(10, 2), c(0, 3)))
  expect_equal(length(c(lsd, 1.5)), 2)
  expect_equal(c(lsd, 1.5), deb_lsd(c(1, 1), c(2, 10), c(3, 0)))
})

test_that("integer coerces to deb_lsd", {
  expect_equal(vctrs::vec_ptype_common(deb_lsd(), integer()), deb_lsd())
  expect_equal(length(vctrs::vec_c(lsd, 1L)), 2)
  expect_equal(vctrs::vec_c(lsd, 1L),
               deb_lsd(c(1, 1), c(2, 0), c(3, 0)))
  expect_equal(vctrs::vec_c(1L, lsd),
               deb_lsd(c(1, 1), c(0, 2), c(0, 3)))
  expect_equal(length(c(lsd, 1L)), 2)
  expect_equal(c(lsd, 1L), deb_lsd(c(1, 1), c(2, 0), c(3, 0)))
})

test_that("deb_lsd coercion works with NA", {
  expect_equal(vctrs::vec_c(NA, lsd), deb_lsd(c(NA, 1), c(NA, 2), c(NA, 3)))
  expect_equal(c(lsd, NA), deb_lsd(c(1, NA), c(2, NA), c(3, NA)))
})

test_that("incompatible types do not work", {
  expect_error(c(lsd, "hello"))
  expect_error(c(lsd, TRUE))
})

# deb_decimal coercion ----------------------------------------------------

test_that("deb_decimal coerces to itself", {
  # Successful
  expect_equal(vctrs::vec_ptype_common(deb_decimal(), deb_decimal()),
               deb_decimal())
  expect_equal(length(vctrs::vec_c(dec, dec2)), 3)
  expect_equal(vctrs::vec_c(dec, dec2), dec3)
  expect_equal(length(c(dec, dec2)), 3)
  expect_equal(c(dec, dec2), dec3)
  expect_equal(c(dec_s, dec_s2), dec_s3)
  expect_equal(c(dec_d, dec_d2), dec_d3)

  # With different units: follows hierarchy
  expect_equal(deb_unit(c(dec, dec_s)), deb_unit(c(dec_s, dec)))
  expect_equal(deb_unit(c(dec, dec_d)), deb_unit(c(dec_d, dec)))
  expect_equal(deb_unit(c(dec_d, dec_s)), deb_unit(c(dec_s, dec_d)))
  expect_equal(c(dec_s, dec2), dec3)
  expect_equal(c(dec_d, dec2), dec3)
  expect_equal(c(dec_d, dec_s2), dec_s3)

  # Errors
  expect_error(vctrs::vec_c(dec, deb_decimal(4.5, bases = c(50, 16))),
               bases_error)
  expect_error(c(dec, deb_decimal(4.5, bases = c(50, 16))),
               bases_error)
})

test_that("double coerces to deb_decimal", {
  expect_equal(vctrs::vec_ptype_common(deb_decimal(), double()), deb_decimal())
  expect_equal(length(vctrs::vec_c(dec, 4.5)), 3)
  expect_equal(vctrs::vec_c(dec, 4.5), deb_decimal(c(1.35, 2.5, 4.5)))
  expect_equal(vctrs::vec_c(4.5, dec), deb_decimal(c(4.5, 1.35, 2.5)))
  expect_equal(length(c(dec, 4.5)), 3)
  expect_equal(c(dec, 4.5), deb_decimal(c(1.35, 2.5, 4.5)))
})

test_that("integer coerces to deb_decimal", {
  expect_equal(vctrs::vec_ptype_common(deb_decimal(), integer()), deb_decimal())
  expect_equal(length(vctrs::vec_c(dec, 4L)), 3)
  expect_equal(vctrs::vec_c(dec, 4L), deb_decimal(c(1.35, 2.5, 4)))
  expect_equal(vctrs::vec_c(4L, dec), deb_decimal(c(4, 1.35, 2.5)))
  expect_equal(length(c(dec, 4L)), 3)
  expect_equal(c(dec, 4L), deb_decimal(c(1.35, 2.5, 4)))
})

test_that("deb_decimal coercion works with NA", {
  expect_equal(vctrs::vec_c(NA, dec), deb_decimal(c(NA, 1.35, 2.5)))
  expect_equal(c(dec, NA), deb_decimal(c(1.35, 2.5, NA)))
})

test_that("incompatible types do not work", {
  expect_error(c(dec, "hello"))
  expect_error(c(dec, TRUE))
})


# deb_lsd and deb_decimal coercion ----------------------------------------

test_that("deb_decimal coerces to deb_lsd", {
  # Successful
  expect_equal(vctrs::vec_c(lsd, dec2), deb_lsd(c(1, 3), c(2, 5), c(3, 0)))
  expect_equal(c(lsd, dec2), deb_lsd(c(1, 3), c(2, 5), c(3, 0)))
  expect_equal(vctrs::vec_c(dec2, lsd), deb_lsd(c(3, 1), c(5, 2), c(0, 3)))
  # Errors
  expect_error(vctrs::vec_c(lsd, deb_decimal(5.5, bases = c(50, 16))),
               bases_error)
  expect_error(vctrs::vec_c(dec, deb_lsd(1, 2, 3, bases = c(50, 16))),
               bases_error)
})
