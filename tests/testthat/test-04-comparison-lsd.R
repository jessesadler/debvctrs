## Test equality and comparison ##

lsd1 <- deb_lsd(1, 2, 3)
lsd2 <- deb_lsd(5, 6, 8)
normalize <- c(lsd1, lsd2, NA, deb_lsd(c(3, 2), c(40, 84), c(80, 65)))
dec1 <- deb_decimal(1.1125)
dec2 <- deb_decimal(8.825)
dec3 <- deb_decimal(c(1.1125, NA, 5.225, 3.2875, 1.1125))
dec_s <- deb_decimal(22.25, "s")
dec_s2 <- deb_decimal(176.5, "s")
dec_d <- deb_decimal(267, "d")
dec_d2 <- deb_decimal(2118, "d")

bases_error <- paste0("`bases` attributes must be equal to combine ",
                      "<deb_lsd> or <deb_decimal> objects.")

# Equality ----------------------------------------------------------------

test_that("Equality works with deb_lsd", {
  expect_true(lsd1 == deb_lsd(1, 2, 3))
  expect_false(lsd1 == lsd2)
  expect_true(lsd1 != lsd2)
  expect_true(lsd1 == deb_lsd(0, 20, 27)) # normalization
  expect_equal(unique(normalize), normalize[c(1:3, 5)])
  expect_true(anyDuplicated(normalize))
  expect_equal(is.na(normalize), c(FALSE, FALSE, TRUE, FALSE, FALSE))
  # Error with different bases
  expect_error(lsd1 == deb_lsd(5, 6, 8, bases = c(20, 16)),
               bases_error)
})

test_that("Equality works with deb_decimal", {
  expect_true(dec1 == deb_decimal(1.1125))
  expect_false(dec1 == dec2)
  expect_true(dec1 != dec2)
  expect_equal(dec3 == dec_s, c(TRUE, NA, FALSE, FALSE, TRUE))
  # Different units
  expect_true(dec1 == dec_s)
  expect_true(dec1 == dec_d)
  expect_true(dec_s == dec_d)
  expect_false(dec1 != dec_s)

  # Functions
  expect_equal(unique(dec3), dec3[-5])
  expect_true(anyDuplicated(dec3))
  expect_equal(is.na(dec3), c(FALSE, TRUE, FALSE, FALSE, FALSE))
  # Error with different bases
  expect_error(dec1 == deb_decimal(1.1125, bases = c(24, 12)),
               bases_error)
})


# Comparison --------------------------------------------------------------

test_that("Comparison logical operators work", {
  # deb_lsd
  expect_true(lsd2 > lsd1)
  expect_true(lsd2 <= deb_lsd(4, 26, 8))
  expect_false(lsd2 > deb_lsd(4, 26, 8))
  expect_true(lsd2 > dec1)
  expect_true(lsd2 > 5)
  expect_false(lsd2 < 5)

  # deb_decimal
  expect_true(dec1 < dec2)
  expect_true(dec1 < dec_s2)
  expect_true(dec1 < dec_d2)
  expect_true(dec_s < dec_d2)
  expect_true(dec2 > 5)
  expect_false(dec2 < 5)

  # Error with different bases
  expect_error(lsd1 < deb_lsd(15, 6, 8, bases = c(20, 16)), bases_error)
  expect_error(dec1 < deb_decimal(11.125, bases = c(24, 12)), bases_error)
})

test_that("Comparison functions work", {
  # median and quantile not implemented yet
  expect_equal(min(normalize, na.rm = TRUE), lsd1)
  expect_equal(max(normalize, na.rm = TRUE), deb_lsd(2, 84, 65))
  expect_equal(range(normalize, na.rm = TRUE), c(lsd1, deb_lsd(2, 84, 65)))
  expect_equal(sort(normalize), normalize[c(1, 2, 4, 5)])

  expect_equal(min(dec3, na.rm = TRUE), dec1)
  expect_equal(max(dec3, na.rm = TRUE), deb_decimal(5.225))
  expect_equal(range(dec3, na.rm = TRUE), c(dec1, deb_decimal(5.225)))
  expect_equal(sort(dec3), dec3[c(1, 1, 4, 3)])
})
