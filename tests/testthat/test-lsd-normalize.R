## Test normalization ##

x <- deb_lsd(5, 82, 56)
neg <- deb_lsd(-5, -82, -56)
decimal <- deb_lsd(5.875, 84.325, 62.0999)
floating <- deb_lsd(9.45, 0, 0)

bases <- c(50, 16)
x_bases <- deb_lsd(5, 82, 56, bases)
neg_bases <- deb_lsd(-5, -82, -56, bases)
decimal_bases <- deb_lsd(5.875, 84.325, 62.0999, bases)

multi <- c(x, neg, decimal)
multi_bases <- c(x_bases, neg_bases, decimal_bases)

test_that("is_negative works", {
  expect_true(is_negative(neg))
  expect_false(is_negative(x))
  expect_false(is_negative(deb_lsd(5, -8, 11)))
  expect_equal(is_negative(multi), c(FALSE, TRUE, FALSE))
})

test_that("decimal_check works", {
  expect_equal(length(decimal_check(multi)), 3)
  expect_equal(decimal_check(x), x)
  expect_equal(decimal_check(decimal), deb_lsd(5, 101, 71.9999))
  expect_equal(decimal_check(deb_lsd(-5.875, -84.325, -62.0999)),
               deb_lsd(-5, -101, -71.9999))
  expect_equal(decimal_check(decimal_bases), deb_lsd(5, 128, 63.2999, bases))
  # Floating point problems
  expect_equal(vctrs::field(decimal_check(floating), "d"), 12)
})

test_that("lsd_normalize and lsd_normalize_neg work", {
  expect_equal(lsd_normalize(x), deb_lsd(9, 6, 8))
  expect_equal(lsd_normalize(x_bases), deb_lsd(6, 35, 8, bases))
  expect_equal(lsd_normalize_neg(neg), deb_lsd(-9, -6, -8))
  expect_equal(lsd_normalize_neg(neg_bases), deb_lsd(-6, -35, -8, bases))
})

test_that("it comes together with deb_normalize", {
  expect_equal(deb_normalize(x), deb_lsd(9, 6, 8))
  expect_equal(deb_normalize(floating), deb_lsd(9, 9, 0))
  expect_equal(deb_normalize(deb_lsd(9, -6, 8)), deb_lsd(8, 14, 8))
  expect_equal(deb_normalize(multi),
               deb_lsd(c(9, -9, 10), c(6, -6, 6), c(8, -8, 11.9999)))
  expect_equal(deb_normalize(multi_bases),
               deb_lsd(c(6, -6, 7), c(35, -35, 31), c(8, -8, 15.2999), bases))
})

test_that("deb_normalize works with numeric vector", {
  # Errors
  expect_error(deb_normalize(TRUE),
               "`x` must be a <deb_lsd> vector or a numeric vector of length 3.")
  expect_error(deb_normalize(4), "`x` must be a numeric vector of length 3.")
  expect_error(deb_normalize(1:4), "`x` must be a numeric vector of length 3.")
  # It works
  expect_s3_class(deb_normalize(c(5, 82, 56)), "deb_lsd")
  expect_equal(deb_normalize(c(5, 82, 56)), deb_lsd(9, 6, 8))
  expect_equal(deb_normalize(c(-5, -82, -56)), deb_lsd(-9, -6, -8))
  expect_equal(deb_normalize(c(5, 82, 56), bases), deb_lsd(6, 35, 8, bases))
})
