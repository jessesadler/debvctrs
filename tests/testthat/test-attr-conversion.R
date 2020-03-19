## Test bases and unit conversions ##

lsd1 <- deb_lsd(5, 6, 8)
bases2 <- c(40, 24)
lsd2 <- deb_lsd(5, 13, 8, bases2)
dec_l <- deb_decimal(16 / 3)
dec_s <- deb_decimal(106 + 2 / 3, unit = "s")
dec_d <- deb_decimal(1280, unit = "d")
dec_lb2 <- deb_decimal(16 / 3, bases = bases2)
dec_sb2 <- deb_decimal(213 + 1 / 3, unit = "s", bases = bases2)
dec_db2 <- deb_decimal(5120, unit = "d", bases = bases2)

# Bases conversion --------------------------------------------------------

test_that("Bases conversion works with deb_lsd objects", {
  expect_equal(deb_convert_bases(lsd1, to = c(20, 12)), lsd1)
  expect_equal(deb_convert_bases(lsd1, to = bases2), lsd2)
  expect_equal(deb_convert_bases(lsd2, to = c(20, 12)), lsd1)
  # Error with non-deb object
  expect_error(deb_convert_bases("a", c(20, 12)),
               "`x` must be a <deb_lsd> or <deb_decimal> vector.")
})

test_that("Bases conversion works with deb_decimal objects", {
  expect_equal(deb_convert_bases(dec_l, to = c(20, 12)), dec_l)
  expect_equal(deb_convert_bases(dec_l, to = bases2), dec_lb2)
  expect_equal(deb_convert_bases(dec_s, to = bases2), dec_sb2)
  expect_equal(deb_as_lsd(deb_convert_bases(dec_s, to = bases2)), lsd2)
  expect_equal(deb_convert_bases(dec_d, to = bases2), dec_db2)
  expect_equal(deb_as_lsd(deb_convert_bases(dec_d, to = bases2)), lsd2)
})

# Unit conversion ---------------------------------------------------------

test_that("Unit conversion works", {
  expect_equal(deb_convert_unit(dec_l, to = "l"), dec_l)
  expect_equal(deb_convert_unit(dec_l, to = "s"), dec_s)
  expect_equal(deb_convert_unit(dec_l, to = "d"), dec_d)
  expect_equal(deb_convert_unit(dec_sb2, to = "d"), dec_db2)
  expect_equal(deb_convert_unit(dec_sb2, to = "l"), dec_lb2)
  expect_equal(deb_convert_unit(dec_db2, to = "l"), dec_lb2)
  expect_equal(deb_convert_unit(dec_db2, to = "s"), dec_sb2)
  expect_equal(deb_as_lsd(deb_convert_unit(dec_db2, to = "l")), lsd2)
  # Errors
  expect_error(deb_convert_unit(dec_l, to = "hello"))
  expect_error(deb_convert_unit(lsd1), "`x` must be a <deb_decimal> object.")
})
