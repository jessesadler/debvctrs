## Test arithmetic with deb_lsd and deb_decimal ##

lsd1 <- deb_lsd(1, 16, 9)
lsd2 <- deb_lsd(5, 6, 8)
lsd3 <- c(lsd1, lsd2, NA, deb_lsd(c(3, 7), c(12, 15), c(1, 11)))
bases2 <- c(50, 16)
lsd_bases <- deb_lsd(c(1, 5), c(16, 6), c(9, 8), bases = bases2)
lsd_round <- deb_lsd(5, 19, 11.8755)
neg_round <- deb_lsd(-5, -19, -11.8755)
round1 <- deb_lsd(6, 0, 0)
round2 <- deb_lsd(-6, 0, 0)
decimal1 <- deb_decimal(1.8375)
decimal2 <- deb_decimal(36.75, unit = "s")
decimal3 <- deb_decimal(c(1.8375, NA, 5.225, 3.2875, 1.1125))


# Sum and mean ------------------------------------------------------------

test_that("sum and mean with deb_lsd work", {
  # na.rm argument not working
  expect_equal(sum(lsd3), deb_lsd(18, 11, 5))
  expect_equal(sum(lsd1, lsd2), deb_lsd(7, 3, 5))
  expect_equal(sum(lsd_bases), deb_lsd(6, 23, 1, bases = bases2))
  # Error with different bases
  expect_error(sum(lsd3, lsd_bases),
               "`bases` attributes must be equal to combine <deb_lsd> or <deb_decimal> objects.")
  expect_equal(mean(lsd3), deb_lsd(4, 12, 10.25))
})

test_that("sum and mean work with deb_decimal", {
  # na.rm argument not working
  expect_equal(sum(decimal1, deb_decimal(1.5)), deb_decimal(3.3375))
  expect_equal(sum(decimal3), deb_decimal(sum(as.numeric(decimal3), na.rm = TRUE)))
  # Errors
  expect_error(sum(decimal1, deb_decimal(1.5, bases = bases2)),
               "`bases` attributes must be equal to combine <deb_lsd> or <deb_decimal> objects.")
  expect_error(sum(decimal1, decimal2),
               "`unit` attributes must be equal to combine <deb_decimal> objects.")
  expect_equal(mean(decimal3), deb_decimal(2.865625))
})

test_that("sum and mean work with deb-style objects and numeric", {
  expect_equal(sum(lsd1, decimal1), deb_lsd(3, 13, 6))
  expect_equal(sum(lsd1, decimal1), sum(lsd1, decimal2))
  expect_equal(sum(lsd3, 1.8375, 3), deb_lsd(23, 8, 2))
  expect_equal(sum(decimal3, 3.5), deb_decimal(sum(1.8375, 5.225, 3.2875, 1.1125, 3.5)))
})


# Round family with deb_lsd -----------------------------------------------

test_that("round family works with deb_lsd", {
  # round
  expect_equal(round(lsd_round), round1)
  expect_equal(round(neg_round), round2)
  expect_equal(round(deb_lsd(5, 49, 15.6, bases2)), deb_lsd(6, 0, 0, bases2))
  expect_equal(round(lsd_round, 3), deb_lsd(5, 19, 11.876))
  # signif
  expect_equal(signif(lsd_round, 3), deb_lsd(5, 19, 11.9))
  expect_equal(signif(lsd_round, 2), round1)
  # ceiling
  expect_equal(ceiling(lsd_round), round1)
  expect_equal(ceiling(neg_round), deb_lsd(-5, -19, -11))
  # floor
  expect_equal(floor(lsd_round), deb_lsd(5, 19, 11))
  expect_equal(floor(neg_round), round2)
  # trunc
  expect_equal(trunc(lsd_round), deb_lsd(5, 19, 11))
  expect_equal(trunc(neg_round), deb_lsd(-5, -19, -11))
})


# deb_lsd arithmetic operators --------------------------------------------
test_that("Arithmetic operators work with two deb_lsd objects", {
  # plus
  expect_equal(lsd1 + lsd2, deb_lsd(7, 3, 5))
  expect_equal(lsd_bases + deb_lsd(2, 10, 5, bases2),
               deb_lsd(c(3, 7), c(26, 16), c(14, 13), bases2))
  expect_error(lsd1 + lsd_bases,
               "`bases` attributes must be equal to combine <deb_lsd> or <deb_decimal> objects.")
  # minus
  expect_equal(lsd1 - lsd2, deb_lsd(-3, -9, -11))
  expect_equal(lsd2 - lsd1, deb_lsd(3, 9, 11))
  # division
  expect_equal(deb_lsd(10, 13, 4) / lsd2, 2)
  # Incompatible op
  expect_error(lsd1 * lsd2)
})

test_that("Arithmetic operators work with deb_lsd and numeric", {
  # deb_lsd and numeric
  expect_equal(lsd2 * 3, deb_lsd(16, 0, 0))
  expect_equal(lsd_bases * 2, deb_lsd(c(2, 10), c(33, 13), c(2, 0), bases2))
  expect_equal(lsd2 / 2, deb_lsd(2, 13, 4))
  expect_error(lsd1 + 3)
  # numeric and deb_lsd
  expect_equal(3 * lsd2, deb_lsd(16, 0, 0))
  expect_error(3 / lsd1)
})


# deb_decimal arithmetic operators ----------------------------------------

test_that("Arithmetic operators work with two deb_decimal objects", {
  expect_equal(decimal1 + deb_decimal(1.5), deb_decimal(3.3375))
  expect_equal(decimal1 - deb_decimal(1.5), deb_decimal(0.3375))
  expect_equal(deb_decimal(3.25) / deb_decimal(6.5), 0.5)
  # Errors
  expect_error(decimal1 + decimal2,
               "`unit` attributes must be equal to combine <deb_decimal> objects.")
  expect_error(decimal1 + deb_decimal(1.5, bases = bases2),
               "`bases` attributes must be equal to combine <deb_lsd> or <deb_decimal> objects.")
  expect_error(decimal1 * decimal3)
})

test_that("Arithmetic operators work with deb_lsd and numeric", {
  # deb_decimal and numeric
  expect_equal(decimal1 + 1.5, deb_decimal(3.3375))
  expect_equal(decimal1 - 1.5, deb_decimal(0.3375))
  expect_equal(decimal2 * 2, deb_decimal(73.5, unit = "s"))
  expect_equal(deb_decimal(2.5)^2, deb_decimal(6.25))
  expect_equal(decimal2 / 3, deb_decimal(12.25, unit = "s"))
  expect_equal(decimal2 %% 3, deb_decimal(0.75, unit = "s"))
  expect_equal(decimal2 %/% 3, deb_decimal(12, unit = "s"))
  # numeric and deb_decimal
  expect_equal(1.5 + decimal1, deb_decimal(3.3375))
  expect_equal(1.5 - decimal1, deb_decimal(-0.3375))
  expect_equal(2 * decimal2, deb_decimal(73.5, unit = "s"))
  expect_error(2 / decimal2)
})


# Unary operators ---------------------------------------------------------

test_that("unary operators work", {
  expect_equal(-lsd_round, neg_round)
  expect_equal(+lsd_bases, lsd_bases)
  expect_equal(-decimal1, deb_decimal(-1.8375))
  expect_equal(+decimal2, decimal2)
})

# deb_lsd and deb_decimal arithmetic operators ----------------------------

test_that("Arithmetic operators work with deb_lsd and deb_decimal", {
  # deb_lsd and deb_decimal
  expect_equal(lsd2 + decimal1, deb_lsd(7, 3, 5))
  expect_equal(lsd2 + decimal2, deb_lsd(7, 3, 5))
  expect_equal(lsd_bases + deb_decimal(2.5, bases = bases2),
               deb_lsd(c(3, 7), c(41, 31), c(9, 8), bases2))
  expect_equal(lsd2 - decimal1, deb_lsd(3, 9, 11))
  expect_equal(lsd2 / deb_decimal(2 + 2/3), 2)
  # deb_decimal and deb_lsd
  expect_equal(decimal1 + lsd2, deb_lsd(7, 3, 5))
  expect_equal(decimal1 - lsd2, deb_lsd(-3, -9, -11))
  expect_equal(deb_decimal(10 + 2/3) / lsd2, 2)
  # Errors
  expect_error(lsd_bases + decimal1,
               "`bases` attributes must be equal to combine <deb_lsd> or <deb_decimal> objects.")
  expect_error(decimal1 + lsd_bases,
               "`bases` attributes must be equal to combine <deb_lsd> or <deb_decimal> objects.")
  expect_error(lsd1 * decimal1)
  expect_error(decimal1 * lsd1)
})
