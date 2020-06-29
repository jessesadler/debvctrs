## Test mathematical functions with deb_lsd and deb_decimal ##

lsd1 <- deb_lsd(1, 16, 9)
lsd2 <- deb_lsd(5, 6, 8)
lsd3 <- c(lsd1, lsd2, NA, deb_lsd(c(3, 7), c(12, 15), c(1, 11)))
lsd4 <- c(lsd1, lsd2, deb_lsd(c(3, 7), c(12, 15), c(1, 11)))
bases2 <- c(50, 16)
lsd_bases <- deb_lsd(c(1, 5), c(16, 6), c(9, 8), bases = bases2)
lsd_round <- deb_lsd(5, 19, 11.8755)
neg_round <- deb_lsd(-5, -19, -11.8755)
multi_decimal <- deb_lsd(2, 3.3, 2.2)
round1 <- deb_lsd(6, 0, 0)
round2 <- deb_lsd(-6, 0, 0)

dec_l <- deb_decimal(1.8375)
dec_s <- deb_decimal(36.75, unit = "s")
dec_d <- deb_decimal(552, unit = "d")
dec2 <- deb_decimal(c(1.8375, NA, 5.225, 3.2875, 1.1125))
dec3 <- deb_decimal(c(1.8375, 5.225, 3.2875, 1.1125))

bases_error <- paste0("`bases` attributes must be equal to combine ",
                      "<deb_lsd> or <deb_decimal> vectors.")

# vec_math ----------------------------------------------------------------
test_that("vec_math has error message for unimplemented functions", {
  expect_error(prod(lsd4), "not implemented.$")
  expect_error(sin(lsd4), "not implemented.$")
})


# Math group --------------------------------------------------------------
test_that("sum and mean with deb_lsd work", {
  expect_equal(sum(lsd3), deb_lsd(NA, NA, NA))
  expect_equal(sum(lsd3, na.rm = TRUE), deb_lsd(18, 11, 5))
  expect_equal(sum(deb_lsd(c(1, 1), c(2, 1), c(3, NA)), na.rm = TRUE),
               deb_lsd(1, 2, 3))
  expect_equal(sum(lsd1, lsd2), deb_lsd(7, 3, 5))
  expect_equal(sum(lsd_bases), deb_lsd(6, 23, 1, bases = bases2))
  expect_equal(mean(c(lsd1, lsd2)), deb_lsd(3, 11, 8.5))
  expect_equal(mean(lsd3, na.rm = TRUE), deb_lsd(4, 12, 10.25))
  expect_equal(mean(deb_lsd(c(1, 5), c(42, 30), c(13, 15), bases = bases2)),
               deb_lsd(3, 36, 14, bases2))
  # Mean only takes first object
  expect_equal(mean(lsd4, lsd3), deb_lsd(4, 12, 10.25))
  # Error with different bases
  expect_error(sum(lsd3, lsd_bases), bases_error)
})

test_that("sum and mean work with deb_decimal", {
  expect_equal(sum(dec_l, deb_decimal(1.5)), deb_decimal(3.3375))
  expect_equal(sum(dec_s, deb_decimal(20, "s")), deb_decimal(56.75, "s"))
  expect_equal(sum(dec_d, deb_decimal(48, "d")), deb_decimal(600, "d"))
  expect_equal(sum(dec2, na.rm = TRUE),
               deb_decimal(sum(as.numeric(dec2), na.rm = TRUE)))
  # Different units work
  expect_equal(sum(dec_l, dec_s), deb_decimal(3.675))
  expect_equal(sum(dec_s, dec_d, dec_l), deb_decimal(5.975))
  expect_equal(sum(dec_d, dec_s), deb_decimal(82.75, unit = "s"))
  # Errors
  expect_error(sum(dec_l, deb_decimal(1.5, bases = bases2)), bases_error)
  expect_equal(mean(dec2, na.rm = TRUE), deb_decimal(2.865625))
})

test_that("sum works with deb-style vectors and numeric", {
  expect_equal(sum(lsd1, dec_l), deb_lsd(3, 13, 6))
  expect_equal(sum(lsd1, dec_l), sum(lsd1, dec_s))
  expect_equal(sum(lsd3, 1.8375, 3, na.rm = TRUE), deb_lsd(23, 8, 2))
  expect_equal(sum(dec2, 3.5, na.rm = TRUE),
               deb_decimal(sum(1.8375, 5.225, 3.2875, 1.1125, 3.5)))
})

test_that("cumulative functions work", {
  # cumsum
  expect_equal(cumsum(deb_lsd(rep(1, 5), rep(1, 5), rep(1, 5))),
               deb_lsd(1:5, 1:5, 1:5))
  expect_equal(cumsum(lsd4),
               c(lsd1, sum(lsd4[1:2]), sum(lsd4[1:3]), sum(lsd4[1:4])))
  expect_equal(as.numeric(cumsum(dec3)), cumsum(as.numeric(dec3)))
  # cummin
  expect_equal(cummin(lsd4), deb_lsd(rep(1, 4), rep(16, 4), rep(9, 4)))
  expect_equal(cummin(dec3), c(rep(dec3[[1]], 3), dec3[[4]]))
  # cummax
  expect_equal(cummax(lsd4), c(lsd1, lsd2, lsd2, lsd4[[4]]))
  expect_equal(cummax(dec3), c(dec_l, rep(dec3[[2]], 3)))
})

# Round family with deb_lsd -----------------------------------------------

test_that("round family works with deb_lsd", {
  # round
  expect_equal(round(lsd_round), round1)
  expect_equal(round(neg_round), round2)
  expect_equal(round(deb_lsd(5, 49, 15.6, bases2)), deb_lsd(6, 0, 0, bases2))
  expect_equal(round(lsd_round, 3), deb_lsd(5, 19, 11.876))
  expect_equal(round(multi_decimal), round(deb_normalize(multi_decimal)))
  # signif
  expect_equal(signif(lsd_round, 3), deb_lsd(5, 19, 11.9))
  expect_equal(signif(lsd_round, 2), round1)
  # ceiling
  expect_equal(ceiling(lsd_round), round1)
  expect_equal(ceiling(neg_round), deb_lsd(-5, -19, -11))
  expect_equal(ceiling(multi_decimal), ceiling(deb_normalize(multi_decimal)))
  # floor
  expect_equal(floor(lsd_round), deb_lsd(5, 19, 11))
  expect_equal(floor(neg_round), round2)
  expect_equal(floor(multi_decimal), floor(deb_normalize(multi_decimal)))
  # trunc
  expect_equal(trunc(lsd_round), deb_lsd(5, 19, 11))
  expect_equal(trunc(neg_round), deb_lsd(-5, -19, -11))
  expect_equal(trunc(multi_decimal), trunc(deb_normalize(multi_decimal)))
})

test_that("abs works with deb_lsd vectors", {
  expect_equal(abs(lsd_round), lsd_round)
  expect_equal(abs(neg_round), lsd_round)
})
