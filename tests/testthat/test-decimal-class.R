## Test deb_decimal class ##

x <- c(1.125, 2.5, 3.3)
y <- deb_decimal(x)

test_that("new_decimal works", {
  expect_equal(class(new_decimal()),
               c("deb_decimal", "vctrs_vctr"))
  expect_equal(length(new_decimal()), 0)
  expect_equal(length(new_decimal(x)), 3)
})

test_that("deb_decimal works", {
  # Basics
  expect_equal(class(deb_decimal(x)),
               c("deb_decimal", "vctrs_vctr"))
  expect_true(deb_is_decimal(y))
  expect_false(deb_is_decimal(3))
  # NA
  expect_true(is.na(deb_decimal(NA)))
  expect_equal(is.na(deb_decimal(c(1.25, 3, NA))), c(FALSE, FALSE, TRUE))
  # Data is correct
  expect_equal(as.numeric(y), x)
  expect_equal(deb_unit(y), "l")
  expect_equal(deb_unit(deb_decimal(x, unit = "s")), "s")
  expect_equal(deb_unit(deb_decimal(x, unit = "d")), "d")
  expect_error(deb_decimal(x, unit = "hello"))
  expect_equal(deb_bases(y), c(s = 20L, d = 12L))
  expect_equal(deb_bases(deb_decimal(x, bases = c(60, 16))), c(s = 60L, d = 16L))
})

test_that("deb_decimal prints", {
  expect_that(print(y), prints_text())
  expect_that(print(deb_decimal(NA)), prints_text())
  expect_equal(vctrs::vec_ptype_abbr(y), "l[20s:12d]")
  expect_equal(vctrs::vec_ptype_abbr(deb_decimal(1, unit = "s", bases = c(60, 16))),
               "s[60s:16d]")
  expect_equal(unit_word(deb_decimal(1, "s")), "solidus")
  expect_equal(unit_word(deb_decimal(1, "d")), "denarius")
})
