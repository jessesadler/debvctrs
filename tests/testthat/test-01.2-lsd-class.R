## Test deb_lsd class ##

x <- deb_lsd(c(1, 2, 3), c(4, 5, 6), c(7, 8, 9))

test_that("new_lsd works", {
  expect_equal(length(new_lsd()), 0)
  expect_equal(class(new_lsd()), c("deb_lsd", "vctrs_rcrd", "vctrs_vctr"))
  expect_equal(length(new_lsd(c(1, 2, 3), c(4, 5, 6), c(7, 8, 9))), 3)
})

test_that("deb_lsd works", {
  # Prototype
  expect_equal(length(deb_lsd()), 0)
  expect_equal(class(deb_lsd()), c("deb_lsd", "vctrs_rcrd", "vctrs_vctr"))
  # Basics
  expect_equal(class(deb_lsd(1, 2, 3)),
               c("deb_lsd", "vctrs_rcrd", "vctrs_vctr"))
  expect_true(deb_is_lsd(x))
  expect_false(deb_is_lsd(3))
  # NA
  expect_true(is.na(deb_lsd(3, 4, NA)))
  expect_equal(is.na(deb_lsd(c(1, 2, 3), c(4, 5, 6), c(7, 8, NA))),
               c(FALSE, FALSE, TRUE))
  # Data is correct
  expect_equal(vctrs::field(x, "l"), c(1, 2, 3))
  expect_equal(vctrs::field(x, "s"), c(4, 5, 6))
  expect_equal(vctrs::field(x, "d"), c(7, 8, 9))
  expect_equal(deb_bases(x), c(20L, 12L))
  expect_equal(deb_bases(deb_lsd(1, 2, 3, bases = c(60, 16))), c(60L, 16L))
})

test_that("deb_lsd prints", {
  expect_that(print(x), prints_text())
  expect_that(print(deb_lsd(c(1, 2, 3), c(4, 5, 6), c(7, 8, NA))),
              prints_text())
  expect_equal(vctrs::vec_ptype_abbr(x), "lsd[20s:12d]")
  expect_equal(vctrs::vec_ptype_abbr(deb_lsd(1, 2, 3, bases = c(60, 16))),
               "lsd[60s:16d]")
})
