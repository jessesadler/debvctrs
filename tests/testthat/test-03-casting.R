## Test casting with deb_lsd and deb_decimal ##

library(vctrs)

lsd <- deb_lsd(c(NA, 2, 3), c(NA, 4, 5), c(NA, 6, 9))
lsd1 <- deb_lsd(1, 2, 3)
lsd_alt <- deb_lsd(1, 25, 4, bases = c(50, 16))
lsd3 <- deb_lsd(c(1, 2, 3), c(2, 4, 5), c(3, 6, 9))

dec <- deb_decimal(c(NA, 2.225, 3.2875))
dec_l <- deb_decimal(1.1125)
dec_bases <- deb_decimal(1.505, bases = c(50, 16))
dec_s <- deb_decimal(22.25, unit = "s")
dec_s2 <- deb_decimal(c(NA, 15, 25), unit = "s")
dec_s3 <- deb_decimal(c(22.25, 15, 25), unit = "s")
dec_d <- deb_decimal(267, unit = "d")
dec_d2 <- deb_decimal(c(NA, 180, 300), unit = "d")
dec_d3 <- deb_decimal(c(267, 180, 300), unit = "d")
dec3 <- deb_decimal(c(1.1125, 2.225, 3.2875))

bases_error <- paste0("`bases` attributes must be equal to combine ",
                      "<deb_lsd> or <deb_decimal> objects.")

# Test with vec_cast ------------------------------------------------------

test_that("vec_cast works for deb_lsd", {
  # deb_lsd to deb_lsd: checks for equal bases
  expect_equal(vec_cast(lsd, deb_lsd()), lsd)
  expect_equal(vec_cast(lsd_alt, deb_lsd(bases = c(50, 16))), lsd_alt)
  expect_error(vec_cast(lsd_alt, deb_lsd()),
               bases_error)

  # deb_lsd with double and integer
  expect_equal(vec_cast(lsd1, numeric()), 1.1125)
  expect_equal(vec_cast(1.1125, deb_lsd()), lsd1)
  expect_equal(vec_cast(1:3, deb_lsd()), deb_lsd(1:3, 0, 0))
  expect_error(vec_cast(lsd, integer()))
  # deb_lsd to character
  expect_equal(vec_cast(lsd, character()), c(NA, "2:4s:6d", "3:5s:9d"))
  # NA and incompatible cast from boilerplate
  expect_equal(vec_cast(NA, deb_lsd()), deb_lsd(NA, NA, NA))
  expect_error(vec_cast(factor("hello"), deb_lsd()))
})

test_that("vec_cast works for deb_decimal", {
  # deb_decimal to deb_decimal: checks for equal bases
  expect_equal(vec_cast(dec, deb_decimal()), dec)
  expect_equal(vec_cast(dec_bases, deb_decimal(bases = c(50, 16))),
               dec_bases)
  expect_equal(vec_cast(dec_s, deb_decimal(unit = "s")), dec_s)
  expect_equal(vec_cast(dec_d, deb_decimal(unit = "d")), dec_d)
  expect_error(vec_cast(dec_bases, deb_decimal()), bases_error)
  # Convert units: see also test-conversion
  expect_equal(vec_cast(dec_l, deb_decimal(unit = "s")), dec_s)
  expect_equal(vec_cast(dec_l, deb_decimal(unit = "d")), dec_d)
  expect_equal(vec_cast(dec_s, deb_decimal()), dec_l)
  expect_equal(vec_cast(dec_s, deb_decimal(unit = "d")), dec_d)
  expect_equal(vec_cast(dec_d, deb_decimal()), dec_l)
  expect_equal(vec_cast(dec_d, deb_decimal(unit = "s")), dec_s)

  # deb_decimal with double and integer
  expect_equal(vec_cast(dec, numeric()), c(NA, 2.225, 3.2875))
  expect_equal(vec_cast(1.1125, deb_decimal()), dec_l)
  expect_equal(vec_cast(1:3, deb_decimal()), deb_decimal(1:3))
  expect_error(vec_cast(dec, integer()))
  # deb_lsd to character
  expect_equal(vec_cast(dec, character()), c(NA, "2.225", "3.2875"))
  # NA and incompatible cast from boilerplate
  expect_equal(vec_cast(NA, deb_decimal()), deb_decimal(NA))
  expect_error(vec_cast(factor("hello"), deb_decimal()))
})

test_that("vec_cast works with both deb_lsd and deb_decimal", {
  # Successful
  expect_equal(vec_cast(dec, deb_lsd()), lsd)
  expect_equal(vec_cast(lsd, deb_decimal()), dec)
  # Units dealt with correctly
  expect_equal(vec_cast(dec_s, deb_lsd()), lsd1)
  # Alt bases and units work if provided to prototype
  expect_equal(vec_cast(dec_bases, deb_lsd(bases = c(50, 16))),
               lsd_alt)
  expect_equal(vec_cast(lsd_alt, deb_decimal(bases = c(50, 16))),
               dec_bases)
  expect_equal(vec_cast(dec_s, deb_decimal(unit = "s")), dec_s)

  # Errors when x has different bases or units than default if not changed
  expect_error(vec_cast(lsd_alt, deb_lsd()), bases_error)
  expect_error(vec_cast(dec_bases, deb_decimal()), bases_error)
  expect_error(vec_cast(dec_bases, deb_lsd()), bases_error)
  expect_error(vec_cast(lsd_alt, deb_decimal()), bases_error)
})


# Test casting methods ----------------------------------------------------

test_that("deb_as_lsd works", {
  expect_equal(deb_as_lsd(lsd_alt), lsd_alt)
  expect_equal(deb_as_lsd(dec), lsd)
  expect_equal(deb_as_lsd(dec_d), lsd1)
  expect_equal(deb_as_lsd(1.1125), lsd1)
  expect_equal(deb_as_lsd(1.505, bases = c(50, 16)), lsd_alt)
  expect_equal(deb_as_lsd(NA), deb_lsd(NA, NA, NA))
  expect_error(deb_as_lsd(factor("hello")))
})

test_that("deb_as_decimal works", {
  expect_equal(deb_as_decimal(dec_bases), dec_bases)
  expect_equal(deb_as_decimal(dec_s), dec_s)
  expect_equal(deb_as_decimal(lsd), dec)
  expect_equal(deb_as_decimal(lsd1, unit = "s"), dec_s)
  expect_equal(deb_as_decimal(lsd1, unit = "d"), dec_d)
  expect_equal(deb_as_decimal(1.1125), dec_l)
  expect_equal(deb_as_decimal(1.505, bases = c(50, 16)), dec_bases)
  expect_equal(deb_as_decimal(22.25, unit = "s"), dec_s)
  expect_equal(deb_as_decimal(NA), deb_decimal(NA))
  expect_error(deb_as_decimal(factor("hello")))
})


# Test assignment subsetting ----------------------------------------------

test_that("assignment subsetting works", {
  # deb_lsd
  lsd[[1]] <- lsd1
  expect_equal(lsd, lsd3)
  lsd[[1]] <- 1.1125
  expect_equal(lsd, lsd3)
  lsd[[1]] <- NA
  expect_equal(lsd, lsd)

  # deb_decimal
  dec[[1]] <- dec_l
  expect_equal(dec, dec3)
  dec[[1]] <- 1.1125
  expect_equal(dec, dec3)
  dec[[1]] <- NA
  expect_equal(dec, dec)

  # deb_decimal with different units
  dec[[1]] <- dec_s
  expect_equal(dec, dec3)
  dec[[1]] <- dec_d
  expect_equal(dec, dec3)
  dec_s2[[1]] <- dec_l
  expect_equal(dec_s2, dec_s3)
  dec_s2[[1]] <- dec_d
  expect_equal(dec_s2, dec_s3)

  dec_d2[[1]] <- dec_l
  expect_equal(dec_d2, dec_d3)
  dec_d2[[1]] <- dec_s
  expect_equal(dec_d2, dec_d3)

  # Successful deb_lsd and deb_decimal
  lsd[[1]] <- dec_l
  expect_equal(lsd, lsd3)
  lsd[[1]] <- dec_s
  expect_equal(lsd, lsd3)
  dec[[1]] <- lsd1
  expect_equal(dec, dec3)

  # Errors due to attribute mismatches
  expect_error(lsd[[1]] <- lsd_alt, bases_error)
  expect_error(lsd[[1]] <- dec_bases, bases_error)
  expect_error(dec[[1]] <- dec_bases, bases_error)
  expect_error(dec[[1]] <- lsd_alt, bases_error)
})
