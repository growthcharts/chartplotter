context("curve_interpolation")

# with single observation per group
data1 <- data.frame(
  id = c(1, 1, 1, 2, 3, 3, 3),
  age = c(0, 0.2, 1.2, 0.5, 0.1, 1, 1.3),
  hgt = c(52, 60, 78, 69, 62, 78, 82),
  stringsAsFactors = FALSE)

# with two or more observations per group
data2 <- data.frame(
  id = c(1, 1, 1, 2, 2, 3, 3, 3),
  age = c(0, 0.2, 1.2, 0.5, 0.7, 0.1, 1, 1.3),
  hgt = c(52, 60, 78, 69, 70, NA, NA, NA),
  stringsAsFactors = FALSE)

ref <- clopus::nl2009[["nl2009.mhgtNL"]]

test_that("handles id with single observations", {
  expect_equal(c(52, 60.0, 68.4, 75.9, 78.0, 69.0, 62.0, 72.5, 78.0, 82.0),
               curve_interpolation(data1, xname = "age", yname = "hgt", xout = seq(0, 1.3, 0.5),
                                   reference = ref)$hgt,
               tolerance = 0.001)
  })

test_that("handles id with all missing outcome measurements", {
  expect_equal(c(52, 60.0, 68.4, 75.9, 78.0, 69.0, 70.0, NA, NA, NA, NA),
               curve_interpolation(data2, xname = "age", yname = "hgt", xout = seq(0, 1.3, 0.5),
                                   reference = ref)$hgt,
               tolerance = 0.001)
})

# first, a case where it works
data3a <- data.frame(
  id = c(2, 2),
  wgt = c(1.25, 2.10),
  hgt = c(58.1, 63.6),
  stringsAsFactors = FALSE)
ref3 <- clopus::nl1997[["nl1997.fwfhNLA"]]

test_that("handles wfh correctly", {
  expect_equal(7L,
               nrow(curve_interpolation(data3a, xname = "hgt",
                                        yname = "wgt", zname = "wfh_z",
                                        xout = ref3@table@table$x,
                                        reference = ref3)))})

# problem case
# wfh preterm, two height observations below 50 cm
data3 <- data.frame(
  id = c(2, 2),
  wgt = c(1.25, 2.10),
  hgt = c(38, 43.6),
  stringsAsFactors = FALSE)
test_that("handles wfh with heights below 50cm correctly", {
  expect_equal(2L,
               nrow(curve_interpolation(data3, xname = "hgt",
                                        yname = "wgt", zname = "wfh_z",
                                        xout = ref3@table@table$x,
                                        reference = ref3)))})
