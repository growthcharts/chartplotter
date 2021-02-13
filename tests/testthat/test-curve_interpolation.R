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
refcode <- "nl_1997_hgt_male_nl"

test_that("handles id with single observations", {
  expect_equal(curve_interpolation(data1, xname = "age", yname = "hgt",
                                   xout = seq(0, 1.3, 0.5), refcode = refcode)$hgt,
               c(52, 60, 68.077, 75.662, 78, 69, 62, 72.725, 78, 82))
})

test_that("handles id with all missing outcome measurements", {
  expect_equal(curve_interpolation(data2, xname = "age", yname = "hgt",
                                   xout = seq(0, 1.3, 0.5), ref = refcode)$hgt,
               c(52, 60, 68.077, 75.662, 78, 69, 70, NA, NA))
})

xyz <- data.frame(x = numeric(0), y = numeric(0))
test_that("handles input with zero rows", {
  expect_silent(curve_interpolation(xyz))
})


# first, a case where it works
# data3a <- data.frame(
#   id = c(2, 2),
#   wfh = c(4, 5),
#   hgt = c(58.1, 63.6),
#   stringsAsFactors = FALSE)
#
# test_that("handles wfh correctly", {
#   expect_equal(7L,
#                nrow(curve_interpolation(data3a, xname = "hgt",
#                                         yname = "wfh",
#                                         xout = seq(50, 120, by = 2),
#                                         refcode = "nl_1997_wfh_male_nl")))})

# problem case
# wfh preterm, two height observations below 50 cm
# data3 <- data.frame(
#   id = c(2, 2, 2, 2),
#   wgt = c(1.25, 2.10, 3, 4),
#   hgt = c(38, 43.6, 51, 54),
#   stringsAsFactors = FALSE)
# test_that("handles wfh with heights below 50cm correctly", {
#   expect_equal(7L,
#                nrow(curve_interpolation(data3, xname = "hgt",
#                                         yname = "wgt", zname = "wfh_z",
#                                         xout = ref3@table@table$x,
#                                         reference = ref3)))})

# curve interpolation for D-score of a pre-term
# rule = 2 produces odd results
# data("installed.cabinets", package = "jamestest")
# ind <- installed.cabinets$smocc[["Kevin S"]]
# xyz <- data.frame(ind@dsc)[, c("x", "y", "z")]
# data <- data.frame(id = 0, xyz)
# refcode <- "nl_2014_dsc_male_34"
# xout <- seq(0, 2, 0.1)
# z <- curve_interpolation(data, xout = xout, refcode = refcode, rule = 1)
