context("make_xname")

xnames <- c(
  "sex", "ga", "bw",
  "hgt_z_0", "hgt_z_1", "hgt_z_3",
  "wgt_z_0", "wgt_z_1", "wgt_z_2",
  "junk"
)
test_that("make_xname has length 1", {
  expect_equal(length(make_xname(
    yname = "hgt", xnames = xnames,
    user_model = 1,
    current_age = 1
  )), 1)
  expect_equal(length(make_xname(
    yname = "hgt", xnames = xnames,
    user_model = 1,
    current_age = 2
  )), 1)
  expect_equal(length(make_xname(
    yname = "hgt", xnames = xnames,
    user_model = 2,
    current_age = 3
  )), 4)
  expect_equal(length(make_xname(
    yname = "hgt", xnames = xnames,
    user_model = 4,
    current_age = 2
  )), 8)
  expect_equal(length(make_xname(
    yname = "hgt", xnames = xnames,
    user_model = NA,
    current_age = 2
  )), 0)
})
