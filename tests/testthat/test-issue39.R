context("james-issue39 - chartplotter issue 10")

# Situation where birth length is missing, and we cannot interpolate WFH curves
fn1 <- testthat::test_path("issue39-1.json")
tgt <- bdsreader::read_bds(fn1)
chartcode <- chartcatalog::create_chartcode(agegrp = "0-4y")
test_that("Issue 39 no longer appears", {
  expect_silent(
    f <- chartplotter::process_chart(
      target = tgt,
      chartcode = chartcode
    )
  )
})

# This is the original code that evades the error by adding a measurement
# fn2 <- testthat::test_path("issue39-2.json")
# tgt <- bdsreader::read_bds(fn2)
# chartcode <- chartcatalog::create_chartcode(agegrp = "0-4y")
# f <- chartplotter::process_chart(target = tgt, chartcode = chartcode)
# grid::grid.draw(f)
