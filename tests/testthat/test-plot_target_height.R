
fn <- system.file("extdata", "bds_v1.0", "terneuzen", "T_3254.json", package = "jamesdemodata")
ind <- bdsreader::read_bds(fn)

g <- process_chart(ind, chartcode = "NJCH")
grid::grid.draw(g)

test_that("Adds target height to NJCH", {
  expect_silent(process_chart(ind, chartcode = "NJCH"))
})

