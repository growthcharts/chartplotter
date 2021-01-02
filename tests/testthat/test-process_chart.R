context("process_chart")

library(brokenstick)
library(clopus)

#data("installed.cabinets", package = "jamestest")
#ind <- installed.cabinets$smocc[["Laura S"]]

#g <- process_chart(ind, chartcode = "NJAA",
#                   dnr = "smocc", period = c(0.5, 1.1667), nmatch = 10, break_ties = TRUE)
#
# test_that("terneuzen donordata yields matches", {
#   expect_silent(process_chart(ind, chartcode = "NJCH",
#                    dnr = "terneuzen", period = c(2, 18),
#                    nmatch = 10, break_ties = TRUE))
# })

data("installed.cabinets", package = "jamestest")
ind <- installed.cabinets$smocc[["Laura S"]]
test_that("prediction line connects last observation to prediction", {
  # warns for mutate_ in curvematching::calculate_matches()
  expect_warning(process_chart(ind, chartcode = "NJCH",
                   dnr = "terneuzen", period = c(3, 10),
                   nmatch = 25, break_ties = TRUE,
                   show_realized = TRUE, show_future = TRUE))
})


ind <- installed.cabinets$smocc[["Kevin S"]]
test_that("Kevin S is drawn silently", {
  # warns for mutate_ in curvematching::calculate_matches()
  expect_silent(process_chart(ind, chartcode = "PJAAN34", dnr = "smocc", period = c(0.6, 1.1667),
                               nmatch = 10, exact_ga = FALSE, break_ties = TRUE,
                               show_future = TRUE, show_realized = TRUE, curve_interpolation = TRUE))
})

ind <- installed.cabinets$smocc[["Kevin S"]]
test_that("Kevin S predict hdc using lollypop.preterm", {
  # warns for mutate_ in curvematching::calculate_matches()
  expect_silent(process_chart(ind, chartcode = "PJAAN34", dnr = "lollypop.preterm", period = c(0.6, 1.1667),
                              nmatch = 10, show_future = TRUE, show_realized = TRUE))
})


# g <- process_chart(ind, chartcode = "NJBH",
#               dnr = "terneuzen", period = c(0.9, 2),
#               nmatch = 10, break_ties = TRUE, show_future = TRUE,
#               show_realized = TRUE)
#
# g <- process_chart(ind, chartcode = "NJAH",
#                 dnr = "terneuzen", period = c(0.5, round(14/12, 4)),
#                 nmatch = 10, break_ties = TRUE,
#                 curve_interpolation = TRUE, show_realized = TRUE,
#                 show_future = TRUE)

