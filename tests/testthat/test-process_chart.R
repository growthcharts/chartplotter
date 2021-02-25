context("process_chart")

library(brokenstick)
library(jamesyzy)

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
test_that("Kevin S predict hdc using lollypop", {
  # warns for mutate_ in curvematching::calculate_matches()
  expect_silent(process_chart(ind, chartcode = "PJAAN34", dnr = "lollypop", period = c(0.6, 1.1667),
                              nmatch = 10, show_future = TRUE, show_realized = TRUE))
})

# problematic json file not_a_vector.json identified by Allegro Sultum - Feb 2020
# library(clopus)
# fn <- system.file("extdata", "test", "not_a_vector.json", package = "jamestest")
# js <- jsonlite::toJSON(jsonlite::fromJSON(fn), auto_unbox = TRUE)
# ind <- minihealth::convert_bds_individual(js)
# saveRDS(ind, file = "inst/testdata/ind.rds")

ind <- readRDS(system.file("testdata", "ind.rds", package = "chartplotter"))
test_that("AS case Feb 2020 is silent", {
  # warns for mutate_ in curvematching::calculate_matches()
  expect_silent(process_chart(individual = ind, chartcode = "NMAH", dnr = "0-2",
                              period = numeric(0), nmatch = 0))
  })

ind <- installed.cabinets$smocc[["Laura S"]]
test_that("Head circumference plots on NMCO", {
  expect_silent(process_chart(ind, chartcode = "NMCO"))
})

ind <- installed.cabinets$terneuzen[["T 3254"]]
test_that("Height plots on NJCH", {
  expect_silent(process_chart(ind, chartcode = "NJCH"))
})

# Study conversion
#fn <- system.file("extdata", "terneuzen", "T_6021.json", package = "jamestest")
#js <- jsonlite::toJSON(jsonlite::fromJSON(fn), auto_unbox = TRUE)
#ind <- minihealth::convert_bds_individual(js)
g <- process_chart(ind, chartcode = "NMCH")
grid::grid.draw(g)
# ind <- installed.cabinets$terneuzen[["T 6021"]]
test_that("Height plots on NMCH", {
  expect_silent(process_chart(ind, chartcode = "NMCH"))
})


# Curve of one point not plotted - solved 25/2/21
# msg <- "{\"Referentie\":\"0680ab73-aed5-4cfd-be08-6afacd5d288d\",\"OrganisatieCode\":1234,\"Applicatie\":\"MLCAS 2.00.6993\",\"ClientGegevens\":{\"Elementen\":[{\"Bdsnummer\":19,\"Waarde\":\"2\",\"Omschrijving\":\"Geslacht\"},{\"Bdsnummer\":20,\"Waarde\":\"20210127\",\"Omschrijving\":\"Geboortedatum\"},{\"Bdsnummer\":82,\"Waarde\":\"256\",\"Omschrijving\":\"Zwangerschapsduur\"},{\"Bdsnummer\":91,\"Waarde\":\"2\",\"Omschrijving\":\"RokenTijdensDeZwangerschap\"},{\"Bdsnummer\":110,\"Waarde\":\"2670\",\"Omschrijving\":\"Geboortegewicht\"},{\"Bdsnummer\":238,\"Waarde\":\"1730\",\"Omschrijving\":\"LengteBiologischeMoeder\"},{\"Bdsnummer\":240,\"Waarde\":\"1780\",\"Omschrijving\":\"LengteBiologischeVader\"}],\"Groepen\":[{\"Elementen\":[{\"Bdsnummer\":62,\"Waarde\":\"02\",\"Omschrijving\":\"RelatieTotJeugdigeOuderVerzorger\"},{\"Bdsnummer\":63,\"Waarde\":\"19850114\",\"Omschrijving\":\"GeboortedatumOuderVerzorger\"},{\"Bdsnummer\":71,\"Waarde\":\"6030\",\"Omschrijving\":\"GeboortelandOuderVerzorger\"},{\"Bdsnummer\":66,\"Waarde\":\"09\",\"Omschrijving\":\"OpleidingOuderVerzorger\"}]},{\"Elementen\":[{\"Bdsnummer\":62,\"Waarde\":\"01\",\"Omschrijving\":\"RelatieTotJeugdigeOuderVerzorger\"},{\"Bdsnummer\":63,\"Waarde\":\"19800818\",\"Omschrijving\":\"GeboortedatumOuderVerzorger\"},{\"Bdsnummer\":71,\"Waarde\":\"6030\",\"Omschrijving\":\"GeboortelandOuderVerzorger\"},{\"Bdsnummer\":66,\"Waarde\":\"09\",\"Omschrijving\":\"OpleidingOuderVerzorger\"}]}]},\"Contactmomenten\":[{\"Tijdstip\":\"20210225\",\"Elementen\":[{\"Bdsnummer\":235,\"Waarde\":\"540\",\"Omschrijving\":\"Lengte\"},{\"Bdsnummer\":245,\"Waarde\":\"3410\",\"Omschrijving\":\"Gewicht\"},{\"Bdsnummer\":252,\"Waarde\":\"351\",\"Omschrijving\":\"Hoofdomtrek\"}]}]}"
#
# ind <- james::convert_bds_ind(txt = msg)
# g <- process_chart(ind, chartcode = "NJAA")

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

