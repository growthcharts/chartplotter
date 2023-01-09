context("process_chart")

library(brokenstick)
library(nlreferences)

test_that("returns empty chart if not a list", {
          expect_silent(process_chart(NULL, chartcode = "NJAA"))
})

target <- list(
  psn = NA_character_,
  xyz = data.frame()
)

test_that("returns empty chart if there is are no data", {
  expect_silent(process_chart(target, chartcode = "NJAA"))
})

fn <- system.file("extdata", "bds_v1.0", "smocc", "Laura_S.json", package = "jamesdemodata")
ind <- bdsreader::read_bds(fn)

g <- process_chart(ind, chartcode = "NJAA",
                  dnr = "smocc", period = c(0.5, 1.1667), nmatch = 10, break_ties = TRUE)

test_that("terneuzen donordata yields matches", {
  expect_silent(process_chart(ind, chartcode = "NJCH",
                   dnr = "terneuzen", period = c(2, 18),
                   nmatch = 10, break_ties = TRUE))
})

test_that("prediction line connects last observation to prediction", {
  # does not warn anymore for mutate_ in curvematching::calculate_matches()
  expect_silent(process_chart(ind,
    chartcode = "NJCH",
    dnr = "terneuzen", period = c(3, 10),
    nmatch = 25, break_ties = TRUE,
    show_realized = TRUE, show_future = TRUE
  ))
})

fn <- system.file("extdata", "bds_v1.0", "smocc", "Kevin_S.json", package = "jamesdemodata")
ind <- bdsreader::read_bds(fn)
test_that("Kevin S is drawn silently", {
  # warns for mutate_ in curvematching::calculate_matches()
  expect_silent(process_chart(ind,
    chartcode = "PJAAN34", dnr = "smocc", period = c(0.6, 1.1667),
    nmatch = 10, exact_ga = FALSE, break_ties = TRUE,
    show_future = TRUE, show_realized = TRUE, curve_interpolation = TRUE
  ))
})

test_that("Kevin S predict hdc using lollypop", {
  # warns for mutate_ in curvematching::calculate_matches()
  expect_silent(process_chart(ind,
    chartcode = "PJAAN34", dnr = "lollypop", period = c(0.6, 1.1667),
    nmatch = 10, show_future = TRUE, show_realized = TRUE
  ))
})

# problematic json file not_a_vector.json identified by Allegro Sultum - Feb 2020
fn <- system.file("extdata", "bds_v1.0", "test", "not_a_vector.json", package = "jamesdemodata")
ind <- bdsreader::read_bds(fn)
test_that("AS case Feb 2020 is silent", {
  expect_silent(process_chart(
    target = ind, chartcode = "NMAH", dnr = "0-2",
    period = numeric(0), nmatch = 0
  ))
})

fn <- system.file("extdata", "bds_v1.0", "smocc", "Laura_S.json", package = "jamesdemodata")
ind <- bdsreader::read_bds(fn)
test_that("Head circumference plots on NMCO", {
  expect_silent(process_chart(ind, chartcode = "NMCO"))
})

fn <- system.file("extdata", "bds_v1.0", "terneuzen", "T_3254.json", package = "jamesdemodata")
ind <- bdsreader::read_bds(fn)
test_that("Height plots on NJCH", {
  expect_silent(process_chart(ind, chartcode = "NJCH"))
})

# Do not allow D-score prediction beyond 24 months
fn <- system.file("extdata", "bds_v1.0", "smocc", "Laura_S.json", package = "jamesdemodata")
ind <- bdsreader::read_bds(fn)
test_that("D-score prediction does not go beyond 24 months", {
  expect_silent(g <- process_chart(ind,
    chartcode = "NMBD", period = c(1, 3),
    nmatch = 10, show_realized = TRUE, show_future = TRUE
  ))
})

# Test 5 - errors
jtf <- system.file("extdata", "bds_v1.0", "test", paste0("test", 1:24, ".json"), package = "jamesdemodata")
ind <- bdsreader::read_bds(jtf[5])

test_that("test5.json passes individual_to_donordata()", {
  expect_silent(process_chart(ind, chartcode = "NJBA", nmatch = 1, period = c(0, 1)))
  })


# g <- process_chart(ind, chartcode = "NMBD", period = c(1, 2),
#                    nmatch = 10, show_realized = TRUE, show_future = TRUE)
# grid::grid.draw(g)

# correct curve interpolation for WFH
# ind <- installed.cabinets$terneuzen[["T 163"]]
# g <- process_chart(ind, chartcode = "NMCR", period = c(10, 20),
#                    nmatch = 0, show_realized = TRUE, show_future = TRUE)
# grid::grid.draw(g)


# Study conformat
fn <- system.file("extdata", "bds_v1.0", "terneuzen", "T_6021.json", package = "jamesdemodata")
ind <- bdsreader::read_bds(fn)
test_that("Height plots on NMCH", {
  expect_silent(process_chart(ind, chartcode = "NMCH"))
})


# Curve of one point not plotted - solved 25/2/21
# msg <- "{\"Referentie\":\"0680ab73-aed5-4cfd-be08-6afacd5d288d\",\"OrganisatieCode\":1234,\"Applicatie\":\"MLCAS 2.00.6993\",\"ClientGegevens\":{\"Elementen\":[{\"Bdsnummer\":19,\"Waarde\":\"2\",\"Omschrijving\":\"Geslacht\"},{\"Bdsnummer\":20,\"Waarde\":\"20210127\",\"Omschrijving\":\"Geboortedatum\"},{\"Bdsnummer\":82,\"Waarde\":\"256\",\"Omschrijving\":\"Zwangerschapsduur\"},{\"Bdsnummer\":91,\"Waarde\":\"2\",\"Omschrijving\":\"RokenTijdensDeZwangerschap\"},{\"Bdsnummer\":110,\"Waarde\":\"2670\",\"Omschrijving\":\"Geboortegewicht\"},{\"Bdsnummer\":238,\"Waarde\":\"1730\",\"Omschrijving\":\"LengteBiologischeMoeder\"},{\"Bdsnummer\":240,\"Waarde\":\"1780\",\"Omschrijving\":\"LengteBiologischeVader\"}],\"Groepen\":[{\"Elementen\":[{\"Bdsnummer\":62,\"Waarde\":\"02\",\"Omschrijving\":\"RelatieTotJeugdigeOuderVerzorger\"},{\"Bdsnummer\":63,\"Waarde\":\"19850114\",\"Omschrijving\":\"GeboortedatumOuderVerzorger\"},{\"Bdsnummer\":71,\"Waarde\":\"6030\",\"Omschrijving\":\"GeboortelandOuderVerzorger\"},{\"Bdsnummer\":66,\"Waarde\":\"09\",\"Omschrijving\":\"OpleidingOuderVerzorger\"}]},{\"Elementen\":[{\"Bdsnummer\":62,\"Waarde\":\"01\",\"Omschrijving\":\"RelatieTotJeugdigeOuderVerzorger\"},{\"Bdsnummer\":63,\"Waarde\":\"19800818\",\"Omschrijving\":\"GeboortedatumOuderVerzorger\"},{\"Bdsnummer\":71,\"Waarde\":\"6030\",\"Omschrijving\":\"GeboortelandOuderVerzorger\"},{\"Bdsnummer\":66,\"Waarde\":\"09\",\"Omschrijving\":\"OpleidingOuderVerzorger\"}]}]},\"Contactmomenten\":[{\"Tijdstip\":\"20210225\",\"Elementen\":[{\"Bdsnummer\":235,\"Waarde\":\"540\",\"Omschrijving\":\"Lengte\"},{\"Bdsnummer\":245,\"Waarde\":\"3410\",\"Omschrijving\":\"Gewicht\"},{\"Bdsnummer\":252,\"Waarde\":\"351\",\"Omschrijving\":\"Hoofdomtrek\"}]}]}"
#
# ind <- james::convert_bds_ind(txt = msg)
# g <- process_chart(ind, chartcode = "NJAA")

# test with NULL individual
# ind <- NULL
# g <- process_chart(ind, chartcode = "NJAH")
# grid::grid.draw(g)

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

# Illustrate sawtooth problem with preterm reference selection - resolved 2023/01/09 SvB
# fn <- system.file("extdata", "bds_v2.0", "smocc", "Laura_S.json", package = "jamesdemodata")
# fn <- system.file("extdata", "bds_v2.0", "lollypop", "Anouk_P.json", package = "jamesdemodata")
# fn <- system.file("extdata", "bds_v2.0", "test", "test1.json", package = "jamesdemodata")
# # data2 <- read_json(fn, simplifyVector = TRUE)
# ind <- bdsreader::read_bds(fn)
# g <- process_chart(ind, chartcode = "NMAH")
# grid::grid.draw(g)
