#' Plots the growth chart, optionally including matches
#'
#' @param individual An S4 object of class \code{individual}
#'   containing data of the individual
#' @param chartcode  A string with chart code
#' @param curve_interpolation A logical indicating whether curve
#'   interpolation shoud be applied.
#' @param quiet Logical indicating whether chart code should be
#'   written to standard output. Default is \code{quiet = TRUE}.
#' @param con        A connection on which the donor data reside. The
#' default (\code{NULL}) reads from \code{donordata} package.
#' @param dnr        A string with the name of the donor data
#'   (currently available are \code{smocc}, \code{terneuzen},
#'   \code{lollypop.preterm} or \code{lollypop.term})
#' @param period A vector of length 2 with left and right ages
#'   (decimal age). If \code{length(period) == 0L}, then no curve
#'   matching is done
#' @param nmatch     Integer. Number of matches needed. When
#'   \code{nmatch == 0L} no matches are sought.
#' @param user_model Model number (1-4), indicating type of model the
#'   user wants. See details.
#' @param exact_sex  A logical indicating whether sex should be
#'   matched exactly
#' @param exact_ga   A logical indicating whether gestational age
#'   should be matched exactly
#' @param break_ties A logical indicating whether ties should broken
#'   randomly. The default (\code{TRUE}) breaks ties randomly.
#' @param show_realized A logical indicating whether the realized
#'   growth of the target child should be drawn
#' @param show_future A logical indicating whether the predicted
#'   growth of the target child should be drawn
#' @param clip        A logical indicating whether clipping is needed
#' @seealso \code{\link[chartcatalog]{create_chartcode}}
#' @details
#' # The meaning of the \code{user_model} parameter is as follows:
#' \describe{
#' \item{1}{most recent measurement only}
#' \item{2}{sex + growth curve up to current}
#' \item{3}{2 + all complete covariates}
#' \item{4}{3 + growth curves up to current, other measures}
#' }
#' @export
process_chart <- function(individual,
                          chartcode,
                          curve_interpolation = TRUE,
                          quiet = TRUE,
                          con = NULL,
                          dnr = c("smocc", "terneuzen", "lollypop.preterm",
                                  "lollypop.term", "pops"),
                          period = numeric(0),
                          nmatch = 0L,
                          user_model = 2L,
                          exact_sex = TRUE,
                          exact_ga = FALSE,
                          break_ties = TRUE,
                          show_realized = FALSE,
                          show_future = FALSE,
                          clip = TRUE)
{
  dnr <- match.arg(dnr)

  # load growthchart, return early if there is a problem
  g <- load_chart(chartcode)
  if (is.null(g) || !is.grob(g)) return(placeholder(name = chartcode))
  ynames <- get_ynames(chartcode)

  # match if needed
  if (nmatch == 0L | length(period) == 0L) {
    # no matches needed
    matches <- vector("list", length(ynames))
    names(matches) <- ynames
    matches <- lapply(matches, function(x) integer(0))
  } else {
    # get donor data
    donor <- load_child_data(con = con, dnr = dnr)
    donor <- restore_factors(donor, f = c("sex", "etn", "edu"))

    # find matches for measurements on chart
    matches <- find_matches(individual = individual,
                            donor = donor,
                            dnr = dnr,
                            ynames = ynames,
                            nmatch = nmatch,
                            period = period,
                            user_model = user_model,
                            exact_sex = exact_sex,
                            exact_ga = exact_ga,
                            break_ties = break_ties)
  }

  # set the palette
  parsed <- parse_chartcode(chartcode)
  old_pal <- palette(chartbox::palettes[parsed$population, ])

  # set data points
  if (!quiet) cat("chartcode: ", chartcode, "\n")

  g <- set_curves(g = g, individual = individual,
                  curve_interpolation = curve_interpolation,
                  matches = matches,
                  dnr = dnr,
                  period = period,
                  show_realized = show_realized,
                  show_future = show_future,
                  clip = clip)

  grid.draw(g)

  palette(old_pal)
  invisible(g)
}
