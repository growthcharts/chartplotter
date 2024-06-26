#' Plots the growth chart, optionally including matches
#'
#' @param target A list with elements `"psn"` and `"xyz"`, e.g. as produced by
#'   `bdsreader::read_bds()`
#' @param chartcode  A string with chart code
#' @param curve_interpolation A logical indicating whether curve
#'   interpolation shoud be applied.
#' @param quiet Logical indicating whether chart code should be
#'   written to standard output. Default is `quiet = TRUE`.
#' @param con        A connection on which the donor data reside. The
#' default (`NULL`) reads from `donorloader` package.
#' @param dnr        A string with the name of the donor data
#'   (currently available are `smocc`, `terneuzen`,
#'   `lollypop`, `pops`, `0-2`, `2-4` and `4-18`). The default (`NULL`)
#'   sets the donor data according to `period[2]`.
#' @param period A vector of length 2 with left and right ages
#'   (decimal age). If `length(period) == 0L`, then no curve
#'   matching is done
#' @param nmatch     Integer. Number of matches needed. When
#'   `nmatch == 0L` no matches are sought.
#' @param user_model Model number (1-4), indicating type of model the
#'   user wants. See details.
#' @param exact_sex  A logical indicating whether sex should be
#'   matched exactly
#' @param exact_ga   A logical indicating whether gestational age
#'   should be matched exactly
#' @param break_ties A logical indicating whether ties should broken
#'   randomly. The default (`TRUE`) breaks ties randomly.
#' @param show_realized A logical indicating whether the realized
#'   growth of the target child should be drawn
#' @param show_future A logical indicating whether the predicted
#'   growth of the target child should be drawn
#' @param clip        A logical indicating whether clipping is needed
#' @seealso [chartcatalog::create_chartcode()]
#' @details
#' # The meaning of the `user_model` parameter is as follows:
#' \describe{
#' \item{1}{most recent measurement only}
#' \item{2}{sex + growth curve up to current}
#' \item{3}{2 + all complete covariates}
#' \item{4}{3 + growth curves up to current, other measures}
#' }
#' @return A `gTree` that can be rendered by `grid::grid.draw()`.
#' @examples
#' \dontrun{
#' library(grid)
#' fn <- system.file("extdata", "bds_v2.0", "smocc", "Laura_S.json", package = "jamesdemodata")
#' target <- bdsreader::read_bds(fn)
#' g <- process_chart(target,
#'   chartcode = "NJAA", show_realized = TRUE, show_future = TRUE,
#'   dnr = "0-2", period = c(0.5, 1.1667), nmatch = 10)
#' grid.draw(g)
#'
#' # using lollypop for matching
#' g <- process_chart(target,
#'   chartcode = "NJAA", show_realized = TRUE, show_future = TRUE,
#'   dnr = "2-4", period = c(0.5, 1.1667), nmatch = 10
#' )
#' grid.draw(g)
#' }
#' @export
process_chart <- function(target,
                          chartcode,
                          curve_interpolation = TRUE,
                          quiet = TRUE,
                          con = NULL,
                          dnr = NULL,
                          period = numeric(0),
                          nmatch = 0L,
                          user_model = 2L,
                          exact_sex = TRUE,
                          exact_ga = FALSE,
                          break_ties = TRUE,
                          show_realized = FALSE,
                          show_future = FALSE,
                          clip = TRUE) {
  # set the donor data dnr
  dnr <- set_dnr(dnr, period)

  # load growthchart, return early if there is a problem
  g <- load_chart(chartcode)
  if (is.null(g) || !is.grob(g)) {
    return(placeholder(name = paste(chartcode, "ontbreekt")))
  }

  # set the palette
  # set green/blue for D-score plots despite their WHOblue and WHOpink defaults
  palettes <- chartbox::palettes
  parsed <- parse_chartcode(chartcode)
  if (parsed$side == "dsc") {
    if (parsed$week == "40") palette(palettes["NL", ])
    else palette(palettes["PT", ])
  } else {
    palette(palettes[parsed$population,])
  }

  # return empty chart if target is not a list
  if (!is.list(target)) {
    return(g)
  }

  # return empty chart if time target has zero rows
  time <- timedata(target)
  if (!nrow(time)) {
    return(g)
  }

  # calculate matches, if needed
  ynames <- unname(chartcatalog::get_ynames(chartcode))

  # return empty chart if there's nothing to plot
  if (!length(ynames)) {
    return(g)
  }

  if (!nmatch || !length(period) || is.null(dnr)) {
    # no matches needed
    matches <- vector("list", length(ynames))
    names(matches) <- ynames
    matches <- lapply(matches, function(x) integer(0))
  } else {
    # find matches for measurements on chart
    matches <- find_matches(
      target = target,
      con = con,
      dnr = dnr,
      ynames = ynames,
      nmatch = nmatch,
      period = period,
      user_model = user_model,
      exact_sex = exact_sex,
      exact_ga = exact_ga,
      break_ties = break_ties
    )
  }

  if (!quiet) cat("chartcode: ", chartcode, "\n")

  # set data points
  set_curves(
    g = g, target = target, ynames = ynames,
    curve_interpolation = curve_interpolation,
    nmatch = nmatch,
    matches = matches,
    dnr = dnr,
    period = period,
    show_realized = show_realized,
    show_future = show_future,
    clip = clip
  )
}

set_dnr <- function(dnr, period) {
  if (!is.null(dnr)) {
    dnr <- switch(dnr,
                  "0-2" = "smocc",
                  "2-4" = "lollypop",
                  "4-18" = "terneuzen",
                  dnr
    )
  } else {
    # use period[2] to set dnr
    if (length(period) != 2L || !is.numeric(period)) {
      dnr <- NULL
    } else {
      to <- period[2L]
      dnr <- "terneuzen"
      if (to <= 2) dnr <- "smocc"
      if (2 < to && to <= 4) dnr <- "lollypop"
    }
  }
  return(dnr)
}
