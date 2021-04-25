#' Plots the growth chart, optionally including matches
#'
#' @param target Tibble with person data, e.g. as produced by
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
#'   `lollypop` and `pops`)
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
#' library(bdsreader)
#' fn <- system.file("examples", "maria.json", package = "bdsreader")
#' fn <- system.file("examples", "Laura_S.json", package = "bdsreader")
#' target <- read_bds(fn)
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
                          dnr = c(
                            "0-2", "2-4", "4-18",
                            "smocc", "lollypop", "terneuzen", "pops"
                          ),
                          period = numeric(0),
                          nmatch = 0L,
                          user_model = 2L,
                          exact_sex = TRUE,
                          exact_ga = FALSE,
                          break_ties = TRUE,
                          show_realized = FALSE,
                          show_future = FALSE,
                          clip = TRUE) {
  dnr <- match.arg(dnr)

  # make the switch to the donordata solution per age group
  dnr <- switch(dnr,
    "0-2" = "smocc",
    "2-4" = "lollypop",
    "4-18" = "terneuzen",
    dnr
  )

  # load growthchart, return early if there is a problem
  g <- load_chart(chartcode)
  if (is.null(g) || !is.grob(g)) {
    return(placeholder(name = paste(chartcode, "ontbreekt")))
  }

  # set the palette
  parsed <- parse_chartcode(chartcode)
  old_pal <- palette(chartbox::palettes[parsed$population, ])

  # return empty chart if target does not have person attribute
  if (!hasName(attributes(target), "person")) {
    return(g)
  }

  # return empty chart if target has zero rows
  if (!nrow(target)) {
    return(g)
  }

  # calculate matches, if needed
  ynames <- get_ynames(chartcode)

  # return empty chart if there's nothing to plot
  if (!length(ynames)) {
    return(g)
  }

  if (!nmatch || !length(period)) {
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
    g = g, target = target,
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
