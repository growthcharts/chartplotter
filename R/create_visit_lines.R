#' Create two visit lines
#'
#' @param chartcode The chart code, usually constructed by \code{chartbox::create_chartcode()}
#' @param yname     Character indicating the measure
#' @param period    A vector of length 2 with left and right ages, or \code{numeric(0)} (default) if there are
#'                  no ages
#' @return An object of class \code{grob} with the visit lines
create_visit_lines <- function(chartcode, yname, period = numeric(0)) {
  if (length(period) == 0L) return(placeholder("visit_lines"))
  axeslist <- make_xy_axeslist(chartcode, yname)
  tx <- get_tx(chartcode, yname)
  gridlinesGrob(xy.axes = axeslist$xy.axes,
                at = list(tx(period), NULL),
                lwd = 2,
                col = list(2L, NULL),
                lty = 2L,
                lend = "butt",
                name = "visit_lines")
}

make_xy_axeslist <- function(chartcode, yname) {
  parsed <- parse_chartcode(chartcode)

  week <- as.numeric(parsed$week)
  if (is.na(week)) week <- 40

  # determine arrangement parameter value
  if (week > 36) arrangement <- 40
  if (week < 25) arrangement <- 25
  if (week >= 25 & week <= 36) arrangement <- ifelse(week %% 2 == 1, week, week - 1)
  arrangement <- as.character(arrangement)

  form <- get.form.design(design = parsed$design,
                          arrangement = arrangement,
                          yname = yname)
  do.call(axesGrob, list(form = form))
}

