#' Create two visit lines
#'
#' @param chartcode The chart code, usually constructed by \code{chartbox::create_chartcode()}
#' @param yname     Character indicating the measure
#' @param period    A vector of length 2 with left and right ages, or \code{numeric(0)} (default) if there are
#'                  no ages
#' @return An object of class \code{grob} with the visit lines
create_visit_lines <- function(chartcode, yname, period = numeric(0)) {
  if (length(period) == 0L) return(placeholder("visit_lines"))

  xya <- get_axes_design(chartcode, yname)$xya
  xy.axes <- list(
    bot = list(x = xya[xya[, "a"] == 1L, "x"],
               y = xya[xya[, "a"] == 1L, "y"]),
    lft = list(x = xya[xya[, "a"] == 2L, "x"],
               y = xya[xya[, "a"] == 2L, "y"]),
    top = list(x = xya[xya[, "a"] == 3L, "x"],
               y = xya[xya[, "a"] == 3L, "y"]),
    rgt = list(x = xya[xya[, "a"] == 4L, "x"],
               y = xya[xya[, "a"] == 4L, "y"]))

  gridlinesGrob(xy.axes = xy.axes,
                at = list(period, NULL),
                lwd = 2,
                col = list(2L, NULL),
                lty = 2L,
                lend = "butt",
                name = "visit_lines")
}

# make_xy_axeslist <- function(chartcode, yname) {
#   parsed <- parse_chartcode(chartcode)
#
#   week <- as.numeric(parsed$week)
#   if (is.na(week)) week <- 40
#
#   # determine arrangement parameter value
#   if (week > 36) arrangement <- 40
#   if (week < 25) arrangement <- 25
#   if (week >= 25 & week <= 36) arrangement <- ifelse(week %% 2 == 1, week, week - 1)
#   arrangement <- as.character(arrangement)
#
#   form <- get.form.design(design = parsed$design,
#                           arrangement = arrangement,
#                           yname = yname)
#   do.call(axesGrob, list(form = form))
# }

