#' Create and plot two visit lines
#'
#' @param g A \code{gTree} object, typically loaded by
#'   \code{chartbox::load_chart()}
#' @param yname     Character indicating the measure
#' @param period    A vector of length 2 with left and right ages, or \code{numeric(0)} (default) if there are
#'                  no ages
#' @return An object of class \code{grob} with the visit lines
plot_visit_lines <- function(g, yname, period = numeric(0)) {
  if (length(period) == 0L) return(placeholder("visit_lines"))

  # make xya, taken from chartdesigner::get_axes_design()
  ax <- g$children[[yname]]$children$axes
  xya <- matrix(
    c(ax$children$axis1$xy$x, ax$children$axis2$xy$x,
      ax$children$axis3$xy$x, ax$children$axis4$xy$x,
      ax$children$axis1$xy$y, ax$children$axis2$xy$y,
      ax$children$axis3$xy$y, ax$children$axis4$xy$y,
      rep(1, length(ax$children$axis1$xy$x)),
      rep(2, length(ax$children$axis2$xy$x)),
      rep(3, length(ax$children$axis3$xy$x)),
      rep(4, length(ax$children$axis4$xy$x))),
    ncol = 3L, byrow = FALSE, dimnames = list(NULL, c("x", "y", "a")))

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
