#' Draw gridlines for axes object
#'
#' Return a grob object that contains graphic object of four axes.
#' @param xy.axes The axes definition produced by `chartdesigner:::axesGrob()$xy.axes`
#' @param at A list of length 2 containing the locations where the grid lines
#' should be drawn. The first list component contains the values on the `x` axis
#' (for vertical grid lines). The second component the values on the `y` axis
#' (for horizontal grid lines).
#' @param lwd Scalar or list of length 2 containig line width for grid lines
#' @param lty Scaler or list of length 2 containing the `lty` lint type parameter
#' @param col Scalar or list of length 2 containing color if the grid lines
#' @param vp Viewport to draw to
#' @param name name of the `grob`
#' @param \dots Additional argument passed to `gpar()`
#' @return A `gTree` that contains the gridlines grobs, under child names
#' `vertical` and `horizontal`. A `NULL` component does not draw grid lines.
gridlinesGrob <- function(xy.axes,
                          at = list(NULL, NULL),
                          lwd = 1,
                          col = 2,
                          lty = 1,
                          vp = NULL,
                          name = "gridlines",
                          ...) {
  if (!is.list(lty)) lty <- list(lty, lty)
  if (!is.list(lwd)) lwd <- list(lwd, lwd)
  if (!is.list(col)) col <- list(col, col)

  a <- 1
  gridx <- make.xaxis.gridlines(at[[a]], xy.axes, vp = vp)
  gridx <- editGrob(gridx,
    gp = gpar(
      lty = lty[[a]],
      lwd = lwd[[a]],
      col = col[[a]]
    )
  )

  a <- 2
  gridy <- make.yaxis.gridlines(at[[a]], xy.axes, vp = vp)
  gridy <- editGrob(gridy,
    gp = gpar(
      lty = lty[[a]],
      lwd = lwd[[a]],
      col = col[[a]]
    )
  )

  gTree(childrenvp = vp, children = gList(gridx, gridy), name = name)
}

#' @rdname gridlinesGrob
grid.gridlines <- function(...) {
  grob <- gridlinesGrob(...)
  grid.draw(grob)
  invisible(grob)
}


make.xaxis.gridlines <- function(at, xy.axes, vp = NULL) {
  if (is.null(at)) {
    return(nullGrob())
  }

  bot <- xy.axes$bot
  top <- xy.axes$top

  # calculate grid lines beween bottom and top
  from <- approx(x = bot$x, y = bot$y, xout = at)$y
  to <- approx(x = top$x, y = top$y, xout = at)$y

  segmentsGrob(
    x0 = at, y0 = from,
    x1 = at, y1 = to,
    default.units = "native",
    name = "vertical",
    vp = vp
  )
}


make.yaxis.gridlines <- function(at, xy.axes, vp = NULL) {
  if (is.null(at)) {
    return(nullGrob())
  }

  lft <- xy.axes$lft
  rgt <- xy.axes$rgt

  # calculate grid lines beween left and right
  from <- approx(x = lft$y, y = lft$x, xout = at)$y
  to <- approx(x = rgt$y, y = rgt$x, xout = at)$y

  segmentsGrob(
    x0 = from, y0 = at,
    x1 = to, y1 = at,
    default.units = "native",
    name = "horizontal",
    vp = vp
  )
}

make.empty.gridlines <- function(a) {
  name <- ifelse(a == 1, "vertical", "horizontal")
  segmentsGrob(
    gp = gpar(col = "transparent"),
    name = name
  )
}
