create_lines_prediction <- function(chartcode, yname,
                                    matches, dnr, period,
                                    show_future,
                                    curve_interpolation,
                                    con = NULL) {

  # prediction
  if (length(matches) == 0L | !show_future | length(period) == 0L) {
    lines.prediction <- placeholder("lines.prediction")
    symbols.prediction <- placeholder("symbols.prediction")
  } else {
    child <- load_child_data(con = con, dnr = dnr, ids = matches[[yname]])
    vv <- visit.number(period)
    x <- visit.age(vv[1L]:vv[2L], dnr = dnr)
    vars <- paste(yname, x, sep = "_")
    idx <- vars %in% names(child)
    x <- x[idx]
    vars <- vars[idx]

    # calculate the local prediction as the mean
    # broken stick estimates (in cm) of the matches
    y <- NULL
    if (length(vars) >= 1) y <- colMeans(child[, vars, drop = FALSE])
    xy <- data.frame(id = 0L, x = x, y = y)
    xy <- apply_transforms(xy, chartcode = chartcode, yname = yname,
                           curve_interpolation = curve_interpolation)
    if (nrow(xy) == 0L) {
      lines.prediction <- placeholder("lines.prediction")
      symbols.prediction <- placeholder("symbols.prediction")
    } else {
      lines.prediction <- polylineGrob(x = xy$x, y = xy$y,
                                       default.units = "native",
                                       gp = gpar(lwd = 2, lty = 2, col = "navy"),
                                       name = "lines.prediction")
      symbols.prediction <- pointsGrob(x = xy$x[xy$obs], y = xy$y[xy$obs], pch = 21,
                                       gp = gpar(col = "navy", fill = palette()[4],
                                                 lwd = 2, cex = 0.6),
                                       name = "symbols.prediction")
    }
  }
  gList(lines.prediction = lines.prediction,
        symbols.prediction = symbols.prediction)
}
