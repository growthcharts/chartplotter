create_lines_prediction <- function(chartcode, yname,
                                    matches, dnr, period,
                                    show_future,
                                    curve_interpolation,
                                    con = NULL) {

  # prediction
  if (length(matches[[yname]]) == 0L | !show_future | length(period) == 0L) {
    lines_prediction <- placeholder("lines_prediction")
    symbols_prediction <- placeholder("symbols_prediction")
  } else {
    child <- load_data(con = con, dnr = dnr, element = "child",
                       ids = matches[[yname]])
    vv <- visit_number(period, dnr = dnr)
    x <- visit_age(vv[1L]:vv[2L], dnr = dnr)
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
      lines_prediction <- placeholder("lines_prediction")
      symbols_prediction <- placeholder("symbols_prediction")
    } else {
      lines_prediction <- polylineGrob(x = xy$x, y = xy$y,
                                       default.units = "native",
                                       gp = gpar(lwd = 2, lty = 2, col = "navy"),
                                       name = "lines_prediction")
      symbols_prediction <- pointsGrob(x = xy$x[xy$obs], y = xy$y[xy$obs], pch = 21,
                                       gp = gpar(col = "navy", fill = palette()[4],
                                                 lwd = 2, cex = 0.6),
                                       name = "symbols_prediction")
    }
  }
  gList(lines_prediction = lines_prediction,
        symbols_prediction = symbols_prediction)
}
