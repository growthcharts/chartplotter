create_lines_prediction <- function(chartcode, yname,
                                    matches, dnr, period,
                                    xyz,
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

    # find x_start, age at which lines_prediction begins
    x <- xyz[["x"]]
    before <- x <= period[1L]
    if (sum(before) > 0L)
      before <- x <= max(xyz[before, "obs"] * xyz[before, "x"])

    # prefer x_start as age at last observed data point
    if (any(before)) {
      p <- which.max(unlist(xyz[before, "x"]))
      x_start <- xyz[p, "x"]
      y_start <- xyz[p, "y"]
    }
    else { # if not found, take period1
      x_start <- visit_age(vv[1L], dnr = dnr)
      y_start <- mean(child[, paste(yname, x_start, sep = "_")])
    }

    # find x_end, age at which lines_prediction ends
    x_end <- visit_age(vv[2L], dnr = dnr)
    # find y_end: local prediction equal to the mean of
    # broken stick estimates (in cm) of the matches
    y_end <- mean(child[, paste(yname, x_end, sep = "_")])


    xy <- data.frame(id = 0L,
                     x = c(x_start, x_end),
                     y = c(y_start, y_end))
    xy <- apply_transforms(xy, chartcode = chartcode, yname = yname,
                           curve_interpolation = curve_interpolation)
    # remove symbol from start---
    # FIXME
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
