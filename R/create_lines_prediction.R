create_lines_prediction <- function(chartcode, yname,
                                    matches, dnr,
                                    period,
                                    xyz,
                                    show_future,
                                    curve_interpolation,
                                    con = NULL) {
    # return early
    if (length(matches[[yname]]) == 0L | !show_future | length(period) == 0L) {
    lines_prediction <- placeholder("lines_prediction")
    symbols_prediction <- placeholder("symbols_prediction")
    return(gList(lines_prediction = lines_prediction,
                 symbols_prediction = symbols_prediction))
  }

  # create the prediction line
  child <- load_data(con = NULL, dnr = dnr, element = "child",
                     ids = matches[[yname]])

  # precalculate begin and end, we need visit_number() to "round" to nearest visit
  vv <- visit_number(period, dnr = dnr)
  x_start <- visit_age(vv[1L], dnr = dnr)
  x_end <- visit_age(vv[2L], dnr = dnr)
  y_start <- mean(pull(child[, paste(yname, x_start, sep = "_")]),
                  na.rm = TRUE)
  y_end <- mean(pull(child[, paste(yname, x_end, sep = "_")]),
                na.rm = TRUE)

  # find x_start, age at which lines_prediction begins
  xyz <- as.data.frame(xyz)
  x <- xyz$x
  before <- x <= period[1L]
  if (sum(before) > 0L)
    before <- x <= max(xyz$x[before])

  # prefer x_start as age at last observed data point
  if (any(before)) {
    p <- which.max(xyz$x[before])
    x_start <- xyz$x[p]
    y_start <- xyz$y[p]
  }
  else { # if not found, take period1
    x_start <- x_start
    y_start <- y_start
  }

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

  gList(lines_prediction = lines_prediction,
        symbols_prediction = symbols_prediction)
}
