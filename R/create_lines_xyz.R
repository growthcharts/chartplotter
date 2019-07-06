create_lines_xyz <- function(xyz, period = numeric(0),
                             show_realized = FALSE) {

  if (length(period) == 0L) {
    if (nrow(xyz) == 0L) {
      linesbefore <- placeholder("linesbefore")
      symbolsbefore <- placeholder("symbolsbefore")
    } else {
      xy <- na.omit(xyz[, c("x", "y")])
      linesbefore <- polylineGrob(x = xy$x,
                                  y = xy$y,
                                  id.lengths = nrow(xy),
                                  default.units = "native",
                                  gp = gpar(lwd = 2, lty = 1, col = "red"),
                                  name = "linesbefore")
      symbolsbefore <- pointsGrob(x = xyz$x[xyz$obs],
                                  y = xyz$y[xyz$obs],
                                  pch = 21,
                                  gp = gpar(col = "red", fill = palette()[4],
                                            lwd = 2, cex = 0.6),
                                  name = "symbolsbefore")
    }
    linesafter <- placeholder("linesafter")
    symbolsafter <- placeholder("symbolsafter")
  }
  else {
    browser()
    # from here, if period of length 2
    x <- xyz[["x"]]
    y <- xyz[["y"]]
    x_sy <- x[xyz$obs]
    y_sy <- y[xyz$obs]

    # split the active case into before and age current visit
    before <- x <= period[1]
    if (sum(before) > 0L)
      before <- x <= max(xyz[before, "obs"] * xyz[before, "x"])
    after  <- !before
    after[sum(before)] <- TRUE
    if (!show_realized) after <- FALSE

    before_sy <- x_sy <= period[1]
    after_sy  <- !before_sy
    if (!show_realized) after_sy <- FALSE

    # draw lines and symbols, target case
    if (!any(before)) {
      linesbefore <- placeholder("linesbefore")
      symbolsbefore <- placeholder("symbolsbefore")
    } else {
      xy <- na.omit(xyz[before, c("x", "y")])
      linesbefore <- polylineGrob(x = xy$x,
                                  y = xy$y,
                                  id.lengths = nrow(xy),
                                  default.units = "native",
                                  gp = gpar(lwd = 2, lty = 1, col = "red"),
                                  name = "linesbefore")
      symbolsbefore <- pointsGrob(x = x_sy[before_sy], y = y_sy[before_sy], pch = 21,
                                  gp = gpar(col = "red", fill = palette()[4],
                                            lwd = 2, cex = 0.6),
                                  name = "symbolsbefore")
    }

    if (!any(after)) {
      linesafter <- placeholder("linesafter")
      symbolsafter <- placeholder("symbolsafter")
    } else {
      linesafter <- polylineGrob(x = x[after], y = y[after],
                                 id.lengths = sum(after),
                                 default.units = "native",
                                 gp = gpar(lwd = 2, lty = 3, col = "red"),
                                 name = "linesafter")
      symbolsafter <- pointsGrob(x = x_sy[after_sy], y = y_sy[after_sy], pch = 21,
                                 gp = gpar(col = "red", fill = palette()[4],
                                           lwd = 2, cex = 0.6),
                                 name = "symbolsafter")
    }
  }

  gList(linesbefore = linesbefore,
        linesafter = linesafter,
        symbolsbefore = symbolsbefore,
        symbolsafter = symbolsafter)
}
