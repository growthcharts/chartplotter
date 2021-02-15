plot_lines_target <- function(data, yname, period = numeric(0),
                              curve_interpolation = TRUE, show_realized = FALSE) {
  m <- filter(data, .data$id == -1 & .data$yname == !!yname & !.data$pred)
  if (!curve_interpolation) m <- filter(m, .data$obs)

  if (length(period) == 0L) {
    if (nrow(m) == 0L) {
      linesbefore <- placeholder("linesbefore")
      symbolsbefore <- placeholder("symbolsbefore")
    } else {
      xy <- na.omit(m[, c("x", "v")])
      linesbefore <- polylineGrob(
        x = xy$x,
        y = xy$v,
        id.lengths = nrow(xy),
        default.units = "native",
        gp = gpar(lwd = 2, lty = 1, col = "red"),
        name = "linesbefore"
      )
      symbolsbefore <- pointsGrob(
        x = m$x[m$obs],
        y = m$v[m$obs],
        pch = 21,
        gp = gpar(
          col = "red", fill = palette()[4],
          lwd = 2, cex = 0.6
        ),
        name = "symbolsbefore"
      )
    }
    linesafter <- placeholder("linesafter")
    symbolsafter <- placeholder("symbolsafter")
  } else {
    # from here, if period of length 2
    x <- m$x
    y <- m$v
    x_sy <- x[m$obs]
    y_sy <- y[m$obs]

    # split the active case into before and age current visit
    before <- x <= period[1L]
    if (sum(before) > 0L) {
      before <- x <= max(m[before, "obs"] * m[before, "x"])
    }
    after <- !before
    if (any(after) & sum(before) > 0L) after[sum(before)] <- TRUE
    if (!show_realized) after <- FALSE

    before_sy <- x_sy <= period[1L]
    after_sy <- !before_sy
    if (!show_realized) after_sy <- FALSE

    # draw lines and symbols, target case
    if (!any(before)) {
      linesbefore <- placeholder("linesbefore")
      symbolsbefore <- placeholder("symbolsbefore")
    } else {
      xy <- na.omit(m[before, c("x", "v")])
      linesbefore <- polylineGrob(
        x = xy$x,
        y = xy$v,
        id.lengths = nrow(xy),
        default.units = "native",
        gp = gpar(lwd = 2, lty = 1, col = "red"),
        name = "linesbefore"
      )
      symbolsbefore <- pointsGrob(
        x = x_sy[before_sy], y = y_sy[before_sy], pch = 21,
        gp = gpar(
          col = "red", fill = palette()[4],
          lwd = 2, cex = 0.6
        ),
        name = "symbolsbefore"
      )
    }

    if (!any(after)) {
      linesafter <- placeholder("linesafter")
      symbolsafter <- placeholder("symbolsafter")
    } else {
      linesafter <- polylineGrob(
        x = x[after], y = y[after],
        id.lengths = sum(after),
        default.units = "native",
        gp = gpar(lwd = 2, lty = 3, col = "red"),
        name = "linesafter"
      )
      symbolsafter <- pointsGrob(
        x = x_sy[after_sy], y = y_sy[after_sy], pch = 21,
        gp = gpar(
          col = "red", fill = palette()[4],
          lwd = 2, cex = 0.6
        ),
        name = "symbolsafter"
      )
    }
  }

  gList(
    linesbefore = linesbefore,
    linesafter = linesafter,
    symbolsbefore = symbolsbefore,
    symbolsafter = symbolsafter
  )
}
