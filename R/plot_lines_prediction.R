plot_lines_prediction <- function(data,
                                  yname,
                                  show_future = FALSE,
                                  curve_interpolation = TRUE) {
  m <- filter(data, .data$id == -1 & .data$yname == !!yname & .data$pred)
  if (!curve_interpolation) m <- slice(m, c(1L, n()))

  # return early
  if (!nrow(m) || !show_future) {
    lines_prediction <- placeholder("lines_prediction")
    symbols_prediction <- placeholder("symbols_prediction")
    return(gList(
      lines_prediction = lines_prediction,
      symbols_prediction = symbols_prediction
    ))
  }

  lines_prediction <- polylineGrob(
    x = m$x,
    y = m$v,
    default.units = "native",
    gp = gpar(lwd = 2, lty = 2, col = "navy"),
    name = "lines_prediction"
  )
  symbols_prediction <- pointsGrob(
    x = m$x[nrow(m)],
    y = m$v[nrow(m)],
    pch = 21,
    gp = gpar(
      col = "navy", fill = palette()[4],
      lwd = 2, cex = 0.6
    ),
    name = "symbols_prediction"
  )

  gList(
    lines_prediction = lines_prediction,
    symbols_prediction = symbols_prediction
  )
}
