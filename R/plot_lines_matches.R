plot_lines_matches <- function(data, yname, curve_interpolation = TRUE) {

  m <- filter(data, .data$id != -1 & .data$yname == yname)
  if (!curve_interpolation) m <- filter(m, .data$obs)

  if (!nrow(m)) {
    return(list(
      matches_lines = placeholder("matches_lines"),
      matches_symbols = placeholder("matches_symbols")
    ))
  }

  matches_lines <- polylineGrob(
    x = m$x, y = m$v,
    id = m$id,
    default.units = "native",
    gp = gpar(lwd = 1.5, lty = 1, col = "gray"),
    name = "matches_lines"
  )
  matches_symbols <- pointsGrob(
    x = m$x[m$obs], y = m$v[m$obs],
    pch = 21,
    gp = gpar(
      col = "gray", fill = "gray",
      lwd = 1, cex = 0.5
    ),
    name = "matches_symbols"
  )
  list(
    matches_lines = matches_lines,
    matches_symbols = matches_symbols
  )
}
