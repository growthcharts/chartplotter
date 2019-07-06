create_lines_matches <- function(chartcode, yname,
                                 matches, dnr = "smocc",
                                 curve_interpolation = TRUE,
                                 con = NULL) {
  curve_interpolation <- FALSE

  # matched cases
  if (length(matches[[yname]]) == 0L) {
    lines.matches <- placeholder("lines.matches")
    symbols.matches <- placeholder("symbols.matches")
  } else {

    time <- load_time_data(con = con, dnr = dnr, ids = matches[[yname]])

    y <- time[, yname, drop = TRUE]
    mis <- is.na(y)
    y <- y[!mis]
    x <- time[!mis, "age", drop = TRUE]

    if (length(y) == 0) {
      lines.matches <- placeholder("lines.matches")
      symbols.matches <- placeholder("symbols.matches")
    } else {

      # apply curve interpolation
      id <- time[!mis ,"id", drop = TRUE]
      bend <- data.frame(
        id = id,
        x = x,
        y = y)
      ci <- apply_transforms(bend, id, chartcode, yname, curve_interpolation)

      id_li <- ci[["id"]]
      x_li <- ci[["x"]]
      y_li <- ci[["y"]]
      x_sy <- ci[["x"]][ci$obs]
      y_sy <- ci[["y"]][ci$obs]

      lines.matches <- polylineGrob(x = x_li, y = y_li,
                                    id = id_li,
                                    default.units = "native",
                                    gp = gpar(lwd = 1.5, lty = 1, col = "gray"),
                                    name = "lines.matches")
      symbols.matches <- pointsGrob(x = x_sy, y = y_sy, pch = 21,
                                    gp = gpar(col = "gray", fill = "gray",
                                              lwd = 1, cex = 0.5),
                                    name = "symbols.matches")
    }
  }
  gList(lines.matches = lines.matches,
        symbols.matches = symbols.matches)
}
