create_matches_lines <- function(chartcode, yname,
                                 matches, dnr = "smocc",
                                 curve_interpolation = TRUE,
                                 con = NULL) {

  # matched cases
  if (length(matches[[yname]]) == 0L) {
    return(list(
      matches_lines = placeholder("matches_lines"),
      matches_symbols = placeholder("matches_symbols")
    ))
  }

  time <- load_data(con = con, dnr = dnr, element = "time",
                    ids = matches[[yname]])

  y <- time[, yname, drop = TRUE]
  mis <- is.na(y)
  y <- y[!mis]
  x <- time[!mis, "age", drop = TRUE]

  if (length(y) == 0L) {
    return(list(
      matches_lines = placeholder("matches_lines"),
      matches_symbols = placeholder("matches_symbols")
    ))
  }

  # apply curve interpolation
  id <- time[!mis ,"id", drop = TRUE]
  bend <- data.frame(
    id = id,
    x = x,
    y = y)
  ci <- apply_transforms(bend, id, chartcode, yname,
                         curve_interpolation = curve_interpolation)

  id_li <- ci[["id"]]
  x_li <- ci[["x"]]
  y_li <- ci[["y"]]
  x_sy <- ci[["x"]][ci$obs]
  y_sy <- ci[["y"]][ci$obs]

  matches_lines <- polylineGrob(x = x_li, y = y_li,
                                id = id_li,
                                default.units = "native",
                                gp = gpar(lwd = 1.5, lty = 1, col = "gray"),
                                name = "matches_lines")
  matches_symbols <- pointsGrob(x = x_sy, y = y_sy, pch = 21,
                                gp = gpar(col = "gray", fill = "gray",
                                          lwd = 1, cex = 0.5),
                                name = "matches_symbols")
  return(
    list(
      matches_lines = matches_lines,
      matches_symbols = matches_symbols))
}
