create_lines_prediction <- function(chartcode, yname,
                                    matches, dnr,
                                    period,
                                    xyz,
                                    show_future,
                                    curve_interpolation,
                                    covariates = NULL,
                                    con = NULL) {
  # return early
  if (length(matches[[yname]]) == 0L | !show_future | length(period) == 0L) {
    lines_prediction <- placeholder("lines_prediction")
    symbols_prediction <- placeholder("symbols_prediction")
    return(gList(lines_prediction = lines_prediction,
                 symbols_prediction = symbols_prediction))
  }

  # create the prediction line

  # SvB: 20/12
  # NOTE: This is double loading. Previously done in create_matches_lines()
  # child <- load_data(con = NULL, dnr = dnr, element = "child",
  #                   ids = matches[[yname]])

  # SvB 20/12
  # CHANGES NEEDED:
  # 1) For target case, calculate Z-score (z_start) at last observed data point before visitline 1 (x_start)
  # 2a) Calculate Z-scores (z2) at visitline 2 (x_end) for all matches by predict.brokenstick
  # 2b) OR, for all matches, find y_end in donor$child, and convert to analysis metric z2 by apply_transforms() - likely to be faster (see find_matches())
  # 3) Average z2 over matches (z_end)
  # 4) Convert (x_start, z_start) and (x_end, z_end) to Y-scale in display metric by apply_transforms(covariates)

  # DIRECT ALTERNATIVE USING ONLY TARGET
  # 1) For target case, calculate Z-score (z_start) at last observed data point before visitline 1 (x_start)
  # 2) For target case, calculate Z-score (z_end) at visitline 2 (x_end) by predict.brokenstick
  # 3) Convert (x_start, z_start) and (x_end, z_end) to display metric by apply_transforms(covariates)

  # get the brokenstick model
  bsm <- load_data(dnr = paste0(dnr, "_bs"))[[yname]]

  # prepare for prediction
  v <- paste0(yname, ".z")
  df <- xyz %>%
    select(-.data$y) %>%
    dplyr::rename(age = .data$x,
                  !! v := .data$z) %>%
    filter(.data$age <= period[1L]) %>%
    mutate(id = 0L) %>%
    select(c("age", v, "id"))

  # predict with brokenstick model (in Z scale)
  x <- c(max(df$age, 0), period[2L])
  z <- predict(bsm, df, x = x, shape = "vector")

  # transform to display scale
  xz <- data.frame(x = x, z = z)
  xy <- apply_transforms(xz,
                         chartcode = chartcode, yname = yname,
                         curve_interpolation = curve_interpolation,
                         covariates = covariates)

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
