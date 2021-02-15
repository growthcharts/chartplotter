calc_predictions <- function(data, chartcode, ynames, dnr, period) {

  predlist <- vector("list", length(ynames))
  names(predlist) <- ynames
  vars <- names(data)

  # prepare data and new_data
  for (yname in ynames) {
    zname <- paste0(yname, "_z")
    df <- data %>%
      filter(.data$yname == !! yname & .data$id == -1 & .data$obs) %>%
      filter(.data$x <= period[1L]) %>%
      arrange(.data$x) %>%
      mutate(age = .data$x,
             !! zname := .data$z)

    # nothing to do
    if (!nrow(df)) {
      return(df %>% select(all_of(!! vars)))
    }

    # grid of data points to append. First = last observation before period[1],
    # last = period[2], in-between points for curve interpolation
    xout <- set_xout(chartcode, yname)
    lo <- min(max(df$x, na.rm = TRUE), period[1L], na.rm = TRUE)
    hi <- period[2L]
    x <- c(lo, xout[xout > lo & xout < hi], hi)
    n <- length(x)
    add <- dplyr::slice_tail(df, n = 1L) %>%
      uncount(!! n) %>%
      mutate(obs = rep(FALSE, !! n),
             pred = rep(TRUE, !! n),
             x = !! x,
             y = rep(NA_real_, !! n),
             z = rep(NA_real_, !! n),
             age = !! x,
             !! zname := rep(NA_real_, !! n))
    df2 <- bind_rows(df, add)

    # predict points with brokenstick model (in Z scale)
    bsm <- load_data(dnr = paste0(dnr, "_bs"))[[yname]]
    z <- tail(predict(bsm, new_data = df2, shape = "vector"), n = n)

    # Design decision. We have three possible ways to plot the predicted curve
    # 1) From start to end, plot predictions (there's a jump at start)
    # 2) Start at observed data, all others are predictions (connect the two curves)
    # 3) Start at observed data, end at prediction, interpolate in between (smooth transition)
    # Opt 1 and 2 might be better predictors at intermediate points, but less visually pleasing
    # We choose here option 3

    # if it exists, overwrite bs estimate by last observed outcome
    # to set a natural start for the prediction curve
    tmp <- df[nrow(df), zname]
    if (length(tmp) && !is.na(tmp)) z[1L] <- as.numeric(tmp)
    add$z[1L] <- z[1L]
    add$z[n] <- z[n]
    #add$z <- approx(y = z[c(1L, n)], x = x[c(1L, n)], xout = x)$y
    add$obs[1L] <- TRUE

    predlist[[yname]] <- add %>%
      select(all_of(!! vars))
  }
  bind_rows(predlist)
}
