add_sds_curves <- function(g, data) {

  if (inherits(g, "plotly")) {
    g <- add_sds_traces_plotly(g, data)
  }
  else if (inherits(g, "ggplot")) {
    g <- add_sds_traces_ggplot2(g, data)
  }
  else {
    stop("g is not a plotly or ggplot object")
  }
  return(g)
}

add_sds_traces_plotly <- function(g, data) {

  # format extreme percentile labels with one decimal digit
  p <- pnorm(data$zd) * 100
  pf <- format(p, digit = 1, nsmall = 0, trim = TRUE)
  p1 <- format(p, digit = 1, nsmall = 1, trim = TRUE)
  idx <- !is.na(data$zd) & (data$zd < -2 | data$zd > 2)
  pf[idx] <- p1[idx]

  g <- g %>%
    add_trace(
      data = data, split = ~yname_label,
      type = "scatter",
      mode = "lines+markers",
      marker = list(size = 10),
      line = list(width = 2.5),
      hoverinfo = 'text',
      text = ~paste0('</br>', yname_label,
                     '</br>', y, ' ', yname_unit,
                     ' (', format(zd, nsmall = 2),
                     '; P', pf, ')')
    )
  return(g)
}

add_sds_traces_ggplot2 <- function(g, data) {
  # data <- drop_na(data, "zd")
  g <- g +
    geom_line(data = data, lwd = 0.8) +
    geom_point(data = data, size = 3)
  return(g)
}
