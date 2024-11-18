#' Plot target height
#'
#' Calculate and plot target height and target height range for a child on
#' the growth charts with designs C and D.
#'
#' @param child A data frame with columns
#' @param yname Variable code for vertical axis
#' @param parsed A list with parsed arguments
#' @param options A list with options (`show` and `alpha`)
#' @return A list of grobs
plot_target_height <- function(child, yname, parsed, options) {
  if (!options$show ||
      !is.data.frame(child) ||
      !length(child) ||
      !parsed$design %in% c("C", "D")) {
    return(gList(
      target_height_range = placeholder("target_height_range"),
      target_height_estimate = placeholder("target_height_estimate"))
    )
  }

  pop <- parsed$population
  pop <- ifelse(pop %in% c("PT", "DS"), "NL", pop)

  th <- growthscreener::calculate_th(
    hgtf = child$hgtf,
    hgtm = child$hgtm,
    sex = child$sex,
    etn = pop,
    support_missing_hgtf = TRUE)

  if (is.na(th[1L])) {
    return(gList(
      target_height_range = placeholder("target_height_range"),
      target_height_estimate = placeholder("target_height_estimate"))
    )
  }

  z <- qnorm(1 - (1 - options$alpha) / 2)
  target_height_range <- linesGrob(
    x = c(20.5, 20.5),
    y = th[1L] + c(-z, z) * th[2L],
    default.units = "native",
    gp = gpar(lwd = 2, lty = 1, col = "red"),
    name = "target_height_range"
  )
  target_height_estimate <- pointsGrob(
    x = 20.5,
    y = th[1L],
    pch = 10,
    gp = gpar(
      col = "red", fill = palette()[4],
      lwd = 2, cex = 1
    ),
    name = "target_height_estimate"
  )

  gList(
    target_height_range = target_height_range,
    target_height_estimate = target_height_estimate
  )
}
