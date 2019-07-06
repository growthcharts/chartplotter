#' Set data points in a graphic object
#'
#' Sets the data points in a graphic object. The function will identify
#' and set all \code{ynames} in the graph.
#' @param g A \code{gTree} object, typically loaded by
#'   \code{chartbox::load_chart()}
#' @param individual An S4 object of class \code{individual} containing data of the individual
#' @param curve_interpolation Flag indicating whether curves should bend to
#' reference curves
#' @param matches List of vector containing the id's of the matches in the donor data.
#' List elements should be named after \code{ynames}.
#' The default value (\code{NULL}) indicates that no matches are plotted.
#' @param dnr Name of the donor data (currently available are
#' \code{smocc}, \code{terneuzen}, \code{lollypop.preterm} or \code{lollypop.term})
#' @param period A vector of length 2 with left and right ages. If
#' \code{length(period) == 0L}, then no matches are plotted
#' @param show_realized A logical indicating whether the realized growth of the target
#' child should be drawn
#' @param show_future A logical indicating whether the predicted growth of the target
#' child should be drawn
#' @param clip A logical indicating whether clipping is needed
#' @return The grid object \code{g} with data points added.
#' @export
set_curves <- function(g, individual,
                       curve_interpolation = TRUE,
                       matches = NULL, dnr = NULL,
                       period = numeric(0),
                       show_realized = FALSE,
                       show_future = FALSE,
                       clip = TRUE) {

  chartcode <- g$name
  ynames <- get_ynames(chartcode)

  if (is.individual(individual) & length(ynames) > 0) {
    for (yname in ynames) {

      # obtain data and apply data transforms
      xyz <- get_xyz(individual, yname)
      xyz <- apply_transforms(xyz,
                              chartcode = chartcode,
                              yname = yname,
                              curve_interpolation = curve_interpolation)

      # create visit lines grob
      visit_lines <- create_visit_lines(chartcode, yname, period)

      # plot curves of target individual
      if (yname == "wfh") period <- numeric(0)  # cannot predict for wfh
      tx <- get_tx(chartcode, yname)
      ind_gList <- create_lines_xyz(xyz, tx(period), show_realized)

      # plot curves of matches
      mat_gList <- create_lines_matches(chartcode, yname,
                                        matches, dnr,
                                        curve_interpolation)

      # create the prediction
      pre_gList <- create_lines_prediction(chartcode, yname,
                                           matches, dnr, period,
                                           show_future,
                                           curve_interpolation)

      # now put everything into a clipped grob
      clipper <- clipGrob(name = "clipper")
      curves <- gTree(name = "data",
                      children = gList(clipper, visit_lines,
                                       mat_gList, pre_gList, ind_gList))

      g <- setGrob(gTree = g,
                   gPath = gPath(yname, "modules", "data"),
                   newGrob = curves)
    }
  }

  # remove rectangle
  g <- editGrob(grob = g, gPath = gPath("page"),
                gp = gpar(col = "transparent", fill = "grey97"))

  invisible(g)
}
