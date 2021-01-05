#' Set data points in a graphic object
#'
#' Sets the data points in a graphic object. The function will identify
#' and set all \code{ynames} in the graph.
#' @param g A \code{gTree} object, typically loaded by
#'   \code{chartbox::load_chart()}
#' @param matches List of vector containing the id's of the matches in
#'   the donor data. List elements should be named after
#'   \code{ynames}. The default value (\code{NULL}) indicates that no
#'   matches are plotted.
#' @inheritParams process_chart
#' @return The grid object \code{g} with data points added.
#' @export
set_curves <- function(g, individual,
                       curve_interpolation = TRUE,
                       nmatch = 0L,
                       matches = NULL,
                       dnr = NULL,
                       period = numeric(0),
                       show_realized = FALSE,
                       show_future = FALSE,
                       clip = TRUE) {

  chartcode <- g$name
  ynames <- get_ynames(chartcode)

  if (is.individual(individual) & length(ynames) > 0) {
    for (yname in ynames) {

      # needed for fixing the display transform
      covariates <- list(yname = yname,
                         sex = individual@sex,
                         ga = individual@ga)

      # obtain data and apply data transforms
      xyz <- get_xyz(individual, yname)
      xyz <- apply_transforms(xyz,
                              chartcode = chartcode,
                              yname = yname,
                              curve_interpolation = curve_interpolation,
                              covariates = covariates)

      # create visit lines grob
      tx <- get_tx(chartcode, yname)
      if (nmatch > 0L)
        visit_lines <- create_visit_lines(g, yname, tx(period))
      else
        visit_lines <- create_visit_lines(g, yname, period = numeric(0))

      # plot curves of target individual
      if (nmatch > 0L & !yname %in%  c("wfh"))
        ind_gList <- create_lines_xyz(xyz, tx(period),
                                      show_realized)
      else
        ind_gList <- create_lines_xyz(xyz, numeric(0),
                                      show_realized)

      # plot curves of matches
      mat_gList <- create_matches_lines(chartcode, yname,
                                        matches, dnr,
                                        curve_interpolation,
                                        covariates)

      # calculate "look into future" line
      pre_gList <- create_lines_prediction(chartcode, yname,
                                           matches, dnr,
                                           period,
                                           get_xyz(individual, yname),
                                           show_future,
                                           curve_interpolation,
                                           covariates)

      # now put everything into a clipped grob
      clipper <- clipGrob(name = "clipper")
      curves <- gTree(name = "data",
                      children = gList(clipper, visit_lines,
                                       mat_gList$matches_lines,
                                       mat_gList$matches_symbols,
                                       pre_gList, ind_gList))

      g <- setGrob(gTree = g,
                   gPath = gPath(yname, "modules", "data"),
                   newGrob = curves)
    }
  }

  invisible(g)
}
