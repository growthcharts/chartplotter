#' Obtain the knots from a broken stick model
#'
#' @param object An object of class \code{brokenstick}
#' @param what A character vector of length 1. Valid values are
#' \code{"all"}, \code{"knots"}, \code{"boundary"} or \code{"droplast"}.
#' The default is \code{what = "all"}.
#' @return A vector with knot locations, either both, internal only or
#' boundary only. The result is \code{NULL} if \code{object} does not
#' have proper class. The function can return \code{numeric(0)} if
#' there are no internal knots.
#' @details
#' JAMES version. The original functions belongs in the \code{brokenstick}
#' package.
#' @export
get_knots <- function(object,
                      what = c("all", "knots", "boundary", "droplast")) {
  if (!inherits(object, c("brokenstick"))) {
    return(NULL)
  }

  what <- match.arg(what)
  if (inherits(object, "brokenstick")) {
    knots <- object$knots
    boundary <- object$boundary
  }

  internal <- knots[knots > boundary[1] & knots < boundary[2]]

  result <- switch(what,
                   all = c(boundary[1], internal, boundary[2]),
                   knots = internal,
                   boundary = boundary,
                   droplast = c(boundary[1], internal)
  )
  return(result)
}
