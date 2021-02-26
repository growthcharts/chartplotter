#' Obtain an empty gTree object that can act as a temporary placeholder
#'
#' @aliases placeholder
#' @param name A string indicating the name of the `gTree` object
#' @return An object of class `gTree` with name `name`
placeholder <- function(name = "") gTree(NULL, name = name)
