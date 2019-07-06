#' Find the visit number given age and donor data name
#'
#' @param age Numeric vector with decimal age
#' @param dnr Name of donor dataset
#' @return A vector of length \code{length(age)}
#' @examples
#' visit.number(age = c(0.5, 1))
#' visit.number(age = c(0.5, 1), "pops")
#' @export
visit.number <- function(age, dnr = "smocc") {
  brk <- get_breakpoints(dnr)
  round(approx(x = brk$age, y = brk$visit, xout = age, rule = 2)$y)
}

#' Find the visit age given the visit number and donor data name
#'
#' @param number Integer, visit numbers
#' @param dnr Name of donor dataset
#' @return A vector of length \code{length(number)}
#' @examples
#' visit.age(1:3)
#' visit.age(1:3, "pops")
#' @export
visit.age <- function(number, dnr = "smocc") {
  brk <- get_breakpoints(dnr)
  brk[brk$visit %in% number, "age"]
}

#' Find the visit label given the visit number and donor data name
#'
#' @param number Integer, visit numbers
#' @param dnr Name of donor dataset
#' @return A vector of length \code{length(number)}
#' @examples
#' visit.label(1:3)
#' visit.label(1:3, "pops")
#' @export
visit.label <- function(number, dnr = "smocc") {
  brk <- get_breakpoints(dnr)
  brk[brk$visit %in% number, "label"]
}
