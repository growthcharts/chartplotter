#' Find the visit number given age and donor data name
#'
#' @param age Numeric vector with decimal age
#' @param dnr Name of donor dataset
#' @return `visit_number` returns a vector of length `length(age)`
#' @rdname visit
#' @examples
#' chartplotter:::visit_number(age = c(0.5, 1))
#' chartplotter:::visit_number(age = c(0.5, 1), "pops")
visit_number <- function(age, dnr = "smocc") {
  brk <- get_breakpoints(dnr)
  round(approx(x = brk$age, y = brk$visit, xout = age, rule = 2)$y)
}

#' Find the visit age given the visit number and donor data name
#'
#' @param number Integer, visit numbers
#' @return `visit_age` returns a vector of length `length(number)`
#' @rdname visit
#' @examples
#' chartplotter:::visit_age(1:3)
#' chartplotter:::visit_age(1:3, "pops")
visit_age <- function(number, dnr = "smocc") {
  brk <- get_breakpoints(dnr)
  brk[brk$visit %in% number, "age"]
}

#' Find the visit label given the visit number and donor data name
#'
#' @rdname visit
#' @return `visit_label` returns a vector of length `length(number)`
#' @examples
#' chartplotter:::visit_label(1:3)
#' chartplotter:::visit_label(1:3, "pops")
visit_label <- function(number, dnr = "smocc") {
  brk <- get_breakpoints(dnr)
  brk[brk$visit %in% number, "label"]
}
