#' Creates the model specifications for xname
#'
#' @param yname    String indicating the dependent variable
#' @param xnames   Character vector of available names to choose from.
#'   It is assumed all variables named in `xnames` have complete
#'   data. Any brokenstick variable should have names like
#'   `paste(yname, d, sep = "_")`, where `d` is a vector
#'   contuining decimal ages.
#' @param current_age Current age (in decimal years)
#' @param hat A list with brokenstick prediction for the target that
#' contains the names of the brokenstick proedictions. The entry is
#' `NULL` if these estimates cannot be made. In that case, this
#' function does not include the previous yname scores into the
#' matching.
#' @inheritParams process_chart
#' @return A character vector with the `xname` specification
#' @examples
#' xnames <- c(
#'   "sex", "ga", "bw", "hgt_0", "hgt_1", "hgt_3",
#'   "wgt_0", "wgt_1", "wgt_2", "junk"
#' )
#'
#' # note that it does not return hgt2
#' chartplotter:::make_xname(
#'   yname = "hgt", xnames = xnames, user_model = 2,
#'   current_age = 2
#' )
make_xname <- function(yname,
                       xnames,
                       user_model,
                       current_age,
                       hat = NULL) {
  covariates <- c(
    "sex", "etn", "ga", "bw", "twin", "smo",
    "edu", "agem", "hgtf", "wgtf", "hgtm", "wgtm",
    "durbrst"
  )
  ynames <- c("hgt", "wgt", "hdc", "bmi", "wfh", "dsc")
  yname <- match.arg(yname, ynames)
  # read names from hat if brokenstick estimates are present
  # else create names
  if (missing(hat)) {
    xn <- get_xname(yname, xnames)
  } else {
    xn <- names(hat[[yname]])
  }
  xa <- get_age(xn)

  idx <- xa <= current_age
  switch(as.numeric(user_model),
    intersect(xn[idx][sum(idx)], xnames),
    intersect(c(xn[idx], "sex"), xnames),
    intersect(c(xn[idx], covariates), xnames),
    {
      xy <- unname(unlist(sapply(
        ynames,
        function(x) {
          xn <- get_xname(x, xnames)
          xa <- get_age(xn)
          xn[xa <= current_age]
        },
        simplify = FALSE
      )))
      intersect(c(xy, covariates), xnames)
    },
    character()
  )
}
