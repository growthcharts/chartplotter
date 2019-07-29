#' Find matches on all outcomes shown on the active chart
#'
#' @param ynames    A vector with the names of the response variables
#'   for which matches are sought, e.g. \code{ynames = c("hdc",
#'   "hgt")}.
#' @inheritParams process_chart
#' @return List with components with names \code{ynames}. Each
#'   component is a vector of integers representing the id's of the
#'   matches cases from \code{donor}. Returns \code{integer(0)} if no
#'   matches are found.
#' @seealso \code{\link[curvematching]{calculate_matches}},
#'   \code{\link[curvematching]{extract_matches}}
find_matches <- function(individual,
                         con,
                         dnr,
                         ynames,
                         nmatch = 5L,
                         period,
                         user_model = 2L,
                         exact_sex = FALSE,
                         exact_ga = FALSE,
                         break_ties = TRUE) {

  # preliminary exit if we need no matches
  matches <- vector("list", length(ynames))
  names(matches) <- ynames
  if (length(ynames) == 0L | nmatch == 0L)
    return(lapply(matches, function(x) integer(0)))

  # convert individual data into donordata format
  # return if that cannot be done
  idf <- individual_to_donordata(individual, element = "child")
  if (nrow(idf) == 0L) return(lapply(matches, function(x) integer(0)))
  idf$istarget <- TRUE
  idf$keep <- TRUE

  # add individual data to `donor`
  # take care that individual data are added as last because calculate_matches()
  # returns the row number
  donor <- load_data(con = con, dnr = dnr, element = "child")

  data <- donor %>%
    mutate(keep = .data$id != idf$id,
           istarget = FALSE)
  data <- data %>%
    bind_rows(idf) %>%
    restore_factors(f = c("sex", "etn", "edu"))

  # names of complete variables in the data
  xnames_complete <- names(data)[!unlist(lapply(data, anyNA))]

  # define model variables
  e_name <- c("sex", "ga")[c(exact_sex, exact_ga)]
  t_name <- character()
  xnames <- sapply(ynames,
                   function(x) make_xname(x,
                                          xnames_complete,
                                          user_model = user_model,
                                          current_age = period[1L]),
                   simplify = FALSE)
  names(xnames) <- ynames

  # calculate rows of the matches
  for (yname in ynames) {
    m <- calculate_matches(data = data,
                           condition = .data$istarget == TRUE,
                           subset = .data$keep == TRUE,
                           y_name = paste(yname, period[2L], sep = "_"),
                           x_name = xnames[[yname]],
                           e_name = e_name,
                           t_name = t_name,
                           k = as.numeric(nmatch),
                           break_ties = break_ties)
    matches[[yname]] <- extract_matches(m)
  }

  # convert to id
  lapply(matches, function(x) get_id(rows = x, donor = donor))
}

get_id <- function(rows, donor) {
  donor$id[rows]
}
