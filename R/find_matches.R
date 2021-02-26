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
  if (length(ynames) == 0L | nmatch == 0L) {
    return(lapply(matches, function(x) integer(0)))
  }

  # convert individual data into donordata format
  # return if that cannot be done
  target <- individual_to_donordata(individual)
  if (nrow(target$child) == 0L) {
    return(lapply(matches, function(x) integer(0)))
  }
  target$child$istarget <- TRUE
  target$child$keep <- TRUE

  # load model collection
  bs <- load_data(dnr = paste0(dnr, "_bs"))

  # fetch potential donor data for target
  donor <- load_data(con = con, dnr = dnr, element = "child")
  data <- donor %>%
    mutate(
      keep = .data$id != target$child$id,
      istarget = FALSE
    )

  # add individual data to `donor`
  # take care that individual data are added as last because calculate_matches()
  # returns the row number
  data <- data %>%
    bind_rows(target$child) %>%
    restore_factors(f = c("sex", "etn", "edu"))

  # add the brokenstick estimates for target child at all break ages,
  # but using only the child's data up to the "current age" (period[1])
  for (yname in ynames) {

    # get the brokenstick model
    bsm <- bs[[yname]]

    # get the observed target data up to period[1L]
    if (yname %in% c("wfh")) {
      xz <- tibble()
    } else {
      idx <- target$time$age <= period[1L]
      zname <- paste(yname, "z", sep = "_")
      xz <- target$time[idx, c("id", "age", zname, "sex", "ga")]
    }

    # predict according to the brokenstick model
    # store predicted Z-scores in last line of data
    if (!is.null(bsm) && nrow(xz)) {
      zhat <- predict(bsm, new_data = xz, x = "knots", shape = "vector")
      zhat_names <- paste(yname, "z", get_knots(bsm), sep = "_")
      data[nrow(data), zhat_names] <- as.list(zhat)
    }
  }

  # names of complete variables in the data
  # Note: 2020/12/31: Selecting the complete variables is very frail
  xnames_complete <- names(data)[!unlist(lapply(data, anyNA))]

  # define model variables
  # FIXME
  # double use of ga
  # 1. we use ga to choose the Z-score transform yzy::transform_z()
  # 2. we use here ga in the predictive model.
  # Need to check whether this double application is useful.
  # Also, do we really want to have yname_period[2L] as the outcome measure?
  e_name <- c("sex", "ga")[c(exact_sex, exact_ga)]
  t_name <- character()
  xnames <- sapply(ynames,
    function(x) {
      make_xname(x,
        xnames_complete,
        user_model = user_model,
        current_age = period[1L]
      )
    },
    simplify = FALSE
  )
  names(xnames) <- ynames

  # calculate rows of the matches
  for (yname in ynames) {
    m <- calculate_matches(
      data = data,
      condition = .data$istarget == TRUE,
      subset = .data$keep == TRUE,
      y_name = paste(yname, "z", period[2L], sep = "_"),
      x_name = xnames[[yname]],
      e_name = e_name,
      t_name = t_name,
      k = as.numeric(nmatch),
      break_ties = break_ties
    )
    matches[[yname]] <- extract_matches(m)
  }

  # convert to id
  lapply(matches, function(x) get_id(rows = x, donor = donor))
}

get_id <- function(rows, donor) {
  donor$id[rows]
}
