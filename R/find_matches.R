#' Find matches on all outcomes shown on the active chart
#'
#' @param ynames    A vector with the names of the response variables
#'   for which matches are sought, e.g. `ynames = c("hdc",
#'   "hgt")`.
#' @inheritParams process_chart
#' @return List with components with names `ynames`. Each
#'   component is a vector of integers representing the id's of the
#'   matches cases from `donor`. Returns `integer(0)` if no
#'   matches are found.
#' @seealso [curvematching::calculate_matches()],
#'   [curvematching::extract_matches()]
find_matches <- function(target,
                         con,
                         dnr,
                         ynames,
                         nmatch = 5L,
                         period,
                         user_model = 2L,
                         exact_sex = FALSE,
                         exact_ga = FALSE,
                         break_ties = TRUE) {

  matches <- vector("list", length(ynames))
  names(matches) <- ynames

  # return early if needed
  if (!nmatch || !length(period) || !length(ynames) || !nrow(target)) {
    return(lapply(matches, function(x) integer(0)))
  }

  # initialize person data
  child <- persondata(target) %>%
    mutate(istarget = TRUE,
           keep = TRUE)

  # load model collection
  bs <- load_data(dnr = paste0(dnr, "_bs"))

  # fetch potential donor data for target
  # set keep to FALSE if donor$id and child$id match
  donor <- load_data(con = con, dnr = dnr, element = "child") %>%
    mutate(
      keep = .data$id != (!! child)$id,
      istarget = FALSE
    )

  # add target child to `donor`
  # take care that target child is added as last because calculate_matches()
  # returns the row number
  # data <- bind_rows(data, child)

  # get the observed target data up to period[1L]
  xz <- target %>%
    filter(.data$age <= (!!period)[1L]) %>%
    mutate(id = (!!child)$id) %>%
    select(all_of(c("id", "age", "z", "yname")))

  # add the brokenstick estimates for target child at all break ages,
  # but using only the child's data up to the "current age" (period[1])
  hat <- vector("list", length(ynames))
  names(hat) <- ynames
  for (yname in ynames) {

    # get the brokenstick model
    bsm <- bs[[yname]]
    if (is.null(bsm)) {
      next
    }

    # prepare child measurements
    zname <- paste0(yname, "_z")
    xzy <- xz %>%
      filter(.data$yname == !! yname) %>%
      rename(!! zname := .data$z)

    # predict according to the brokenstick model
    # store predicted brokenstick values
    if (!is.null(bsm) && nrow(xzy)) {
      zhat <- predict(bsm, newdata = xzy, x = "knots", shape = "vector")
      names(zhat) <- paste(yname, "z", get_knots(bsm), sep = "_")
      hat[[yname]] <- data.frame(as.list(zhat))
    }
  }
  child <- bind_cols(persondata(target), hat)

  # names of complete variables in the data
  # Note: 2020/12/31: Selecting the complete variables is very frail
  xnames_complete <- names(donor)[!unlist(lapply(donor, anyNA))]

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
    m <- calculate_matches2(
      data = donor,
      newdata = child,
      subset = TRUE,
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
