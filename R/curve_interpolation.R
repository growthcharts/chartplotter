#' Interpolates the growth curves along the references
#'
#' We would normally connect the points of a growth curve by
#' straight lines. This is OK when ages are relatively close
#' together. However, for ages that are more apart, the straight
#' line gives an ugly representation that - moreover - underestimates
#' real growth during that interval.
#'
#' The \code{curve_interpolation()} function provides a solution
#' for this. It defines a number of additional points (specified
#' through the \code{xout} argument), and calculates the linear
#' interpolation between these points in the Z-scores metric. After
#' that, the Z-scores are transformed back to the original scale,
#' so the points to plot then follow the curvy reference lines.
#'
#' If the user specifies argument \code{zname}, then the function
#' applies only the transformation \code{z2y()} after interpolation,
#' and skips the input transformation \code{y2z()}. This
#' mechanism creates synthetic values for the output \code{yname}.
#'
#'@param data A data frame in long format, with columns for person
#'  number (named \code{id}), the x and y variables.
#'@param xname Name of the variable on the horizontal axis.
#'@param yname Name of the variable on the vertical axis.
#'@param zname Name of the variable with Z-scores
#'@param xout A vector of values for the horizontal axis in the scale
#'  of the variable \code{xname}. Only \code{xout} points within the
#'  data range of \code{xname} are interpolated.
#'@param reference The reference needed to calculate the Z-score
#'  scores and the back-transformation to measured scores.
#'  An object of class \code{clopus::reference}. If not specified,
#'  then the function expect argument \code{covariates}.
#'@param rule The \code{rule} argument passed down to \code{approx}. The
#'default here is \code{rule = 2L}, so any extrapolations beyond the
#'ranges of the reference take the closest value (min or max).
#'@param covariates A list with covariate values \code{sex} and \code{ga}. Used
#'to define a fixed \code{clopus::transform_z()} and \code{clopus::transform_y()}.
#'@return A \code{tibble} with five columns: \code{id}, xname, yname,
#'zname and \code{obs}. The \code{obs} variables signals whether
#'the point is observed or not.
#'@seealso \code{\link[clopus]{reference-class}}
#'@author Stef van Buuren, 2019
#'@examples
#'data <- data.frame(
#'  id = c(1, 1, 1, 2, 3, 3, 3),
#'  age = c(0, 0.2, 1.2, 0.5, 0.1, 1, 1.3),
#'  hgt = c(52, 60, 78, 69, 62, 78, 82))
#'ref <- clopus::nl2009[["nl2009.mhgtNL"]]
#'xout <- seq(0, 3, 0.2)
#'int <- curve_interpolation(data, xname = "age", yname = "hgt", xout = xout, reference = ref)
#'int
#'@export
curve_interpolation <- function(data, xname = "x", yname = "y",
                                zname = NULL,
                                xout = numeric(0),
                                reference = NULL,
                                rule = 2L,
                                covariates = NULL) {

  if (!is.null(reference) && !is.reference(reference))
    stop("Argument `reference` not of class `reference`")

  # Skip transform to Z-score if data has a column with name zname
  skip <- FALSE
  if (!is.null(zname)) {
    if (hasName(data, zname)) skip <- TRUE
  } else {
    zname <- paste(yname, "z", sep = "_")
  }

  # Transform to Z-values (analysis metric)
  if (!skip) {
    if (is.null(reference)) {
      observed <- data %>%
        select(.data$id, !! xname, !! yname)
      df <- data.frame(y = observed[[yname]],
                       x = observed[[xname]],
                       sex = covariates$sex,
                       ga = covariates$ga)
      names(df) <- c(covariates$yname, "age", "sex", "ga")
      observed <- observed %>%
        mutate(obs = TRUE,
               !! zname :=
                 as.numeric(transform_z(df, ynames = covariates$yname)[[paste0(covariates$yname, ".z")]]))
    } else {
      observed <- data %>%
        select(.data$id, !! xname, !! yname) %>%
        mutate(obs = TRUE,
               !! zname := y2z(y = data[[yname]], x = data[[xname]],
                               ref = reference, rule = rule))
    }
  } else {
    # keep y values for sorting below, else set to NA
    if (hasName(data, yname)) {
      observed <- data %>%
        select(.data$id, !! xname, !! zname, !! yname) %>%
        mutate(obs = TRUE)
    } else {
      observed <- data %>%
        select(.data$id, !! xname, !! zname) %>%
        mutate(obs = TRUE,
               !! yname := NA_real_)
    }
  }

  # create bending points
  rng <- suppressWarnings(range(data[, xname, drop = TRUE], finite = FALSE))
  if (any(sapply(rng, is.infinite))) rng <- c(0, 0)

  grid <-
    as_tibble(expand.grid(id = unique(data$id),
                          xout = xout)) %>%
    mutate(obs = FALSE,
           !! xname := .data$xout) %>%
    select(-.data$xout) %>%
    bind_rows(observed) %>%
    arrange(!!! rlang::syms(c("id", xname, yname, zname))) %>%
    distinct(!!! rlang::syms(c("id", xname)), .keep_all = TRUE)

  # remove any bending points outside the observation intervals
  grid <- grid %>%
    group_by(.data$id) %>%
    mutate(mn = min(.data[[xname]][.data$obs], na.rm = TRUE),
           mx = max(.data[[xname]][.data$obs], na.rm = TRUE),
           lower  = .data[[xname]] < .data$mn,
           higher = .data[[xname]] > .data$mx) %>%
    filter(!.data$lower & !.data$higher) %>%
    select(-.data$mn, -.data$mx, -.data$lower, -.data$higher)

  # isolate data with fewer than two valid xy pairs (approx fails)
  minimal2 <- grid %>%
    summarise(do_approx = sum(!(is.na(.data[[zname]][.data$obs])) |
                                is.na(.data[[xname]][.data$obs])) >= 2L)

  grid <- grid %>%
    left_join(minimal2, by = "id")

  singletons <- grid %>%
    filter(!.data$do_approx) %>%
    ungroup() %>%
    select(one_of(c("id", xname, yname, zname, "obs")))

  # select subset with bending points
  grid <- grid %>%
    filter(.data$do_approx)

  # if there are bending points left:
  # interpolate and backtransform to y
  if (nrow(grid) > 0L) {
    grid <- grid %>%
      mutate(!!zname := approx(x = .data[[xname]], y = .data[[zname]],
                               xout = .data[[xname]], rule = rule)$y) %>%
      ungroup()

    if (is.reference(reference)) {
      # use z2y() directly
      grid <- grid %>%
        mutate(yt = z2y(z = .data[[zname]], x = .data[[xname]],
                        ref = reference, rule = rule))
    } else {
      # use transform_y()
      df <- data.frame(z = grid[[zname]],
                       x = grid[[xname]],
                       sex = covariates$sex,
                       ga = covariates$ga)
      names(df) <- c(paste0(covariates$yname, ".z"), "age", "sex", "ga")
      if (covariates$yname == "wfh") names(df)[2] <- "hgt"
      grid <- grid %>%
        mutate(yt = as.numeric(transform_y(df, ynames = covariates$yname)[[covariates$yname]]))
    }

    # overwrite any NA's in yname
    ov <- grid %>%
      filter(is.na(.data[[yname]])) %>%
      mutate(!!yname := .data$yt)
    grid <- grid %>%
      filter(!is.na(.data[[yname]])) %>%
      bind_rows(ov) %>%
      select(one_of(c("id", xname, yname, zname, "obs")))
  }

  # append singletons and sort
  grid <- grid %>%
    bind_rows(singletons) %>%
    arrange(!!! rlang::syms(c("id", xname)))

}
