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
#'  An object of class \code{clopus::reference}.
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
                                zname = NULL, xout, reference) {

  if (!is.reference(reference)) stop("Argument `reference` not of class `reference`")

  # automatic zname
  if (is.null(zname))
    zname <- paste(yname, "z", sep = "_")

  # select observed data
  observed <- data %>%
    select(.data$id, !! xname, !! yname) %>%
    mutate(obs = TRUE,
           !! zname := y2z(y = data[[yname]], x = data[[xname]],
                           ref = reference))

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
    arrange(!!! rlang::syms(c("id", xname, yname))) %>%
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
  # transform to z, interpolate and backtransform to y
  if (nrow(grid) > 0L) {
    grid <- grid %>%
      mutate(!!zname := approx(x = .data[[xname]], y = .data[[zname]],
                               xout = .data[[xname]])$y) %>%
      ungroup() %>%
      mutate(yt = z2y(z = .data[[zname]], x = .data[[xname]],
                      ref = reference))

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
  grid %>%
    bind_rows(singletons) %>%
    arrange(!!! rlang::syms(c("id", xname)))
}
