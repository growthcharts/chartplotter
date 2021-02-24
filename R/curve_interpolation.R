#' Interpolates of growth curves along reference centiles
#'
#' By convention, we connect the points of a growth curve by
#' straight lines. This is OK when ages are relatively close
#' together. However, for ages that are more apart, the straight
#' line gives an ugly representation that doesn't follow the real
#' growth during the interval.
#'
#' The \code{curve_interpolation()} function provides a solution
#' for this. It defines a number of additional points (specified
#' through the \code{xout} argument), and calculates the linear
#' interpolation between these points in the Z-scores metric. After
#' that, the procedure transforms back the interpolated Z-scores
#' to the original scale, so the curve points follow the curvy
#' centiles.
#'
#' @param data A data frame in long format, with columns for person
#'   number (named \code{id}), the x and y variables.
#' @param xname Name of the variable on the horizontal axis.
#' @param yname Name of the variable on the vertical axis.
#' @param xout A vector of values for the horizontal axis in the scale
#'  of the variable \code{xname}. Only \code{xout} points within the
#'  data range of \code{xname} are interpolated.
#' @param refcode The name of reference needed to calculate the Z-score
#'  scores and the back-transformation to measured scores. There can only
#'  be one reference.
#' @param rule The \code{rule} argument passed down to \code{approx}. The
#' default here is \code{rule = 2L}, so any extrapolations beyond the
#' ranges of the reference take the closest value (min or max).
#' @return A \code{tibble} with five columns: \code{id}, xname, yname,
#' zname and \code{obs}. The \code{obs} variables signals whether
#' the point is observed or not.
#' @seealso [centile::y2z()], [centile::z2y()]
#' @author Stef van Buuren, 2021
#' @examples
#' data <- data.frame(
#'   id = c(1, 1, 1, 2, 3, 3, 3),
#'   age = c(0, 0.2, 1.2, 0.5, 0.1, 1, 1.3),
#'   hgt = c(52, 60, 78, 69, 62, 78, 82)
#' )
#' refcode <- "nl_1997_hgt_male_nl"
#' xout <- seq(0, 3, 0.2)
#' int <- curve_interpolation(data,
#'   xname = "age", yname = "hgt",
#'   xout = xout, refcode = refcode
#' )
#' int
#' @export
curve_interpolation <- function(data, xname = "x", yname = "y",
                                xout = numeric(0),
                                refcode = NULL,
                                rule = 2L) {
  if (nrow(data) == 0L) {
    return(tibble(
      id = numeric(0),
      x = numeric(0), y = numeric(0), z = numeric(0),
      obs = logical(0)
    ))
  }

  # Initialisations
  if (is.null(refcode)) {
    message("No reference given.")
  }
  refcode <- refcode[[1L]]
  zname <- paste(yname, "z", sep = "_")

  # Transform to Z-values (analysis metric)
  observed <- data %>%
    select(.data$id, !!xname, !!yname) %>%
    mutate(
      obs = TRUE,
      !!zname := centile::y2z(
        y = data[[yname]], x = data[[xname]],
        refcode = refcode, pkg = "jamesyzy", rule = rule
      )
    )

  # create bending points
  rng <- suppressWarnings(range(data[, xname, drop = TRUE], finite = FALSE))
  if (any(sapply(rng, is.infinite))) rng <- c(0, 0)

  grid <-
    as_tibble(expand.grid(
      id = unique(data$id),
      xout = xout
    )) %>%
    mutate(
      obs = FALSE,
      !!xname := .data$xout
    ) %>%
    select(-.data$xout) %>%
    bind_rows(observed) %>%
    arrange(!!!rlang::syms(c("id", xname, yname, zname))) %>%
    distinct(!!!rlang::syms(c("id", xname)), .keep_all = TRUE)

  # remove any bending points outside the observation intervals
  grid <- grid %>%
    group_by(.data$id) %>%
    mutate(
      mn = suppressWarnings(min(.data[[xname]][.data$obs & !is.na(.data[[zname]])], na.rm = TRUE)),
      mx = suppressWarnings(max(.data[[xname]][.data$obs & !is.na(.data[[zname]])], na.rm = TRUE)),
      lower = .data[[xname]] < .data$mn & !.data$obs,
      higher = .data[[xname]] > .data$mx & !.data$obs
    ) %>%
    filter(!.data$lower & !.data$higher) %>%
    select(-.data$mn, -.data$mx, -.data$lower, -.data$higher)

  # isolate data with fewer than two valid xy pairs (approx fails)
  minimal2 <- grid %>%
    summarise(do_approx = sum(!(is.na(.data[[zname]][.data$obs])) | is.na(.data[[xname]][.data$obs])) >= 2L)

  grid <- grid %>%
    left_join(minimal2, by = "id")

  singletons <- grid %>%
    filter(!.data$do_approx) %>%
    ungroup() %>%
    select(all_of(c("id", xname, yname, zname, "obs")))

  # select subset with bending points
  grid <- grid %>%
    filter(.data$do_approx)

  # if there are bending points left:
  # interpolate and back-transform to y
  if (nrow(grid) > 0L) {
    grid <- grid %>%
      mutate(!!zname := approx(
        x = .data[[xname]], y = .data[[zname]],
        xout = .data[[xname]], rule = rule
      )$y) %>%
      ungroup()

    grid <- grid %>%
      mutate(yt = centile::z2y(
        z = .data[[zname]], x = .data[[xname]],
        refcode = refcode, pkg = "jamesyzy", rule = rule
      ))

    # overwrite any NA's in yname
    ov <- grid %>%
      filter(is.na(.data[[yname]])) %>%
      mutate(!!yname := .data$yt)
    grid <- grid %>%
      filter(!is.na(.data[[yname]])) %>%
      bind_rows(ov) %>%
      select(all_of(c("id", xname, yname, zname, "obs")))
  }

  # append singletons and sort
  grid <- grid %>%
    bind_rows(singletons) %>%
    arrange(!!!rlang::syms(c("id", xname)))
}
