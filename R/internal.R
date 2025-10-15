apply_transforms_y <- function(y, chartcode, yname) {
  # transform ty(y) in reference-transform sequence
  ty <- get_ty(chartcode, yname)
  sq <- get_seq(chartcode, yname)
  # hack: we must use sq <- "rt" for WFH
  # The table ynames_lookup in chartcatalog return "tr", because these are based
  # on the 1980 data (which was modeled in a different way than the WFH data
  # of 1997
  if (yname == "wfh") {
    sq <- "rt"
  }
  if (sq == "rt") {
    return(ty(y))
  }
  y
}

apply_transforms_x <- function(x, chartcode, yname) {
  # always apply x transform to graph
  tx <- get_tx(chartcode, yname)
  tx(x)
}

set_xout <- function(chartcode, yname) {
  # set xout equal to construction grid
  design <- substr(chartcode, 3L, 3L)
  if (design == "A") {
    return(round(seq(0.5, 15, 0.5) / 12, 4L))
  }
  if (design == "B" & yname %in% c("hgt", "dsc")) {
    return(round(c(0.5, 0.75, 1:48) / 12, 4L))
  }
  if (design == "B" & yname %in% c("hdc", "wfh")) {
    return(round(seq(0.1, 4, by = 0.1), 4L))
  }
  if (design == "C") {
    return(round(seq(1, 21, by = 0.5), 4L))
  }
  if (design == "E") {
    return(round(c(0.5, 0.75, 1:48) / 12, 4L))
  }
  numeric(0)
}

get_age <- function(xn) {
  # parse xname to get age
  x <- sapply(xn, strsplit, split = "_")
  as.numeric(unlist(lapply(x, `[`, 3L)))
}

get_xname <- function(yname, xnames) {
  xnames[grep(paste0(yname, "_"), xnames)]
}

restore_factors <- function(data, f = NULL) {
  if (is.null(f)) {
    return(data)
  }
  for (i in 1:length(f)) {
    v <- f[i]
    if (v %in% names(data)) data[, v] <- as.factor(data[, v, drop = TRUE])
  }
  data
}

safe_approx <- function(
  x,
  y = NULL,
  xout,
  method = "linear",
  n = 50,
  rule = 1,
  f = 0,
  ties = mean,
  na.rm = TRUE
) {
  # ---- helpers -------------------------------------------------------------
  coerce_ties_fun <- function(ties) {
    if (is.function(ties)) {
      return(ties)
    }
    if (is.character(ties) && length(ties) >= 1L) {
      key <- ties[[1L]]
      if (identical(key, "ordered")) {
        return(function(z) z[1L])
      }
      if (identical(key, "mean")) {
        return(mean)
      }
      if (identical(key, "min")) {
        return(min)
      }
      if (identical(key, "max")) return(max)
    }
    stop(
      "'ties' must be a function or one of 'ordered', 'mean', 'min', 'max'.",
      call. = FALSE
    )
  }

  scalarize_rule <- function(rule) {
    if (length(rule) < 1L) {
      return(1L)
    }
    as.integer(rule[[1L]])
  }

  # ---- inputs --------------------------------------------------------------
  # Mirror approx() behavior if y is NULL
  if (is.null(y)) {
    y <- x
    x <- seq_along(y)
  }

  # drop NA pairs
  mis <- is.na(x) | is.na(y)
  x <- x[!mis]
  y <- y[!mis]

  # nothing left
  if (!length(x)) {
    return(list(x = xout, y = rep(NA_real_, length(xout))))
  }

  # single point -> constant or NA depending on rule
  if (length(x) == 1L) {
    r <- scalarize_rule(rule)
    y1 <- y[[1L]]
    yout <- if (r == 2L) {
      rep(y1, length(xout))
    } else {
      ifelse(xout == x[[1L]], y1, NA_real_)
    }
    return(list(x = xout, y = yout))
  }

  # all x identical -> collapse with ties and apply rule
  if (length(unique(x)) == 1L) {
    tie_fun <- coerce_ties_fun(ties)
    y_c <- tie_fun(y)
    r <- scalarize_rule(rule)
    yout <- if (r == 2L) {
      rep(y_c, length(xout))
    } else {
      ifelse(xout == x[[1L]], y_c, NA_real_)
    }
    return(list(x = xout, y = yout))
  }

  # delegate to approx(); ensure ties is either function or "ordered"
  tie_arg <- if (is.function(ties)) {
    ties
  } else {
    key <- ties[[1L]]
    if (identical(key, "ordered")) "ordered" else coerce_ties_fun(key)
  }

  stats::approx(
    x = x,
    y = y,
    xout = xout,
    method = method,
    n = n,
    rule = scalarize_rule(rule),
    f = f,
    ties = tie_arg,
    na.rm = na.rm
  )
}

# from bdsreader
persondata <- function(x) {
  stopifnot(is.list(x))
  stopifnot("psn" %in% names(x))
  return(x[["psn"]])
}

# from bdsreader
timedata <- function(x) {
  stopifnot(is.list(x))
  stopifnot("xyz" %in% names(x))
  return(x[["xyz"]])
}
