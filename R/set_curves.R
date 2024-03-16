#' Set data points in a graphic object
#'
#' Sets the data points in a graphic object. The function will identify
#' and set all `ynames` in the graph.
#' @param g A `gTree` object, typically loaded by
#'   `chartbox::load_chart()`
#' @param ynames    A vector with the names of the response variables
#'   for which matches are sought, e.g. `ynames = c("hdc",
#'   "hgt")`.
#' @param matches List of vector containing the id's of the matches in
#'   the donor data. List elements should be named after
#'   `ynames`. The default value (`NULL`) indicates that no
#'   matches are plotted.
#' @inheritParams process_chart
#' @return The grid object `g` with data points added.
set_curves <- function(g,
                       target,
                       ynames,
                       curve_interpolation = TRUE,
                       nmatch = 0L,
                       matches = NULL,
                       dnr = NULL,
                       period = numeric(0),
                       show_realized = FALSE,
                       show_future = FALSE,
                       clip = TRUE) {
  chartcode <- g$name
  if (!nmatch) period <- numeric(0)

  # get target data
  child <- persondata(target)
  time <- timedata(target)
  data <- time %>%
    select(all_of(c("age", "xname", "yname", "x", "y"))) %>%
    filter(.data$yname %in% ynames) %>%
    drop_na("y") %>%
    mutate(
      id = -1,
      sex = child$sex,
      ga = child$ga,
      x2 = .data$x
    ) %>%
    select(all_of(c("id", "age", "xname", "yname", "x", "y", "sex", "ga", "x2")))
  # For WFH, temporary sort on age to get correct time sequence, use x2 to store x
  idx <- data$yname == "wfh"
  data$x[idx] <- data$age[idx]

  # get data of matches
  time <- vector("list", length(ynames))
  names(time) <- ynames
  for (yname in ynames) {
    time[[yname]] <- load_data(dnr = dnr, element = "time", ids = matches[[yname]]) %>%
      mutate(
        xhgt = .data$hgt,
        wfh = .data$wgt
      ) %>%
      select(any_of(c("id", "age", "sex", "ga", "xhgt", yname)))
  }
  time <- time %>%
    bind_rows()
  if (!nrow(time)) {
    time <- time %>%
      select(all_of(c("id", "sex", "ga")))
  } else {
    time <- time %>%
      pivot_longer(cols = any_of(ynames), names_to = "yname", values_to = "y") %>%
      drop_na("y") %>%
      mutate(
        x = as.numeric(ifelse(.data$yname == "wfh", .data$xhgt, .data$age)),
        xname = as.character(ifelse(.data$yname == "wfh", "hgt", "age"))
      ) %>%
      arrange(.data$id, .data$yname, .data$x, .data$age) %>%
      select(all_of(c("id", "age", "xname", "yname", "x", "y", "sex", "ga")))
  }

  # rbind target and matches
  data <- data %>%
    bind_rows(time) %>%
    mutate(obs = TRUE)

  # define grid for synthetic observations
  ynames <- unique(data$yname)
  lohi <- data %>%
    group_by(.data$id, .data$yname) %>%
    summarise(
      id = first(.data$id),
      xname = first(.data$xname),
      sex = first(.data$sex),
      ga = first(.data$ga),
      lo = suppressWarnings(min(.data$x, na.rm = TRUE)),
      hi = suppressWarnings(max(.data$x, na.rm = TRUE)),
      .groups = "drop"
    )
  xl <- vector("list", nrow(lohi))
  for (i in seq_along(xl)) {
    xo <- set_xout(chartcode, lohi$yname[i])
    xl[[i]] <- xo[xo > lohi$lo[i] & xo < lohi$hi[i]]
  }
  lng <- sapply(xl, length)
  if (any(lng)) {
    synt <- tidyr::uncount(lohi, weights = !!lng) %>%
      mutate(
        x = unlist(!!xl),
        obs = FALSE
      ) %>%
      select(-c("lo", "hi"))
  } else {
    synt <- tibble(yname = character(0))
  }

  # append synthetic data
  data <- data %>%
    bind_rows(synt) %>%
    arrange(.data$id, .data$yname, .data$x)

  # For wfh, interpolate hgt from age, and overwrite data$x with hgt
  idx <- data$yname == "wfh"
  if (any(idx)) {
    data$x[idx] <- safe_approx(x = data$x[idx], y = data$x2[idx],
                               xout = data$x[idx], ties = list("ordered", mean))$y
  }

  # add Z-scores
  data <- data %>%
    select(-"age") %>%  # fool set_refcodes()
    mutate(refcode_z = nlreferences::set_refcodes(.)) %>%
    mutate(
      z = centile::y2z(
        y = .data$y,
        x = .data$x,
        refcode = .data$refcode_z,
        pkg = "nlreferences",
        rule = 2L
      ),
      pred = FALSE
    )

  # calculate brokenstick predictions
  pred <- calculate_predictions(data,
    chartcode = chartcode, ynames = ynames,
    dnr = dnr, period = period
  )

  # linear interpolation in Z-scale
  data <- data %>%
    bind_rows(pred) %>%
    group_by(.data$id, .data$yname, .data$pred) %>%
    mutate(z = safe_approx(
      x = .data$x, y = .data$z, xout = .data$x,
      ties = mean)$y) %>%
    ungroup()

  # set refcode as target's sex and ga
  refcode_y <- data %>%
    mutate(
      sex = child$sex,
      ga = child$ga
    ) %>%
    mutate(refcode_y = nlreferences::set_refcodes(.)) %>%
    pull(.data$refcode_y)

  # convert to display metric
  data <- data %>%
    mutate(
      refcode_y = !!refcode_y,
      v = centile::z2y(
        z = .data$z,
        x = .data$x,
        refcode = .data$refcode_y,
        pkg = "nlreferences",
        rule = 2L
      )
    )

  # select essential fields for plotting
  plotdata <- data %>%
    select(all_of(c("id", "yname", "obs", "pred", "x", "y", "z", "v")))

  # for debugging
  # utils::write.table(data,
  #                    file = "data.txt", quote = FALSE, sep = "\t", na = "",
  #                    row.names = FALSE
  # )

  # plot loop
  for (yname in ynames) {

    # cannot yet perform prediction for WFH
    if (yname == "wfh") period <- numeric(0)

    # obtain data and apply data transforms
    data <- plotdata %>%
      filter(.data$yname == !!yname) %>%
      mutate(
        v = apply_transforms_y(.data$v, chartcode, !!yname),
        x = apply_transforms_x(.data$x, chartcode, !!yname)
      )

    # create visit lines grob
    tx <- get_tx(chartcode, yname)
    p <- tx(period)
    visit_lines <- plot_visit_lines(g, yname, p)

    # plot curves of target
    ind_gList <- plot_lines_target(data,
      yname = yname, period = p,
      curve_interpolation = curve_interpolation,
      show_realized = show_realized
    )

    # plot curves of matches
    mat_gList <- plot_lines_matches(data,
      yname = yname,
      curve_interpolation = curve_interpolation
    )

    # calculate "look into future" line
    pre_gList <- plot_lines_prediction(data,
      yname = yname,
      show_future = show_future,
      curve_interpolation = curve_interpolation
    )

    # now put everything into a clipped grob
    clipper <- clipGrob(name = "clipper")
    curves <- gTree(
      name = "data",
      children = gList(
        clipper, visit_lines,
        mat_gList$matches_lines,
        mat_gList$matches_symbols,
        pre_gList, ind_gList
      )
    )

    g <- setGrob(
      gTree = g,
      gPath = gPath(yname, "modules", "data"),
      newGrob = curves
    )
  }

  invisible(g)
}
