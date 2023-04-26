#' Plots the flat chart using ggplot or plotly
#'
#' @inheritParams process_chart
#' @param chartcodes A vector of chartcodes from which traces should be extracted
#' @param plot_library Either `plotly` (default) or `ggplot2`
#' @param verbose Logical. Set to `TRUE` to track down why traces may be missing
#' @param draw_grob Logical. Should chart be plotted on current device?
#' Default is `FALSE`. For internal use only.
#' @param \dots Arguments passed down to `centile::y2z()`
#' @return Object of class `plotly` or `gg`
#' @examples
#' \dontrun{
#' fn <- system.file("extdata", "bds_v2.0", "smocc", "Laura_S.json", package = "jamesdemodata")
#' target <- bdsreader::read_bds(fn)
#' g <- process_flatchart(target, chartcodes = "NMAA", plot_library = "plotly")
#' g
#' }
#' @export
process_flatchart <- function(target,
                              chartcodes,
                              plot_library = c( "plotly", "ggplot2"),
                              verbose = FALSE,
                              draw_grob = FALSE,
                              ...) {
  plot_library <- match.arg(plot_library)

  # extract ynames
  ynames <- lapply(chartcodes, get_ynames)
  chartcodes2 <- rep(chartcodes, sapply(ynames, length))
  ynames <- unlist(ynames)

  # no ynames: RETURN empty dutch 0-15 chart
  if (!length(ynames)) {
    g <- init_sds(design = "A",
                  language = "dutch",
                  plot_library = plot_library)
    return(list(charts = NULL, data = NULL, g = g))
  }

  # create table of requested charts from chartcodes
  refcodes <- mapply(FUN = chartcatalog::get_refcode,
                     chartcode = chartcodes2,
                     yname = ynames)
  charts <- tibble(chartcode = names(ynames),
                   yname = ynames,
                   refcode = refcodes)
  parsed <- sapply(charts$chartcode, parse_chartcode)
  parsed <- as_tibble(apply(parsed, 1L, unlist))
  charts <- tibble(charts, parsed)

  # create design/language groups that we cannot plot simultaneously
  # remove duplicate references within groups
  charts <- charts %>%
    mutate(design = ifelse(.data$design == "E", "B", .data$design)) %>%
    group_by(.data$design, .data$language) %>%
    distinct(.data$refcode, .keep_all = TRUE)

  # for development, take first group
  # TODO: should add list processing here, for every language/design group
  charts <- charts[group_indices(charts) == 1L, ]

  # return empty chart if target is not a list
  if (!is.list(target)) {
    g <- init_sds(design = charts$design[1L],
                  language = charts$language[1L],
                  ynames = charts$yname,
                  plot_library = plot_library)
    return(list(charts = charts, data = NULL, g = g))
  }

  # return empty chart if time target has zero rows
  data <- timedata(target) %>%
    filter(.data$yname %in% ynames)
  if (!nrow(data)) {
    g <- init_sds(design = charts$design[1L],
                  language = charts$language[1L],
                  ynames = charts$yname,
                  plot_library = plot_library)
    return(list(charts = charts, data = data, g = g))
  }

  # Calculate Z-scores in display metric


  # data$yt <- data$y
  # data$aget <- data$age
  # data$zd <- NA_real_
  data$gp <- NA_integer_
  dlist <- vector(mode = "list", length = nrow(charts))
  for (i in seq_len(length(dlist))) {
    yname <- charts$yname[i]
    chartcode <- charts$chartcode[i]
    df <- data[data$yname == charts$yname[i], ]
    df$gp <- i
    df$pop <- charts$population[i]
    df$sex <- charts$sex[i]
    df$zref <- charts$refcode[i]
    df$zd <- centile::y2z(y = df$y, x = df$x, refcode = df$zref,
                          pkg = "nlreferences", verbose = verbose, ...)
    dlist[[i]] <- df
  }
  data <- bind_rows(dlist)

  # add labels for legends and hovertext
  data <- data %>%
    mutate(yname_label = dplyr::case_match(.data$yname,
                                           "hgt" ~ "Lengte",
                                           "wgt" ~ "Gewicht naar leeftijd",
                                           "wfh" ~ "Gewicht naar lengte",
                                           "hdc" ~ "Hoofdomtrek",
                                           "bmi" ~ "Body Mass Index",
                                           "dsc" ~ "D-score"),
           yname_unit = dplyr::case_match(.data$yname,
                                          "hgt" ~ "cm",
                                          "wgt" ~ "kg",
                                          "wfh" ~ "kg",
                                          "hdc" ~ "cm",
                                          "bmi" ~ "kg/m^2",
                                          "dsc" ~ "D"),
    )

  # create template per design/language
  g <- init_sds(design = charts$design[1L],
                language = charts$language[1L],
                ynames = charts$yname,
                plot_library = plot_library)
  g <- add_sds_curves(g = g, data = data)

  if (draw_grob && inherits(g, "ggplot")) {
    grid.draw(g)
  }
  if (draw_grob && inherits(g, "plotly")) {
    print(g)
  }

  return(list(charts = charts, data = data, g = g))
}
