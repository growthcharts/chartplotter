#' Create empty SDS plot
#'
#' @param design Either "A" (0-15m), "B" (0-4y) or "C" (0-21y)
#' @param language Either "dutch" or "english". Only "dutch" is currently
#' supported
#' @param ynames Character vector with `yname`. Needed only for ggplot2 to set the
#' legend with active trace lines.
#' @param plot_library Either "ggplot2" or "plotly"
#' @param \dots Argument passed down (`width = 16` and `height = 12`) to fixate the
#' aspect ratio.
#' @export
init_sds <- function(design = c("A", "B", "C"),
                     language = c("dutch", "english"),
                     ynames = "",
                     plot_library = c("plotly", "ggplot2"),
                     ...) {

  design <- match.arg(design)
  language <- match.arg(language)
  plot_library <- match.arg(plot_library)

  switch(plot_library,
         plotly = init_sds_plotly(design, language, ...),
         ggplot2 = init_sds_ggplot2(design, language, ynames, ...),
         init_sds_plotly(design, language, ...)
  )
}

init_sds_ggplot2 <- function(design,
                             language,
                             ynames,
                             width = 16,
                             height = 12,
                             ...) {
  # set the color palette for each trace
  cb_lines <- c("#999999", "#E69F00", "#56B4E9", "#33B882",
                "#F0E442", "#0072B2", "#FF0000BB", "#763A7E")
  names(cb_lines) <- c("unused1", "dsc", "wfh", "hdc",
                       "unused2","wgt", "hgt", "bmi")
  cb_symbols <- c("#00AB66", "#5CCAE8", "#E15D2A", "#2440C0",
                  "#1F8AFF", "#248BC0")
  names(cb_symbols) <- c("NL", "MA", "TU", "HS",
                         "PT", "WHO")
  ratio <- switch(design,
                  "A" = (15.5 / 8) / (width / height),
                  "B" = ((4 + 1/12) / 8) / (width / height),
                  "C" = (21 / 8) / (width / height),
                  1)
  tx <- switch(design,
               "A" = function(x) x * 12,
               function(x) x)
  ylab <- switch(language,
                 "dutch" = "Standaard Deviatie Score (SDS)",
                 "english" = "Standard Deviation Score (SDS)",
                 "Standard Deviation Score (SDS)")
  xlab <- switch(design,
                 "A" = "Leeftijd in maanden",
                 "B" = "Leeftijd in jaren/maanden",
                 "C" = "Leeftijd in jaren",
                 NULL)
  lim <- switch(design,
                "A" = list(x = c(-0.5, 15.5), y = c(-4, 4)),
                "B" = list(x = c(-1/12, 4 + 1/12), y = c(-4, 4)),
                "C" = list(x = c(-0.5, 21.5), y = c(-4, 4)),
                NULL)
  breaks <- switch(design,
                   "A" = list(x = seq(0, 15, 1), y = seq(-3, 3, 1)),
                   "B" = list(x = seq(0, 4, 0.25), y = seq(-3, 3, 1)),
                   "C" = list(x = seq(0, 20, 2), y = seq(-3, 3, 1)),
                   NULL)
  labels <- switch(design,
                   "A" = breaks,
                   "B" = list(x = c(0, 3, 6, 9, 1, 3, 6, 9, 2, 3, 6, 9, 3, 3, 6, 9, 4),
                              y = breaks$y),
                   "C" = list(x = seq(0, 20, 2), y = seq(-3, 3, 1)),
                   NULL)
  relsize <- switch(design,
                    "A" = 1.2,
                    "B" = c(rep(c(1.6, 1, 1, 1), 4), 1.6),
                    "C" = 1.2,
                    NULL)
  vline <- switch(design,
                  "A" = list(xintercept = seq(0, 15, 0.5),
                             lwd = c(0.3, rep(c(0.1, 0.3), 15))),
                  "B" = list(xintercept = seq(0, 4, 0.25),
                             lwd = c(0.3, rep(c(0.1, 0.1, 0.1, 0.3), 4))),
                  "C" = list(xintercept = seq(0, 21, 1),
                             lwd = c(rep(c(0.3, 0.1), 10), 0.3, 0.1)),
                  NULL)
  data <- tibble(age = numeric(),
                 zd = numeric(),
                 yname = character(),
                 sex = character(),
                 gp = integer())

  legend_labels <- c("Lengte",
                     "Gewicht naar leeftijd",
                     "Gewicht naar lengte",
                     "BMI",
                     "Hoofdomtrek",
                     "D-score")
  names(legend_labels) <- c("hgt", "wgt", "wfh", "bmi", "hdc", "dsc")
  selected <- intersect(names(legend_labels), ynames)
  legend_labels <- legend_labels[selected]

  g <- ggplot(data = data, aes(x = tx(.data$age), y = .data$zd,
                               colour = .data$yname,
                               shape = .data$sex, group = .data$gp)) +
    theme_light() +
    theme(plot.background = element_rect(fill = "#F7F7F7", colour = NA),
          panel.background = element_rect(fill = "white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(size = rel(relsize)),
          axis.text.y = element_text(size = rel(1.2)),
          legend.position = c(1, 0),
          legend.justification = c(1, 0),
          legend.text = element_text(size = 12),
          legend.background = element_rect(fill = "white", colour = "grey70", linewidth =  0.25),
          legend.key = element_blank()) +
    guides(colour = guide_legend(title = NULL)) +
    scale_color_manual(values = cb_lines,
                       limits = names(legend_labels),
                       labels = legend_labels) +
    scale_x_continuous(expand = expansion(), breaks = breaks$x,
                       labels = labels$x) +
    scale_y_continuous(expand = expansion(), breaks = breaks$y,
                       labels = labels$y) +
    coord_fixed(ratio = ratio, xlim = lim$x, ylim = lim$y) +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = -2, ymax = 2,
             fill = rgb(224, 241, 229, maxColorValue = 255)) +
    #geom_vline(xintercept = breaks$x, col = "#00AB66",
    #           lwd = c(rep(c(0.15, 0.3), 15))) +
    geom_hline(yintercept = seq(-3.5, 3.5, 1), lwd = 0.1, col = "#00AB66") +
    geom_hline(yintercept = -3:3, lwd = c(0.3, 0.3, 0.3, 0.5, 0.3, 0.3, 0.3),
               col = "#00AB66") +
    geom_vline(xintercept = vline$xintercept, lwd = vline$lwd, col = "#00AB66") +
    xlab(xlab) +
    ylab(ylab)

  return(g)
}
