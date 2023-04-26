init_sds_plotly <- function(design,
                            language,
                            width = 16,
                            height = 12,
                            ...) {
  yname_colors <- c("#999999", "#E69F00", "#56B4E9", "#33B882",
                    "#F0E442", "#0072B2", "#FF0000BB", "#763A7E")
  names(yname_colors) <- c("unused1", "D-score", "Gewicht naar lengte", "Hoofdomtrek",
                           "unused2","Gewicht naar leeftijd", "Lengte", "Body Mass Index")
  tx <- switch(design,
               "A" = function(x) x * 12,
               function(x) x)
  xaxis_dtick <- switch(design,
                        "A" = 1,
                        "B" = 0.25,
                        "C" = 1,
                        1)
  xaxis_title <- switch(design,
                        "A" = "Leeftijd in maanden",
                        "B" = "Leeftijd in jaren/maanden",
                        "C" = "Leeftijd in jaren",
                        NULL)
  xaxis_range <- switch(design,
                        "A" = list(-0.5, 15.5),
                        "B" = list(-1/12, 4 + 1/12),
                        "C" = list(-0.5, 21.5),
                        NULL)
  xaxis_ticklabelstep <- switch(design,
                                "A" = 1,
                                "B" = 4,
                                "C" = 2,
                                1)
  yaxis_range <- list(-4, 4)
  yaxis_title <- switch(language,
                        "dutch" = "Standaard Deviatie Score (SDS)",
                        "english" = "Standard Deviation Score (SDS)",
                        "Standard Deviation Score (SDS)")
  ratio <- (diff(unlist(xaxis_range) * height)) / (diff(unlist(yaxis_range)) * width)
  data <- tibble(age = numeric(),
                 zd = numeric(),
                 yname_label = character(),
                 yname_unit = character(),
                 sex = character(),
                 gp = integer())

  g <- plot_ly(data = data,
               x = ~tx(age), y = ~zd,
               color = ~yname_label, colors = yname_colors,
               type = "scatter", mode = "marker") %>%
    config(toImageButtonOptions = list(
      format = 'svg',
      filename = paste(format(Sys.time(), "%Y%m%d-%H%M%S")),
      height = NULL,
      width = NULL),
      modeBarButtonsToRemove = c('select', 'lasso2d', 'zoomIn', 'zoomOut', 'autoScale'),
      displaylogo = FALSE) %>%
    layout(paper_bgcolor = "#F7F7F7F7",
           plot_bgcolor = "#FFFFFFFF",
           xaxis = list(
             constrain = "domain",
             constraintoward = "left",
             dtick = xaxis_dtick,
             range = xaxis_range,
             rangemode = "tozero",
             scaleanchor = "y",
             scaleratio = 1 / ratio,
             tick0 = 0,
             ticklabelstep = xaxis_ticklabelstep,
             title = xaxis_title,
             zeroline = FALSE
           ),
           yaxis = list(
             constrain = "domain",
             dtick = 0.5,
             range = yaxis_range,
             scaleanchor = "x",
             scaleratio = ratio,
             tick0 = 0,
             ticklabelstep = 2,
             title = yaxis_title,
             zeroline = TRUE,
             zerolinecolor = "#00AB6688",
             zerolinewidth = 2
           ),
           shapes = list(
             fillcolor = "#00AB66",
             line = list(width = 0),
             opacity = 0.1,
             type = "rect",
             x0 = 0, x1 = 30,
             y0 = -2, y1 = 2),
           legend = list(
             bgcolor = "transparent",
             x = 1,
             y = 1,
             xanchor = "right"
           )
    )
  return(g)
}
