#' @importFrom brokenstick   get_knots
#' @importFrom centile       y2z z2y
#' @importFrom chartbox      load_chart
#' @importFrom chartcatalog  get_breakpoints get_refcode get_reference_call
#'                           get_seq get_tx get_ty get_ynames parse_chartcode
#' @importFrom curvematching calculate_matches2 extract_matches
#' @importFrom donorloader   load_data
#' @importFrom dplyr         %>% all_of any_of arrange as_tibble bind_cols bind_rows
#'                           distinct filter first group_by group_indices
#'                           left_join mutate
#'                           n pull rename select slice slice_tail
#'                           summarise tibble ungroup
#' @importFrom ggplot2       aes annotate coord_fixed
#'                           element_blank element_rect element_text
#'                           expansion
#'                           geom_hline geom_line geom_point geom_vline
#'                           ggplot guides guide_legend rel
#'                           scale_color_manual scale_x_continuous scale_y_continuous
#'                           theme theme_light xlab ylab
#' @importFrom grDevices     palette rgb
#' @importFrom grid          clipGrob editGrob gList gPath gTree gpar
#'                           grid.draw is.grob nullGrob pointsGrob
#'                           polylineGrob removeGrob segmentsGrob
#'                           setGrob
#' @importFrom nlreferences  set_refcodes
#' @importFrom plotly        add_trace add_lines add_markers config layout
#'                           plot_ly plotly_build plotly_data
#' @importFrom rlang         .data :=
#' @importFrom stats         approx na.omit pnorm predict
#' @importFrom tidyr         drop_na pivot_longer uncount
#' @importFrom utils         hasName tail
NULL

globalVariables(".")
