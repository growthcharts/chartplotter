#' @importFrom brokenstick   get_knots
#' @importFrom centile       y2z z2y
#' @importFrom chartbox      load_chart
#' @importFrom chartcatalog  get_breakpoints get_seq
#'                           get_tx get_ty get_ynames parse_chartcode
#' @importFrom curvematching calculate_matches2 extract_matches
#' @importFrom donorloader   load_data
#' @importFrom dplyr         %>% all_of any_of arrange as_tibble bind_cols bind_rows
#'                           distinct filter first group_by left_join mutate
#'                           n pull rename select slice slice_tail
#'                           summarise tibble ungroup
#' @importFrom grDevices     palette
#' @importFrom grid          clipGrob editGrob gList gPath gTree gpar
#'                           grid.draw is.grob nullGrob pointsGrob
#'                           polylineGrob removeGrob segmentsGrob
#'                           setGrob
#' @importFrom nlreferences  set_refcodes
#' @importFrom rlang         .data :=
#' @importFrom stats         approx na.omit predict
#' @importFrom tidyr         drop_na pivot_longer uncount
#' @importFrom utils         hasName tail
NULL

globalVariables(".")
