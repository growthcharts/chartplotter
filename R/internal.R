apply_transforms_y <- function(y, chartcode, yname) {
  # transform ty(y) in reference-transform sequence
  ty <- get_ty(chartcode, yname)
  sq <- get_seq(chartcode, yname)
  if (sq == "rt") return(ty(y))
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
  if (design == "A") return(round(seq(0.5, 15, 0.5) / 12, 4L))
  if (design == "B" & yname == "wfh") return(round(seq(50, 120, by = 2), 4L))
  if (design == "B" & yname == "hgt") return(round(c(0.5, 0.75, 1:48) / 12, 4L))
  if (design == "B" & yname == "hdc") return(round(seq(0.1, 4, by = 0.1), 4L))
  if (design == "C" & yname == "wfh") return(round(seq(60, 184, by = 4), 4L))
  if (design == "C") return(round(seq(1, 21, by = 0.5), 4L))
  if (design == "E") return(round(c(0.5, 0.75, 1:48) / 12, 4L))
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
  if (is.null(f)) return(data)
  for (i in 1:length(f)) {
    v <- f[i]
    if (v  %in% names(data)) data[, v] <- as.factor(data[, v, drop = TRUE])
  }
  data
}
