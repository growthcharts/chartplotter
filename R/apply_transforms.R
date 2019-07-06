apply_transforms <- function(xyz,
                             id = rep(0L, nrow(xyz)),
                             chartcode, yname,
                             curve_interpolation) {
  tx <- get_tx(chartcode, yname)
  ty <- get_ty(chartcode, yname)
  sq <- get_seq(chartcode, yname)

  # transform
  if (sq == "tr" & !is.null(xyz$y)) xyz$y <- ty(xyz$y)

  # curve interpolation
  if (nrow(xyz) > 0L) {
    xyz$id  <- id
    xyz$obs <- TRUE
    if (curve_interpolation) {
      ref <- get_reference(chartcode, yname)
      xout <- set_xout(chartcode, yname)
      xyz <- curve_interpolation(data = xyz,
                                 xname = "x",
                                 yname = "y",
                                 xout = xout,
                                 reference = ref)
    }
  }

  # transform
  if (sq == "rt" & !is.null(xyz$y)) xyz$y <- ty(xyz$y)
  xyz$x <- tx(xyz$x)

  xyz
}

set_xout <- function(chartcode, yname) {
  # set xout equal to construction grid
  design <- substr(chartcode, 3L, 3L)
  if (design == "A") return(seq(0.5, 15, 0.5) / 12)
  if (design == "B" & yname == "wfh") return(seq(50, 120, by = 2))
  if (design == "B" & yname == "hgt") return(c(0.5, 0.75, 1:48) / 12)
  if (design == "B" & yname == "hdc") return(seq(0.1, 4, by = 0.1))
  if (design == "C" & yname == "wfh") return(seq(60, 184, by = 4))
  if (design == "C") return(seq(1, 21, by = 0.5))
  if (design == "E") return(c(0.5, 0.75, 1:48) / 12)
  numeric(0)
}

