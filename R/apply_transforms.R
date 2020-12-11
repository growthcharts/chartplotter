apply_transforms <- function(xyz,
                             id = rep(0L, nrow(xyz)),
                             chartcode, yname,
                             curve_interpolation) {

  tx <- get_tx(chartcode, yname)
  ty <- get_ty(chartcode, yname)
  sq <- get_seq(chartcode, yname)

  # FIXME
  # NOTE: It would be better if we define "rt" and "tr" as part of the
  # Z-score calculation.

  # transform ty(y) in transform-reference sequence if needed and possible
  if (sq == "tr" && hasName(xyz, "y")) xyz$y <- ty(xyz$y)

  # curve interpolation
  # skip Z-score calculation if xyz has a variable called "z"
  if (nrow(xyz) > 0L) {
    xyz$id  <- id
    xyz$obs <- TRUE
    if (curve_interpolation) {
      ref <- get_reference(chartcode, yname)
      xout <- set_xout(chartcode, yname)
      if (hasName(xyz, "z")) {
        xyz <- curve_interpolation(data = xyz,
                                   xname = "x",
                                   yname = "y",
                                   zname = "z",
                                   xout = xout,
                                   reference = ref)
      } else {
        xyz <- curve_interpolation(data = xyz,
                                   xname = "x",
                                   yname = "y",
                                   xout = xout,
                                   reference = ref)
      }
    }
  }

  # transform ty(y) in reference-transform sequence if needed
  if (sq == "rt") xyz$y <- ty(xyz$y)

  # apply x transform
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

