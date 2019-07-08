restore_factors <- function(data, f = NULL) {
  if (is.null(f)) return(data)
  for (i in 1:length(f)) {
    v <- f[i]
    if (v  %in% names(data)) data[, v] <- as.factor(data[, v, drop = TRUE])
  }
  data
}
