# function for creating lagged versions of x up to order k
# x MUST be an xts object in order for merge() to work correctly
lagIt <- function(x, k) {
  stopifnot(is.xts(x))
  y <- x
  for (i in 1:k) {
    this <- lag(y, k = i, na.pad = FALSE)
    x <- merge.xts(x, this, join = "inner")
  }
  return(x)
}