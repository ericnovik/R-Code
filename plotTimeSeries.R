ggplot_ts <- function(x, trellis = TRUE, scales = "free_y", ...) {
  # x is a data structure in a **wide** format that is:
  #    1) convertible to a dataframe
  #    2) contains dates as rownames()
  #    3) contains numerical observations in each column
  
  require('ggplot2')
  theme_set(theme_bw())
  
  require('reshape2')
  
  x <- as.data.frame(x)
  x$date <- as.Date(rownames(x))
  x <- melt(x, id = "date")
  p <- qplot(date, value, data = x, geom = "line", colour = variable, ...)
  if (trellis) {
    p <- p + facet_wrap( ~ variable, scales = scales, ...)  
  }
  p
}
