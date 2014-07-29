#source('Rstart.R')
#setwd('~/Google Drive/R/')

require('ggplot2')
require('plyr')
require('reshape2')

### Setting up backgound defaults

# change the default background color
old.theme <- theme_set(theme_bw())

## To change the panel's background color, use the following code:
# myplot + opts(panel.background = theme_rect(fill='green', colour='red'))

## To change the color of the plot (but not the color of the panel), you can do:
# myplot + opts(plot.background = theme_rect(fill='green', colour='red'))

set.seed(1410) # Make the sample reproducible
dsmall <- diamonds[sample(nrow(diamonds), 100), ]

qplot(carat, price, data = diamonds)
qplot(log(carat), log(price), data = diamonds)
qplot(carat, x * y * z, data = diamonds)

# Mapping point colour to diamond colour (left), and point shape to cut
# quality (right).
qplot(carat, price, data = dsmall, colour = color)
qplot(carat, price, data = dsmall, shape = cut)

# contrast this with plot()
# default color palette in R: palette()
#[1] "black"   "red"     "green3"  "blue"    "cyan"    "magenta" "yellow" 
#[8] "gray" 

# starting from red, as opposed to default black
png('iris.png', 640, 480)
par (mar=c(3,3,2,1), mgp=c(2,.7,0), tck=-.012, las=1)
with(iris, plot(Sepal.Length, Sepal.Width, col=as.numeric(Species)+1, pch=20))
lbs <- levels(iris$Species)
legend('topright', legend=lbs, 
       col=2:4, cex=0.7, pch=20, box.lwd=0.5, pt.cex=0.6)
dev.off()

# now with qplot in one line of code
png('irisq.png', 640, 480)
qplot(Sepal.Length, Sepal.Width, data = iris, colour = Species, xlim=c(4,8))
dev.off()


# Reducing the alpha value from 1/10 (left) to 1/100 (middle) to 1/200
# (right) makes it possible to see where the bulk of the points lie.
qplot(carat, price, data = diamonds, alpha = I(1/10))
qplot(carat, price, data = diamonds, alpha = I(1/100))
qplot(carat, price, data = diamonds, alpha = I(1/200))

# Smooth curves add to scatterplots of carat vs.\ price. The dsmall
# dataset (left) and the full dataset (right).
# If you want to turn the confidence interval off, use se = FALSE.
qplot(carat, price, data = dsmall, geom = c("point", "smooth"))
qplot(carat, price, data = diamonds, geom = c("point", "smooth"))

# The effect of the span parameter.  (Left) \code{span = 0.2}, and
# (right) \code{span = 1}.
qplot(carat, price, data = dsmall, geom = c("point", "smooth"), 
      span = 0.2)
qplot(carat, price, data = dsmall, geom = c("point", "smooth"), 
      span = 1)

# The effect of the formula parameter, using a generalised additive
# model as a smoother.  (Left) \code{formula = y ~ s(x)}, the default;
# (right) \code{formula = y ~ s(x, bs = "cs")}.
library(mgcv)
qplot(carat, price, data = dsmall, geom = c("point", "smooth"), 
      method = "gam", formula = y ~ s(x))
qplot(carat, price, data = dsmall, geom = c("point", "smooth"), 
      method = "gam", formula = y ~ s(x, bs = "cs"))

# The effect of the formula parameter, using a linear model as a
# smoother.  (Left) \code{formula = y ~ x}, the default; (right)
# \code{formula = y ~ ns(x, 5)}.
library(splines)
qplot(carat, price, data = dsmall, geom = c("point", "smooth"), 
      method = "lm")
qplot(carat, price, data = dsmall, geom = c("point", "smooth"), 
      method = "lm", formula = y ~ ns(x,5))

# Using jittering (left) and boxplots (right) to investigate the
# distribution of price per carat, conditional on colour.  As the
# colour improves (from left to right) the spread of values decreases,
# but there is little change in the centre of the distribution.
qplot(color, price / carat, data = diamonds, geom = "jitter")
qplot(color, price / carat, data = diamonds, geom = "boxplot")


# Varying the alpha level.  From left to right: $1/5$, $1/50$, $1/200$.
# As the opacity decreases we begin to see where the bulk of the data
# lies.  However, the boxplot still does much better.
qplot(color, price / carat, data = diamonds, geom = "jitter",
      alpha = I(1 / 5))
qplot(color, price / carat, data = diamonds, geom = "jitter",
      alpha = I(1 / 50))
qplot(color, price / carat, data = diamonds, geom = "jitter",
      alpha = I(1 / 200))

# Displaying the distribution of diamonds.  (Left) \code{geom =
# "histogram"} and (right) \code{geom = "density"}.
qplot(carat, data = diamonds, geom = "histogram")
qplot(carat, data = diamonds, geom = "density")

# Varying the bin width on a histogram of carat reveals interesting
# patterns.  Binwidths from left to right: 1, 0.1 and 0.01 carats. Only
# diamonds between 0 and 3 carats shown.
qplot(carat, data = diamonds, geom = "histogram", binwidth = 1, 
      xlim = c(0,3))
qplot(carat, data = diamonds, geom = "histogram", binwidth = 0.1,
      xlim = c(0,3))
qplot(carat, data = diamonds, geom = "histogram", binwidth = 0.01,
      xlim = c(0,3))

# Mapping a categorical variable to an aesthetic will automatically
# split up the geom by that variable.  (Left) Density plots are
# overlaid and (right) histograms are stacked.
qplot(carat, data = diamonds, geom = "density", colour = color)
qplot(carat, data = diamonds, geom = "histogram", fill = color)

# Change the background color
qplot(Sepal.Width, data = iris, geom = "density", 
      main='Sepal Width Densities', color = Species) +
      opts(panel.background = theme_rect(fill='white'))

qplot(Sepal.Width, data = iris, geom = "density", 
      main='Sepal Width Densities', color = Species) + theme_bw()

# Bar charts of diamond colour.  The left plot shows counts and the
# right plot is weighted by \code{weight = carat} to show the total
# weight of diamonds of each colour.
qplot(color, data = diamonds, geom = "bar")
qplot(color, data = diamonds, geom = "bar", weight = carat) +
  scale_y_continuous("carat")

# Two time series measuring amount of unemployment.  (Left) Percent of
# population that is unemployed and (right) median number of weeks
# unemployed.  Plots created with {\tt geom="line"}.
qplot(date, unemploy / pop, data = economics, geom = "line")
qplot(date, uempmed, data = economics, geom = "line")

# Path plots illustrating the relationship between percent of people
# unemployed and median length of unemployment.  (Left) Scatterplot
# with overlaid path.  (Right) Pure path plot coloured by year.
year <- function(x) as.POSIXlt(x)$year + 1900
qplot(unemploy / pop, uempmed, data = economics, 
      geom = c("point", "path"))
qplot(unemploy / pop, uempmed, data = economics, 
      geom = "path", colour = year(date)) + scale_area() + theme_bw()

# Histograms showing the distribution of carat conditional on colour.
# (Left) Bars show counts and (right) bars show densities (proportions
# of the whole).  The density plot makes it easier to compare
# distributions ignoring the relative abundance of diamonds within each
# colour. High-quality diamonds (colour D) are skewed towards small
# sizes, and as quality declines the distribution becomes more flat.
qplot(carat, data = diamonds, facets = color ~ ., 
      geom = "histogram", binwidth = 0.1, xlim = c(0, 3))
qplot(carat, ..density.., data = diamonds, facets = color ~ .,
      geom = "histogram", binwidth = 0.1, xlim = c(0, 3))

qplot(Sepal.Width, ..density.., data = iris, facets = Species ~ .,
      geom = "histogram", binwidth = 0.1)

qplot(Sepal.Width, data = iris, facets = Species ~ .,
      geom = "density")

qplot(
  carat, price, data = dsmall, 
  xlab = "Price ($)", ylab = "Weight (carats)",  
  main = "Price-weight relationship"
)

qplot(
  carat, price/carat, data = dsmall, 
  ylab = expression(frac(price,carat)), 
  xlab = "Weight (carats)",  
  main="Small diamonds", 
  xlim = c(.2,1)
)
qplot(carat, price, data = dsmall, log = "xy")

# A scatterplot of engine displacement in litres (displ) vs.  average
# highway miles per gallon (hwy).  Points are coloured according to
# number of cylinders.  This plot summarises the most important factor
# governing fuel economy: engine size.
qplot(displ, hwy, data = mpg, colour = factor(cyl))

# size, a colour and a shape. These attributes are called aesthetics
# Points, lines and bars are all examples of geometric objects, or geoms. 
# Geoms determine the “type” of the plot

# More complicated plots don't have their own names.  This plot takes
# Figure~\ref{fig:mpg} and adds a regression line to each group.  What
# would you call this plot?
qplot(displ, hwy, data=mpg, colour=factor(cyl)) + 
      geom_smooth(data=subset(mpg, cyl != 5), method="lm") + theme_bw()

# A more complex plot with facets and multiple layers.
qplot(displ, hwy, data=mpg, facets = . ~ year) + geom_smooth() + theme_bw()
qplot(Sepal.Length, Petal.Width, data=iris, facets = . ~ Species) +
      geom_smooth(method='lm') + theme_bw()

# Together, the data, mappings, stat, geom and position adjustment form a layer.

# Examples of legends from four different scales.  From left to right:
# continuous variable mapped to size, and to colour, discrete variable
# mapped to shape, and to colour.  The ordering of scales seems
# upside-down, but this matches the labelling of the $y$-axis: small
# values occur at the bottom.
x <- 1:10
y <- factor(letters[1:5])
qplot(x, x, size = x) + opts(keep = "legend_box")
qplot(x, x, 1:10, colour = x) + opts(keep = "legend_box")
qplot(y, y, 1:10, shape = y) + opts(keep = "legend_box")
qplot(y, y, 1:10, colour = y) + opts(keep = "legend_box")

# Examples of axes and grid lines for three coordinate systems:
# Cartesian, semi-log and polar. The polar coordinate system
# illustrates the difficulties associated with non-Cartesian
# coordinates: it is hard to draw the axes well.
x1 <- c(1,10)
y1 <- c(1, 5)
p <- qplot(x1, y1, geom="blank", xlab=NULL, ylab=NULL) 
p 
p + coord_trans(y="log10")
p + coord_polar()

p <- qplot(displ, hwy, data = mpg, colour = factor(cyl))
summary(p)
p
# Save plot object to disk
save(p, file = "plot.rdata")
# Load from disk
load("plot.rdata")
# Save png to disk
ggsave("plot.pdf", width = 8, height = 5)

# Chapter 4: Build a Plot layer by layer
# qplot(), permits only a single dataset and a single set of 
# aesthetic mappings. Need ggplot()

p <- ggplot(diamonds, aes(carat, price, colour = cut))
p <- p + layer(geom = "point")  
p
# layer(geom, geom_params, stat, stat_params, data, mapping, position)

p <- ggplot(diamonds, aes(x=carat))
p <- p + layer(
  geom = "bar", 
  geom_params = list(fill = "steelblue"),
  stat = "bin",
  stat_params = list(binwidth = 2)
)
p

# same as above
p <- ggplot(diamonds, aes(x=carat))
p <- p + geom_histogram(binwidth = 2, fill = "steelblue")
p

ggplot(msleep, aes(sleep_rem / sleep_total, awake)) + 
  geom_point()
# which is equivalent to
qplot(sleep_rem / sleep_total, awake, data = msleep)

# You can add layers to qplot too:
qplot(sleep_rem / sleep_total, awake, data = msleep) + 
  geom_smooth()
# This is equivalent to 
qplot(sleep_rem / sleep_total, awake, data = msleep, 
      geom = c("point", "smooth"))
# or
ggplot(msleep, aes(sleep_rem / sleep_total, awake)) + 
  geom_point() + geom_smooth()

p <- ggplot(msleep, aes(sleep_rem / sleep_total, awake))
summary(p)

p <- p + geom_point()
summary(p)

# best fit line
require(scales)
bestfit <- geom_smooth(method = "lm", se = F, 
                       colour = alpha("steelblue", 0.5), size = 2)
qplot(sleep_rem, sleep_total, data = msleep) + bestfit
qplot(awake, brainwt, data = msleep, log = "y") + bestfit
qplot(bodywt, brainwt, data = msleep, log = "xy") + bestfit

# modifying the plot by simply changing the original dataframe
p <- ggplot(mtcars, aes(mpg, wt, colour = cyl)) + geom_point()
p
mtcars <- transform(mtcars, mpg = mpg ^ 2)
p %+% mtcars

aes(x = weight, y = height, colour = age)

aes(weight, height, colour = sqrt(age))

p <- ggplot(mtcars)
summary(p)

p <- p + aes(wt, hp)
summary(p)

p <- ggplot(mtcars, aes(x = mpg, y = wt))
p + geom_point()

# Overriding aesthetics.  (Left) Overriding colour with {\tt
# factor(cyl)} and (right) overriding y-position with {\tt disp}
p + geom_point(aes(colour = factor(cyl)))
p + geom_point(aes(y = disp))

# difference between setting and mapping
p <- ggplot(mtcars, aes(mpg, wt))
p + geom_point(colour = "darkblue")  
p + geom_point(aes(colour = "darkblue"))

# The difference between (left) setting colour to \code{"darkblue"} and
# (right) mapping colour to \code{"darkblue"}.  When \code{"darkblue"}
# is mapped to colour, it is treated as a regular value and scaled with
# the default colour scale.  This results in pinkish points and a
# legend.
qplot(mpg, wt, data=mtcars, colour = I("darkblue"))
qplot(mpg, wt, data=mtcars, colour = "darkblue")


# (Left) Correctly specifying {\tt group = Subject} produces one line
# per subject.  (Right) A single line connects all observations.  This
# pattern is characteristic of an incorrect grouping aesthetic, and is
# what we see if the group aesthetic is omitted, which in this case is
# equivalent to {\tt group = 1}.
data(Oxboys, package="nlme")
p <- ggplot(Oxboys, aes(age, height, group = Subject)) + 
  geom_line()
p

qplot(age, height, data=Oxboys, group = Subject, geom="line")
qplot(age, height, data=Oxboys, geom="line")

p + geom_smooth(aes(group = Subject), method="lm", se = F)
p + geom_smooth(aes(group = 1), method="lm", size = 2, se = F)

boysbox <- ggplot(Oxboys, aes(Occasion, height)) + geom_boxplot()
boysbox
boysbox + geom_line(aes(group = Subject), colour = "#3366FF")

# (Left) If boxplots are used to look at the distribution of heights at
# each occasion (a discrete variable), the default grouping works
# correctly.  (Right) If trajectories of individual boys are overlaid
# with {\tt geom\_line()}, then {\tt aes(group = Subject)} is needed
# for the new layer.
qplot(Occasion, height, data=Oxboys, geom="boxplot")
qplot(Occasion, height, data=Oxboys, geom="boxplot") +
  geom_line(aes(group = Subject), colour="#3366FF")

# For lines and paths, the aesthetics of the line segment are
# determined by the aesthetic of the beginning observation.  If colour
# is categorical (left) there is no meaningful way to interpolate
# between adjacent colours.  If colour is continuous (right), there is,
# but this is not done by default.
df <- data.frame(x = 1:3, y = 1:3, colour = c(1,3,5))
qplot(x, y, data=df, colour=factor(colour), size = I(5)) + 
  geom_line(aes(group = 1), size = 2)
qplot(x, y, data=df, colour=colour, size = I(5)) + geom_line(size = 2)

xgrid <- with(df, seq(min(x), max(x), length = 50))
interp <- data.frame(
  x = xgrid,
  y = approx(df$x, df$y, xout = xgrid)$y,
  colour = approx(df$x, df$colour, xout = xgrid)$y  
)
qplot(x, y, data = df, colour = colour, size = I(5)) + 
  geom_line(data = interp, size = 2)

# Splitting apart a bar chart (left) produces a plot (right) that has
# the same outline as the original.
qplot(color, data = diamonds)
qplot(color, data = diamonds, fill = cut)

ggplot(diamonds, aes(carat)) + 
  geom_histogram(aes(y = ..density..), binwidth = 0.1)
# same as above
qplot(carat, ..density.., data = diamonds, geom="histogram", 
      binwidth = 0.1)

# Three position adjustments applied to a bar chart.  From left to
# right, stacking, filling and dodging.
dplot <- ggplot(diamonds, aes(clarity, fill = cut))
dplot + geom_bar(position = "stack")
dplot + geom_bar(position = "fill")
dplot + geom_bar(position = "dodge")

# The identity positon adjustment is not useful for bars, (left)
# because each bar obscures the bars behind.  (Right) It is useful for
# lines, however, because lines do not have the same problem.
dplot + geom_bar(position = "identity")
qplot(clarity, data = diamonds, geom="line", colour = cut, 
      stat="bin", group=cut)

d <- ggplot(diamonds, aes(carat)) + xlim(0, 3)
d + stat_bin(aes(ymax = ..count..), binwidth = 0.1, geom = "area")
d + stat_bin(
  aes(size = ..density..), binwidth = 0.1, 
  geom = "point", position="identity"
)
d + stat_bin(
  aes(y = 1, fill = ..count..), binwidth = 0.1, 
  geom = "tile", position="identity"
) 

# Three variations on the histogram. (Left) A frequency polygon;
# (middle) a scatterplot with both size and height mapped to frequency;
# (right) a heatmap representing frequency with colour.
d <- ggplot(diamonds, aes(carat)) + xlim(0, 3)
d + stat_bin(aes(ymax = ..count..), binwidth = 0.1, 
             geom = "area")
d + stat_bin(aes(size = ..density..), binwidth = 0.1, 
             geom = "point", position="identity")
d + stat_bin(aes(y=1, fill = ..count..), binwidth = 0.1, 
             geom = "tile", position="identity") + scale_y_continuous("")

require(nlme, quiet = TRUE, warn.conflicts = FALSE)
model <- lme(height ~ age, data = Oxboys, 
             random = ~ 1 + age | Subject)
oplot <- ggplot(Oxboys, aes(age, height, group = Subject)) + 
  geom_point() #change to geam_line()

age_grid <- seq(-1, 1, length = 10)
subjects <- unique(Oxboys$Subject)

preds <- expand.grid(age = age_grid, Subject = subjects)
preds$height <- predict(model, preds)

oplot + geom_line(data = preds, colour = "#3366FF", size= 0.4)

Oxboys$fitted <- predict(model)
Oxboys$resid <- with(Oxboys, fitted - height)

oplot %+% Oxboys + aes(y = resid) + geom_smooth(aes(group=1))

model2 <- update(model, height ~ age + I(age ^ 2))
Oxboys$fitted2 <- predict(model2)
Oxboys$resid2 <- with(Oxboys, fitted2 - height)

oplot %+% Oxboys + aes(y = resid2) + geom_smooth(aes(group=1))


## Chapter 5, Toolbox

library(effects)

# The basic geoms applied to the same data.  Many give rise to to named
# plots (from top left to bottom right): scatterplot, bar chart, line
# chart, area chart, path plot, labelled scatterplot, image/level plot
# and polygon plot.  Observe the different axis ranges for the bar,
# area and tile plots: these geoms take up space outside the range of
# the data, and so push the axes out.
df <- data.frame(
  x = c(3, 1, 5), 
  y = c(2, 4, 6), 
  label = c("a","b","c")
)
p <- ggplot(df, aes(x, y, label = label)) + 
  xlab(NULL) + ylab(NULL)
p + geom_point() + opts(title = "geom_point")
p + geom_bar(stat="identity") + 
  opts(title = "geom_bar(stat=\"identity\")")
p + geom_line() + opts(title = "geom_line")
p + geom_area() + opts(title = "geom_area")
p + geom_path() + opts(title = "geom_path")
p + geom_text() + opts(title = "geom_text")
p + geom_tile() + opts(title = "geom_tile")
p + geom_polygon() + opts(title = "geom_polygon")

# (Left) Never rely on the default parameters to get a revealing view
# of the distribution.  (Right) Zooming in on the x axis, {\tt xlim =
# c(55, 70)}, and selecting a smaller bin width, {\tt binwidth = 0.1},
# reveals far more detail. We can see that the distribution is slightly
# skew-right. Don't forget to include information about important
# parameters (like bin width) in the caption.
theme_set(old.theme)
qplot(depth, data=diamonds, geom="histogram")
qplot(depth, data=diamonds, geom="histogram", xlim=c(55, 70), binwidth=0.1)
old.theme <- theme_set(theme_bw())

# Three views of the distribution of depth and cut.  From top to
# bottom: faceted histogram, a conditional density plot, and frequency
# polygons.  All show an interesting pattern: as quality increases, the
# distribution shifts to the left and becomes more symmetric.
depth_dist <- ggplot(diamonds, aes(depth)) + xlim(58, 68)
depth_dist + 
  geom_histogram(aes(y = ..density..), binwidth = 0.1) +
  facet_grid(cut ~ .)
depth_dist + geom_histogram(aes(fill = cut), binwidth = 0.1, 
                            position = "fill")
depth_dist + geom_freqpoly(aes(y = ..density.., colour = cut), 
                           binwidth = 0.5) 

# The boxplot geom can be use to see the distribution of a continuous
# variable conditional on a discrete varable like cut (left), or
# continuous variable like carat (right).  For continuous variables,
# the group aesthetic must be set to get multiple boxplots.  Here {\tt
# group = round\_any(carat, 0.1, floor)} is used to get a boxplot for
# each 0.1 carat bin.
qplot(cut, depth, data=diamonds, geom="boxplot")
qplot(carat, depth, data=diamonds, geom="boxplot", 
      group = round_any(carat, 0.1, floor), xlim = c(0, 3))

# The jitter geom can be used to give a crude visualisation of 2d
# distributions with a discrete component.  Generally this works better
# for smaller datasets.  Car class vs. continuous variable city mpg
# (top) and discrete variable drive train (bottom).
qplot(class, cty, data=mpg, geom="jitter")
qplot(class, drv, data=mpg, geom="jitter")

# The density plot is a smoothed version of the histogram.  It has
# desirable theoretical properties, but is more difficult to relate
# back to the data.  A density plot of depth (left), coloured by cut
# (right).
qplot(depth, data=diamonds, geom="density", xlim = c(54, 70))
qplot(depth, data=diamonds, geom="density", xlim = c(54, 70), 
      fill = cut, alpha = I(0.2))

# Modifying the glyph used can help with mild to moderate overplotting.
# From left to right: the default shape, {\tt shape = 1} (hollow
# points), and {\tt shape= "."} (pixel points).
df <- data.frame(x = rnorm(5000), y = rnorm(5000))
norm <- ggplot(df, aes(x, y))
norm + geom_point()
norm + geom_point(shape = 4, colour='red')
norm + geom_point(shape = ".") # Pixel sized

# Using alpha blending to alleviate overplotting in sample data from a
# bivariate normal.  Alpha values from left to right: 1/3, 1/5, 1/10.
norm + geom_point(colour = "black", alpha = 1/3)
norm + geom_point(colour = "black", alpha = 1/5)
norm + geom_point(colour = "black", alpha = 1/10)

# A plot of table vs. depth from the diamonds data, showing the use of
# jitter and alpha blending to alleviate overplotting in discrete data.
# From left to right: geom point, geom jitter with default jitter, geom
# jitter with horizontal jitter of 0.5 (half the gap between bands),
# alpha of 1/10, alpha of 1/50, alpha of 1/200.
td <- ggplot(diamonds, aes(table, depth)) + 
  xlim(50, 70) + ylim(50, 70)
td + geom_point()
td + geom_jitter()
jit <- position_jitter(width = 0.5)
td + geom_jitter(position = jit)
td + geom_jitter(position = jit, colour = "black", alpha = 1/3)
td + geom_jitter(position = jit, colour = "black", alpha = 1/5)
td + geom_jitter(position = jit, colour = "black", alpha = 1/10)

# Binning with, top row, square bins, and bottom row, hexagonal bins.
# Left column uses default parameters, middle column {\tt bins = 10},
# and right column {\tt binwidth = c(0.02, 200)}.  Legends have been
# omitted to save space.
d <- ggplot(diamonds, aes(carat, price)) +
  opts(legend.position = "none")
d + stat_bin2d()
d + stat_bin2d(bins = 10) + scale_colour_gradient(high = "red") 
d + stat_bin2d(binwidth=c(0.02, 200)) 
d + stat_binhex()
d + stat_binhex(bins = 10)
d + stat_binhex(binwidth=c(0.02, 200))

# Using density estimation to model and visualise point densities.
# (Top) Image displays of the density; (bottom) point and contour based
# displays.
d <- ggplot(diamonds, aes(carat, price)) + 
  opts(legend.position = "none")
d + geom_point() + geom_density2d()
d + stat_density2d(geom = "point", aes(size = ..density..), 
                   contour = F) + scale_area(0.2, 1.5)
d + stat_density2d(geom = "tile", aes(fill = ..density..), 
                   contour = F) 
last_plot() + scale_fill_gradient(limits = c(1e-5,8e-4))

d <- ggplot(iris, aes(Sepal.Length, Petal.Width))
d + geom_point() + geom_density2d()

d <- ggplot(iris, aes(Sepal.Length, Petal.Width))
d + geom_point() + geom_density2d(aes(Petal.Length))
require(rgl)
with(iris, plot3d(Sepal.Length, Petal.Width, Petal.Length, col=Species))

# Example using the borders function.  (Left) All cities with
# population (as of January 2006) of greater than half a million,
# (right) cities in Texas.
library(maps)
data(us.cities)
big_cities <- subset(us.cities, pop > 500000)
qplot(long, lat, data = big_cities) + borders("state", size = 0.1) +
  scale_x_continuous(breaks = NA) + scale_y_continuous(breaks = NA) +
  xlab("") + ylab("")

tx_cities <- subset(us.cities, country.etc == "TX")
ggplot(tx_cities, aes(long, lat)) +
  borders("county", "texas", colour = "grey70") +
  geom_point(colour = "black", alpha = 0.5)

# Two choropleth maps showing number of assaults (left) and the ratio
# of assaults to murders (right).
states <- map_data("state")
arrests <- USArrests
names(arrests) <- tolower(names(arrests))
arrests$region <- tolower(rownames(USArrests))
choro <- merge(states, arrests, sort=FALSE, by = "region")

# Reorder the rows because order matters when drawing polygons
# and merge destroys the original ordering
choro <- choro[order(choro$order), ]
qplot(long, lat, data = choro, group = group, 
      fill = assault, geom = "polygon")

qplot(long, lat, data = choro, group = group, 
      fill = assault / murder, geom = "polygon")

qplot(long, lat, data = choro, group = group, 
      fill = rape / murder, geom = "polygon")

scale_colour_continuous <- function(...) scale_colour_gradient(low =
   "blue", high = "red", na.value="grey50", ...)

ia <- map_data("county", "iowa")
mid_range <- function(x) mean(range(x, na.rm = TRUE))
centres <- ddply(ia, .(subregion), 
                 colwise(mid_range, .(lat, long)))
ggplot(ia, aes(long, lat)) + 
  geom_polygon(aes(group = group), 
               fill = NA, colour = "grey60") +
                 geom_text(aes(label = subregion), data = centres, 
                           size = 5, angle = 45)

d <- subset(diamonds, carat < 2.5 & 
  rbinom(nrow(diamonds), 1, 0.2) == 1)
d$lcarat <- log10(d$carat)
d$lprice <- log10(d$price)

# Remove overall linear trend
detrend <- lm(lprice ~ lcarat, data = d)
d$lprice2 <- resid(detrend)

mod <- lm(lprice2 ~ lcarat * color, data = d)

library(effects)
effectdf <- function(...) {
  suppressWarnings(as.data.frame(effect(...)))
}
color <- effectdf("color", mod)
both1 <- effectdf("lcarat:color", mod)

carat <- effectdf("lcarat", mod, default.levels = 50)
both2 <- effectdf("lcarat:color", mod, default.levels = 3)

# Data transformed to remove most obvious effects.  (Left) Both x and y
# axes are log10 transformed to remove non-linearity.  (Right) The
# major linear trend is removed.
qplot(lcarat, lprice, data=d, colour = color)
qplot(lcarat, lprice2, data=d, colour = color)

# Displaying uncertainty in model estimates for colour.  (Left)
# Marginal effect of colour.  (Right) conditional effects of colour for
# different levels of carat.  Error bars show 95\% pointwise confidence
# intervals.
fplot <- ggplot(mapping = aes(y = fit, ymin = lower, ymax = upper)) +
  ylim(range(both2$lower, both2$upper))
fplot %+% color + aes(x = color) + geom_point() + geom_errorbar()

scale_colour_continuous <- function(...) scale_colour_gradient(low =
  "blue", high = "red", na.value="grey50", ...)

fplot %+% both2 + 
  aes(x = color, colour = lcarat, group = interaction(color, lcarat)) +
  geom_errorbar() + geom_line(aes(group=lcarat)) + scale_colour_continuous()
#  scale_colour_gradient()

# Displaying uncertainty in model estimates for carat.  (Left) marginal
# effect of carat.  (Right) conditional effects of carat for different
# levels of colour.  Bands show 95\% point-wise confidence intervals.
fplot %+% carat + aes(x = lcarat) + geom_smooth(stat="identity")

ends <- subset(both1, lcarat == max(lcarat))
fplot %+% both1 + aes(x = lcarat, colour = color) +
  geom_smooth(stat="identity") + 
  scale_colour_hue() + opts(legend.position = "none") +
  geom_text(aes(label = color, x = lcarat + 0.02), ends)

# Examples of \code{stat_summary} in use.  (Top) Continuous x with,
# from left to right, median and line, \f{median_hilow} and smooth,
# mean and line, and \f{mean_cl_boot} and smooth.  (Bottom) Discrete x
# with, from left to right, \f{mean} and point, \f{mean_cl_normal} and
# error bar, \f{median_hilow} and point range, and \f{median_hilow} and
# crossbar.  Note that \ggplot displays the full range of the data, not
# just the range of the summary statistics.
m <- ggplot(movies, aes(year, rating))
m + stat_summary(fun.y = "median", geom = "line")
m + stat_summary(fun.data = "median_hilow", geom = "smooth")
m + stat_summary(fun.y = "mean", geom = "line")
m + stat_summary(fun.data = "mean_cl_boot", geom = "smooth")
m2 <- ggplot(movies, aes(round(rating), log10(votes)))
m2 + stat_summary(fun.y = "mean", geom = "point")
m2 + stat_summary(fun.data = "mean_cl_normal", geom = "errorbar")
m2 + stat_summary(fun.data = "median_hilow", geom = "pointrange")
m2 + stat_summary(fun.data = "median_hilow", geom = "crossbar")

midm <- function(x) mean(x, trim = 0.5)
m2 + 
  stat_summary(aes(colour = "trimmed"), fun.y = midm, 
               geom = "point") +
                 stat_summary(aes(colour = "raw"), fun.y = mean, 
                              geom = "point") + 
                                scale_colour_hue("Mean")

iqr <- function(x, ...) {
  qs <- quantile(as.numeric(x), c(0.25, 0.75), na.rm = T)
  names(qs) <- c("ymin", "ymax")
  qs
}
m + stat_summary(fun.data = "iqr", geom="ribbon")

# plot annotations
unemp <- qplot(date, unemploy, data=economics, geom="line", 
               xlab = "", ylab = "No. unemployed (1000s)")

presidential <- presidential[-(1:3), ]

yrng <- range(economics$unemploy)
xrng <- range(economics$date)
#unemp + geom_vline(aes(xintercept = start), data = presidential)

require(scales)
unemp + geom_rect(aes(NULL, NULL, xmin = start, xmax = end, 
                      fill = party), ymin = yrng[1], ymax = yrng[2], alpha=0.2,
                  data = presidential) + scale_fill_manual(values = 
                  c("blue", "red"))

last_plot() + geom_text(aes(x = start, y = yrng[1], label = name), 
                        data = presidential, size = 3, hjust = 0, vjust = 0)

caption <- paste(strwrap("Unemployment rates in the US have 
  varied a lot over the years", 40), collapse="\n")

unemp + geom_text(aes(x, y, label = caption), 
                  data = data.frame(x = xrng[2], y = yrng[2]), 
                  hjust = 1, vjust = 1, size = 4)

highest <- subset(economics, unemploy == max(unemploy))
unemp + geom_point(data = highest, 
                   size = 3, colour = alpha("red", 0.5))

# Using size to display weights.  No weighting (left), weighting by
# population (centre) and by area (right).
qplot(percwhite, percbelowpoverty, data = midwest)
qplot(percwhite, percbelowpoverty, data = midwest, 
      size = poptotal / 1e6) + scale_area("Population\n(millions)", 
                                          breaks = c(0.5, 1, 2, 4))
qplot(percwhite, percbelowpoverty, data = midwest, size = area) + 
  scale_area()

# An unweighted line of best fit (left) and weighted by population size
# (right).
lm_smooth <- geom_smooth(method = lm, size = 1)
qplot(percwhite, percbelowpoverty, data = midwest) + lm_smooth 
qplot(percwhite, percbelowpoverty, data = midwest, 
      weight = popdensity, size = popdensity) + lm_smooth

# The difference between an unweighted (left) and weighted (right)
# histogram.  The unweighted histogram shows number of counties, while
# the weighted histogram shows population.  The weighting considerably
# changes the interpretation!
qplot(percbelowpoverty, data = midwest, binwidth = 1)
qplot(percbelowpoverty, data = midwest, weight = poptotal, 
      binwidth = 1) + ylab("population")

# scales, axes and legends
plot <- qplot(cty, hwy, data = mpg)
plot

# This doesn't work because there is a mismatch between the
# variable type and the default scale
plot + aes(x = drv)

# Correcting the default manually resolves the problem.
plot + aes(x = drv) + scale_x_discrete()

# Adjusting the default parameters of a scale. (Top left) The plot with
# default scale.  (Top right) Adding the default scale by hand doesn't
# change the appearance of the plot.  (Bottom left) Adjusting the
# parameters of the scale to tweak the legend.  (Bottom right) Using a
# different colour scale: Set1 from the ColorBrewer colours.
p <- qplot(sleep_total, sleep_cycle, data = msleep, colour = vore)
p 
# Explicitly add the default scale
p + scale_colour_hue()

# Adjust parameters of the default, here changing the appearance 
# of the legend
p + scale_colour_hue("What does\nit eat?", 
                     breaks = c("herbi", "carni", "omni", NA), 
                     labels = c("plants", "meat", "both", "don't know"))

# Use a different scale
p + scale_colour_brewer(palette = "Set2")

# try my own
p <- qplot(Petal.Length, Sepal.Width, data=iris, colour=Species)
p
p + scale_colour_hue()
p + scale_colour_brewer(palette = "Set1")

# A demonstration of the different forms legend title can take.
p <- qplot(cty, hwy, data = mpg, colour = displ)
p
p + scale_x_continuous("City mpg")
p + xlab("City mpg")
p + ylab("Highway mpg")
p + labs(x = "City mpg", y = "Highway", colour = "Displacement") +
  scale_colour_gradient(breaks = c(2,7))

p + xlab(expression(frac(miles, gallon)))

# The difference between breaks and limits.  (Left) default plot with
# {\tt limits = c(4, 8), breaks = 4:8}, (middle) {\tt breaks =
# c(5.5,6.5)} and (right) {\tt limits = c(5.5,6.5)}.  The effect on the
# x axis (top) and colour legend (bottom)
p <- qplot(cyl, wt, data = mtcars)
p
p + scale_x_continuous(breaks = c(5.5, 6.5))
p + scale_x_continuous(limits = c(5.5, 6.5))
p <- qplot(wt, cyl, data = mtcars, colour = cyl)
p
p + scale_colour_gradient(breaks = c(5.5, 6.5))
p + scale_colour_gradient(limits = c(5.5, 6.5))

# A scatterplot of diamond price vs.\ carat illustrating the difference
# between log transforming the scale (left) and log transforming the
# data (right).  The plots are identical, but the axis labels are
# different.
qplot(log10(carat), log10(price), data = diamonds)
qplot(carat, price, data = diamonds) + 
  scale_x_log10() + scale_y_log10()

# A time series of personal savings rate.  (Left) The default
# appearance, (middle) breaks every 10 years, and (right) scale
# restricted to 2004, with YMD date format.  Measurements are recorded
# at the end of each month.
plot <- qplot(date, psavert, data = economics, geom = "line") + 
  ylab("Personal savings rate") +
  geom_hline(xintercept = 0, colour = "grey50")
plot
plot + scale_x_date(breaks = "5 years")
plot + scale_x_date(
  limits = as.Date(c("2004-01-01", "2005-01-01"))
)

# Density of eruptions with three colour schemes.  (Left) Default
# gradient colour scheme, (middle) customised gradient from white to
# black and (right) 3 point gradient with midpoint set to the mean
# density.
f2d <- with(faithful, MASS::kde2d(eruptions, waiting, 
                                  h = c(1, 10), n = 50))
df <- with(f2d, cbind(expand.grid(x, y), as.vector(z)))
names(df) <- c("eruptions", "waiting", "density")
erupt <- ggplot(df, aes(waiting, eruptions, fill = density)) +
  geom_tile() +
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0))
erupt + scale_fill_gradient(limits = c(0, 0.04))
erupt + scale_fill_gradient(limits = c(0, 0.04), 
                            low = "white", high = "black") 
erupt + scale_fill_gradient2(limits = c(-0.04, 0.04), 
                             midpoint = mean(df$density)) 


# Gradient colour scales using perceptually well-formed palettes
# produced by the \code{vcd} package.  From left to right: sequential,
# diverging and heat hcl palettes.  Each scale is produced with
# \code{scale_fill_gradientn} with \code{colours} set to
# \code{rainbow_hcl(7)}, \code{diverge_hcl(7)} and \code{heat_hcl(7)}.
library(vcd)
fill_gradn <- function(pal) {
  scale_fill_gradientn(colours = pal(7), limits = c(0, 0.04))
}
erupt + fill_gradn(rainbow_hcl)
erupt + fill_gradn(diverge_hcl)
erupt + fill_gradn(heat_hcl)

RColorBrewer::display.brewer.all()

# Three ColorBrewer palettes, Set1 (left), Set2 (middle) and Pastel1
# (right), applied to points (top) and bars (bottom).  Bright colours
# work well for points, but are overwhelming on bars.  Subtle colours
# work well for bars, but are hard to see on points.
point <- qplot(brainwt, bodywt, data = msleep, log = "xy", 
               colour = vore)
area <- qplot(log10(brainwt), data = msleep, fill = vore, 
              binwidth = 1)

point + scale_colour_brewer(palette = "Set1")
point + scale_colour_brewer(palette = "Set2")
point + scale_colour_brewer(palette = "Pastel1")
area + scale_fill_brewer(palette = "Set1")
area + scale_fill_brewer(palette = "Set2")
area + scale_fill_brewer(palette = "Pastel1")

# Scale manual used to create custom colour (left and middle) and shape
# (right) scales.
plot <- qplot(brainwt, bodywt, data = msleep, log = "xy")
plot + aes(colour = vore) + 
  scale_colour_manual(values = c("red", "orange", "yellow", 
                                "green", "blue"))
colours <- c(carni = "red", "NA" = "orange", insecti = "yellow", 
             herbi = "green", omni = "blue")
#plot + aes(colour = vore) + scale_colour_manual(values = colours)
plot + aes(shape = vore) + 
  scale_shape_manual(values = c(1, 2, 6, 0, 23))

huron <- data.frame(year = 1875:1972, level = LakeHuron)
ggplot(huron, aes(year)) +
  geom_line(aes(y = level - 5), colour = "blue") + 
  geom_line(aes(y = level + 5), colour = "red")

ggplot(huron, aes(year)) +
  geom_line(aes(y = level - 5, colour = "below")) + 
  geom_line(aes(y = level + 5, colour = "above"))

ggplot(huron, aes(year)) +
  geom_line(aes(y = level - 5, colour = "below")) + 
  geom_line(aes(y = level + 5, colour = "above")) + 
  scale_colour_manual("Direction", values=c("below" = "blue", "above" = "red"))

require('pprobeData')

date <- row.names(xassetPrices)
assets <- data.frame(date, xassetPrices)
ggplot(assets, aes(as.Date(date))) + 
  geom_line(aes(y=XA101), colour='red') +
  geom_line(aes(y=XA103), colour='blue')

qplot(date, pop, data=economics, geom="line")
qplot(date, pop, data=economics, geom="line", log="y")
qplot(date, pop, data=subset(economics, date > as.Date("2006-1-1")), geom="line")
qplot(date, pop, data=economics, size=unemploy/pop, geom="line")

# A plot of R colours in Luv space.  A legend is unnecessary, because
# the colour of the points represents itself: the data and aesthetic
# spaces are the same.
x <- colors()
luv <- as.data.frame(convertColor(t(col2rgb(x)), "sRGB", "Luv"))
qplot(u, v, data=luv, colour = x, size = I(3)) + scale_colour_identity() +
  coord_equal()

# Legends produced by geom: point, line, point and line, and bar.
p <- ggplot(diamonds[1:100, ], aes(price, carat, colour = cut)) +  
  opts(keep = "legend_box")
p + geom_point()
p + geom_line()
p + geom_point() + geom_line()
p + geom_bar(binwidth = 100) + aes(fill = cut, y = ..count..)

# Colour legend, shape legend, colour + shape legend.
p <- ggplot(diamonds[1:100, ], aes(price, carat)) +  
  geom_point() + 
  opts(keep = "legend_box")
p + aes(colour = cut)
p + aes(shape = cut)
p + aes(shape = cut, colour = cut)
############# 3d plotting

# 1) static 3d plotting
require('scatterplot3d')
head(iris)
cl <- vector(mode="character", length=nrow(iris))
cl[iris$Species=="setosa"] <- "red"
cl[iris$Species=="versicolor"] <- "green"
cl[iris$Species=="virginica"] <- "blue"

with(iris, scatterplot3d(Sepal.Length, Sepal.Width, Petal.Length, 
                         color=cl, pch=20))

# 2) dynamic 3d plotting
require('rgl')
open3d()
with(iris, plot3d(Sepal.Length, Sepal.Width, Petal.Length, 
                  col=(as.numeric(Species)+1)))

# Chapter 7 Positioning 
mpg2 <- subset(mpg, cyl != 5 & drv %in% c("4", "f"))

#qplot(cty, hwy, data = mpg2) + facet_grid(. ~ .)

qplot(cty, hwy, data = mpg2) + facet_grid(. ~ cyl)

qplot(cty, data = mpg2, geom="histogram", binwidth = 2) +
  facet_grid(cyl ~ .)

qplot(cty, hwy, data = mpg2) + facet_grid(drv ~ cyl)

# Graphical margins work like margins of a contingency table to give
# unconditioned views of the data.  A plot faceted by number of
# cylinders and drive train (left) is supplemented with margins
# (right).
p <- qplot(displ, hwy, data = mpg2) +
  geom_smooth(method = "lm", se = F)
p + facet_grid(cyl ~ drv) 
p + facet_grid(cyl ~ drv, margins = T)

qplot(displ, hwy, data = mpg2) + 
  geom_smooth(aes(colour = drv), method = "lm", se = F) + 
  facet_grid(cyl ~ drv, margins = T) 

# Movie rating distribution by decade.
movies$decade <- round_any(movies$year, 10, floor)
qplot(rating, ..density.., data=subset(movies, decade > 1890),
      geom="histogram", binwidth = 0.5) + 
        facet_wrap(~ decade, ncol = 4)

genre <- movies[, 18:24]
genre <- melt(genre)
genre.counts <- dcast(genre, variable ~ value)
colnames(genre.counts) <- c('genre', 'no', 'yes')
qplot(genre, yes, data=genre.counts, geom="bar", fill=genre)
qplot(rating, votes, data=movies, size=budget, colour=as.factor(Action))

# Fixed scales (left) have the same scale for each facet, while free
# scales (right) have a different scale for each facet.
p <- qplot(cty, hwy, data = mpg)
p + facet_wrap(~ cyl)
p + facet_wrap(~ cyl, scales = "free")

# Free scales are particularly useful when displaying multiple time
# series measured on different scales.
em <- melt(economics, id = "date")
qplot(date, value, data = em, geom = "line", group = variable) + 
  facet_grid(variable ~ ., scale = "free_y")

qplot(date, value, data = em, geom = "line") + 
  facet_grid(variable ~ ., scale = "free_y")

# A dotplot showing the range of city gas mileage for each model of
# car. (Left) Models ordered by average mpg, and (right) faceted by
# manufacturer with \code{scales="free_y"} and \code{space = "free"}.
# The {\tt strip.text.y} theme setting has been used to rotate the
# facet labels.
mpg3 <- within(mpg2, {
  model <- reorder(model, cty)
  manufacturer <- reorder(manufacturer, -cty)
})
models <- qplot(cty, model, data = mpg3)

models
models + facet_grid(manufacturer ~ ., scales = "free", 
                    space = "free") +  opts(strip.text.y = theme_text())

# The differences between faceting vs. grouping, illustrated with a
# log-log plot of carat vs. price with four selected colours.
xmaj <- c(0.3, 0.5, 1,3, 5)
xmin <- as.vector(outer(1:10, 10^c(-1, 0)))
ymaj <- c(500, 1000, 5000, 10000)
ymin <- as.vector(outer(1:10, 10^c(2,3,4)))
dplot <- ggplot(subset(diamonds, color %in% c("D","E","G","J")), 
                aes(carat, price, colour = color)) + 
                  scale_x_log10(breaks = xmaj, labels = xmaj, minor = xmin) + 
                  scale_y_log10(breaks = ymaj, labels = ymaj, minor = ymin) + 
                  scale_colour_hue(limits = levels(diamonds$color)) + 
                  opts(legend.position = "none")

dplot + geom_point()
dplot + geom_point() + facet_grid(. ~ color)

dplot + geom_smooth(method = lm, se = F, fullrange = T)
dplot + geom_smooth(method = lm, se = F, fullrange = T) + 
  facet_grid(. ~ color)

# Dodging (top) vs. faceting (bottom) for a completely crossed pair of
# variables.
qplot(color, data=diamonds, geom = "bar", fill = cut, 
      position="dodge")
qplot(cut, data = diamonds, geom = "bar", fill = cut) + 
  facet_grid(. ~ color) + 
  opts(axis.text.x = theme_text(angle = 90, hjust = 1, size = 8, 
                                colour = "grey50"))

# For nested data, there is a clear advantage to faceting (top and
# middle) compared to dodging (bottom), because it is possible to
# carefully control and label the facets.  For this example, the top
# plot is not useful, but it will be useful in situations where the
# data is almost crossed, i.e.\ where a single combination is missing.
mpg4 <- subset(mpg, manufacturer %in% 
  c("audi", "volkswagen", "jeep"))
mpg4$manufacturer <- as.character(mpg4$manufacturer)
mpg4$model <- as.character(mpg4$model)

base <- ggplot(mpg4, aes(fill = model)) + 
  geom_bar(position = "dodge") + 
  opts(legend.position = "none")

base + aes(x = model) + 
  facet_grid(. ~ manufacturer)

last_plot() +  
  facet_grid(. ~ manufacturer, scales = "free_x", space = "free")
base + aes(x = manufacturer)

# Three ways of breaking a continuous variable into discrete bins.
# From top to bottom: bins of length one, six bins of equal length, six
# bins containing equal numbers of points.
mpg2$disp_ww <- cut_interval(mpg2$displ, length = 1)
mpg2$disp_wn <- cut_interval(mpg2$displ, n = 6)
mpg2$disp_nn <- cut_number(mpg2$displ, n = 6)

plot <- qplot(cty, hwy, data = mpg2) + labs(x = NULL, y = NULL)
plot + facet_wrap(~ disp_ww, nrow = 1)
plot + facet_wrap(~ disp_wn, nrow = 1)
plot + facet_wrap(~ disp_nn, nrow = 1)

# A set of examples illustrating what a line and rectangle look like
# when displayed in a variety of coordinate systems.  From top left to
# bottom right: Cartesian, polar with x position mapped to angle, polar
# with y position mapped to angle, flipped, transformed with log in y
# direction, and equal scales.
rect <- data.frame(x = 50, y = 50)
line <- data.frame(x = c(1, 200), y = c(100, 1))
base <- ggplot(mapping = aes(x, y)) + 
  geom_tile(data = rect, aes(width = 50, height = 50)) + 
  geom_line(data = line)
base
base + coord_polar("x")
base + coord_polar("y")
base + coord_flip()
base + coord_trans(y = "log10")
base + coord_equal()

# How coordinate transformations work: converting a line in Cartesian
# coordinates to a line in polar coordinates.  The original x position
# is converted to radius, and the y position to angle.
r_grid <- seq(0, 1, length = 15)
theta_grid <- seq(0, 3 / 2 * pi, length = 15)
extents <- data.frame(r = range(r_grid), theta = range(theta_grid))
base <- ggplot(extents, aes(r, theta)) + opts(aspect.ratio = 1) +
  scale_y_continuous(expression(theta))

base + geom_point(colour = "red", size = 4) + geom_line()
pts <- data.frame(r = r_grid, theta = theta_grid)
base + geom_line() + geom_point(data = pts)
base + geom_point(data = pts)

xlab <- scale_x_continuous(expression(x == r * sin(theta)))
ylab <- scale_y_continuous(expression(x == r * cos(theta)))
polar <- base %+% pts + aes(x = r * sin(theta), y = r * cos(theta)) + 
  xlab + ylab
polar + geom_point()
polar + geom_point() + geom_path()
polar + geom_point(data=extents, colour = "red", size = 4) + geom_path() 

# Setting limits on the coordinate system, vs setting limits on the
# scales.  (Left) Entire dataset; (middle) x scale limits set to (325,
# 500); (right) coordinate system x limits set to (325, 500).  Scaling
# the coordinate limits performs a visual zoom, while setting the scale
# limits subsets the data and refits the smooth.
(p <- qplot(disp, wt, data=mtcars) + geom_smooth())
p + scale_x_continuous(limits = c(325, 500))
p + coord_cartesian(xlim = c(325, 500))

# Setting limits on the coordinate system, vs. setting limits on the
# scales.  (Left) Entire dataset; (middle) x scale limits set to (0,
# 2); (right) coordinate x limits set to (0, 2).  Compare the size of
# the bins: when you set the scale limits, there are the same number of
# bins but they each cover a smaller region of the data; when you set
# the coordinate limits, there are fewer bins and they cover the same
# amount of data as the original.
(d <- ggplot(diamonds, aes(carat, price)) + 
  stat_bin2d(bins = 25, colour="grey70") + 
  opts(legend.position = "none")) 
#d + scale_x_continuous(limits = c(0, 2))
d + coord_cartesian(xlim = c(0, 2))

# (Left) A scatterplot and smoother with engine displacement on x axis
# and city mpg on y axis.  (Middle) Exchanging \var{cty} and
# \var{displ} rotates the plot 90 degrees, but the smooth is fit to the
# rotated data.  (Right) using \code{coord_flip} fits the smooth to the
# original data, and then rotates the output, this is a smooth curve of
# x conditional on y.
qplot(displ, cty, data = mpg) + geom_smooth()
qplot(cty, displ, data = mpg) + geom_smooth()
qplot(cty, displ, data = mpg) + geom_smooth() + coord_flip()

# (Left) A scatterplot of carat vs. price on log base 10 transformed
# scales.  A linear regression summarises the trend: $\log(y) = a + b *
# \log(x)$.  (Right) The previous plot backtransformed (with {\tt
# coord\_trans(x = "pow10", y = "pow10")}) onto the original scales.
# The linear trend line now becomes geometric, $y = k * c^x$, and
# highlights the lack of expensive diamonds for larger carats.
qplot(carat, price, data = diamonds, log = "xy") + 
  geom_smooth(method = "lm")
library(scales)
last_plot() + coord_trans(x = exp_trans(10), y = exp_trans(10))

# (Left) A stacked bar chart.  (Middle) The stacked bar chart in polar
# coordinates, with x position mapped to radius and y position mapped
# to angle, \code{coord_polar(theta = "y")}).  This is more commonly
# known as a pie chart.  (Right) The stacked bar chart in polar
# coordinates with the opposite mapping, \code{coord_polar(theta =
# "x")}.  This is sometimes called a bullseye chart.
# Stacked barchart
(pie <- ggplot(mtcars, aes(x = factor(1), fill = factor(cyl))) +
  geom_bar(width = 1))
# Pie chart
pie + coord_polar(theta = "y")

# The bullseye chart
pie + coord_polar()

################################################################################
## Chapter 9, Manipulating Data
options(digits = 2, width = 60)

# Select the smallest diamond in each colour
head(ddply(diamonds, .(color), subset, carat == min(carat)))

# Select the two smallest diamonds
ddply(diamonds, .(color), subset, order(carat) <= 2)

# Select the 1% largest diamonds in each group
ddply(diamonds, .(color), subset, carat > 
  quantile(carat, 0.99))

# Select all diamonds bigger than the group average
ddply(diamonds, .(color), subset, price > mean(price))

# Within each colour, scale price to mean 0 and variance 1
d.c <- ddply(diamonds, .(color), transform, price = scale(price))

# Subtract off group mean
ddply(diamonds, .(color), transform, 
      price = price - mean(price))

nmissing <- function(x) sum(is.na(x))
nmissing(msleep$name)
nmissing(msleep$brainwt)

nmissing_df <- colwise(nmissing)
nmissing_df(msleep)
# This is shorthand for the previous two steps
colwise(nmissing)(msleep)

msleep2 <- msleep[, -6] # Remove a column to save space
numcolwise(median)(msleep2, na.rm = T)
numcolwise(quantile)(msleep2, na.rm = T)
numcolwise(quantile)(msleep2, probs = c(0.25, 0.75), 
                     na.rm = T)

ddply(msleep2, .(vore), numcolwise(median), na.rm = T)
ddply(msleep2, .(vore), numcolwise(mean), na.rm = T)

my_summary <- function(df) {
  with(df, data.frame(
    pc_cor = cor(price, carat, method = "spearman"),
    lpc_cor = cor(log(price), log(carat))
  ))
}
ddply(diamonds, .(cut), my_summary)
ddply(diamonds, .(color), my_summary)

# A plot showing the smoothed trends for price vs. carat for each
# colour of diamonds.  With the full range of carats (left), the
# standard errors balloon after around two carats because there are
# relatively few diamonds of that size.  Restricting attention to
# diamonds of less than two carats (right) focuses on the region where
# we have plenty of data.
qplot(carat, price, data = diamonds, geom = "smooth", 
      colour = color)
dense <- subset(diamonds, carat < 2)
qplot(carat, price, data = dense, geom = "smooth", 
      colour = color,  fullrange = TRUE)

# Figure~\ref{fig:smooth} with all statistical calculations performed
# by hand.  The predicted values (left), and with standard errors
# (right).
library(mgcv)
smooth <- function(df) {
  mod <- gam(price ~ s(carat, bs = "cs"), data = df)
  grid <- data.frame(carat = seq(0.2, 2, length = 50))
  pred <- predict(mod, grid, se = T)
  
  grid$price <- pred$fit
  grid$se <- pred$se.fit
  grid
}
smoothes <- ddply(dense, .(color), smooth)
qplot(carat, price, data = smoothes, colour = color, 
      geom = "line")
qplot(carat, price, data = smoothes, colour = color, 
      geom = "smooth", ymax = price + 2 * se, ymin = price - 2 * se)

mod <- gam(price ~ s(carat, bs = "cs") + color, data = dense)
grid <- with(diamonds, expand.grid(
  carat = seq(0.2, 2, length = 50),
  color = levels(color)
))
grid$pred <- predict(mod, grid)
qplot(carat, pred, data = grid, colour = color, geom = "line")

# When the economics dataset is stored in wide format, it is easy to
# create separate time series plots for each variable (left and
# centre), and easy to create scatterplots comparing them (right).
qplot(date, uempmed, data = economics, geom = "line")
qplot(date, unemploy, data = economics, geom = "line")
qplot(unemploy, uempmed, data = economics) + geom_smooth()

# The two methods of displaying both series on a single plot produce
# identical plots, but using long data is much easier when you have
# many variables.  The series have radically different scales, so we
# only see the pattern in the \code{unemploy} variable. You might not
# even notice \code{uempmed} unless you're paying close attention: it's
# the line at the bottom of the plot.
ggplot(economics, aes(date)) + 
  geom_line(aes(y = unemploy, colour = "unemploy")) + 
  geom_line(aes(y = uempmed, colour = "uempmed")) + 
  scale_colour_hue("variable")

emp <- melt(economics, id = "date", 
            measure = c("unemploy", "uempmed"))
qplot(date, value, data = emp, geom = "line", colour = variable)

# When the series have very different scales we have two alternatives:
# left, rescale the variables to a common scale, or right, display the
# variables on separate facets and using free scales.
range01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / diff(rng)
}
emp2 <- ddply(emp, .(variable), transform, value = range01(value))
qplot(date, value, data = emp2, geom = "line", 
      colour = variable, linetype = variable)
qplot(date, value, data = emp, geom = "line") + 
  facet_grid(variable ~ ., scales = "free_y")

popular <- subset(movies, votes > 1e4)
ratings <- popular[, 7:16]
ratings$.row <- rownames(ratings)
molten <- melt(ratings, id = ".row")

# Variants on the parallel coordinates plot to better display the
# patterns in this highly discrete data.  To improve the default pcp
# (top left) we experiment with alpha blending (top right), jittering
# (bottom left) and then both together (bottom right).
pcp <- ggplot(molten, aes(variable, value, group = .row))
pcp + geom_line()
pcp + geom_line(colour = alpha("black", 1 / 20))
jit <- position_jitter(width = 0.25, height = 2.5)
pcp + geom_line(position = jit)
pcp + geom_line(colour = alpha("black", 1 / 20), position = jit)

cl <- kmeans(ratings[1:10], 6)
ratings$cluster <- reorder(factor(cl$cluster), popular$rating)
levels(ratings$cluster) <- seq_along(levels(ratings$cluster))
molten <- melt(ratings, id = c(".row", "cluster"))

# Displaying cluster membership on a parallel coordinates plot.  (Left)
# Individual movies coloured by group membership and (right) group
# means.
pcp_cl <- ggplot(molten, 
                 aes(variable, value, group = .row, colour = cluster)) 
pcp_cl + geom_line(position = jit, alpha = 1/5)
pcp_cl + stat_summary(aes(group = cluster), fun.y = mean, 
                      geom = "line")

# Faceting allows us to display each group in its own panel,
# highlighting the fact that there seems to be considerable variation
# within each group, and suggesting that we need more groups in our
# clustering.
pcp_cl + geom_line(position = jit, colour = alpha("black", 1/5)) +
  facet_wrap(~ cluster)

# A simple linear model that doesn't fit the data very well.
qplot(displ, cty, data = mpg) + geom_smooth(method = "lm")
mpgmod <- lm(cty ~ displ, data = mpg)

# (Left) Basic fitted values-residual plot.  (Middle) With standardised
# residuals.  (Right) With size proportional to Cook's distance.  It is
# easy to modify the basic plots when we have access to all of the
# data.
mod <- lm(cty ~ displ, data = mpg)
basic <- ggplot(mod, aes(.fitted, .resid)) +
  geom_hline(yintercept = 0, colour = "grey50", size = 0.5) + 
  geom_point() + 
  geom_smooth(size = 0.5, se = F)
basic
basic + aes(y = .stdresid)
basic + aes(size = .cooksd) + scale_area("Cook's distance")

# Adding variables from the original data can be enlightening.  Here
# when we add the number of cylinders we see that instead of a
# curvi-linear relationship between displacement and city mpg, it is
# essentially linear, conditional on the number of cylinders.
full <- basic %+% fortify(mod, mpg)
full + aes(colour = factor(cyl))
full + aes(displ, colour = factor(cyl))

fortify.Image <- function(model, data, ...) {
  colours <- channel(model, "x11")[,,]
  colours <- colours[, rev(seq_len(ncol(colours)))]
  melt(colours, c("x", "y"))
}

library(EBImage)
img <- readImage("http://had.co.nz/me.jpg", TrueColor)
qplot(x, y, data = img, fill = value, geom="tile") + 
  scale_fill_identity() + coord_equal()











