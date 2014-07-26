require('mcsm')
require('manipulate')
n_sim <- 10000
n <- 6
p <- 0.3

y <- rgamma(n_sim, n, rate = p / (1-p))
x <- rpois(n_sim, y)
hist(x, freq = FALSE, col = 'grey', breaks = 40)
lines(1:50, dnbinom(1:50, n, p), lwd = 2, col = "sienna")

require('manipulate')

manipulate( 
{
  x <- seq(-10, 10, length = 1000)
  d <- dcauchy(x, scale = scale, location = location, log = Log)
  plot(d ~ x, type = "l")
},

scale = slider(1, 10, step = 1),
location = slider(0, 10, step = 1),
Log = picker("FALSE" = FALSE, "TRUE" = TRUE)
)
