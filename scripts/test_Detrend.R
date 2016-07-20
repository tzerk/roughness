library(pracma)

d <- data.frame(x = 1:1000, y = jitter(1:1000, factor = 200))

plot(d)

d2 <- detrend(d[,2])

plot(d[,1], d2, xlim = c(0, 1000), ylim = c(0, 1000))


#

lm <- lm(y ~ x, d)


#plot(d)
#abline(lm, col = "red")

d3 <- d[,2] - lm$fitted.values

plot(d[,1], d3, xlim = c(0, 1000), ylim = c(0, 1000))
