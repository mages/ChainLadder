## page 268
## Example 2 - Simulated data

origin <- 1978 : 1991
dev <- 0:13

dev.trend <- matrix(-0.2, nrow=14,ncol=14)
dev.trend[,1] <- 0
dev.trend[row(dev.trend) > 15-col(dev.trend)] <- NA
dimnames(dev.trend) <-  list(origin=origin, dev=dev)


cal.trend <- matrix(0.15, nrow=14,ncol=14)
dimnames(cal.trend) <-  list(origin=origin, dev=dev)
cal.trend[row(cal.trend) > 15-col(cal.trend)] <- NA
cal.trend[row(cal.trend)+col(cal.trend)<=6] <- 0.1
cal.trend[row(cal.trend)+col(cal.trend)==7] <- 0.3
cal.trend[1,1] <- 0
### Cumulative trends
cum.dev.trend <- t(apply(dev.trend, 1, cumsum))
cum.cal.trend <- cal.trend
for(i in 1:5){
    cum.cal.trend[row(cal.trend)+col(cal.trend)==i+1] <- 0.1*(i-1)
}
cum.cal.trend[row(cal.trend)+col(cal.trend)==7] <- 0.1*4+0.3
for(i in 7:14){
    cum.cal.trend[row(cal.trend)+col(cal.trend)==i+1] <- 0.1*4+0.3+0.15*(i-6)
}

y <- expand.grid( list(origin=origin, dev=dev))
y$cal <- with(y, origin + dev)
y$dev.trend <- as.vector(dev.trend)
y$cal.trend <- as.vector(cal.trend)
y$cum.dev.trend <- as.vector(cum.dev.trend)
y$cum.cal.trend <- as.vector(cum.cal.trend)
y$paid <- 11.51293 + with(y, cum.dev.trend +  cum.cal.trend)
y <- na.omit(y)


library(lattice)
xyplot(paid ~ dev , groups=origin, data=subset(y, origin<1984), t="l",
       main="Figure 15\nPlot of log(Paid) data against delay\nfor the first six accident years")


## Random noise
## page 270
set.seed(3)
y$paid.noise <- y$paid + rnorm(nrow(y), 0, 0.1)

xyplot(paid.noise ~ dev , groups=origin, data=subset(y, origin<1984), t="l",
       main="Figure 16\n Trend plus randomness for the first six accident years")


model <- lm(paid.noise ~  dev + cal, data=y)
## Table 3.1 page 274
summary(model)
op <- par(mfrow=c(2,2))
plot(model)
par(op)

y$res <- resid(model)

## Figure 19 page 275
op <- par(mfrow=c(2,2))
plot(res ~ dev, data=y)
lines(x=unique(y$dev),y=as.vector(by(y, list(dev=y$dev),
                    function(x) mean(x$res))), col="red")
plot(res ~ origin, data=y)
lines(x=unique(y$origin), y=as.vector(by(y, list(dev=y$origin),
                    function(x) mean(x$res))), col="red")
plot(res ~ cal, data=y)
lines(x=unique(y$cal), y=as.vector(by(y, list(dev=y$cal),
                    function(x) mean(x$res))), col="red")
plot(res ~ paid.noise, data=y)
lines(lowess(y$paid.noise, y$res), col="red")
par(op)

 pairs(y[c("paid.noise", "dev", "cal")], panel=panel.smooth)
### Add payment year trends to the model

y$cal.cut <- cut(y$cal, c(1977, 1978, 1981, 1983, 1991))
model.cal.trends <- lm(paid.noise ~  dev + I(cal-1977) + cal.cut, data=y)
## Table 3.2 page 277
summary(model.cal.trends)
op <- par(mfrow=c(2,2))
plot(model.cal.trends)
par(op)

x <- c(1:10)
y <- c(1:5, 12, 14, 16, 18, 20)

inf <- factor(c(rep(1,5), 2, rep(3,4)))
