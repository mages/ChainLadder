\name{M3IR5}
\Rdversion{1.1}
\alias{M3IR5}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data(M3IR5)}
\format{
  The format is:
 int [1:14, 1:14] 108651 98706 133106 125731 161765 226364 228411 277868 302519 393525 ...
 - attr(*, "dimnames")=List of 2
  ..$ origin: chr [1:14] "1978" "1979" "1980" "1981" ...
  ..$ dev   : chr [1:14] "1" "2" "3" "4" ...
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(M3IR5)

sim <- expand.grid(dimnames(M3IR5))
sim$value <- as.vector(M3IR5)

require(lattice)
# Figure 16
xyplot(log(value) ~ dev, groups=origin, data=sim, t="l", auto.key=list(space="right"))

 sim$origin <- as.numeric(sim$origin) 
 sim$dev <- as.numeric(sim$dev)  - 1
 sim$cal <- sim$origin + sim$dev - 1
# Table 3.1
summary(lm(log(value) ~ cal + dev, data=sim))

crm2 <- t(matrix(c(11073, 6427, 1839, 766, 14799, 9357, 2344, NA, 15636, 10523, NA,NA,16913, NA,NA,NA), nrow=4))
dimnames(crm2) <- list(origin=0:3, dev=0:3)

crm <- expand.grid(dimnames(crm2))
crm$value <- as.vector(crm2)
crm <- na.omit(crm)

}
\keyword{datasets}
