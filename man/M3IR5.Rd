\name{M3IR5}
\Rdversion{1.1}
\alias{M3IR5}
\docType{data}
\title{
Run off triangle of claims data
}
\description{
Run off triangle of simulated incremental claims data
}
\usage{data(M3IR5)}
\format{
  A matrix with simulated incremental claims of 14 accident years and 14 development years.
}
%%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%%}
\source{Appendix A8 in 
	\cite{ B. Zehnwirth. Probabilistic Development Factor Models
	  with Applications to Loss Reserve Variability, Prediction
	  Intervals, and Risk Based Capital. \emph{Casualty Actuarial
	    Science Forum.} Spring 1994. Vol. 2.}
}
%%\references{
%%  ~~ possibly secondary sources and usages ~~
%%}
\examples{

M3IR5
plot(M3IR5)
plot(incr2cum(M3IR5), lattice=TRUE)
}
\keyword{datasets}
%data(M3IR5)
%
%sim <- expand.grid(dimnames(M3IR5))
%sim$value <- as.vector(M3IR5)
%
%require(lattice)
%# Figure 16
%xyplot(log(value) ~ dev, groups=origin, data=sim, t="l", auto.key=list(space="right"))
%
%sim$origin <- as.numeric(sim$origin) 
%sim$dev <- as.numeric(sim$dev)  - 1
%sim$cal <- sim$origin + sim$dev 
%# Table 3.1
%summary(lm(log(value) ~ dev + cal, data=sim))
