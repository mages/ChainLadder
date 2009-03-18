\name{chris}
\Rdversion{1.1}
\alias{chris}
\docType{data}
\title{
  Incremental claims triangle
}
\description{
Incremental claims triangle with four accident years and four development years  
}
\usage{data(chris)}
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
Faculty and Institute of Actuaries, Claims Reserving Manual v.2
(09/1997), Section D5, Regression Models Based on Log-Incremental
Payments, S Christofides  
}
\examples{

# transform from triangle into long format
chrisl <- expand.grid(dimnames(chris))
chrisl$value <- as.vector(chris)
chrisl <- na.omit(chrisl)
lm(log(value) ~ origin + dev, data=chrisl)
model.matrix(log(value) ~ dev + origin-1, data=chrisl)
co=summary(lm(log(value) ~ origin + dev, data=chrisl))$coef[,1]
c(co[1], co[1]+co[2], co[1]+co[3], co[1]+co[4], co[5:7])

}
\keyword{datasets}
