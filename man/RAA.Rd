\name{RAA}
\alias{RAA}
\docType{data}
\title{Run off triangle of accumulated claims data}
\description{Run-off triangle of Automatic Factultative business in General Liability}
  
\usage{data(RAA)}
\format{
  A matrix with 10 accident years and 10 development years.
}
\source{Historical Loss Development, \emph{Reinsurance Association of Ammerica
    (RAA)}, \bold{1991}, p.96}
\references{See Also: Which Stochastic Model is Underlying
  the Chain Ladder Method?, Thomas Mack,  1994, \emph{Insurance
    Mathematics and Economics},  \bold{15, 2/3}, 133-138

  P.D.England and R.J.Verrall, \emph{Stochastic Claims Reserving in
    General Insurance}, British Actuarial Journal, \bold{Vol. 8},
  pp443-544, 2002
}
\examples{
RAA
matplot(t(RAA), type="l")
}
\keyword{datasets}
