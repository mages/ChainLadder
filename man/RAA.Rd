\name{RAA}
\alias{RAA}
\docType{data}
\title{Run off triangle of accumulated claims data}
\description{Run-off triangle of Automatic Factultative business in General Liability}
  
\usage{data(RAA)}
\format{
  A matrix with 10 accident years and 10 development years.
}
\source{
	\cite{Historical Loss Development, \emph{Reinsurance Association of Ammerica
    (RAA)}, \bold{1991}, p.96}
}
\references{See Also: 
		\cite{Which Stochastic Model is Underlying
  the Chain Ladder Method?, Thomas Mack, \emph{Insurance	
    Mathematics and Economics},  \bold{15, 2/3}, pp133-138, 1994	}
    
  \cite{P.D.England and R.J.Verrall, Stochastic Claims Reserving in
    General Insurance, \emph{British Actuarial Journal}, \bold{Vol. 8},
  pp443-544, 2002}
}
\examples{
RAA
plot(RAA)
plot(RAA, lattice=TRUE)
}
\keyword{datasets}