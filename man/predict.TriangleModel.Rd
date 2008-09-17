\name{predict.TriangleModel}
\alias{predict.TriangleModel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Prediction of a claims triangle}
\description{
  The function is internally used by \code{\link{MackChainLadder}} to forecast future claims.
}
\usage{
\method{predict}{TriangleModel}(object,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{a list with two items: \code{Models}, \code{Triangle}
  \describe{
    \item{\code{Models}}{list of linear models for each development period
      }
    \item{\code{Triangle}}{input triangle to forecast
      }
    }
  }
  \item{\dots}{ not in use}
}
%\details{ \code{Models}  list linear models
%
%  
%}
\value{
  \item{FullTriangle}{forecasted claims triangle}
}
%\references{ ~put references to the literature/web site here ~ }
\author{Markus Gesmann}
%\note{ ~~further notes~~ 
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{ See Also \code{\link{MackChainLadder}} }
\examples{


 RAA    
 MCL=MackChainLadder(RAA)
 MCL
 plot(MCL)
 MCL[["Models"]][[1]]   # Model for first development period
 summary( MCL[["Models"]][[1]]) # Look at the model stats
 op=par(mfrow=c(2,2)) # plot residuals
   plot( MCL[["Models"]][[1]])
 par(op)

 # Let's include an intercept in our model
 newModel <- update(MCL[["Models"]][[1]], y ~ x+1, 
              weights=1/MCL[["Triangle"]][1:9,1],
              data=data.frame(x=MCL[["Triangle"]][1:9,1], 
                              y=MCL[["Triangle"]][1:9,2])
               ) 

# view the new model
 summary(newModel)
 op=par(mfrow=c(2,2)) 
   plot( newModel )
 par(op)

 # change the model for dev. period one to the newModel
 MCL2=MCL
 MCL2[["Models"]][[1]] = newModel
 predict(MCL2) # predict the full triangle with the new model 
 #(only the last origin year will be affected)

 MCL2[["FullTriangle"]] <-  predict(MCL2)
 MCL2[["FullTriangle"]] 
 MCL2   # Std. Errors have not been re-estimated!
 # plot the res
 
 plot(MCL2, title="Change MCL Model")
 
} 
\keyword{ models }