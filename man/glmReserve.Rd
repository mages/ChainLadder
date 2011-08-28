\name{glmReserve}
\alias{glmReserve}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
GLM-based Reserving Model 
}
\description{
This function implements loss reserving models within the generalized linear model framework. It takes accident year and development lag as mean predictors in estimating the ultimate loss reserves, and provides both analytical formula and bootstrapping method to compute the associated prediction errors.
}
\usage{
glmReserve(triangle, var.power = 1, link.power = 0, cum = TRUE, 
      mse.method = "formula", nsim = 1000, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{triangle}{
    A object of  class \code{\link{triangle}}. 
}
  \item{var.power}{
    The index (p) of the power variance function \eqn{var(\mu)=\mu^p}. Default to \code{p=1}, which is the over-dispersed Poisson model. See \code{\link[statmod]{tweedie}}.
}
  \item{link.power}{
  The index of power link function. The default \code{link.power=0} produces a log-link. See \code{\link[statmod]{tweedie}}.
}
  \item{cum}{
  A logical value indicating whether the input triangle is on the cumulative or the incremental scale. If \code{TRUE}, \code{triangle} will be converted to incremental losses and then a GLM is fit. 
}
  \item{mse.method}{
  A character indicating whether the prediction error should be computed analytically (\code{mse.method="formula"}) or via bootstrapping (\code{mse.method="bootstrap"})
}
  \item{nsim}{
  Number of simulations to be performed in the bootstrapping, with a default value of 1000.
}
  \item{\dots}{
  Arguments to be passed onto \code{\link[stats]{glm}}. It is important that \code{offset} and \code{weight} should not be specified. Otherwise, an error will be reported and the program will quit. 
}
}
\details{
 This function takes an insurance loss triangle, converts it to incremental losses if necessary, and fits the data with a generalized linear model where the mean structure includes both the accident year and the development lag effects. The distributions allowed are the exponential family that admits a power variance function, that is, \eqn{var(\mu)=\mu^p}. This subclass of distributions is usually called the Tweedie distribution and includes many commonly used distributions as special cases. This function does not allow the user to specify the GLM options through the usual \code{\link[stats]{family}} argument, but instead, it uses the \code{\link[statmod]{tweedie}} family internally so that the user still has full control of the distribution forms and link functions through the two arguments \code{var.power} and \code{link.power}. The argument \code{var.power} determines which specific distribution is to be used, and \code{link.power} determines the form of the link function. See details in \code{\link[statmod]{tweedie}}. 
 
Also, the function allows certain measures of exposures to be used as an offset term in the underlying GLM. To do this, the user should not use the usual \code{offset} argument in \code{glm}. Instead, one specifies the exposure measure for each accident year through the \code{exposure} attributes of \code{triangle}. Make sure that these exposures are in the orignal scale (no log transformations for example), and they are in the order consistent with the accident years. The function will make any necessary transformation of the exposure when fitting the GLM. For example, if the link function is \code{log}, then the log of the exposure is used as the offset, not the original exposure. See the examples below. Moreover, the user MUST NOT supply the typical \code{offset} or \code{weight} as arguments in \dots. \code{offset} should be specified as above, while \code{weight} is not implemented (due to prediction reasons). 

Two methods are available to assess the prediction error of the estimated loss reserves. One is using analytical formula (\code{mse.method="formula"}) derived from first-order Taylor approximations. The other is using bootstrapping (\code{mse.method="bootstrap"}) that reconstructs the triangles \code{nsim} times by resampling with replacement from the GLM residuals. Each time a new triangle is formed, GLM is fitted and corresponding loss reserves are generated. Then the \code{nsim} sets of reserves are used to compute the estimation variance, which, when adjusted by the lost degree of freedoms and combined with the process variance, will yield the prediction variance. See England and Verrall (1999) for details. 
}

\value{The output mainly includes components of a \code{glm} object, and is thus made into the class \code{glm}. Besides the slots that a typical \code{glm} object possesses (see \code{\link[stats]{glm}}), several additional slots are also returned:
\item{summary}{A data frame containing the predicted loss reserve statistics.}
\item{Triangle}{The input triangle.}
\item{FullTriangle}{The completed triangle, where empty cells in the original triangle are filled with model predictions.}
\item{scale}{scale parameter calculated using Pearson residuals.}
}
\references{
England P. and Verrall R. (1999). Analytic and bootstrap estimates of prediction errors in claims reserving. \emph{Insurance: Mathematics and Economics}, 25, 281-293.
}
\author{
Wayne Zhang \email{actuary_zhang@hotmail.com}
}

\seealso{
See also \code{\link[stats]{glm}}, \code{\link[statmod]{tweedie}} and \code{\link{MackChainLadder}}.
}
\examples{

data(GenIns)

# over-dispersed Poisson: reproduce ChainLadder estimates
fit1 <- glmReserve(GenIns)
fit1$summary

# plot of standardized residuals
plot(fitted(fit1),resid(fit1)/sqrt(fit1$scale))

# Gamma GLM:
fit2 <- glmReserve(GenIns,var.power=2)
fit2$summary

# Now suppose we have an exposure measure
# we can put it as an offset term in the model
# to do this, use the "exposure" attributes of the 'triangle'
expos <- (7 + 1:10*0.4)*1000000
GenIns2 <- GenIns
attr(GenIns2,"exposure") <- expos
fit3 <- glmReserve(GenIns2)
fit3$summary 

# use bootstrapping to compute prediction error
\dontrun{
set.seed(11)
fit4 <- glmReserve(GenIns,mse.method="bootstrap",nsim=1000)
fit4$summary
}
}

\keyword{models }

