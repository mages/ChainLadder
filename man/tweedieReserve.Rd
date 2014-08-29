\name{tweedieReserve}
\alias{tweedieReserve}
\title{
Tweedie Stochastic Reserving Model
}
\description{
This function implements loss reserving models within the generalized linear model framework in order to generate the full predictive distribution for loss reserves. Besides, it generates also the "1-yr Risk View" useful to derive the Reserve Risk Capital in a Solvency 2 framework. Finally, it gives to the user the opportunity to properly assess the Model Error changing different model parameters.
}
\usage{
tweedieReserve(triangle, var.power = 1, 
                    link.power = 0, design.type = c(1, 1, 0), 
                    rereserving = FALSE, cum = TRUE, exposure = FALSE, 
                    bootstrap = 1, boot.adj = 0, nsim = 1000, 
                    proc.err = TRUE, p.optim = F, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{triangle}{
     An object of class \code{\link{triangle}}.
}
  \item{var.power}{
    The index (p) of the power variance function \eqn{V(\mu)=\mu^p}. Default to \code{p = 1}, which is the over-dispersed Poisson model. If \code{NULL}, it will be assumed to be in \code{(1, 2)} and estimated using the \code{cplm} package.  See \code{\link[statmod]{tweedie}}.
}
  \item{link.power}{
  The index of power link function. The default \code{link.power = 0} produces a log link. See \code{\link[statmod]{tweedie}}.
}
  \item{design.type}{
  It's a 3 dimension array that specifies the design matrix underlying the GLM. The dimensions represent respectively: Origin Year, Development Year and Calendar Year. Accepted values are: \code{0} (not modeled), \code{1} (modeled as factor) and \code{2} (modeled as variable). Default to \code{c(1,1,0)}, which is the common specification in actuarial literature (Origin and Development Year as factors, Calendar Year not modeled).
}
  \item{rereserving}{
  Boolean, if \code{TRUE} the 1-yr risk view loss reserve distribution is derived. Default to \code{FALSE} (ATTENTION: it could materially increase run-time).
}
  \item{cum}{
  Boolean, indicating whether the input triangle is on the
  cumulative or the incremental scale. If \code{TRUE}, then
  \code{triangle} is assumed to be on the cumulative scale, and it will
  be converted to incremental losses internally before a GLM is fitted. 
}
  \item{exposure}{
  Boolean, if \code{TRUE} the \code{exposure} defined in the \code{\link{triangle}} object is specified as \code{offset} in the GLM model specification. Default to \code{FALSE}.
}
  \item{bootstrap}{
  Integer, it specifies the type of bootstrap for parameter error. Accepted values are: \code{0} (disabled), \code{1} (parametric), \code{2} (semi-parametric). Default to \code{1}.
}
  \item{boot.adj}{
  Integer, it specified the methodology when using semi-parametric bootstrapping. Accepted values are: \code{0} (cycles until all the values of the pseudo-triangle are >= 0), \code{1} (overwrite negative values to 0.01). Default to \code{0} (ATTENTION: it could materially increase run-time when semi-parametric bootstrap is used)
}
  \item{nsim}{
  Integer, number of simulations to derive the loss reserve distribution. Default to \code{1000} (ATTENTION: high num of simulations could materially increase run-time, in particular if a re-reserving alghorithm is used as well)
}
  \item{proc.err}{
  Boolean, if \code{TRUE} a process error (coherent with the specified model) is added to the forecasted distribution. Default to \code{TRUE}.
}
  \item{p.optim}{
  Boolean, if \code{TRUE} the model estimates the MLE for the Tweedie's \code{p} parameter. Default to \code{FALSE}. Recommended to use at the beginning of the Stochastic Reserving Analysis.
}
  \item{\dots}{
  Arguments to be passed onto the function \code{\link[stats]{glm}} or \code{cpglm} such as \code{contrasts} or \code{control}. It is important that \code{offset} and \code{weight} should not be specified. Otherwise, an error will be reported and the program will quit.
}
}
%\details{
% [tbd]
%}
\value{The output is an object of class \code{"glm"} that has the following components:
  \item{call}{the matched call.}
  \item{summary}{A data frame containing the predicted loss reserve statistics. The following items are displayed: 
    \itemize{
      \item \code{Latest}: Latest paid
      \item \code{Det.Reserve}: Deterministic reserve, i.e. the MLE GLM estimate of the Reserve
      \item \code{Ultimate}: Ultimate cost, defined as \code{Latest+Det.Reserve}
      \item \code{Dev.To.Date}: Development to date, defined as \code{Latest/Ultimate}
    }
    [AVAILABLE ONLY IF \code{bootstrap>0}]
    \itemize{
      \item \code{Expected.Reserve}: The expected reserve, defined as the average of the reserve       simulations. Should be roughly as \code{Det.Reserve}.
      \item \code{Prediction.Error}: The prediction error of the reserve, defined as sqrt of the simulations. Please note that if \code{proc.err=FALSE}, this field contains only the parameter error given by the bootstrap.
      \item \code{CoV}: Coefficient of Variation, defined as \code{Prediction.Error/Expected.Reserve}.
      \item \code{Expected Ultimate}: The expected ultimate, defined as \code{Expected.Reserve+Latest}.
    }
    [AVAILABLE ONLY IF \code{bootstrap>0 & reserving=TRUE}]
    \itemize{
      \item \code{Expected.Reserve_1yr}: The reserve derived as sum of next year payment and the expected value of the re-reserve at the end of the year. It should be similar to both \code{Expected.Reserve} and \code{Det.Reserve}. If it isn't, it's recommended to change regression structure and parameters.
      \item \code{Prediction.Error_1yr}: The prediction error of the prospective Claims Development Result (CDR), as defined by Wuthrich (CDR=R(0)-X-R(1)). 
      \item \code{Emergence.Pattern}: It's the emergence pattern defined as \code{Prediction.Error_1yr/Prediction.Error}.
    }
  }
  \item{Triangle}{The input triangle.}
  \item{FullTriangle}{The completed triangle, where empty cells in the original triangle are filled with model predictions.}
  \item{model}{The fitted GLM, a class of \code{"glm"} or \code{"cpglm"}. It is most convenient to work with this component when model fit information is wanted. }
  
  \item{scale}{The dispersion parameter phi}
  \item{bias}{The model bias, defined as \code{bias<-sqrt(n/d.f)}}
  \item{GLMReserve}{Deterministic reserve, i.e. the MLE GLM estimate of the Reserve}
  \item{gamma_y}{When the calendar year is used, it displays the observed and fitted calendar year (usually called "gamma"") factors.}
  \item{res.diag}{It's a dataframe for residual diagnostics. It contains:
    \itemize{
      \item \code{unscaled}: The GLM Pearson residuals.
      \item \code{unscaled.biasadj}: The GLM Person residuals adjusted by the bias, i.e. \code{unscaled.biasadj=unscaled*bias}. 
      \item \code{scaled}: The GLM Person scaled residuals, i.e. \code{scaled=unscaled/sqrt(phi)}.
      \item \code{scaled, biasadj}: The GLM Person scaled residuals adjusted by the bias, i.e. \code{scaled.biasadj=scaled*bias}. 
      \item \code{dev}: Development year.
      \item \code{origin}: Origin year.
      \item \code{cy}: Calendar year.
      
    }
  }
[IF \code{boostrap>1}]
  \item{distr.res_ult}{The full distribution "Ultimate View"}
[IF \code{rereserve=TRUE}] 
  \item{distr.res_1yr}{The full distribution "1yr View"}
}

\references{

[GS05]  Gigante, Sigalotti (2005), "Model risk in claims reserving with generalized linear models", \emph{Giornale dell'Istituto Italiano degli Attuari, Volume LXVIII}, 55-87

[EV02]		England, Verrall (2002), "Stochastic claims reserving in general insurance", \emph{B.A.J. 8, III}, 443-544

[EV06]		England, Verrall (2006), "Predictive distributions of outstanding liabilities in general insurance", \emph{A.A.S. 1, II}, 221-270	

[PSW09]	Peters, Shevchenko, Wuethrich (2009), "Model uncertainty in claims reserving within Tweedie's compound poisson models", \emph{Astin Bulletin 39(1)}, 1-33

[RV98]		Renshaw, Verrall (1998), "A stochastic model underlying the chain-ladder technique", \emph{B.A.J. 4, IV}, 903-923
}
\author{
 Alessandro Carrato MSc FIA OA \email{alessandro.carrato@gmail.com}
}
\note{
 [tbd]
}

\section{Warning}{
  [tbd]
}

\seealso{
 See Also as \code{\link{help}}
}
\examples{
## R code of your examples
## R code of your examples
res <- tweedieReserve(UKMotor, rereserving=TRUE)
RC_report(res)
## One year reserving risk 
RC_report(res)$RC_1yr

## Alternative apporach using bootstrap chain ladder
## Use bootstrap and chainladder to estimate one year reserve risk
M <- MackChainLadder(UKMotor, est.sigma="Mack")
# Chain ladder link ratios
f <- M$f
# Latest triangle
latest <- summary(M)$ByOrigin$Latest
# Bootstrap chain ladder
set.seed(1) # set seed to have a replicatable example
B <- BootChainLadder(UKMotor, R=1000, process.distr="od.pois")


# Get next year indices
n <- ncol(UKMotor)
ny <- (col(UKMotor) == (nrow(UKMotor) - row(UKMotor) + 2))
# Extract the 1 in 200 triangle
triangle.ny <- apply(B$IBNR.Triangles, 3, 
                 function(x){
                   next.year <- x[col(x) == (nrow(x) - row(x) + 2)]
                   sum(next.year)
                 })
triangle.ny.995 <- B$IBNR.Triangles[,,order(triangle.ny)[round(B$R*0.995)]]
# Create a triangle with additional 1 in 200 diagonal from the Bootstrap
UKMotor.ny <- cum2incr(UKMotor)
UKMotor.ny[ny] <- triangle.ny.995[ny]
# Apply chain ladder again
(UKMotor.ny <- t(apply(UKMotor.ny,1,cumsum)))
f.ny <- sapply((n-1):1, function(i){
  sum(UKMotor.ny[1:(i+1), n-i+1])/sum(UKMotor.ny[1:(i+1), n-i])
})
(f.ny <- c(f.ny[-(n-1)],1))
full.triangle.ny <- UKMotor.ny
for(k in 2:(n-1)){ 
  full.triangle.ny[(n-k+2):n, k+1] <- full.triangle.ny[(n-k+2):n,k]*f[k]
}
## Re-reserving 1 in 200 IBNR
(sum(re.reserve.995 <- full.triangle.ny[,n] - rev(latest)))
# Compare to the glm approach
RC_report(res)$RC_1yr
 
}
\keyword{ models }
