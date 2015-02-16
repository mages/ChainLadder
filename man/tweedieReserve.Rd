\name{tweedieReserve}
\alias{tweedieReserve}
\title{
Tweedie Stochastic Reserving Model
}
\encoding{UTF-8}
\description{
This function implements loss reserving models within the generalized
linear model framework in order to generate the full predictive 
distribution for loss reserves. Besides, it generates also the one year 
risk view useful to derive the reserve risk capital in a Solvency II 
framework. Finally, it allows the user to validate the model error while changing different model parameters, as the regression structure and diagnostics on the Tweedie p parameter.
}
\usage{
tweedieReserve(triangle, var.power = 1, 
                    link.power = 0, design.type = c(1, 1, 0), 
                    rereserving = FALSE, cum = TRUE, exposure = FALSE, 
                    bootstrap = 1, boot.adj = 0, nsim = 1000, 
                    proc.err = TRUE, p.optim = FALSE,
                    progressBar = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{triangle}{
     An object of class \code{\link{triangle}}.
}
  \item{var.power}{
    The index (p) of the power variance function \eqn{V(\mu)=\mu^p}. 
    Default to \code{p = 1}, which is the over-dispersed Poisson model. 
    If \code{NULL}, it will be assumed to be in \code{(1, 2)} and 
    estimated using the \code{cplm} package.  
    See \code{\link[statmod]{tweedie}}.
}
  \item{link.power}{
  The index of power link function. The default \code{link.power = 0} 
  produces a log link. See \code{\link[statmod]{tweedie}}.
}
  \item{design.type}{
  It's a 3 dimension array that specifies the design matrix underlying 
  the GLM. The dimensions represent respectively: origin period, 
  development and calendar period. Accepted values are: \code{0} (not   
  modelled), \code{1} (modelled as factor) and \code{2} (modelled as 
  variable). Default to \code{c(1,1,0)}, which is the common 
  specification in actuarial literature (origin and development period 
  as factors, calendar period not modelled). If a parameter for the calendar period is specified,       a linear regression on the log CY parameter is fitted to estimate future values, thus is recommended to validate them running a plot of the gamma values (see output \code{gamma_y}) . 
}
  \item{rereserving}{
  Boolean, if \code{TRUE} the one year risk view loss reserve 
  distribution is derived. Default to \code{FALSE}. Note, the runtime 
  can materially increase if set to \code{TRUE}.
}
  \item{cum}{
  Boolean, indicating whether the input triangle is
  cumulative or incremental along the development period. 
  If \code{TRUE}, then
  \code{triangle} is assumed to be on the cumulative scale, and it will
  be converted to incremental losses internally before a GLM is fitted. 
}
  \item{exposure}{
  Boolean, if \code{TRUE} the \code{exposure} defined in the 
  \code{\link{triangle}} object is specified as \code{offset} in the GLM 
  model specification. Default to \code{FALSE}.
}
  \item{bootstrap}{
  Integer, it specifies the type of bootstrap for parameter error.
  Accepted values are: \code{0} (disabled), \code{1} (parametric), 
  \code{2} (semi-parametric). Default to \code{1}.
}
  \item{boot.adj}{
  Integer, it specified the methodology when using semi-parametric 
  bootstrapping. Accepted values are: \code{0} (cycles until all the 
  values of the pseudo-triangle are >= 0), \code{1} (overwrite negative 
  values to 0.01). Default to \code{0}. Note, runtime can materially 
  increase when set to  \code{0}, as it could struggle to find pseudo-triangles >= 0)
}
  \item{nsim}{
  Integer, number of simulations to derive the loss reserve distribution.   
  Default to \code{1000}. Note, high num of simulations could materially 
  increase runtime, in particular if a re-reserving algorithm is used 
  as well.
}
  \item{proc.err}{
  Boolean, if \code{TRUE} a process error (coherent with the specified 
  model) is added to the forecasted distribution. Default to \code{TRUE}.
}
  \item{p.optim}{
  Boolean, if \code{TRUE} the model estimates the MLE for the Tweedie's 
  \code{p} parameter. Default to \code{FALSE}. Recommended to use to validate the Tweedie's     \code{p} parameter.
}
\item{progressBar}{
  Boolean, if \code{TRUE} a progress bar will be shown in the console to 
  give an indication of bootstrap progress.
}
  \item{\dots}{
  Arguments to be passed onto the function \code{\link[stats]{glm}} or 
  \code{cpglm} such as \code{contrasts} or \code{control}. It is 
  important that \code{offset} and \code{weight} should not be specified. 
  Otherwise, an error will be reported and the program will quit.
}
}
%\details{
% [tbd]
%}
\value{The output is an object of class \code{"glm"} that has the following components:
  \item{call}{the matched call.}
  \item{summary}{A data frame containing the predicted loss reserve 
  statistics. The following items are displayed: 
    \itemize{
      \item \code{Latest}: Latest paid
      \item \code{Det.Reserve}: Deterministic reserve, i.e. the MLE GLM 
      estimate of the Reserve
      \item \code{Ultimate}: Ultimate cost, defined as 
      \code{Latest+Det.Reserve}
      \item \code{Dev.To.Date}: Development to date, defined as 
      \code{Latest/Ultimate}
    }
    The following items are available if \code{bootstrap>0}
    \itemize{
      \item \code{Expected.Reserve}: The expected reserve, defined as 
      the average of the reserve simulations. 
      Should be roughly as \code{Det.Reserve}.
      \item \code{Prediction.Error}: The prediction error of the reserve, 
      defined as sqrt of the simulations. Please note that if 
      \code{proc.err=FALSE}, this field contains only the parameter 
      error given by the bootstrap.
      \item \code{CoV}: Coefficient of Variation, defined as 
      \code{Prediction. Error/Expected.Reserve}.
      \item \code{Expected Ultimate}: The expected ultimate, defined as 
      \code{Expected.Reserve+Latest}.
    }
    The following items are availbale if \code{bootstrap>0 & reserving=TRUE}
    \itemize{
      \item \code{Expected.Reserve_1yr}: The reserve derived as sum of 
      next year payment and the expected value of the re-reserve at the 
      end of the year. It should be similar to both 
      \code{Expected.Reserve} and \code{Det.Reserve}. If it isn't, it's 
      recommended to change regression structure and parameters.
      \item \code{Prediction.Error_1yr}: The prediction error of the 
      prospective Claims Development Result (CDR), as defined by 
      Wüthrich (CDR=R(0)-X-R(1)). 
      \item \code{Emergence.Pattern}: It's the emergence pattern defined 
      as \code{Prediction.Error_1yr/Prediction.Error}.
    }
  }
  \item{Triangle}{The input triangle.}
  \item{FullTriangle}{The completed triangle, where empty cells in the 
  original triangle are filled with model predictions.}
  \item{model}{The fitted GLM, a class of \code{glm} or \code{cpglm}. 
  It is most convenient to work with this component when model fit 
  information is wanted. }
  
  \item{scale}{The dispersion parameter phi}
  \item{bias}{The model bias, defined as \code{bias<-sqrt(n/d.f)}}
  \item{GLMReserve}{Deterministic reserve, i.e. the MLE GLM estimate 
  of the Reserve}
  \item{gamma_y}{When the calendar year is used, it displays the observed 
  and fitted calendar year (usually called "gamma"") factors.}
  \item{res.diag}{It's a data frame for residual diagnostics. It 
  contains:
    \itemize{
      \item \code{unscaled}: The GLM Pearson residuals.
      \item \code{unscaled.biasadj}: The GLM Person residuals adjusted by 
      the bias, i.e. \code{unscaled.biasadj=unscaled*bias}. 
      \item \code{scaled}: The GLM Person scaled residuals, i.e. 
      \code{scaled=unscaled/sqrt(phi)}.
      \item \code{scaled, biasadj}: The GLM Person scaled residuals 
      adjusted by the bias, i.e. \code{scaled.biasadj=scaled*bias}. 
      \item \code{dev}: Development year.
      \item \code{origin}: Origin year.
      \item \code{cy}: Calendar year.      
    }
  }
[If \code{boostrap>1}]
  \item{distr.res_ult}{The full distribution "Ultimate View"}
[If \code{rereserve=TRUE}] 
  \item{distr.res_1yr}{The full distribution "1yr View"}
}

\references{

Gigante, Sigalotti. \emph{Model risk in claims reserving with generalized linear models}. 
Giornale dell'Istituto Italiano degli Attuari, Volume LXVIII. 55-87. 2005

England, Verrall. \emph{Stochastic claims reserving in general insurance}. 
B.A.J. 8, III. 443-544. 2002

England, Verrall. \emph{Predictive distributions of outstanding liabilities in general insurance}.
A.A.S. 1, II. 221-270. 2006

Peters, Shevchenko, Wüthrich, \emph{Model uncertainty in claims reserving within 
Tweedie's compound poisson models}. Astin Bulletin 39(1). 1-33. 2009

Renshaw, Verrall. \emph{A stochastic model underlying the chain-ladder technique}.
B.A.J. 4, IV. 903-923. 1998
}
\author{
 Alessandro Carrato MSc FIA OA \email{alessandro.carrato@gmail.com}
}
\note{
 This function was born initially as a fork of the \code{glmReserve} by 
 Wayne Zhang. I would like to thank him for his work that permitted me to 
 speed up my coding.
}

\section{Warning}{
  Note that the runtime can materially increase for certain parameter
  setting. See above for more details.
}

\seealso{
 See also \code{\link{summary.tweedieReserve}}.
}
\examples{
#Verrall's ODP Model is a Tweedie with p=1, log link and origin/development years as factors, thus c(1,1,0)
res <- tweedieReserve(MW2008, var.power=1, link.power=0, 
                           design.type=c(1,1,0), rereserving=TRUE,
                           progressBar=TRUE)

#To get directly ultimate view and respective one year view at selected percentiles
summary(res) 

#To get other interesting statistics
res$summary

#In order to validate the Tweedie p parameter, it's interesting to view the Tweedie's p parameter loglikelihood profile. RERESERVING and BOOTSTRAP can be set to 0 to speed up the process, as they aren't needed.
res <- tweedieReserve(MW2008, p.optim=T, link.power=0, design.type=c(1,1,0), rereserving=FALSE, bootstrap=0, progressBar=TRUE)

#As it's possible to see in the result plot, p=1 fits well the Tweedie model, thus the ODP is a good choice.

#In order to consider an inflation parameter across the AY, it may interesting to change the regression structure to c(0,1,1) to get the same estimates of the Arithmetic Separation Method, as referred in Gigante/Sigalotti.
res <- tweedieReserve(MW2008, var.power=1, link.power=0, 
                           design.type=c(0,1,1), rereserving=TRUE,
                           progressBar=TRUE)

#An assessment on future fitted calendar year values (usually defined as "gamma") is recommended
plot(res$gamma_y)

#Model residuals can be get using the res.diag output
plot(scaled.biasadj~dev,data=res$res.diag) #Development year
plot(scaled.biasadj~cy,data=res$res.diag) #Calendar year
plot(scaled.biasadj~origin,data=res$res.diag) #Origin year

}
\keyword{ models }
