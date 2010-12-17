\name{ClarkCapeCod}
\alias{ClarkCapeCod}
\title{
Clark Cape Cod method
}
\description{
Analyze loss triangle using Clark's Cape Cod method.
}
\usage{
ClarkCapeCod(data, Premium, cumulative = TRUE, adol = TRUE, 
        maxage = c(Inf, Inf), G = "loglogistic")
}
\arguments{
  \item{data}{
A loss triangle in the form of a matrix.
The number of columns must be at least four;
the number of rows may be as few as 1.
The column names of the matrix should be able to be interpreted
as the "age" of the losses in that column.
The row names of the matrix should uniquely define the year of
origin of the losses in that row.
Losses may be inception-to-date or incremental.
}
  \item{Premium}{
The vector of premium to use in the method.
The length should be the same as the number of rows of \code{data}.
}
  \item{cumulative}{
If \code{TRUE} (the default), values in \code{data} are
inception to date.
If \code{FALSE}, \code{data} holds incremental losses.
}
  \item{adol}{
If \code{TRUE} (the default), the growth function should be applied
to the length of time from the average date of loss ("adol")
of losses in the origin year.
If \code{FALSE}, the growth function should be applied
to the length of time since the beginning of the origin year.
}
  \item{maxage}{
The "ultimate" age to which losses should be projected.
This can be a vector of length 1 or 2.
The first element holds the ultimate age as traditionally defined, 
namely the length of time since the beginning of the origin year.
If the second element exists --
which is pertinent only in the case that \code{adol} is \code{TRUE} --
it holds the length of time from the
average date of loss of the origin year.
If the second element does not exist and \code{adol} is \code{TRUE},
its value will be derived as \code{maxage[1]} minus 
the average difference between the ages in \code{colnames(data)},
with a warning if not all differences are the same.
}
  \item{G}{
A \code{character} scalar identifying the "growth function."
The two growth functions defined at this time are "loglogistic"
(the default)
and "weibull".
}
}
\details{
Clark's "Cape Cod" method assumes that the 
incremental losses across development periods in a loss triangle
are independent.
He assumes that the expected value of an incremental loss is
equal to the \emph{theoretical} expected loss ratio (\bold{ELR})
times the on-level premium for the origin year
times the change in the \emph{theoretical}
underlying growth function over the development period.
Clark models the growth function, also called the percent of ultimate,
by either the loglogistic function (a.k.a., "the inverse power curve") 
or the weibull function.
Clark completes his incremental loss model 
by wrapping the expected values within an 
overdispersed poisson (ODP) process where
the "scale factor" 
sigma^2
is assumed to be a known constant for all 
development periods.
 
The parameters of Clark's "Cape Cod" method are therefore:
ELR,
and 
omega
and 
theta
(the parameters of the \bold{loglogistic} and \bold{weibull} growth functions).
Finally, Clark uses maximum likelihood to parameterize his model,
uses the ODP process to estimate process risk, and
uses the Cramer-Rao theorem and the "delta method" to estimate parameter risk.

Clark recommends inspecting the residuals to help assess the 
reasonableness of the model relative to the actual data
(see \code{\link{plot.clark}} below).

}
\value{
A \code{list} of class "clark" with the following components:
    \item{method}{"Cape Cod"}
    \item{growthFunction}{name of the growth function}
    \item{Table65}{the table of losses, standard errors and CVs as shown
    on p. 65 of the paper}
    \item{Table68}{the table of ages, growth function values, 
    "ultimate" losses, and "reserves" as shown on p. 68 
    of the paper}
    \item{par}{the estimated parameters}
    \item{sigma2}{the sigma-squared "scale parameter"}
    \item{origin}{rownames(data) from the "long format" of \code{data}}
    \item{age}{colnames(data) from the "long format" of \code{data}}
    \item{fitted}{the expected values (the "mu's") of the incremental losses}
    \item{residuals}{the difference between the actual and fitted values}
    \item{stdresid}{the standardized residuals 
    = residuals/sqrt(sigma2*fitted)
    (referred to as "normalized residuals" in the paper; see p. 62)}
}
\references{
Clark, David R., 
"LDF Curve-Fitting and Stochastic Reserving: A Maximum Likelihood Approach",
\emph{Casualty Actuarial Society Forum}, Fall, 2003
\url{http://www.casact.org/pubs/forum/03fforum/03ff041.pdf} 
}
\author{
Daniel Murphy
}
\seealso{
\code{\link{ClarkLDF}}
}
\examples{
X <- GenIns
colnames(X) <- 12*as.numeric(colnames(X))
CC.loglogistic  <- ClarkCapeCod(X, Premium=10000000+400000*0:9, maxage=240)
CC.loglogistic

# Clark's "CapeCod method" also works with triangles that have  
# more development periods than origin periods. The Premium
# is a contrived match to the "made up" 'qincurred' data.
ClarkCapeCod(qincurred, Premium=1250+150*0:11, G="loglogistic")

# Method also works for a "triangle" with only one row:
# 1st row of GenIns; need "drop=FALSE" to avoid becoming a vector.
ClarkCapeCod(GenIns[1, , drop=FALSE], Premium=1000000, maxage=20)

# Use of the weibull function generates a warning that the parameter risk 
# approximation results in some negative variances. This may be of small 
# concern since it happens only for older years with near-zero 
# estimated reserves, but the warning should not be disregarded 
# if it occurs with real data.
Y <- ClarkCapeCod(qincurred, Premium=1250+150*0:11, G="weibull")

# The plot of the standardized residuals by age indicates that the more
# mature observations are more loosely grouped than the less mature, just
# the opposite of the behavior under the loglogistic curve.
# This suggests that the model might be improved by analyzing the data 
# in two different "blocks": less vs. more mature. 
# The QQ-plot shows that the tails of the empirical distribution of
# standardized residuals are "fatter" than a standard normal. 
# The fact that the p-value is essentially zero says that there is 
# virtually no chance that the standardized residuals could be 
# considered draws from a standard normal random variable.
# The overall conclusion is that Clark's ODP-based CapeCod model with 
# the weibull growth function does not match up well with the qincurred 
# triangle and these premiums.
plot(Y) 
}
\keyword{ models }
