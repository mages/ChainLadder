\name{ClarkLDF}
\alias{ClarkLDF}
\title{
Clark LDF method
}
\description{
Analyze loss triangle using Clark's LDF (loss development factor) method.
}
\usage{
ClarkLDF(Triangle, cumulative = TRUE, maxage = Inf, 
        adol = TRUE, adol.age = NULL, origin.width = NULL,
        G = "loglogistic")
}
\arguments{
  \item{Triangle}{
A loss triangle in the form of a matrix.
The number of columns must be at least four;
the number of rows may be as few as 1.
The column names of the matrix should be able to be interpreted
as the "age" of the losses in that column.
The row names of the matrix should uniquely define the year of
origin of the losses in that row.
Losses may be inception-to-date or incremental.

The "ages" of the triangle can be "phase shifted" --
i.e., the first age need not be as at the end of the origin period.
(See the Examples section.)
Nor need the "ages" be uniformly spaced.
However, when the ages are not uniformly spaced, 
it would be prudent to specify the \code{origin.width} argument.
}
  \item{cumulative}{
If \code{TRUE} (the default), values in \code{Triangle} are
inception to date.
If \code{FALSE}, \code{Triangle} holds incremental losses.
}
  \item{maxage}{
The "ultimate" age to which losses should be projected.
}
  \item{adol}{
If \code{TRUE} (the default), the growth function should be applied
to the length of time from the average date of loss ("adol")
of losses in the origin year.
If \code{FALSE}, the growth function should be applied
to the length of time since the beginning of the origin year.
}
  \item{adol.age}{
Only pertinent if \code{adol} is \code{TRUE}.
The age of the average date of losses within an origin period
in the same units as the "ages" of the \code{Triangle} matrix.
If \code{NULL} (the default) it will be assumed to be half the width
of an origin period (which would be the case if losses can be assumed
to occur uniformly over an origin period).
}
  \item{origin.width}{
Only pertinent if \code{adol} is \code{TRUE}.
The width of an origin period
in the same units as the "ages" of the \code{Triangle} matrix.
If \code{NULL} (the default) it will be assumed to be the mean difference
in the "ages" of the triangle, 
with a warning if not all differences are equal.
}
  \item{G}{
A \code{character} scalar identifying the "growth function."
The two growth functions defined at this time are "loglogistic"
(the default)
and "weibull".
}
}
\details{
Clark's "LDF method" assumes that the 
incremental losses across development periods in a loss triangle
are independent.
He assumes that the expected value of an incremental loss is
equal to the \emph{theoretical} expected ultimate loss (\bold{U})
(by origin year)
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
 
The parameters of Clark's "LDF method" are therefore:
U,
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
A \code{list} of class "ClarkLDF" with the components listed below.
("Key" to naming convention: all caps represent parameters;
mixed case represent origin-level amounts;
all-lower-case represent observation-level (origin, development age) results.)

    \item{method}{"LDF"}
    \item{growthFunction}{name of the growth function}
    \item{Origin}{names of the rows of the triangle}
    \item{CurrentValue}{the most mature value for each row}
    \item{CurrentAge}{the most mature "age" for each row}
    \item{CurrentAge.used}{the most mature age used; differs from "CurrentAge"
        when adol=TRUE}
    \item{MAXAGE}{same as 'maxage' argument}
    \item{MAXAGE.USED}{the maximum age for development 
        from the average date of loss; 
        differs from MAXAGE when adol=TRUE}
    \item{FutureValue}{the projected loss amounts ("Reserves" in Clark's paper)}
    \item{ProcessSE}{the process standard error of the FutureValue}
    \item{ParameterSE}{the parameter standard error of the FutureValue}
    \item{StdError}{the total standard error (process + parameter) 
        of the FutureValue}
    \item{Total}{a \code{list} with amounts that appear on the "Total" row
        for components "Origin" (="Total"), "CurrentValue", "FutureValue",
        "ProcessSE", "ParameterSE", and "StdError"}
    \item{PAR}{the estimated parameters}
    \item{THETAU}{the estimated parameters for the "ultimate loss" by 
        origin year ("U" in Clark's notation)}
    \item{THETAG}{the estimated parameters of the growth function}
    \item{GrowthFunction}{value of the growth function as of the 
        CurrentAge.used}
    \item{GrowthFunctionMAXAGE}{value of the growth function as of the
        MAXAGE.used}
    \item{SIGMA2}{the estimate of the sigma^2 parameter}
    \item{Ldf}{the "to-ultimate" loss development factor
        (sometimes called the "cumulative development factor")
        as defined in Clark's paper for each origin year}
    \item{LdfMAXAGE}{the "to-ultimate" loss development factor as of 
        the maximum age used in the model}
    \item{TruncatedLdf}{the "truncated" loss development factor for developing
        the current diagonal to
        the maximum age used in the model}
    \item{FutureValueGradient}{the gradient of the FutureValue function}
    \item{origin}{the origin year corresponding to each observed value of incremental loss}
    \item{age}{the age of each observed value of incremental loss}
    \item{fitted}{the expected value of each observed value of incremental loss 
        (the "mu's" of Clark's paper)}
    \item{residuals}{the actual minus fitted value for 
        each observed incremental loss}
    \item{stdresid}{ the standardized residuals for 
        each observed incremental loss
        (= residuals/sqrt(sigma2*fitted), 
        referred to as "normalized residuals" in Clark's paper; see p. 62)}
    \item{FI}{the "Fisher Information" matrix as defined in Clark's paper
    (i.e., without the sigma^2 value)}
    \item{value}{the value of the loglikelihood function at the solution point}
    \item{counts}{the number of calls to the loglikelihood function
        and its gradient function when numerical convergence was achieved}

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
\code{\link{ClarkCapeCod}}
}
\examples{
X <- GenIns
ClarkLDF(X, maxage=20)

# Clark's "LDF method" also works with triangles that have  
# more development periods than origin periods
ClarkLDF(qincurred, G="loglogistic")

# Method also works for a "triangle" with only one row:
# 1st row of GenIns; need "drop=FALSE" to avoid becoming a vector.
ClarkLDF(GenIns[1, , drop=FALSE], maxage=20)

# The age of the first evaluation may be prior to the end of the origin period.
# Here the ages are in units of "months" and the first evaluation 
# is at the end of the third quarter.
X <- GenIns
colnames(X) <- 12 * as.numeric(colnames(X)) - 3
# The indicated liability increases from 1st example above, 
# but not significantly.
ClarkLDF(X, maxage=240)
# When maxage is infinite, the phase shift has a more noticeable impact:
# a 4-5% increase of the overall CV.
x <- ClarkLDF(GenIns, maxage=Inf)
y <- ClarkLDF(X, maxage=Inf)
# Percent change in the bottom line CV:
(tail(y$Table65$TotalCV, 1) - tail(x$Table65$TotalCV, 1)) / tail(x$Table65$TotalCV, 1)

}
\keyword{ models }
