\name{clarkLDF}
\alias{clarkLDF}
\title{
Clark LDF method
}
\description{
Analyze loss triangle using Clark's LDF method.
}
\usage{
clarkLDF(data, cumulative = TRUE, adol = TRUE, 
        maxage = c(Inf, Inf), G = "loglogistic")
}
\arguments{
  \item{data}{
A loss triangle in the form of a matrix.
The column names of the matrix should be able to be interpreted
as the "age" of the losses in that column.
The row names of the matrix should uniquely define the year of
origin of the losses in that row.
Losses may be inception-to-date or incremental.
The triangle should be "regular" in the sense that the
all-year weighted average chain ladder method can be run on the data
(as an initial guess for the parameter vector of ultimate losses).
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
This function uses \code{optim} to perform maximum likelihood
estimation on the parameters of Clark's model:
\cr
\bold{u} = a vector of \emph{a priori} ultimate losses
\cr
and
\cr
\bold{thetaG} = a vector of parameters of the growth function.
}

\value{
A \code{list} with the following components:
\itemize{
    \item method: "LDF"
    \item growthFunction: name of the growth function
    \item Table65: the table of losses, standard errors and CVs as shown
    on p. 65 of the paper
    \item Table64: the table of ages, growth function values, LDFs,
    truncated LDFs, "ultimate" losses, and "reserves" as shown on p. 64 
    of the paper
    \item par: the parameter estimates returned by \code{optim}
    \item sigma2: the sigma-squared "scaling parameter"
    \item LDF: the vector of to-ultimate loss development factors
    \item dR: the gradient of the reserves
    \item origin: rownames(data) from the "long format" of \code{data}
    \item age: colnames(data) from the "long format" of \code{data}
    \item fitted: the expected values (the "mu's") of the incremental losses
    \item residuals: the difference between the actual and fitted values
    \item stdresid: the standardized residuals 
    = residuals/sqrt(sigma2*fitted)
    (referred to as "normalized residuals" in the paper; see p. 62)
    \item FI: the Fisher Information matrix
}
}
\references{
Clark, David R., 
"LDF Curve-Fitting and Stochastic Reserving: A Maximum Likelihood Approach",
\emph{Casualty Actuarial Society Forum}, Fall, 2003
}
\author{
Daniel Murphy
}
\seealso{
\code{clarkCapeCod}
}
\examples{
library(ChainLadder)
X <- GenIns
colnames(X) <- 12*as.numeric(colnames(X))
LDF.loglogistic  <- clarkLDF(X, maxage=240)
print(LDF.loglogistic)
plot(LDF.loglogistic)
}
