# Clark LDF method

Analyze loss triangle using Clark's LDF (loss development factor)
method.

## Usage

``` r
ClarkLDF(Triangle, cumulative = TRUE, maxage = Inf, 
        adol = TRUE, adol.age = NULL, origin.width = NULL,
        G = "loglogistic")
```

## Arguments

- Triangle:

  A loss triangle in the form of a matrix. The number of columns must be
  at least four; the number of rows may be as few as 1. The column names
  of the matrix should be able to be interpreted as the "age" of the
  losses in that column. The row names of the matrix should uniquely
  define the year of origin of the losses in that row. Losses may be
  inception-to-date or incremental.

  The "ages" of the triangle can be "phase shifted" – i.e., the first
  age need not be as at the end of the origin period. (See the Examples
  section.) Nor need the "ages" be uniformly spaced. However, when the
  ages are not uniformly spaced, it would be prudent to specify the
  `origin.width` argument.

- cumulative:

  If `TRUE` (the default), values in `Triangle` are inception to date.
  If `FALSE`, `Triangle` holds incremental losses.

- maxage:

  The "ultimate" age to which losses should be projected.

- adol:

  If `TRUE` (the default), the growth function should be applied to the
  length of time from the average date of loss ("adol") of losses in the
  origin year. If `FALSE`, the growth function should be applied to the
  length of time since the beginning of the origin year.

- adol.age:

  Only pertinent if `adol` is `TRUE`. The age of the average date of
  losses within an origin period in the same units as the "ages" of the
  `Triangle` matrix. If `NULL` (the default) it will be assumed to be
  half the width of an origin period (which would be the case if losses
  can be assumed to occur uniformly over an origin period).

- origin.width:

  Only pertinent if `adol` is `TRUE`. The width of an origin period in
  the same units as the "ages" of the `Triangle` matrix. If `NULL` (the
  default) it will be assumed to be the mean difference in the "ages" of
  the triangle, with a warning if not all differences are equal.

- G:

  A `character` scalar identifying the "growth function." The two growth
  functions defined at this time are "loglogistic" (the default) and
  "weibull".

## Details

Clark's "LDF method" assumes that the incremental losses across
development periods in a loss triangle are independent. He assumes that
the expected value of an incremental loss is equal to the *theoretical*
expected ultimate loss (**U**) (by origin year) times the change in the
*theoretical* underlying growth function over the development period.
Clark models the growth function, also called the percent of ultimate,
by either the loglogistic function (a.k.a., "the inverse power curve")
or the weibull function. Clark completes his incremental loss model by
wrapping the expected values within an overdispersed poisson (ODP)
process where the "scale factor" sigma^2 is assumed to be a known
constant for all development periods.

The parameters of Clark's "LDF method" are therefore: U, and omega and
theta (the parameters of the **loglogistic** and **weibull** growth
functions). Finally, Clark uses maximum likelihood to parameterize his
model, uses the ODP process to estimate process risk, and uses the
Cramer-Rao theorem and the "delta method" to estimate parameter risk.

Clark recommends inspecting the residuals to help assess the
reasonableness of the model relative to the actual data (see
[`plot.clark`](http://mages.github.io/ChainLadder/reference/plot.clark.md)
below).

## Value

A `list` of class "ClarkLDF" with the components listed below. ("Key" to
naming convention: all caps represent parameters; mixed case represent
origin-level amounts; all-lower-case represent observation-level
(origin, development age) results.)

- method:

  "LDF"

- growthFunction:

  name of the growth function

- Origin:

  names of the rows of the triangle

- CurrentValue:

  the most mature value for each row

- CurrentAge:

  the most mature "age" for each row

- CurrentAge.used:

  the most mature age used; differs from "CurrentAge" when adol=TRUE

- MAXAGE:

  same as 'maxage' argument

- MAXAGE.USED:

  the maximum age for development from the average date of loss; differs
  from MAXAGE when adol=TRUE

- FutureValue:

  the projected loss amounts ("Reserves" in Clark's paper)

- ProcessSE:

  the process standard error of the FutureValue

- ParameterSE:

  the parameter standard error of the FutureValue

- StdError:

  the total standard error (process + parameter) of the FutureValue

- Total:

  a `list` with amounts that appear on the "Total" row for components
  "Origin" (="Total"), "CurrentValue", "FutureValue", "ProcessSE",
  "ParameterSE", and "StdError"

- PAR:

  the estimated parameters

- THETAU:

  the estimated parameters for the "ultimate loss" by origin year ("U"
  in Clark's notation)

- THETAG:

  the estimated parameters of the growth function

- GrowthFunction:

  value of the growth function as of the CurrentAge.used

- GrowthFunctionMAXAGE:

  value of the growth function as of the MAXAGE.used

- SIGMA2:

  the estimate of the sigma^2 parameter

- Ldf:

  the "to-ultimate" loss development factor (sometimes called the
  "cumulative development factor") as defined in Clark's paper for each
  origin year

- LdfMAXAGE:

  the "to-ultimate" loss development factor as of the maximum age used
  in the model

- TruncatedLdf:

  the "truncated" loss development factor for developing the current
  diagonal to the maximum age used in the model

- FutureValueGradient:

  the gradient of the FutureValue function

- origin:

  the origin year corresponding to each observed value of incremental
  loss

- age:

  the age of each observed value of incremental loss

- fitted:

  the expected value of each observed value of incremental loss (the
  "mu's" of Clark's paper)

- residuals:

  the actual minus fitted value for each observed incremental loss

- stdresid:

  the standardized residuals for each observed incremental loss (=
  residuals/sqrt(sigma2\*fitted), referred to as "normalized residuals"
  in Clark's paper; see p. 62)

- FI:

  the "Fisher Information" matrix as defined in Clark's paper (i.e.,
  without the sigma^2 value)

- value:

  the value of the loglikelihood function at the solution point

- counts:

  the number of calls to the loglikelihood function and its gradient
  function when numerical convergence was achieved

## References

Clark, David R., "LDF Curve-Fitting and Stochastic Reserving: A Maximum
Likelihood Approach", *Casualty Actuarial Society Forum*, Fall, 2003
<https://www.casact.org/sites/default/files/database/forum_03fforum_03ff041.pdf>

## Author

Daniel Murphy

## See also

[`ClarkCapeCod`](http://mages.github.io/ChainLadder/reference/ClarkCapeCod.md)

## Examples

``` r
X <- GenIns
ClarkLDF(X, maxage=20)
#>  Origin CurrentValue    Ldf UltimateValue FutureValue  StdError  CV%
#>       1    3,901,463  1.171     4,567,994     666,531   261,622 39.3
#>       2    5,339,085  1.217     6,496,508   1,157,423   375,333 32.4
#>       3    4,909,315  1.278     6,274,401   1,365,086   420,492 30.8
#>       4    4,588,268  1.363     6,253,962   1,665,694   483,350 29.0
#>       5    3,873,311  1.487     5,759,792   1,886,481   530,086 28.1
#>       6    3,691,712  1.681     6,206,592   2,514,880   653,577 26.0
#>       7    3,483,130  2.018     7,029,915   3,546,785   847,276 23.9
#>       8    2,864,498  2.709     7,760,999   4,896,501 1,113,865 22.7
#>       9    1,363,294  4.661     6,354,929   4,991,635 1,344,652 26.9
#>      10      344,014 19.091     6,567,720   6,223,706 2,892,103 46.5
#>   Total   34,358,090           63,272,813  28,914,723 4,848,938 16.8

# Clark's "LDF method" also works with triangles that have  
# more development periods than origin periods
ClarkLDF(qincurred, G="loglogistic")
#>  Origin CurrentValue     Ldf UltimateValue FutureValue StdError   CV%
#>    1995        1,100   1.006         1,107           7       17 242.9
#>    1996        1,300   1.008         1,310          10       21 200.1
#>    1997        1,200   1.010         1,212          12       23 184.0
#>    1998        1,298   1.014         1,316          18       27 154.5
#>    1999        1,583   1.019         1,613          30       36 120.5
#>    2000        1,066   1.027         1,095          29       35 122.3
#>    2001        1,411   1.042         1,470          59       51  87.4
#>    2002        1,820   1.070         1,948         128       78  61.2
#>    2003        1,221   1.138         1,389         168       92  54.7
#>    2004        1,212   1.352         1,638         426      162  38.0
#>    2005          422   2.643         1,115         693      280  40.4
#>    2006           13 339.514         4,414       4,401    7,891 179.3
#>   Total       13,646                19,627       5,981    7,891 131.9

# Method also works for a "triangle" with only one row:
# 1st row of GenIns; need "drop=FALSE" to avoid becoming a vector.
ClarkLDF(GenIns[1, , drop=FALSE], maxage=20)
#>  Origin CurrentValue   Ldf UltimateValue FutureValue StdError  CV%
#>       1    3,901,463 1.176     4,589,676     688,213  334,290 48.6
#>   Total    3,901,463           4,589,676     688,213  334,290 48.6

# The age of the first evaluation may be prior to the end of the origin period.
# Here the ages are in units of "months" and the first evaluation 
# is at the end of the third quarter.
X <- GenIns
colnames(X) <- 12 * as.numeric(colnames(X)) - 3
# The indicated liability increases from 1st example above, 
# but not significantly.
ClarkLDF(X, maxage=240)
#>  Origin CurrentValue    Ldf UltimateValue FutureValue  StdError  CV%
#>       1    3,901,463  1.189     4,638,605     737,142   277,588 37.7
#>       2    5,339,085  1.237     6,605,692   1,266,607   397,311 31.4
#>       3    4,909,315  1.301     6,386,651   1,477,336   442,507 30.0
#>       4    4,588,268  1.388     6,369,609   1,781,341   505,579 28.4
#>       5    3,873,311  1.514     5,865,118   1,991,807   550,324 27.6
#>       6    3,691,712  1.710     6,311,201   2,619,489   673,463 25.7
#>       7    3,483,130  2.047     7,128,319   3,645,189   866,256 23.8
#>       8    2,864,498  2.742     7,853,050   4,988,552 1,131,966 22.7
#>       9    1,363,294  4.810     6,556,934   5,193,640 1,389,410 26.8
#>      10      344,014 19.039     6,549,543   6,205,529 2,862,366 46.1
#>   Total   34,358,090           64,264,722  29,906,632 4,983,456 16.7
# When maxage is infinite, the phase shift has a more noticeable impact:
# a 4-5% increase of the overall CV.
x <- ClarkLDF(GenIns, maxage=Inf)
y <- ClarkLDF(X, maxage=Inf)
# Percent change in the bottom line CV:
(tail(y$Table65$TotalCV, 1) - tail(x$Table65$TotalCV, 1)) / tail(x$Table65$TotalCV, 1)
#> numeric(0)
```
