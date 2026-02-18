# Methods for Function summary

Methods for function `summary` to calculate summary statistics from a
"MultiChainLadder" object.

## Usage

``` r
# S4 method for class 'MultiChainLadder'
summary(object, portfolio=NULL,...)
```

## Arguments

- object:

  object of class `"MultiChainLadder"`

- portfolio:

  character strings specifying which triangles to be summed up as
  portfolio.

- ...:

  optional arguments to `summary` methods

## Details

`summary` calculations the summary statistics for each triangle and the
whole portfolio from `portfolio`. `portfolio` defaults to the sum of all
input triangles. It can also be specified as "i+j" format, which means
the sum of the i-th and j-th triangle as portfolio. For example, `"1+3"`
means the sum of the first and third triangle as portfolio.

## Value

The `summary` function returns an object of class
"MultiChainLadderSummary" that has the following slots:

- Triangles:

  input triangles

- FullTriangles:

  predicted triangles

- S.E.Full:

  a list of prediction errors for each cell

- S.E.Est.Full:

  a list of estimation errors for each cell

- S.E.Proc.Full:

  a list of process errors for each cell

- Ultimate:

  predicted ultimate losses for each triangle and portfolio

- Latest:

  latest observed losses for each triangle and portfolio

- IBNR:

  predicted IBNR for each triangle and portfolio

- S.E.Ult:

  a matrix of prediction errors of ultimate losses for each triangle and
  portfolio

- S.E.Est.Ult:

  a matrix of estimation errors of ultimate losses for each triangle and
  portfolio

- S.E.Proc.Ult:

  a matrix of process errors of ultimate losses for each triangle and
  portfolio

- report.summary:

  summary statistics for each triangle and portfolio

- coefficients:

  estimated coefficients from `systemfit`. They are put into the matrix
  format for GMCL

- coefCov:

  estimated variance-covariance matrix returned by `systemfit`

- residCov:

  estimated residual covariance matrix returned by `systemfit`

- rstandard:

  standardized residuals

- fitted.values:

  fitted.values

- residCor:

  residual correlation

- model.summary:

  summary statistics for the cofficients including p-values

- portfolio:

  how portfolio is calculated

## Author

Wayne Zhang <actuary_zhang@hotmail.com>

## See also

See Also
[`MultiChainLadder`](http://mages.github.io/ChainLadder/reference/MultiChainLadder.md)

## Examples

``` r
data(GenIns)
fit.bbmw=MultiChainLadder(list(GenIns),fit.method="OLS", mse.method="Independence")
summary(fit.bbmw)
#> $`Summary Statistics for Input Triangle`
#>           Latest Dev.To.Date   Ultimate       IBNR       S.E    CV
#> 1      3,901,463      1.0000  3,901,463          0         0 0.000
#> 2      5,339,085      0.9826  5,433,719     94,634    75,535 0.798
#> 3      4,909,315      0.9127  5,378,826    469,511   121,700 0.259
#> 4      4,588,268      0.8661  5,297,906    709,638   133,551 0.188
#> 5      3,873,311      0.7973  4,858,200    984,889   261,412 0.265
#> 6      3,691,712      0.7223  5,111,171  1,419,459   411,028 0.290
#> 7      3,483,130      0.6153  5,660,771  2,177,641   558,356 0.256
#> 8      2,864,498      0.4222  6,784,799  3,920,301   875,430 0.223
#> 9      1,363,294      0.2416  5,642,266  4,278,972   971,385 0.227
#> 10       344,014      0.0692  4,969,825  4,625,811 1,363,385 0.295
#> Total 34,358,090      0.6478 53,038,946 18,680,856 2,447,618 0.131
#> 
```
