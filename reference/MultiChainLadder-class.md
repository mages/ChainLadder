# Class "MultiChainLadder" of Multivariate Chain-Ladder Results

This class includes the first and second moment estimation result using
the multivariate reserving methods in chain-ladder. Several primitive
methods and statistical methods are also created to facilitate further
analysis.

## Objects from the Class

Objects can be created by calls of the form
`new("MultiChainLadder", ...)`, or they could also be a result of calls
from `MultiChainLadder` or `JoinFitMse`.

## Slots

- `model`::

  Object of class `"character"`. Either "MCL" or "GMCL".

- `Triangles`::

  Object of class `"triangles"`. Input triangles.

- `models`::

  Object of class `"list"`. Fitted regression models using `systemfit`.

- `coefficients`::

  Object of class `"list"`. Estimated regression coefficients.

- `coefCov`::

  Object of class `"list"`. Estimated variance-covariance matrix of
  coefficients.

- `residCov`::

  Object of class `"list"`. Estimated residual covariance matrix.

- `fit.method`::

  Object of class `"character"`. Could be values of "SUR" or "OLS".

- `delta`::

  Object of class `"numeric"`. Parameter for weights.

- `int`::

  Object of class `"NullNum"`. Indicator of which periods have
  intercepts.

- `mse.ay`::

  Object of class `"matrix"`. Conditional mse for each accident year.

- `mse.ay.est`::

  Object of class `"matrix"`. Conditional estimation mse for each
  accident year.

- `mse.ay.proc`::

  Object of class `"matrix"`. Conditional process mse for each accident
  year.

- `mse.total`::

  Object of class `"matrix"`. Conditional mse for aggregated accident
  years.

- `mse.total.est`::

  Object of class `"matrix"`. Conditional estimation mse for aggregated
  accident years.

- `mse.total.proc`::

  Object of class `"matrix"`. Conditional process mse for aggregated
  accident years.

- `FullTriangles`::

  Object of class `"triangles"`. Completed triangles.

- `restrict.regMat`::

  Object of class `"NullList"`

## Extends

Class
`"`[`MultiChainLadderFit`](http://mages.github.io/ChainLadder/reference/MultiChainLadderFit-class.md)`"`,
directly. Class
`"`[`MultiChainLadderMse`](http://mages.github.io/ChainLadder/reference/MultiChainLadderMse-class.md)`"`,
directly.

## Methods

- \$:

  `signature(x = "MultiChainLadder")`: Method for primitive function
  `"$"`. It extracts a slot of `x` with a specified slot name, just as
  in list.

- \[\[:

  `signature(x = "MultiChainLadder", i = "numeric", j = "missing")`:
  Method for primitive function `"[["`. It extracts the i-th slot of a
  `"MultiChainLadder"` object, just as in list. `i` could be a vector.

- \[\[:

  `signature(x = "MultiChainLadder", i = "character", j = "missing")`:
  Method for primitive function `"[["`. It extracts the slots of a
  `"MultiChainLadder"` object with names in `i`, just as in list. `i`
  could be a vector.

- coef:

  `signature(object = "MultiChainLadder")`: Method for function `coef`,
  to extract the estimated development matrix. The output is a list.

- fitted:

  `signature(object = "MultiChainLadder")`: Method for function
  `fitted`, to calculate the fitted values in the original triangles.
  Note that the return value is a list of fitted valued based on the
  original scale, not the model scale which is first divided by
  \\Y\_{i,k}^{\delta/2}\\.

- names:

  `signature(x = "MultiChainLadder")`: Method for function `names`,
  which returns the slot names of a `"MultiChainLadder"` object.

- plot:

  `signature(x = "MultiChainLadder", y = "missing")`: See
  [`plot,MultiChainLadder,missing-method`](http://mages.github.io/ChainLadder/reference/plot-methods.md).

- residCov:

  `signature(object = "MultiChainLadder")`: S4 generic function and
  method to extract residual covariance from a `"MultiChainLadder"`
  object.

- residCor:

  `signature(object = "MultiChainLadder")`: S4 generic function and
  method to extract residual correlation from a `"MultiChainLadder"`
  object.

- residuals:

  `signature(object = "MultiChainLadder")`: Method for function
  `residuals`, to extract residuals from a system of regression
  equations. These residuals are based on model scale, and will not be
  equivalent to those on the original scale if \\\delta\\ is not set to
  be 0. One should use `rstandard` instead, which is independent of the
  scale.

- resid:

  `signature(object = "MultiChainLadder")`: Same as `residuals`.

- rstandard:

  `signature(model = "MultiChainLadder")`: S4 generic function and
  method to extract standardized residuals from a `"MultiChainLadder"`
  object.

- show:

  `signature(object = "MultiChainLadder")`: Method for `show`.

- summary:

  `signature(object = "MultiChainLadder")`: See
  [`summary,MultiChainLadder-method`](http://mages.github.io/ChainLadder/reference/summary-methods.md).

- vcov:

  `signature(object = "MultiChainLadder")`: Method for function `vcov`,
  to extract the variance-covariance matrix of a `"MultiChainLadder"`
  object. Note that the result is a list of `Bcov`, that is the
  variance-covariance matrix of the vectorized \\B\\.

## Author

Wayne Zhang <actuary_zhang@hotmail.com>

## See also

See also
[`MultiChainLadder`](http://mages.github.io/ChainLadder/reference/MultiChainLadder.md),[`summary,MultiChainLadder-method`](http://mages.github.io/ChainLadder/reference/summary-methods.md)
and
[`plot,MultiChainLadder,missing-method`](http://mages.github.io/ChainLadder/reference/plot-methods.md).

## Examples

``` r
# example for class "MultiChainLadder"
data(liab)
fit.liab <-  MultiChainLadder(Triangles = liab)
fit.liab
#> $`Summary Statistics for Triangle 1`
#>           Latest Dev.To.Date   Ultimate      IBNR     S.E     CV
#> 1        549,589      1.0000    549,589         0       0 0.0000
#> 2        562,795      0.9966    564,740     1,945   1,743 0.8961
#> 3        602,710      0.9913    608,013     5,303   6,720 1.2673
#> 4        784,632      0.9868    795,128    10,496   8,154 0.7769
#> 5        768,373      0.9805    783,649    15,276  10,497 0.6871
#> 6        811,100      0.9688    837,214    26,114  16,269 0.6230
#> 7        896,728      0.9544    939,595    42,867  19,154 0.4468
#> 8      1,022,241      0.9301  1,099,087    76,846  22,726 0.2957
#> 9      1,019,303      0.8818  1,155,890   136,587  29,055 0.2127
#> 10     1,141,750      0.7970  1,432,561   290,811  36,849 0.1267
#> 11     1,174,196      0.6761  1,736,765   562,569  57,024 0.1014
#> 12     1,032,684      0.4999  2,065,835 1,033,151  88,941 0.0861
#> 13       772,971      0.2907  2,658,834 1,885,863 192,733 0.1022
#> 14       204,325      0.0901  2,268,006 2,063,681 282,477 0.1369
#> Total 11,343,397      0.6484 17,494,907 6,151,510 419,293 0.0682
#> 
#> $`Summary Statistics for Triangle 2`
#>          Latest Dev.To.Date   Ultimate      IBNR     S.E      CV
#> 1       391,428       1.000    391,428         0       0  0.0000
#> 2       483,974       1.000    483,839      -135     604 -4.4846
#> 3       540,742       1.001    540,020      -722   1,324 -1.8323
#> 4       485,016       0.998    486,242     1,226   2,868  2.3394
#> 5       507,752       0.998    508,744       992   3,158  3.1844
#> 6       549,693       0.994    552,877     3,184   5,388  1.6923
#> 7       635,452       0.994    639,272     3,820   6,187  1.6193
#> 8       648,365       0.985    658,591    10,226   7,454  0.7289
#> 9       663,152       0.968    684,957    21,805   9,097  0.4172
#> 10      790,901       0.935    846,012    55,111  16,173  0.2935
#> 11      844,159       0.876    963,052   118,893  26,734  0.2249
#> 12      915,109       0.783  1,169,300   254,191  36,722  0.1445
#> 13      909,066       0.617  1,473,826   564,760  53,370  0.0945
#> 14      394,997       0.278  1,423,182 1,028,185 126,538  0.1231
#> Total 8,759,806       0.809 10,821,341 2,061,535 162,464  0.0788
#> 
#> $`Summary Statistics for Triangle 1+2`
#>           Latest Dev.To.Date   Ultimate      IBNR     S.E     CV
#> 1        941,017       1.000    941,017         0       0 0.0000
#> 2      1,046,769       0.998  1,048,579     1,810   1,851 1.0221
#> 3      1,143,452       0.996  1,148,032     4,580   7,859 1.7158
#> 4      1,269,648       0.991  1,281,370    11,722   9,545 0.8143
#> 5      1,276,125       0.987  1,292,393    16,268  12,133 0.7458
#> 6      1,360,793       0.979  1,390,091    29,298  18,913 0.6455
#> 7      1,532,180       0.970  1,578,868    46,688  22,448 0.4808
#> 8      1,670,606       0.951  1,757,679    87,073  25,913 0.2976
#> 9      1,682,455       0.914  1,840,846   158,391  33,294 0.2102
#> 10     1,932,651       0.848  2,278,572   345,921  45,253 0.1308
#> 11     2,018,355       0.748  2,699,816   681,461  72,050 0.1057
#> 12     1,947,793       0.602  3,235,135 1,287,342 112,187 0.0871
#> 13     1,682,037       0.407  4,132,660 2,450,623 222,927 0.0910
#> 14       599,322       0.162  3,691,189 3,091,867 342,127 0.1107
#> Total 20,103,203       0.710 28,316,248 8,213,045 500,607 0.0610
#> 

names(fit.liab)
#>  [1] "model"           "Triangles"       "models"          "coefficients"   
#>  [5] "coefCov"         "residCov"        "fit.method"      "delta"          
#>  [9] "int"             "restrict.regMat" "mse.ay"          "mse.ay.est"     
#> [13] "mse.ay.proc"     "mse.total"       "mse.total.est"   "mse.total.proc" 
#> [17] "FullTriangles"  
fit.liab[[1]]
#> $model
#> [1] "MCL"
#> 
fit.liab$model
#> [1] "MCL"
fit.liab@model
#> [1] "MCL"

do.call("rbind",coef(fit.liab))
#>       eq1_x[[1]] eq2_x[[2]]
#>  [1,]   3.226968  2.2223681
#>  [2,]   1.719491  1.2688125
#>  [3,]   1.352471  1.1200255
#>  [4,]   1.178849  1.0665251
#>  [5,]   1.106443  1.0356290
#>  [6,]   1.054712  1.0168421
#>  [7,]   1.026122  1.0097022
#>  [8,]   1.015121  1.0002188
#>  [9,]   1.012075  1.0038313
#> [10,]   1.006418  0.9994269
#> [11,]   1.004538  1.0038691
#> [12,]   1.005324  0.9989420
#> [13,]   1.003456  0.9997216
vcov(fit.liab)[[1]]
#>             eq1_x[[1]]  eq2_x[[2]]
#> eq1_x[[1]] 0.012892793 0.001805951
#> eq2_x[[2]] 0.001805951 0.004262759
residCov(fit.liab)[[1]]
#>           eq1       eq2
#> eq1 17649.406  3462.246
#> eq2  3462.246 11106.972
head(do.call("rbind",rstandard(fit.liab)))
#>           eq1        eq2
#> 1 -0.93310911 -0.1775342
#> 2 -0.23597735 -0.8092400
#> 3  0.08335632 -0.3371644
#> 4 -0.02340017 -0.4127673
#> 5 -0.97850855 -0.8516139
#> 6  0.39880326 -1.3145969
```
