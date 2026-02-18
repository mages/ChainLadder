# Methods for BootChainLadder objects

`summary`, `print`, `mean`, and `quantile` methods for `BootChainLadder`
objects

## Usage

``` r
# S3 method for class 'BootChainLadder'
summary(object, probs=c(0.75,0.95), ...)

# S3 method for class 'BootChainLadder'
print(x, probs=c(0.75,0.95), ...)

# S3 method for class 'BootChainLadder'
quantile(x, probs=c(0.75, 0.95), na.rm = FALSE,
              names = TRUE, type = 7,...)

# S3 method for class 'BootChainLadder'
mean(x, ...)

# S3 method for class 'BootChainLadder'
residuals(object, ...)
```

## Arguments

- x, object:

  output from
  [`BootChainLadder`](http://mages.github.io/ChainLadder/reference/BootChainLadder.md)

- probs:

  numeric vector of probabilities with values in \[0,1\], see
  [`quantile`](https://rdrr.io/r/stats/quantile.html) for more help

- na.rm:

  logical; if true, any `NA` and `NaN`'s are removed from 'x' before the
  quantiles are computed, see
  [`quantile`](https://rdrr.io/r/stats/quantile.html) for more help

- names:

  logical; if true, the result has a `names` attribute. Set to `FALSE`
  for speedup with many 'probs', see
  [`quantile`](https://rdrr.io/r/stats/quantile.html) for more help

- type:

  an integer between 1 and 9 selecting one of the nine quantile
  algorithms detailed below to be used, see
  [`quantile`](https://rdrr.io/r/stats/quantile.html)

- ...:

  further arguments passed to or from other methods

## Details

`print.BootChainLadder` calls `summary.BootChainLadder` and prints a
formatted version of the summary. `residuals.BootChainLadder` gives the
residual triangle of the expected chain-ladder minus the actual triangle
back.

## Value

`summary.BootChainLadder`, `mean.BootChainLadder`, and
`quantile.BootChainLadder`, give a list with two elements back:

- ByOrigin:

  data frame with summary/mean/quantile statistics by origin period

- Totals:

  data frame with total summary/mean/quantile statistics for all origin
  period

## Author

Markus Gesmann

## See also

See also
[`BootChainLadder`](http://mages.github.io/ChainLadder/reference/BootChainLadder.md)

## Examples

``` r
B <- BootChainLadder(RAA, R=999, process.distr="gamma")
B
#> BootChainLadder(Triangle = RAA, R = 999, process.distr = "gamma")
#> 
#>      Latest Mean Ultimate Mean IBNR IBNR.S.E IBNR 75% IBNR 95%
#> 1981 18,834        18,834         0        0        0        0
#> 1982 16,704        16,911       207      717      204    1,500
#> 1983 23,466        24,130       664    1,303    1,174    3,251
#> 1984 27,067        28,749     1,682    1,876    2,627    5,229
#> 1985 26,180        28,981     2,801    2,225    3,969    7,003
#> 1986 15,852        19,572     3,720    2,510    5,105    8,553
#> 1987 12,314        17,756     5,442    3,283    7,325   11,542
#> 1988 13,112        24,211    11,099    5,044   14,168   20,025
#> 1989  5,395        16,048    10,653    6,142   14,166   22,016
#> 1990  2,063        18,694    16,631   13,628   23,333   41,383
#> 
#>                  Totals
#> Latest:         160,987
#> Mean Ultimate:  213,885
#> Mean IBNR:       52,898
#> IBNR.S.E         18,440
#> Total IBNR 75%:  63,378
#> Total IBNR 95%:  86,371
summary(B)
#> $ByOrigin
#>      Latest Mean Ultimate  Mean IBNR    SD IBNR   IBNR 75%  IBNR 95%
#> 1981  18834      18834.00     0.0000     0.0000     0.0000     0.000
#> 1982  16704      16911.25   207.2453   717.3973   204.0868  1500.393
#> 1983  23466      24130.29   664.2855  1303.4774  1173.6871  3251.474
#> 1984  27067      28748.76  1681.7586  1875.7839  2626.7067  5228.895
#> 1985  26180      28980.64  2800.6365  2225.4811  3968.9805  7003.371
#> 1986  15852      19571.81  3719.8056  2510.0076  5105.1333  8552.575
#> 1987  12314      17755.51  5441.5072  3283.2254  7325.0361 11541.973
#> 1988  13112      24211.45 11099.4500  5044.1807 14168.2720 20025.453
#> 1989   5395      16047.77 10652.7681  6142.1904 14165.8420 22016.093
#> 1990   2063      18693.73 16630.7329 13627.7411 23333.2733 41383.161
#> 
#> $Totals
#>                    Totals
#> Latest:         160987.00
#> Mean Ultimate:  213885.19
#> Mean IBNR:       52898.19
#> SD IBNR:         18440.27
#> Total IBNR 75%:  63378.28
#> Total IBNR 95%:  86370.89
#> 
mean(B)
#> $ByOrigin
#>       Mean IBNR
#> 1981     0.0000
#> 1982   207.2453
#> 1983   664.2855
#> 1984  1681.7586
#> 1985  2800.6365
#> 1986  3719.8056
#> 1987  5441.5072
#> 1988 11099.4500
#> 1989 10652.7681
#> 1990 16630.7329
#> 
#> $Totals
#>               Total
#> Mean IBNR: 52898.19
#> 
quantile(B, c(0.75,0.95,0.99, 0.995))
#> $ByOrigin
#>        IBNR 75%  IBNR 95%  IBNR 99% IBNR 99.5%
#> 1981     0.0000     0.000     0.000      0.000
#> 1982   204.0868  1500.393  3390.162   3905.316
#> 1983  1173.6871  3251.474  4849.996   5233.261
#> 1984  2626.7067  5228.895  7672.609   8328.949
#> 1985  3968.9805  7003.371  9103.607   9994.590
#> 1986  5105.1333  8552.575 11414.829  12731.768
#> 1987  7325.0361 11541.973 15163.927  16641.212
#> 1988 14168.2720 20025.453 25195.163  26429.739
#> 1989 14165.8420 22016.093 28631.988  29945.831
#> 1990 23333.2733 41383.161 55580.473  67118.348
#> 
#> $Totals
#>                Totals
#> IBNR 75%:    63378.28
#> IBNR 95%:    86370.89
#> IBNR 99%:   106649.40
#> IBNR 99.5%: 115226.68
#> 
```
