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
#> 1982 16,704        16,883       179      780      220    1,368
#> 1983 23,466        24,112       646    1,304    1,089    3,174
#> 1984 27,067        28,776     1,709    1,991    2,675    5,289
#> 1985 26,180        28,987     2,807    2,434    3,965    7,628
#> 1986 15,852        19,540     3,688    2,441    4,947    8,199
#> 1987 12,314        17,686     5,372    3,126    7,008   11,280
#> 1988 13,112        24,275    11,163    5,075   14,001   20,196
#> 1989  5,395        16,572    11,177    6,377   14,909   23,720
#> 1990  2,063        19,710    17,647   13,552   25,784   41,999
#> 
#>                  Totals
#> Latest:         160,987
#> Mean Ultimate:  215,376
#> Mean IBNR:       54,389
#> IBNR.S.E         19,370
#> Total IBNR 75%:  65,617
#> Total IBNR 95%:  89,191
summary(B)
#> $ByOrigin
#>      Latest Mean Ultimate  Mean IBNR   SD IBNR   IBNR 75%  IBNR 95%
#> 1981  18834      18834.00     0.0000     0.000     0.0000     0.000
#> 1982  16704      16883.32   179.3249   779.533   220.3238  1367.977
#> 1983  23466      24111.65   645.6530  1304.426  1088.9090  3173.708
#> 1984  27067      28776.20  1709.1990  1990.882  2674.8965  5289.365
#> 1985  26180      28987.16  2807.1556  2434.282  3964.5693  7628.258
#> 1986  15852      19539.69  3687.6936  2440.635  4946.8657  8198.906
#> 1987  12314      17686.37  5372.3663  3125.773  7007.9105 11280.426
#> 1988  13112      24275.25 11163.2478  5075.214 14000.8210 20195.902
#> 1989   5395      16572.34 11177.3363  6377.035 14909.4309 23719.853
#> 1990   2063      19710.25 17647.2453 13552.213 25783.6734 41999.016
#> 
#> $Totals
#>                    Totals
#> Latest:         160987.00
#> Mean Ultimate:  215376.22
#> Mean IBNR:       54389.22
#> SD IBNR:         19370.14
#> Total IBNR 75%:  65617.22
#> Total IBNR 95%:  89191.21
#> 
mean(B)
#> $ByOrigin
#>       Mean IBNR
#> 1981     0.0000
#> 1982   179.3249
#> 1983   645.6530
#> 1984  1709.1990
#> 1985  2807.1556
#> 1986  3687.6936
#> 1987  5372.3663
#> 1988 11163.2478
#> 1989 11177.3363
#> 1990 17647.2453
#> 
#> $Totals
#>               Total
#> Mean IBNR: 54389.22
#> 
quantile(B, c(0.75,0.95,0.99, 0.995))
#> $ByOrigin
#>        IBNR 75%  IBNR 95%  IBNR 99% IBNR 99.5%
#> 1981     0.0000     0.000     0.000      0.000
#> 1982   220.3238  1367.977  3060.579   3629.873
#> 1983  1088.9090  3173.708  4960.796   6270.826
#> 1984  2674.8965  5289.365  7991.119   9064.156
#> 1985  3964.5693  7628.258 10336.713  11558.160
#> 1986  4946.8657  8198.906 10951.729  12853.460
#> 1987  7007.9105 11280.426 14513.349  17229.377
#> 1988 14000.8210 20195.902 26784.041  28168.720
#> 1989 14909.4309 23719.853 28558.182  32446.893
#> 1990 25783.6734 41999.016 53969.268  61867.263
#> 
#> $Totals
#>                Totals
#> IBNR 75%:    65617.22
#> IBNR 95%:    89191.21
#> IBNR 99%:   107677.06
#> IBNR 99.5%: 121189.77
#> 
```
