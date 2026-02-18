# Summary and print function for Mack-chain-ladder

`summary` and `print` methods for a `MackChainLadder` object

## Usage

``` r
# S3 method for class 'MackChainLadder'
summary(object, ...)

# S3 method for class 'MackChainLadder'
print(x, ...)
```

## Arguments

- x, object:

  object of class `"MackChainLadder"`

- ...:

  optional arguments to `print` or `summary` methods

## Details

`print.MackChainLadder` calls `summary.MackChainLadder` and prints a
formatted version of the summary.

## Value

`summary.MackChainLadder` gives a list of two elements back

- ByOrigin:

  data frame with `Latest` (latest actual claims costs), `Dev.To.Date`
  (chain-ladder development to date), `Ultimate` (estimated ultimate
  claims cost), `IBNR` (estimated IBNR), `Mack.S.E` (Mack's estimation
  of the standard error of the IBNR), and `CV(IBNR)` (Coefficient of
  Variance=Mack.S.E/IBNR)

- Totals:

  data frame of totals over all origin periods. The items follow the
  same naming convention as in `ByOrigin` above

## Author

Markus Gesmann

## See also

See also
[`MackChainLadder`](http://mages.github.io/ChainLadder/reference/MackChainLadder.md),
[`plot.MackChainLadder`](http://mages.github.io/ChainLadder/reference/plot.MackChainLadder.md)

## Examples

``` r
 R <- MackChainLadder(RAA)
 R
#> MackChainLadder(Triangle = RAA)
#> 
#>      Latest Dev.To.Date Ultimate   IBNR Mack.S.E CV(IBNR)
#> 1981 18,834       1.000   18,834      0        0      NaN
#> 1982 16,704       0.991   16,858    154      143    0.928
#> 1983 23,466       0.974   24,083    617      592    0.959
#> 1984 27,067       0.943   28,703  1,636      713    0.436
#> 1985 26,180       0.905   28,927  2,747    1,452    0.529
#> 1986 15,852       0.813   19,501  3,649    1,995    0.547
#> 1987 12,314       0.694   17,749  5,435    2,204    0.405
#> 1988 13,112       0.546   24,019 10,907    5,354    0.491
#> 1989  5,395       0.336   16,045 10,650    6,332    0.595
#> 1990  2,063       0.112   18,402 16,339   24,566    1.503
#> 
#>               Totals
#> Latest:   160,987.00
#> Dev:            0.76
#> Ultimate: 213,122.23
#> IBNR:      52,135.23
#> Mack.S.E   26,880.74
#> CV(IBNR):       0.52
 summary(R)
#> $ByOrigin
#>      Latest Dev.To.Date Ultimate       IBNR   Mack.S.E  CV(IBNR)
#> 1981  18834   1.0000000 18834.00     0.0000     0.0000       NaN
#> 1982  16704   0.9908676 16857.95   153.9539   142.9317 0.9284058
#> 1983  23466   0.9743653 24083.37   617.3709   592.1483 0.9591451
#> 1984  27067   0.9429978 28703.14  1636.1422   712.8539 0.4356919
#> 1985  26180   0.9050451 28926.74  2746.7363  1452.0903 0.5286603
#> 1986  15852   0.8128771 19501.10  3649.1032  1994.9878 0.5467063
#> 1987  12314   0.6937737 17749.30  5435.3026  2203.8385 0.4054675
#> 1988  13112   0.5458968 24019.19 10907.1925  5354.3405 0.4909000
#> 1989   5395   0.3362422 16044.98 10649.9841  6331.5430 0.5945120
#> 1990   2063   0.1121047 18402.44 16339.4425 24565.7757 1.5034647
#> 
#> $Totals
#>                  Totals
#> Latest:    1.609870e+05
#> Dev:       7.553740e-01
#> Ultimate:  2.131222e+05
#> IBNR:      5.213523e+04
#> Mack S.E.: 2.688074e+04
#> CV(IBNR):  5.155965e-01
#> 
 summary(R)$ByOrigin$Ultimate
#>  [1] 18834.00 16857.95 24083.37 28703.14 28926.74 19501.10 17749.30 24019.19
#>  [9] 16044.98 18402.44
```
