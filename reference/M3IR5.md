# Run off triangle of claims data

Run off triangle of simulated incremental claims data

## Usage

``` r
data(M3IR5)
```

## Format

A matrix with simulated incremental claims of 14 accident years and 14
development years.

## Source

Appendix A7 in B. Zehnwirth. Probabilistic Development Factor Models
with Applications to Loss Reserve Variability, Prediction Intervals, and
Risk Based Capital. *Casualty Actuarial Science Forum.* Spring 1994.
Vol. 2.

## Examples

``` r
M3IR5
#>       dev
#> origin      1      2      3      4      5      6      7      8      9     10
#>   1978 108651  97529  75879  69418  55542  62875  63697  72468  65114  62436
#>   1979  98706  95216  83025  72396 104914  94174  77103  70538  71747  77567
#>   1980 133106 109743  96365 130993 112860  87108  99698  92494  89224  82117
#>   1981 125731 141478 144336 124854 107034 122015 110514  93517  95885  97626
#>   1982 161765 174888 168704 156514 145495 138954 125480 106927 111179 118054
#>   1983 226364 203191 179136 159835 156670 153108 142187 160637 139511     NA
#>   1984 228411 216837 242050 249422 205644 220996 169549 166858     NA     NA
#>   1985 277868 262472 265375 227499 221660 247187 207918     NA     NA     NA
#>   1986 302519 360015 343485 224336 220334 234427     NA     NA     NA     NA
#>   1987 393525 388054 383425 326081 271278     NA     NA     NA     NA     NA
#>   1988 450855 333667 398276 382277     NA     NA     NA     NA     NA     NA
#>   1989 572576 568013 382277     NA     NA     NA     NA     NA     NA     NA
#>   1990 576021 469724     NA     NA     NA     NA     NA     NA     NA     NA
#>   1991 580068     NA     NA     NA     NA     NA     NA     NA     NA     NA
#>       dev
#> origin    11    12    13    14
#>   1978 57983 56551 48528 39023
#>   1979 68934 70467 43560    NA
#>   1980 78190 78504    NA    NA
#>   1981 83692    NA    NA    NA
#>   1982    NA    NA    NA    NA
#>   1983    NA    NA    NA    NA
#>   1984    NA    NA    NA    NA
#>   1985    NA    NA    NA    NA
#>   1986    NA    NA    NA    NA
#>   1987    NA    NA    NA    NA
#>   1988    NA    NA    NA    NA
#>   1989    NA    NA    NA    NA
#>   1990    NA    NA    NA    NA
#>   1991    NA    NA    NA    NA
plot(M3IR5)

plot(incr2cum(M3IR5), lattice=TRUE)
```
