# quantile function for Mack-chain-ladder

`quantile` methods for a `MackChainLadder` object

## Usage

``` r
# S3 method for class 'MackChainLadder'
quantile(x, probs=c(0.75, 0.95), na.rm = FALSE,
              names = TRUE, type = 7,...)
```

## Arguments

- x:

  object of class `"MackChainLadder"`

- probs:

  numeric vector of probabilities with values in \[0,1\], see
  [`quantile`](https://rdrr.io/r/stats/quantile.html) for more help

- na.rm:

  not used

- names:

  not used

- type:

  not used

- ...:

  not used

## Details

Reserves at the desired quantile using the Cornish-Fisher expansion.

The Cornish-Fisher expansion relies on the first three moments of the
reserve risk distribution: The Best estimate resulting from the
Chain-Ladder projection, the Mack standard deviation and the skewness of
the distribution (for skewness estimation, see references below).

The quantile estimation requires only that the standard Mack assumptions
are met.

For details of the underlying calculations, see references below.

## Value

`quantile.MackChainLadder` gives a list with two elements back:

- ByOrigin:

  data frame with skewness and quantile statistics by origin period

- Totals:

  data frame with total skewness and quantile statistics across all
  origin periods

## References

Eric Dal Moro and Yuriy Krvavych. Probability of sufficiency of Solvency
II Reserve risk margins: Practical approximations. *ASTIN Bulletin*,
47(3), 737-785

Dal Moro, Eric, A Closed-Form Formula for the Skewness Estimation of
Non-Life Reserve Risk Distribution (September 15, 2013). Available at
SSRN: https://ssrn.com/abstract=2344297 or
https://dx.doi.org/10.2139/ssrn.2344297

## Author

Eric Dal Moro <eric_dal_moro@yahoo.com>

## See also

See also
[`MackChainLadder`](http://mages.github.io/ChainLadder/reference/MackChainLadder.md)

## Examples

``` r
M <- MackChainLadder(GenIns, est.sigma="Mack")
quantile(M, c(0.65, 0.75, 0.9))
#> $ByOrigin
#>         Skewness  IBNR 65%  IBNR 75%  IBNR 90%
#> 1   0.0000000000       NaN       NaN       NaN
#> 2   0.0000000000  123739.0  145581.4  191435.9
#> 3  -0.0286634126  516899.3  551912.6  625100.8
#> 4  -0.0432877277  761917.4  800240.3  880168.6
#> 5  -0.0006540419 1085638.2 1161220.1 1319876.2
#> 6   0.1799508151 1567333.2 1689962.3 1954108.1
#> 7   0.0549677343 2388416.0 2551431.7 2896438.1
#> 8   0.2674116868 4224362.7 4489436.4 5067138.8
#> 9   0.2861886611 4613768.9 4908824.4 5553448.6
#> 10  0.3141065593 5090294.9 5506347.5 6418605.5
#> 
#> $Totals
#>                 Totals
#> Skewness  2.142952e-01
#> IBNR 65%: 1.954935e+07
#> IBNR 75%: 2.028376e+07
#> IBNR 90%: 2.187308e+07
#> 
```
