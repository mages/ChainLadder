# Inflate a Triangle based on an Inflation Rate

Inflate the amounts of a Triangle from the latest diagonal based on an
Inflation Rate

## Usage

``` r
inflateTriangle(Triangle, rate)
```

## Arguments

- Triangle:

  claim triangle. Assume columns are the development period, use
  transpose otherwise. A (mxn)-matrix \\C\_{ik}\\ which is filled for
  \\k \leq n+1-i; i=1,\ldots,m; m\geq n \\, see
  [`qpaid`](http://mages.github.io/ChainLadder/reference/qpaid.md) for
  how to use (mxn)-development triangles with m\<n, say higher
  development period frequency (e.g quarterly) than origin period
  frequency (e.g accident years).

- rate:

  Inflation rate to be applied to the triangle according to an
  exponential model

## Details

The sensitivity of projections of ultimate losses based on incurred loss
development factors to changes in the adequacy level of case reserves
increases significantly for the long-tail lines. In particular, if the
adequacy of the case reserve is changing, the estimates of ultimate
losses based on reported claims could be severely distorted. The
function deflates the amounts of latest diagonal to each diagonal of the
triangle according to the inflation rate provided, considering an
exponential model. The purpose of restating the amounts is to have each
diagonal in the triangle at the same level as the latest diagonal (i.e.
latest valuation). Ideally the metrics that should be restated are
average O/S or average claim paid.

## Value

inflateTriangle returns the inflated triangle according to the provided
rate

## References

Berquist, J.R. and Sherman, R.E., Loss Reserve Adequacy Testing: A
Comprehensive, Systematic Approach, *Proceedings of the Casualty
Actuarial Society*, LXIV, 1977, pp.123-184.

## Author

Marco De Virgilis <devirgilis.marco@gmail.com>

## See also

See also
[`qpaid`](http://mages.github.io/ChainLadder/reference/qpaid.md) for
dealing with non-square triangles,
[`checkTriangleInflation`](http://mages.github.io/ChainLadder/reference/checkTriangleInflation.md)
to check Y-o-Y Triangle Inflation Rates,

## Examples

``` r
# Create a Triangle of Average Case O/S

avg <- MedMal$MedMalOutstanding / MedMal$MedMalOpen

# Select a rate of 15% and inflate the average =/S Triangle

inflated_tr <- inflateTriangle(Triangle = avg, rate = .15) 

# Multiply it by open claims and add paymnets to calulate the adjusted Reported Claims Trinagle

adj_reported <- inflated_tr * MedMal$MedMalOpen + MedMal$MedMalPaid

# Calculate the IBNR from the unadjusted Triangle

std_ibnr <- summary(MackChainLadder(MedMal$MedMalReported))$Totals[4, 1]

# Calculate the IBNR from the adjusted Triangle

adj_reported_ibnr <- summary(MackChainLadder(adj_reported))$Totals[4, 1]

# Compare the two

std_ibnr - adj_reported_ibnr
#> [1] 321739113
```
