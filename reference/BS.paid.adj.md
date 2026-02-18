# Berquist-Sherman Paid Claim Development Adjustment

The B-S Paid Claim Development Adjustment methods adjusts paid claims
based on the underlying relation between paid and closed claims.

## Usage

``` r
BS.paid.adj(Triangle.rep.counts = NULL, Triangle.closed, Triangle.paid, 
            ult.counts = NULL, regression.type = "exponential")
```

## Arguments

- Triangle.rep.counts:

  cumulative reported claim counts triangle. Assume columns are the
  development period, use transpose otherwise. A (mxn)-matrix
  \\C\_{ik}\\ which is filled for \\k \leq n+1-i; i=1,\ldots,m; m\geq n
  \\, see
  [`qpaid`](http://mages.github.io/ChainLadder/reference/qpaid.md) .

- Triangle.closed:

  cumulative closed claim counts triangle. Assume columns are the
  development period, use transpose otherwise.

- Triangle.paid:

  cumulative paid claims triangle. Assume columns are the development
  period, use transpose otherwise.

- ult.counts:

  vector of ultimate claim counts.

- regression.type:

  Default = "exponential". Type of regression used in the model, it can
  take 'exponential' or 'linear'. See also 'Details'

## Details

The importance of recognizing the impact of shifts in the rate of
settlement of claims upon historical paid loss data can materially
affect the ultimate projections.

This functions adjusts the paid claims based on the numerical method
described in the B-S paper.

Berquist and Sherman presented a technique to adjust the paid claim
development method for changes in settlement rates. The first step of
the paid claims adjustment is to determine the disposal rates by
accident year and maturity.

The disposal rate is defined as as the cumulative closed claim counts
for each accident year-maturity age cell divided by the selected
ultimate claim count for the particular accident year.

If ultimate claim counts have been provided, they will be used to
calulate the disposal rates, otherwise ultimate claim counts will be
estimated from the cumulative reported claim counts triangle with a
standard development method.

The disposal rates along the latest diagonal will be selected as the
basis for adjusting the closed claim count triangle, The selected
disposal rate for each maturity are multiplied by the ultimate number of
claims to determine the adjusted triangle of closed claim counts.

Berquist and Sherman then use regression analysis to identify a
mathematical formula that approximates the relationship between the
cumulative number of closed claims (X) and cumulative paid claims (Y).
The algorithm gives the possibility, through the choice of the
'regression.type' field, to fit an exponential model, \\Y = a\*e^(bX)\\,
or a linear model, \\Y = a+b\*X\\.

The relation is estimated based on unadjusted closed claim counts and
unadjusted paid claims. Once the regression coefficients are estimated,
they will be used to adjust paid claims based on such coefficients and
the adjusted closed claim counts triangle.

## Value

BS.paid.adj returns the adjusted paid claim triangle

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
[`inflateTriangle`](http://mages.github.io/ChainLadder/reference/inflateTriangle.md)
to inflate a triangle based on an inflation rate,

## Examples

``` r
# Adjust the Triangle of Paid Claims based on Reported Claim Counts

adj_paid <- BS.paid.adj( Triangle.rep.counts = AutoBI$AutoBIReportedCounts,
                         Triangle.closed = AutoBI$AutoBIClosed,
                         Triangle.paid = AutoBI$AutoBIPaid,
                         regression.type = 'exponential' )

# Calculate the IBNR from the standard unadjusted Paid Triangle

std_ibnr <- summary(MackChainLadder(AutoBI$AutoBIPaid))$Totals[4, 1]
#> Warning: 'loglinear' model to estimate sigma_n doesn't appear appropriate. 
#> p-value > 5.
#>  est.sigma will be overwritten to 'Mack'.
#>  Mack's estimation method will be used instead.

# Calculate the IBNR from the adjusted Paid Triangle

adj_ibnr <- summary(MackChainLadder(adj_paid))$Totals[4, 1]

# Compare the two

adj_ibnr
#> [1] 41722.26
std_ibnr
#> [1] 31754.43

## For more examples see:
if (FALSE) { # \dontrun{
 demo(BS.paid.adj)
} # }
```
