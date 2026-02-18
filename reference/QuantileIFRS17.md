# Quantile estimation for the IFRS 17 Risk Adjustment

The Quantile IFRS 17 function provides an estimate of the quantile
attained on the reserve risk distribution that corresponds to the booked
Risk Adjustment plus the Best Estimate.

## Usage

``` r
QuantileIFRS17(MCL, Correlation, RiskMargin)
```

## Arguments

- MCL:

  a list of `MackChainLadder` objects

- Correlation:

  Correlation matrix depicting the correlations between each triangle
  imported. The correlation matrix is of dimension n x n, with n the
  number of items in the list of MackChainLadder objects. For
  correlation estimations between P&C risks, please refer to the article
  of Arbenz et al. below.

- RiskMargin:

  Input the risk margin as a single number. The risk margin corresponds
  to the IFRS 17 risk adjustment. It is estimated outside this function
  and can come from e.g. Solvency 2 standard formula. See International
  Actuarial Association reference below for details on risk adjustment
  calculations.

## Details

The IFRS 17 quantile is a mandatory disclosure when producing Financial
Statements under the IFRS 17 framework: Such quantile reflects the
Probability of Sufficiency of the reserves defined as Best Estimate plus
Risk Adjustment i.e. the probability that the reserves will cover any
negative deviations up to the disclosed quantile.

When a risk measure other than the quantile measure (Value At risk) is
used for determining the Risk Adjustment, the quantile has to be
estimated. The purpose of this function is to provide such an estimation
on deriving the first three moments of the reserve risk distribution.
These moments are estimated on the triangles input into the function.
These triangles are projected using chain-ladder methods and the
standard Best Estimate, Mack volatility and skewness are estimated. The
resulting moments of the different triangles are then aggregated using
Fleishman polynomials.

On using a Cornish-Fisher expansion based on the three aggregated
moments, the Probability of Sufficiency of the reserves including the
Risk Adjustment (given as an input to the function) can be easily
derived.

## Value

QuantileIFRS17 returns a vector with the following elements

- QuantileIFRS_17:

  Quantile attained on the reserve risk distribution with the booked
  Risk Adjustment

- Skewness:

  Skewness of the overall aggregated risk distribution across all
  triangles

- CoV:

  Coefficient of Variation of the overall aggregated risk distribution
  across all triangles

- Reserve:

  Sum of reserves of the input MackChainLadder objects

## References

Thomas Mack. Distribution-free calculation of the standard error of
chain ladder reserve estimates. *Astin Bulletin*. Vol. 23. No 2. 1993.
pp.213:225

Thomas Mack. The standard error of chain ladder reserve estimates:
Recursive calculation and inclusion of a tail factor. *Astin Bulletin*.
Vol. 29. No 2. 1999. pp.361:366

Dal Moro, Krvavych. Probability of sufficiency of Solvency II Reserve
risk margins: Practical approximations. *ASTIN Bulletin*, 47(3), 737-785

P. Arbenz, D. Canestraro (2012) Estimating Copulas for Insurance from
Scarce Observations, Expert Opinion and Prior Information: A Bayesian
Approach, *Astin Bulletin*, vol. 42(1), pages 271-290.

International Actuarial Association (2018) Risk Adjustments for
Insurance Contracts under IFRS 17

## Author

Eric Dal Moro, Yuriy Krvavych

## Note

The use of Fleishman polynomials and Cornish-Fisher expansion imply that
the different risks involved in the triangles inputs should be "close to
normality". If the risks involved in the input triangles are far from
normal distributions (e.g. extreme events, nat cats ...), the proposed
framework will not apply and the quantile derived from the function will
not be relevant.

## See also

See also
[`MackChainLadder`](http://mages.github.io/ChainLadder/reference/MackChainLadder.md),
[`quantile.MackChainLadder`](http://mages.github.io/ChainLadder/reference/quantile.MackChainLadder.md)

## Examples

``` r
QuantileIFRS17(MCL=list(M1=MackChainLadder(RAA, est.sigma = "Mack"), 
         M2=MackChainLadder(GenIns/1000, est.sigma = "Mack")), 
         Correlation=matrix(c(1,0.3, 0.3, 1), ncol=2), 
         RiskMargin = 20000)
#> QuantileIFRS_17             CoV        Skewness         Reserve 
#>    7.871247e-01    3.912851e-01    1.225958e+00    7.081608e+04 
```
