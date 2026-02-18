# PaidIncurredChain

The Paid-incurred Chain model (Merz, Wuthrich (2010)) combines claims
payments and incurred losses information to get a unified ultimate loss
prediction.

## Usage

``` r
PaidIncurredChain(triangleP, triangleI)
```

## Arguments

- triangleP:

  Cumulative claims payments triangle

- triangleI:

  Incurred losses triangle.

## Value

The function returns:

- **Ult.Loss.Origin** Ultimate losses for different origin years.

- **Ult.Loss** Total ultimate loss.

- **Res.Origin** Claims reserves for different origin years.

- **Res.Tot** Total reserve.

- **s.e.** Square root of mean square error of prediction for the total
  ultimate loss.

## Details

The method uses some basic properties of multivariate Gaussian
distributions to obtain a mathematically rigorous and consistent model
for the combination of the two information channels.

We assume as usual that I=J. The model assumptions for the Log-Normal
PIC Model are the following:

- Conditionally, given \\\Theta = (\Phi_0,...,\Phi_I,
  \Psi_0,...,\Psi\_{I-1},\sigma_0,...,\sigma\_{I-1},\tau_0,...,\tau\_{I-1})\\
  we have

  - the random vector \\(\xi\_{0,0},...,\xi\_{I,I},
    \zeta\_{0,0},...,\zeta\_{I,I-1})\\ has multivariate Gaussian
    distribution with uncorrelated components given by \$\$\xi\_{i,j}
    \sim N(\Phi_j,\sigma^2_j),\$\$ \$\$\zeta\_{k,l} \sim
    N(\Psi_l,\tau^2_l);\$\$

  - cumulative payments are given by the recursion \$\$P\_{i,j} =
    P\_{i,j-1} \exp(\xi\_{i,j}),\$\$ with initial value \\P\_{i,0} =
    \exp (\xi\_{i,0})\\;

  - incurred losses \\I\_{i,j}\\ are given by the backwards recursion
    \$\$I\_{i,j-1} = I\_{i,j} \exp(-\zeta\_{i,j-1}),\$\$ with initial
    value \\I\_{i,I}=P\_{i,I}\\.

- The components of \\\Theta\\ are independent and \\\sigma_j,\tau_j \>
  0\\ for all j.

Parameters \\\Theta\\ in the model are in general not known and need to
be estimated from observations. They are estimated in a Bayesian
framework. In the Bayesian PIC model they assume that the previous
assumptions hold true with deterministic \\\sigma_0,...,\sigma_J\\ and
\\\tau_0,...,\tau\_{J-1}\\ and \$\$\Phi_m \sim N(\phi_m,s^2_m),\$\$
\$\$\Psi_n \sim N(\psi_n,t^2_n).\$\$ This is not a full Bayesian
approach but has the advantage to give analytical expressions for the
posterior distributions and the prediction uncertainty.

## Note

The model is implemented in the special case of non-informative priors.

## References

Merz, M., Wuthrich, M. (2010). Paid-incurred chain claims reserving
method. Insurance: Mathematics and Economics, 46(3), 568-579.

## See also

[`MackChainLadder`](http://mages.github.io/ChainLadder/reference/MackChainLadder.md),[`MunichChainLadder`](http://mages.github.io/ChainLadder/reference/MunichChainLadder.md)

## Author

Fabio Concina, <fabio.concina@gmail.com>

## Examples

``` r
PaidIncurredChain(USAApaid, USAAincurred)
#> $Ult.Loss.Origin
#>            [,1]
#>  [1,]  983367.2
#>  [2,] 1078418.3
#>  [3,] 1142422.8
#>  [4,] 1241974.4
#>  [5,] 1367661.6
#>  [6,] 1419425.0
#>  [7,] 1406169.4
#>  [8,] 1394320.2
#>  [9,] 1324249.6
#> 
#> $Ult.Loss
#> [1] 11358009
#> 
#> $Res.Origin
#>         [,1]
#> 1   1219.242
#> 2   2881.254
#> 3   4047.790
#> 4  15324.356
#> 5  42929.648
#> 6  99295.001
#> 7 220869.445
#> 8 428158.234
#> 9 782228.614
#> 
#> $Res.Tot
#> [1] 1596954
#> 
#> $s.e.
#> [1] 110976.9
#> 
```
