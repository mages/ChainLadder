# Methods for Generic Function Mse

`Mse` is a generic function to calculate mean square error estimations
in the chain-ladder framework.

## Usage

``` r
Mse(ModelFit, FullTriangles, ...)

# S4 method for class 'GMCLFit,triangles'
Mse(ModelFit, FullTriangles, ...)
# S4 method for class 'MCLFit,triangles'
Mse(ModelFit, FullTriangles, mse.method="Mack", ...)
```

## Arguments

- ModelFit:

  An object of class "GMCLFit" or "MCLFit".

- FullTriangles:

  An object of class "triangles". Should be the output from a call of
  `predict`.

- mse.method:

  Character strings that specify the MSE estimation method. Only works
  for "MCLFit". Use `"Mack"` for the generazliation of the Mack (1993)
  approach, and `"Independence"` for the conditional resampling approach
  in Merz and Wuthrich (2008).

- ...:

  Currently not used.

## Details

These functions calculate the conditional mean square errors using the
recursive formulas in Zhang (2010), which is a generalization of the
Mack (1993, 1999) formulas. In the GMCL model, the conditional mean
square error for single accident years and aggregated accident years are
calcualted as:

\$\$\hat{mse}(\hat{Y}\_{i,k+1}\|D)=\hat{B}\_k
\hat{mse}(\hat{Y}\_{i,k}\|D) \hat{B}\_k + (\hat{Y}\_{i,k}' \otimes I)
\hat{\Sigma}\_{B_k} (\hat{Y}\_{i,k} \otimes I) +
\hat{\Sigma}\_{\epsilon\_{i_k}}.\$\$

\$\$\hat{mse}(\sum^I\_{i=a_k}\hat{Y}\_{i,k+1}\|D)=\hat{B}\_k
\hat{mse}(\sum^I\_{i=a_k+1}\hat{Y}\_{i,k}\|D) \hat{B}\_k +
(\sum^I\_{i=a_k}\hat{Y}\_{i,k}' \otimes I) \hat{\Sigma}\_{B_k}
(\sum^I\_{i=a_k}\hat{Y}\_{i,k} \otimes I) +
\sum^I\_{i=a_k}\hat{\Sigma}\_{\epsilon\_{i_k}} .\$\$

In the MCL model, the conditional mean square error from Merz and
Wüthrich (2008) is also available, which can be shown to be equivalent
as the following:

\$\$\hat{mse}(\hat{Y}\_{i,k+1}\|D)=(\hat{\beta}\_k \hat{\beta}\_k')
\odot \hat{mse}(\hat{Y}\_{i,k}\|D) + \hat{\Sigma}\_{\beta_k} \odot
(\hat{Y}\_{i,k} \hat{Y}\_{i,k}') + \hat{\Sigma}\_{\epsilon\_{i_k}}
+\hat{\Sigma}\_{\beta_k} \odot \hat{mse}^E(\hat{Y}\_{i,k}\|D) .\$\$

\$\$\hat{mse}(\sum^I\_{i=a_k}\hat{Y}\_{i,k+1}\|D)=(\hat{\beta}\_k
\hat{\beta}\_k') \odot \sum^I\_{i=a_k+1}\hat{mse}(\hat{Y}\_{i,k}\|D) +
\hat{\Sigma}\_{\beta_k} \odot (\sum^I\_{i=a_k}\hat{Y}\_{i,k}
\sum^I\_{i=a_k}\hat{Y}\_{i,k}') +
\sum^I\_{i=a_k}\hat{\Sigma}\_{\epsilon\_{i_k}} +\hat{\Sigma}\_{\beta_k}
\odot \sum^I\_{i=a_k}\hat{mse}^E(\hat{Y}\_{i,k}\|D) .\$\$

For the Mack approach in the MCL model, the cross-product term
\\\hat{\Sigma}\_{\beta_k} \odot \hat{mse}^E(\hat{Y}\_{i,k}\|D) \\in the
above two formulas will drop out.

## Value

`Mse` returns an object of class "MultiChainLadderMse" that has the
following elements:

- mse.ay:

  condtional mse for each accdient year

- mse.ay.est:

  conditional estimation mse for each accdient year

- mse.ay.proc:

  conditional process mse for each accdient year

- mse.total:

  condtional mse for aggregated accdient years

- mse.total.est:

  conditional estimation mse for aggregated accdient years

- mse.total.proc:

  conditional process mse for aggregated accdient years

- FullTriangles:

  completed triangles

## References

Zhang Y (2010). A general multivariate chain ladder model.*Insurance:
Mathematics and Economics*, 46, pp. 588-599.

Zhang Y (2010). Prediction error of the general multivariate chain
ladder model.

## Author

Wayne Zhang <actuary_zhang@hotmail.com>

## See also

See also
[`MultiChainLadder`](http://mages.github.io/ChainLadder/reference/MultiChainLadder.md)`.`
