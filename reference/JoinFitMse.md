# Join Model Fit and Mse Estimation

This function combines first momoent estimation from fitted regression
models and second moment estimation from `Mse` method to construct an
object of class "MultiChainLadder", for which a variety of methods are
defined, such as `summary` and `plot`.

## Usage

``` r
JoinFitMse(models, mse.models)
```

## Arguments

- models:

  fitted regression models, either of class "MCLFit" or "GMCLFit".

- mse.models:

  output from a call to `Mse`, which is of class "MultiChainLadderMse".

## Author

Wayne Zhang <actuary_zhang@hotmail.com>

## See also

See also
[`MultiChainLadder`](http://mages.github.io/ChainLadder/reference/MultiChainLadder.md)`.`
