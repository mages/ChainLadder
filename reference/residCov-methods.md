# Generic function for residCov and residCor

`residCov` and `residCov` are a generic functions to extract residual
covariance and residual correlation from a system of fitted regressions
respectively.

## Usage

``` r
residCov(object,...)
residCor(object,...)

# S4 method for class 'MultiChainLadder'
residCov(object,...)
# S4 method for class 'MultiChainLadder'
residCor(object,...)
```

## Arguments

- object:

  An object of class "MultiChainLadder".

- ...:

  Currently not used.

## Author

Wayne Zhang <actuary_zhang@hotmail.com>

## See also

See also
[`MultiChainLadder`](http://mages.github.io/ChainLadder/reference/MultiChainLadder-class.md).
