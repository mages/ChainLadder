# Join Two Fitted MultiChainLadder Models

This function is created to facilitate the fitting of the multivariate
functions when specifying different models in two different development
periods, especially when separate chain-ladder is used in later periods.

## Usage

``` r
Join2Fits(object1, object2)
```

## Arguments

- object1:

  An object of class "MultiChainLadder"

- object2:

  An object of class "MultiChainLadder"

## Details

The inputs must be of class "MultiChainLadder" because this function
depends on the `model` slot to determine what kind of object is to be
created and returned. If both objects have `"MCL"`, then an object of
class "MCLFit" is created; if one has `"GMCL"` and one has `"MCL"`, then
an object of class "GMCLFit" is created, where the one with `"GMCL"` is
assumed to come from the first development periods; if both have
`"GMCL"`, then an object of class "GMCLFit" is created.

## Author

Wayne Zhang <actuary_zhang@hotmail.com>

## See also

See also
[`MultiChainLadder`](http://mages.github.io/ChainLadder/reference/MultiChainLadder.md)
