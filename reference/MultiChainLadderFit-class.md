# Class "MultiChainLadderFit", "MCLFit" and "GMCLFit"

"MultiChainLadderFit" is a virtual class for the fitted models in the
multivariate chain ladder reserving framework, "MCLFit" is a result from
the interal call `.FitMCL` to store results in model `MCL` and "GMCLFit"
is a result from the interal call `.FitGMCL` to store results in model
`GMCL`. The two classes "MCLFit" and "GMCLFit" differ only in the
presentation of \\B_k\\ and \\\Sigma\_{B_k}\\, and different methods of
`Mse` and `predict` will be dispatched according to these classes.

## Objects from the Class

"MultiChainLadderFit" is a virtual Class: No objects may be created from
it. For "MCLFit" and "GMCLFit", objects can be created by calls of the
form `new("MCLFit", ...)` and `new("GMCLFit", ...)` respectively.

## Slots

- `Triangles`::

  Object of class `"triangles"`

- `models`::

  Object of class `"list"`

- `B`::

  Object of class `"list"`

- `Bcov`::

  Object of class `"list"`

- `ecov`::

  Object of class `"list"`

- `fit.method`::

  Object of class `"character"`

- `delta`::

  Object of class `"numeric"`

- `int`::

  Object of class `"NullNum"`

- `restrict.regMat`::

  Object of class `"NullList"`

## Extends

"MCLFit" and "GMCLFit" extends class `"MultiChainLadderFit"`, directly.

## Methods

No methods defined with class "MultiChainLadderFit" in the signature.

For "MCLFit", the following methods are defined:

- `Mse`:

  `signature(ModelFit = "MCLFit", FullTriangles = "triangles")`:
  Calculate Mse estimations.

- `predict`:

  `signature(object = "MCLFit")`: Predict ultimate losses and complete
  the triangles. The output is an object of class "triangles".

For "GMCLFit", the following methods are defined:

- `Mse`:

  `signature(ModelFit = "GMCLFit", FullTriangles = "triangles")`:
  Calculate Mse estimations.

- `predict`:

  `signature(object = "GMCLFit")`: Predict ultimate losses and complete
  the triangles. The output is an object of class "triangles".

## Author

Wayne Zhang <actuary_zhang@hotmail.com>

## See also

See also
[`Mse`](http://mages.github.io/ChainLadder/reference/Mse-methods.md).

## Examples

``` r
showClass("MultiChainLadderFit")
#> Virtual Class "MultiChainLadderFit" [package "ChainLadder"]
#> 
#> Slots:
#>                                                                       
#> Name:        Triangles          models    coefficients         coefCov
#> Class:       triangles            list            list            list
#>                                                                       
#> Name:         residCov      fit.method           delta             int
#> Class:            list       character         numeric         NullNum
#>                       
#> Name:  restrict.regMat
#> Class:        NullList
#> 
#> Known Subclasses: "GMCLFit", "MCLFit", "MultiChainLadder"
```
