# Class "MultiChainLadderMse"

This class is used to define the structure in storing the MSE results.

## Objects from the Class

Objects can be created by calls of the form
`new("MultiChainLadderMse", ...)`, or as a result of a call to `Mse`.

## Slots

- `mse.ay`::

  Object of class `"matrix"`

- `mse.ay.est`::

  Object of class `"matrix"`

- `mse.ay.proc`::

  Object of class `"matrix"`

- `mse.total`::

  Object of class `"matrix"`

- `mse.total.est`::

  Object of class `"matrix"`

- `mse.total.proc`::

  Object of class `"matrix"`

- `FullTriangles`::

  Object of class `"triangles"`

## Methods

No methods defined with class "MultiChainLadderMse" in the signature.

## Author

Wayne Zhang <actuary_zhang@hotmail.com>

## See also

See Also
[`MultiChainLadder`](http://mages.github.io/ChainLadder/reference/MultiChainLadder-class.md)
and
[`Mse`](http://mages.github.io/ChainLadder/reference/Mse-methods.md).

## Examples

``` r
showClass("MultiChainLadderMse")
#> Class "MultiChainLadderMse" [package "ChainLadder"]
#> 
#> Slots:
#>                                                                   
#> Name:          mse.ay     mse.ay.est    mse.ay.proc      mse.total
#> Class:         matrix         matrix         matrix         matrix
#>                                                    
#> Name:   mse.total.est mse.total.proc  FullTriangles
#> Class:         matrix         matrix      triangles
#> 
#> Known Subclasses: "MultiChainLadder"
```
