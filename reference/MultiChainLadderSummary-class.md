# Class "MultiChainLadderSummary"

This class stores the summary statistics from a "MultiChainLadder"
object. These summary statistics include both model summary and report
summary.

## Objects from the Class

Objects can be created by calls of the form
`new("MultiChainLadderSummary", ...)`, or a call from `summary`.

## Slots

- `Triangles`::

  Object of class `"triangles"`

- `FullTriangles`::

  Object of class `"triangles"`

- `S.E.Full`::

  Object of class `"list"`

- `S.E.Est.Full`::

  Object of class `"list"`

- `S.E.Proc.Full`::

  Object of class `"list"`

- `Ultimate`::

  Object of class `"matrix"`

- `IBNR`::

  Object of class `"matrix"`

- `S.E.Ult`::

  Object of class `"matrix"`

- `S.E.Est.Ult`::

  Object of class `"matrix"`

- `S.E.Proc.Ult`::

  Object of class `"matrix"`

- `report.summary`::

  Object of class `"list"`

- `coefficients`::

  Object of class `"list"`

- `coefCov`::

  Object of class `"list"`

- `residCov`::

  Object of class `"list"`

- `rstandard`::

  Object of class `"matrix"`

- `fitted.values`::

  Object of class `"matrix"`

- `residCor`::

  Object of class `"matrix"`

- `model.summary`::

  Object of class `"matrix"`

- `portfolio`::

  Object of class `"NullChar"`

## Methods

- \$:

  `signature(x = "MultiChainLadderSummary")`: Method for primitive
  function `"$"`. It extracts a slot of `x` with a specified slot name,
  just as in list.

- \[\[:

  `signature(x = "MultiChainLadderSummary", i = "numeric", j = "missing")`:
  Method for primitive function `"[["`. It extracts the i-th slot of a
  `"MultiChainLadder"` object, just as in list. `i` could be a vetor.

- \[\[:

  `signature(x = "MultiChainLadderSummary", i = "character", j = "missing")`:
  Method for primitive function `"[["`. It extracts the slots of a
  `"MultiChainLadder"` object with names in `i`, just as in list. `i`
  could be a vetor.

- names:

  `signature(x = "MultiChainLadderSummary")`: Method for function
  `names`, which returns the slot names of a `"MultiChainLadder"`
  object.

- show:

  `signature(object = "MultiChainLadderSummary")`: Method for `show`.

## Author

Wayne Zhang <actuary_zhang@hotmail.com>

## See also

See also
[`summary,MultiChainLadder-method`](http://mages.github.io/ChainLadder/reference/summary-methods.md),
[`MultiChainLadder-class`](http://mages.github.io/ChainLadder/reference/MultiChainLadder-class.md)

## Examples

``` r
showClass("MultiChainLadderSummary")
#> Class "MultiChainLadderSummary" [package "ChainLadder"]
#> 
#> Slots:
#>                                                                   
#> Name:       Triangles  FullTriangles       S.E.Full   S.E.Est.Full
#> Class:      triangles      triangles           list           list
#>                                                                   
#> Name:   S.E.Proc.Full       Ultimate           IBNR        S.E.Ult
#> Class:           list         matrix         matrix         matrix
#>                                                                   
#> Name:     S.E.Est.Ult   S.E.Proc.Ult report.summary   coefficients
#> Class:         matrix         matrix           list           list
#>                                                                   
#> Name:         coefCov       residCov      rstandard  fitted.values
#> Class:           list           list         matrix         matrix
#>                                                    
#> Name:        residCor  model.summary      portfolio
#> Class:         matrix         matrix       NullChar
```
