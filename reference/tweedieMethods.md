# Reserve Risk Capital Report

Main purpose of this function is to create a report to assess the
reserve risk capital given an object of the
[`tweedieReserve`](http://mages.github.io/ChainLadder/reference/tweedieReserve.md)
class. It displays both the ultimate and one year risk views at given
percentiles.

## Usage

``` r
# S3 method for class 'tweedieReserve'
print(x, ...)
# S3 method for class 'tweedieReserve'
summary(object, q = c(0.5, 0.75, 0.9, 0.95, 0.995),...)
```

## Arguments

- x:

  An object of class
  [`tweedieReserve`](http://mages.github.io/ChainLadder/reference/tweedieReserve.md).

- object:

  An object of class
  [`tweedieReserve`](http://mages.github.io/ChainLadder/reference/tweedieReserve.md).

- q:

  Array of percentiles to be displayed.

- ...:

  Not used

## Value

A list with two items

- Predicton:

  a data.frame with ultimate view reserve risk and the one year view
  reserve risk at the given percentiles.

- Diagnostic:

  Quick diagnostic to show the deterministic reserve vs ultimate view
  and one year view best estimate. If the model is working properly,
  then these three value shouldn't be much different.

## Author

Alessandro Carrato MSc FIA OA <alessandro.carrato@gmail.com>

## See also

See also
[`tweedieReserve`](http://mages.github.io/ChainLadder/reference/tweedieReserve.md).

## Examples

``` r
if (FALSE) { # \dontrun{
tw <- tweedieReserve(MW2008, rereserving = TRUE)
summary(tw)
# For comparison 
CDR.BootChainLadder(BootChainLadder(MW2008))
} # }
```
