# Print function for a cyEffTest object

`print` method for a `cyEffTest` object

## Usage

``` r
# S3 method for class 'cyEffTest'
print(x, ...)
```

## Arguments

- x:

  object of class `cyEffTest`

- ...:

  optional arguments for a `print` method

## Details

`print.cyEffTest` show the print of a `cyEffTest` object.

## Value

`print.cyEffTest` displays the default information resulting from a call
of the cyEffTest method

## Author

Marco De Virgilis <devirgilis.marco@gmail.com>

## See also

See also
[`cyEffTest`](http://mages.github.io/ChainLadder/reference/cyEffTest.md),
[`plot.cyEffTest`](http://mages.github.io/ChainLadder/reference/plot.cyEffTest.md),
[`summary.cyEffTest`](http://mages.github.io/ChainLadder/reference/summary.cyEffTest.md)

## Examples

``` r
 test <- cyEffTest(RAA)
 print(test)
#> Calendar Year Effect
#> 
#> Z = 14
#> 
#> 95%-Range = ( 8.965613 ; 16.78439 )
#> 
#> Calendar Year Effect: FALSE
```
