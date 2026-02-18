# Print function for a dfCorTest object

`print` method for a `dfCorTest` object

## Usage

``` r
# S3 method for class 'dfCorTest'
print(x, ...)
```

## Arguments

- x:

  object of class `dfCorTest`

- ...:

  optional arguments for a `print` method

## Details

`print.dfCorTest` show the print of a `dfCorTest` object.

## Value

`print.dfCorTest` displays the default information resulting from a call
of the dfCorTest method

## Author

Marco De Virgilis <devirgilis.marco@gmail.com>

## See also

See also
[`dfCorTest`](http://mages.github.io/ChainLadder/reference/dfCorTest.md),
[`plot.dfCorTest`](http://mages.github.io/ChainLadder/reference/plot.dfCorTest.md),
[`summary.dfCorTest`](http://mages.github.io/ChainLadder/reference/summary.dfCorTest.md)

## Examples

``` r
 test <- dfCorTest(RAA)
 print(test)
#> Development Factor Correlation
#> 
#> T = 0.06955782
#> 
#> 50%-Range = ( -0.1274666 ; 0.1274666 )
#> 
#> Development Factor Correlation: FALSE
```
