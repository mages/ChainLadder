# Summary function for a dfCorTest object

`summary` method for a `dfCorTest` object

## Usage

``` r
# S3 method for class 'dfCorTest'
summary(object, ...)
```

## Arguments

- object:

  object of class `dfCorTest`

- ...:

  optional arguments for a `summary` method

## Details

`summary.dfCorTest` shows the summary of a `dfCorTest` object.

## Value

`summary.dfCorTest` gives a list of two elements back

- Results:

  data frame containing the summary statistics

- Range:

  data frame containing the upper and lower limits of the confidence
  interval range

## Author

Marco De Virgilis <devirgilis.marco@gmail.com>

## See also

See also
[`dfCorTest`](http://mages.github.io/ChainLadder/reference/dfCorTest.md),
[`plot.dfCorTest`](http://mages.github.io/ChainLadder/reference/plot.dfCorTest.md)

## Examples

``` r
 test <- dfCorTest(RAA)
 summary(test)
#> $Results
#>             Value
#> T      0.06955782
#> E[T]   0.00000000
#> Var[T] 0.03571429
#> 
#> $Range
#>            Value
#> Lower -0.1274666
#> Upper  0.1274666
#> 
```
