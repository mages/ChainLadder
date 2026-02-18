# Print function for a checkTriangleInflation object

`print` method for a `checkTriangleInflation` object

## Usage

``` r
# S3 method for class 'checkTriangleInflation'
print(x, ...)
```

## Arguments

- x:

  object of class `checkTriangleInflation`

- ...:

  optional arguments for a `print` method

## Details

`print.checkTriangleInflation` show the print of a
`checkTriangleInflation` object.

## Value

`print.checkTriangleInflation` displays the default information
resulting from a call of the checkTriangleInflation method

## Author

Marco De Virgilis <devirgilis.marco@gmail.com>

## See also

See also
[`checkTriangleInflation`](http://mages.github.io/ChainLadder/reference/checkTriangleInflation.md),
[`plot.checkTriangleInflation`](http://mages.github.io/ChainLadder/reference/plot.checkTriangleInflation.md),
[`summary.checkTriangleInflation`](http://mages.github.io/ChainLadder/reference/summary.checkTriangleInflation.md)

## Examples

``` r
 test <- checkTriangleInflation(MedMal$MedMalOutstanding / MedMal$MedMalOpen)
 print(test)
#> Triangle Inflation Calculation
#> 
#>                1         2         3         4         5         6        7
#> rate   0.1561905 0.2949749 0.3110902 0.3417400 0.3296170 0.3216367 0.276155
#> R2     0.7995755 0.8946321 0.8578738 0.9405004 0.9887844 0.9831351 1.000000
#> Points 8.0000000 7.0000000 6.0000000 5.0000000 4.0000000 3.0000000 2.000000
```
