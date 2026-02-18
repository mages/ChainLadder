# Summary function for a cyEffTest object

`summary` method for a `cyEffTest` object

## Usage

``` r
# S3 method for class 'cyEffTest'
summary(object, ...)
```

## Arguments

- object:

  object of class `cyEffTest`

- ...:

  optional arguments for a `summary` method

## Details

`summary.cyEffTest` shows the summary of a `cyEffTest` object.

## Value

`summary.cyEffTest` gives a list of three elements back

- Table:

  data frame containing the statistics for each calendar year

- Totals:

  data frame of totals of the main statistics from the dataframe `Table`

- Range:

  data frame containing the upper and lower limits of the confidence
  interval range

## Author

Marco De Virgilis <devirgilis.marco@gmail.com>

## See also

See also
[`cyEffTest`](http://mages.github.io/ChainLadder/reference/cyEffTest.md),
[`plot.cyEffTest`](http://mages.github.io/ChainLadder/reference/plot.cyEffTest.md)

## Examples

``` r
 test <- cyEffTest(RAA)
 summary(test)
#> $Table
#>   j S_j L_j Z_j n m    E_Zj    Var_Zj
#> 1 2   1   1   1 2 0 0.50000 0.2500000
#> 2 3   3   0   0 3 1 0.75000 0.1875000
#> 3 4   3   1   1 4 1 1.25000 0.4375000
#> 4 5   1   3   1 4 1 1.25000 0.4375000
#> 5 6   1   3   1 4 1 1.25000 0.4375000
#> 6 7   2   4   2 6 2 2.06250 0.6210938
#> 7 8   4   4   4 8 3 2.90625 0.8037109
#> 8 9   4   4   4 8 3 2.90625 0.8037109
#> 
#> $Totals
#>           Totals
#> Z      14.000000
#> E[Z]   12.875000
#> Var[Z]  3.978516
#> 
#> $Range
#>           Value
#> Lower  8.965613
#> Upper 16.784387
#> 
```
