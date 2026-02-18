# Triangle information for most recent calendar period.

Return most recent values for all origin periods of a cumulative
development triangle.

## Usage

``` r
getLatestCumulative(Triangle, na.values = NULL)
```

## Arguments

- Triangle:

  a Triangle in matrix format.

- na.values:

  a vector specifying values that should be considered synonymous with
  NA when searching for the rightmost non-NA.

## Value

A vector of most recent non-'NA' (and synonyms, if appropriate) values
of a triangle for all origin periods. The `names` of the vector equal
the origin names of the Triangle. The vector will have additional
attributes: "latestcol" equalling the index of the column in Triangle
corresponding to the row's rightmost entry; "rowsname" equalling the
name of the row dimension of Triangle, if any; "colnames" equalling the
corresponding column name of Triangle, if any; "colsname" equalling the
name of the column dimension of Triangle, if any.

## Author

Ben Escoto, Markus Gesmann, Dan Murphy

## See also

See also
[`as.triangle`](http://mages.github.io/ChainLadder/reference/Triangles.md).

## Examples

``` r
RAA
#>       dev
#> origin    1     2     3     4     5     6     7     8     9    10
#>   1981 5012  8269 10907 11805 13539 16181 18009 18608 18662 18834
#>   1982  106  4285  5396 10666 13782 15599 15496 16169 16704    NA
#>   1983 3410  8992 13873 16141 18735 22214 22863 23466    NA    NA
#>   1984 5655 11555 15766 21266 23425 26083 27067    NA    NA    NA
#>   1985 1092  9565 15836 22169 25955 26180    NA    NA    NA    NA
#>   1986 1513  6445 11702 12935 15852    NA    NA    NA    NA    NA
#>   1987  557  4020 10946 12314    NA    NA    NA    NA    NA    NA
#>   1988 1351  6947 13112    NA    NA    NA    NA    NA    NA    NA
#>   1989 3133  5395    NA    NA    NA    NA    NA    NA    NA    NA
#>   1990 2063    NA    NA    NA    NA    NA    NA    NA    NA    NA
getLatestCumulative(RAA)
#>  1981  1982  1983  1984  1985  1986  1987  1988  1989  1990 
#> 18834 16704 23466 27067 26180 15852 12314 13112  5395  2063 
#> attr(,"latestcol")
#> 1981 1982 1983 1984 1985 1986 1987 1988 1989 1990 
#>   10    9    8    7    6    5    4    3    2    1 
#> attr(,"rowsname")
#> [1] "origin"
#> attr(,"colnames")
#>  [1] "10" "9"  "8"  "7"  "6"  "5"  "4"  "3"  "2"  "1" 
#> attr(,"colsname")
#> [1] "dev"
Y <- matrix(c(1,  2,  3,
              4,  5,  0, 
              6, NA, NA), byrow=TRUE, nrow=3)
getLatestCumulative(Y) # c(3, 0, 6) 
#> [1] 3 0 6
#> attr(,"latestcol")
#> [1] 3 3 1
getLatestCumulative(Y, na.values = 0) # c(3, 5, 6) 
#> [1] 3 5 6
#> attr(,"latestcol")
#> [1] 3 2 1
```
