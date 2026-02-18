# Prediction of a claims triangle

The function is internally used by
[`MackChainLadder`](http://mages.github.io/ChainLadder/reference/MackChainLadder.md)
to forecast future claims.

## Usage

``` r
# S3 method for class 'TriangleModel'
predict(object,...)
# S3 method for class 'ChainLadder'
predict(object,...)
```

## Arguments

- object:

  a list with two items: `Models`, `Triangle`

  `Models`

  :   list of linear models for each development period

  `Triangle`

  :   input triangle to forecast

- ...:

  not in use

## Value

- FullTriangle:

  forecasted claims triangle

## Author

Markus Gesmann

## See also

See also
[`chainladder`](http://mages.github.io/ChainLadder/reference/chainladder.md),
[`MackChainLadder`](http://mages.github.io/ChainLadder/reference/MackChainLadder.md)

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

CL <- chainladder(RAA)
CL
#> $Models
#> $Models[[1]]
#> 
#> Call:
#> lm(formula = y ~ x + 0, data = data.frame(x = Triangle[, i], 
#>     y = Triangle[, i + 1]), weights = weights[, i]/Triangle[, 
#>     i]^delta[i])
#> 
#> Coefficients:
#>     x  
#> 2.999  
#> 
#> 
#> $Models[[2]]
#> 
#> Call:
#> lm(formula = y ~ x + 0, data = data.frame(x = Triangle[, i], 
#>     y = Triangle[, i + 1]), weights = weights[, i]/Triangle[, 
#>     i]^delta[i])
#> 
#> Coefficients:
#>     x  
#> 1.624  
#> 
#> 
#> $Models[[3]]
#> 
#> Call:
#> lm(formula = y ~ x + 0, data = data.frame(x = Triangle[, i], 
#>     y = Triangle[, i + 1]), weights = weights[, i]/Triangle[, 
#>     i]^delta[i])
#> 
#> Coefficients:
#>     x  
#> 1.271  
#> 
#> 
#> $Models[[4]]
#> 
#> Call:
#> lm(formula = y ~ x + 0, data = data.frame(x = Triangle[, i], 
#>     y = Triangle[, i + 1]), weights = weights[, i]/Triangle[, 
#>     i]^delta[i])
#> 
#> Coefficients:
#>     x  
#> 1.172  
#> 
#> 
#> $Models[[5]]
#> 
#> Call:
#> lm(formula = y ~ x + 0, data = data.frame(x = Triangle[, i], 
#>     y = Triangle[, i + 1]), weights = weights[, i]/Triangle[, 
#>     i]^delta[i])
#> 
#> Coefficients:
#>     x  
#> 1.113  
#> 
#> 
#> $Models[[6]]
#> 
#> Call:
#> lm(formula = y ~ x + 0, data = data.frame(x = Triangle[, i], 
#>     y = Triangle[, i + 1]), weights = weights[, i]/Triangle[, 
#>     i]^delta[i])
#> 
#> Coefficients:
#>     x  
#> 1.042  
#> 
#> 
#> $Models[[7]]
#> 
#> Call:
#> lm(formula = y ~ x + 0, data = data.frame(x = Triangle[, i], 
#>     y = Triangle[, i + 1]), weights = weights[, i]/Triangle[, 
#>     i]^delta[i])
#> 
#> Coefficients:
#>     x  
#> 1.033  
#> 
#> 
#> $Models[[8]]
#> 
#> Call:
#> lm(formula = y ~ x + 0, data = data.frame(x = Triangle[, i], 
#>     y = Triangle[, i + 1]), weights = weights[, i]/Triangle[, 
#>     i]^delta[i])
#> 
#> Coefficients:
#>     x  
#> 1.017  
#> 
#> 
#> $Models[[9]]
#> 
#> Call:
#> lm(formula = y ~ x + 0, data = data.frame(x = Triangle[, i], 
#>     y = Triangle[, i + 1]), weights = weights[, i]/Triangle[, 
#>     i]^delta[i])
#> 
#> Coefficients:
#>     x  
#> 1.009  
#> 
#> 
#> 
#> $Triangle
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
#> 
#> $delta
#> [1] 1 1 1 1 1 1 1 1 1
#> 
#> $weights
#>       dev
#> origin 1  2  3  4  5  6  7  8  9 10
#>   1981 1  1  1  1  1  1  1  1  1  1
#>   1982 1  1  1  1  1  1  1  1  1 NA
#>   1983 1  1  1  1  1  1  1  1 NA NA
#>   1984 1  1  1  1  1  1  1 NA NA NA
#>   1985 1  1  1  1  1  1 NA NA NA NA
#>   1986 1  1  1  1  1 NA NA NA NA NA
#>   1987 1  1  1  1 NA NA NA NA NA NA
#>   1988 1  1  1 NA NA NA NA NA NA NA
#>   1989 1  1 NA NA NA NA NA NA NA NA
#>   1990 1 NA NA NA NA NA NA NA NA NA
#> 
#> attr(,"class")
#> [1] "ChainLadder"   "TriangleModel" "list"         
predict(CL)
#>       dev
#> origin    1         2         3        4        5        6        7        8
#>   1981 5012  8269.000 10907.000 11805.00 13539.00 16181.00 18009.00 18608.00
#>   1982  106  4285.000  5396.000 10666.00 13782.00 15599.00 15496.00 16169.00
#>   1983 3410  8992.000 13873.000 16141.00 18735.00 22214.00 22863.00 23466.00
#>   1984 5655 11555.000 15766.000 21266.00 23425.00 26083.00 27067.00 27967.34
#>   1985 1092  9565.000 15836.000 22169.00 25955.00 26180.00 27277.85 28185.21
#>   1986 1513  6445.000 11702.000 12935.00 15852.00 17649.38 18389.50 19001.20
#>   1987  557  4020.000 10946.000 12314.00 14428.00 16063.92 16737.55 17294.30
#>   1988 1351  6947.000 13112.000 16663.88 19524.65 21738.45 22650.05 23403.47
#>   1989 3133  5395.000  8758.905 11131.59 13042.60 14521.43 15130.38 15633.68
#>   1990 2063  6187.677 10045.834 12767.13 14958.92 16655.04 17353.46 17930.70
#>       dev
#> origin        9       10
#>   1981 18662.00 18834.00
#>   1982 16704.00 16857.95
#>   1983 23863.43 24083.37
#>   1984 28441.01 28703.14
#>   1985 28662.57 28926.74
#>   1986 19323.01 19501.10
#>   1987 17587.21 17749.30
#>   1988 23799.84 24019.19
#>   1989 15898.45 16044.98
#>   1990 18234.38 18402.44
```
