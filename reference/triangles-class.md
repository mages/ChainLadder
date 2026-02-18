# S4 Class "triangles"

This is a S4 class that has "list" in the data part. This class is
created to facilitate validation and extraction of data.

## Objects from the Class

Objects can be created by calls of the form `new("triangles", ...)`, or
use `as(...,"triangles")`, where `...` is a "list".

## Slots

- `.Data`::

  Object of class `"list"`

## Extends

Class `"list"`, from data part. Class `"vector"`, by class "list",
distance 2.

## Methods

- Mse:

  `signature(ModelFit = "GMCLFit", FullTriangles = "triangles")`: See
  [`Mse`](http://mages.github.io/ChainLadder/reference/Mse-methods.md)

- Mse:

  `signature(ModelFit = "MCLFit", FullTriangles = "triangles")`: See
  [`Mse`](http://mages.github.io/ChainLadder/reference/Mse-methods.md)

- \[:

  `signature(x = "triangles", i = "missing", j = "numeric", drop = "logical")`:
  Method for primitive function "\[" to subset certain columns. If
  `drop=TRUE`, rows composed of all "NA"s are removed. Dimensions are
  not dropped.

- \[:

  `signature(x = "triangles", i = "missing", j = "numeric", drop = "missing")`:
  Method for primitive function "\[" to subset certain columns, where
  rows composed of all "NA"s are removed. Dimensions are not dropped.

- \[:

  `signature(x = "triangles", i = "numeric", j = "missing", drop = "logical")`:
  Method for primitive function "\[" to subset certain rows. If
  `drop=TRUE`, columns composed of all "NA"s are removed. Dimensions are
  not dropped.

- \[:

  `signature(x = "triangles", i = "numeric", j = "missing", drop = "missing")`:
  Method for primitive function "\[" to subset certain rows, where
  columns composed of all "NA"s are removed. Dimensions are not dropped.

- \[:

  `signature(x = "triangles", i = "numeric", j = "numeric", drop = "missing")`:
  Method for primitive function "\[" to subset certain rows and columns.
  Dimensions are not dropped.

- \[\<-:

  `signature(x = "triangles", i = "numeric", j = "numeric", value = "list")`:
  Method for primitive function "\[\<-" to replace one cell in each
  triangle with values specified in `value`.

- coerce:

  `signature(from = "list", to = "triangles")`: Method to construct a
  "triangles" object from "list".

- dim:

  `signature(x = "triangles")`: Method to get the dimensions. The return
  value is a vector of length 3, where the first element is the number
  of triangles, the sencond is the number of accident years, and the
  third is the number of development years.

- cbind2:

  `signature(x = "triangles", y="missing")`: Method to column bind all
  triangles using `cbind` internally.

- rbind2:

  `signature(x = "triangles", y="missing")`: Method to row bind all
  triangles using `rbind` internally.

## Author

Wayne Zhang <actuary_zhang@hotmail.com>

## See also

See also
[`MultiChainLadder`](http://mages.github.io/ChainLadder/reference/MultiChainLadder.md)

## Examples

``` r
data(auto)

# "coerce"
auto <- as(auto,"triangles")  # transform "list" to be "triangles"

# method for "["
auto[,4:6,drop=FALSE] # rows of all NA's not dropped
#> An object of class "triangles"
#> [[1]]
#>         [,1]   [,2]   [,3]
#>  [1,] 305107 327850 340669
#>  [2,] 303182 328932 340948
#>  [3,] 345542 367760 377999
#>  [4,] 340669 359979 369248
#>  [5,] 354490 372376 382738
#>  [6,] 365780 386725     NA
#>  [7,] 367357     NA     NA
#>  [8,]     NA     NA     NA
#>  [9,]     NA     NA     NA
#> [10,]     NA     NA     NA
#> 
#> [[2]]
#>         [,1]   [,2]   [,3]
#>  [1,] 347726 350995 353598
#>  [2,] 349295 351038 351583
#>  [3,] 384699 387678 387954
#>  [4,] 384819 380914 380163
#>  [5,] 409322 394154 392802
#>  [6,] 406711 406503     NA
#>  [7,] 400540     NA     NA
#>  [8,]     NA     NA     NA
#>  [9,]     NA     NA     NA
#> [10,]     NA     NA     NA
#> 
#> [[3]]
#>         [,1]   [,2]   [,3]
#>  [1,]  77398  88079  95695
#>  [2,]  92356 104958 112399
#>  [3,]  88435 102044 112672
#>  [4,]  98063 113149 121515
#>  [5,] 104936 117663 126180
#>  [6,] 108835 121326     NA
#>  [7,] 111987     NA     NA
#>  [8,]     NA     NA     NA
#>  [9,]     NA     NA     NA
#> [10,]     NA     NA     NA
#> 
auto[,4:6]      # drop rows of all NA's
#> An object of class "triangles"
#> [[1]]
#>        [,1]   [,2]   [,3]
#> [1,] 305107 327850 340669
#> [2,] 303182 328932 340948
#> [3,] 345542 367760 377999
#> [4,] 340669 359979 369248
#> [5,] 354490 372376 382738
#> [6,] 365780 386725     NA
#> [7,] 367357     NA     NA
#> 
#> [[2]]
#>        [,1]   [,2]   [,3]
#> [1,] 347726 350995 353598
#> [2,] 349295 351038 351583
#> [3,] 384699 387678 387954
#> [4,] 384819 380914 380163
#> [5,] 409322 394154 392802
#> [6,] 406711 406503     NA
#> [7,] 400540     NA     NA
#> 
#> [[3]]
#>        [,1]   [,2]   [,3]
#> [1,]  77398  88079  95695
#> [2,]  92356 104958 112399
#> [3,]  88435 102044 112672
#> [4,]  98063 113149 121515
#> [5,] 104936 117663 126180
#> [6,] 108835 121326     NA
#> [7,] 111987     NA     NA
#> 

auto[8:10, ,drop=FALSE] #columns of all NA's not dropped
#> An object of class "triangles"
#> [[1]]
#>        [,1]   [,2]   [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
#> [1,] 127177 244249 317972   NA   NA   NA   NA   NA   NA    NA
#> [2,] 128631 246803     NA   NA   NA   NA   NA   NA   NA    NA
#> [3,] 126288     NA     NA   NA   NA   NA   NA   NA   NA    NA
#> 
#> [[2]]
#>        [,1]   [,2]   [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
#> [1,] 378754 361097 369328   NA   NA   NA   NA   NA   NA    NA
#> [2,] 351081 335507     NA   NA   NA   NA   NA   NA   NA    NA
#> [3,] 329236     NA     NA   NA   NA   NA   NA   NA   NA    NA
#> 
#> [[3]]
#>       [,1]  [,2]  [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
#> [1,] 31803 63471 92439   NA   NA   NA   NA   NA   NA    NA
#> [2,] 40559 77667    NA   NA   NA   NA   NA   NA   NA    NA
#> [3,] 46285    NA    NA   NA   NA   NA   NA   NA   NA    NA
#> 
auto[8:10, ]       #columns of all NA's  dropped
#> An object of class "triangles"
#> [[1]]
#>        [,1]   [,2]   [,3]
#> [1,] 127177 244249 317972
#> [2,] 128631 246803     NA
#> [3,] 126288     NA     NA
#> 
#> [[2]]
#>        [,1]   [,2]   [,3]
#> [1,] 378754 361097 369328
#> [2,] 351081 335507     NA
#> [3,] 329236     NA     NA
#> 
#> [[3]]
#>       [,1]  [,2]  [,3]
#> [1,] 31803 63471 92439
#> [2,] 40559 77667    NA
#> [3,] 46285    NA    NA
#> 

auto[1:2,1]
#> An object of class "triangles"
#> [[1]]
#>        [,1]
#> [1,] 101125
#> [2,] 102541
#> 
#> [[2]]
#>        [,1]
#> [1,] 325423
#> [2,] 323627
#> 
#> [[3]]
#>       [,1]
#> [1,] 19827
#> [2,] 22331
#> 

# replacement method
auto[1:2,1] <- list(1,2,3)
auto[1,2]
#> An object of class "triangles"
#> [[1]]
#>        [,1]
#> [1,] 209921
#> 
#> [[2]]
#>        [,1]
#> [1,] 336426
#> 
#> [[3]]
#>       [,1]
#> [1,] 44449
#> 
   
dim(auto)
#> [1]  3 10 10

cbind2(auto[1:2,1])
#>      [,1] [,2] [,3]
#> [1,]    1    2    3
#> [2,]    1    2    3
rbind2(auto[1:2,1])
#>      [,1]
#> [1,]    1
#> [2,]    1
#> [3,]    2
#> [4,]    2
#> [5,]    3
#> [6,]    3
```
