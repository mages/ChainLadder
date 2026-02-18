# Convert Triangle from wide to long

Given a Triangle in matrix ("wide") format, convert to data.frame
("long") format.

## Usage

``` r
as.LongTriangle(Triangle, varnames = names(dimnames(Triangle)), 
                value.name = "value", na.rm = TRUE)
```

## Arguments

- Triangle:

  a loss "triangle". Must be a `matrix`.

- varnames:

  `character` names for the columns that will store the `rownames` and
  `colnames` of matrix `Triangle`. Defaults to
  `names(dimnames(Triangle))` if available. If not provided, uses
  c("origin", "dev").

- value.name:

  column name to be given to the matrix values that will be stored in
  the data.frame. Defaults to "value".

- na.rm:

  should NA values be excluded from the data.frame? Defaults to TRUE.

## Details

Unlike the as.data.frame.triangle method, and Unlike the 'melt' method
in the 'reshape2' package, this function returns a data.frame where the
rownames and colnames of Triangle are stored as *factors*. This can be a
critical feature when the order of the levels of the columns is
important. For example, when a Triangle is plotted, the order of the
origin and dev dimensions is important. See Examples section.

## Value

A `data.frame`.

## Author

Daniel Murphy

## See also

`as.data.frame.triangle`

## Examples

``` r
as.LongTriangle(GenIns)
#>    origin dev   value
#> 1       1   1  357848
#> 2       2   1  352118
#> 3       3   1  290507
#> 4       4   1  310608
#> 5       5   1  443160
#> 6       6   1  396132
#> 7       7   1  440832
#> 8       8   1  359480
#> 9       9   1  376686
#> 10     10   1  344014
#> 11      1   2 1124788
#> 12      2   2 1236139
#> 13      3   2 1292306
#> 14      4   2 1418858
#> 15      5   2 1136350
#> 16      6   2 1333217
#> 17      7   2 1288463
#> 18      8   2 1421128
#> 19      9   2 1363294
#> 21      1   3 1735330
#> 22      2   3 2170033
#> 23      3   3 2218525
#> 24      4   3 2195047
#> 25      5   3 2128333
#> 26      6   3 2180715
#> 27      7   3 2419861
#> 28      8   3 2864498
#> 31      1   4 2218270
#> 32      2   4 3353322
#> 33      3   4 3235179
#> 34      4   4 3757447
#> 35      5   4 2897821
#> 36      6   4 2985752
#> 37      7   4 3483130
#> 41      1   5 2745596
#> 42      2   5 3799067
#> 43      3   5 3985995
#> 44      4   5 4029929
#> 45      5   5 3402672
#> 46      6   5 3691712
#> 51      1   6 3319994
#> 52      2   6 4120063
#> 53      3   6 4132918
#> 54      4   6 4381982
#> 55      5   6 3873311
#> 61      1   7 3466336
#> 62      2   7 4647867
#> 63      3   7 4628910
#> 64      4   7 4588268
#> 71      1   8 3606286
#> 72      2   8 4914039
#> 73      3   8 4909315
#> 81      1   9 3833515
#> 82      2   9 5339085
#> 91      1  10 3901463
if (FALSE) { # \dontrun{
ggplot(as.LongTriangle(GenIns), 
       aes(x = dev, y = value, group = origin, color = origin)) + geom_line()
} # }
```
