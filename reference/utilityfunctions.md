# Cumulative and incremental triangles

Functions to convert between cumulative and incremental triangles

## Usage

``` r
incr2cum(Triangle, na.rm=FALSE)
cum2incr(Triangle)
```

## Arguments

- Triangle:

  triangle. Assume columns are the development period, use transpose
  otherwise.

- na.rm:

  logical. Should missing values be removed?

## Details

`incr2cum` transforms an incremental triangle into a cumulative
triangle, `cum2incr` provides the reserve operation.

## Value

Both functions return a `triangle`.

## Author

Markus Gesmann, Christophe Dutang

## See also

See also
[`as.triangle`](http://mages.github.io/ChainLadder/reference/Triangles.md)

## Examples

``` r
# See the Taylor/Ashe example in Mack's 1993 paper

#original triangle
GenIns
#>       dev
#> origin      1       2       3       4       5       6       7       8       9
#>     1  357848 1124788 1735330 2218270 2745596 3319994 3466336 3606286 3833515
#>     2  352118 1236139 2170033 3353322 3799067 4120063 4647867 4914039 5339085
#>     3  290507 1292306 2218525 3235179 3985995 4132918 4628910 4909315      NA
#>     4  310608 1418858 2195047 3757447 4029929 4381982 4588268      NA      NA
#>     5  443160 1136350 2128333 2897821 3402672 3873311      NA      NA      NA
#>     6  396132 1333217 2180715 2985752 3691712      NA      NA      NA      NA
#>     7  440832 1288463 2419861 3483130      NA      NA      NA      NA      NA
#>     8  359480 1421128 2864498      NA      NA      NA      NA      NA      NA
#>     9  376686 1363294      NA      NA      NA      NA      NA      NA      NA
#>     10 344014      NA      NA      NA      NA      NA      NA      NA      NA
#>       dev
#> origin      10
#>     1  3901463
#>     2       NA
#>     3       NA
#>     4       NA
#>     5       NA
#>     6       NA
#>     7       NA
#>     8       NA
#>     9       NA
#>     10      NA

#incremental triangle
cum2incr(GenIns)
#>       dev
#> origin      1       2       3       4      5      6      7      8      9    10
#>     1  357848  766940  610542  482940 527326 574398 146342 139950 227229 67948
#>     2  352118  884021  933894 1183289 445745 320996 527804 266172 425046    NA
#>     3  290507 1001799  926219 1016654 750816 146923 495992 280405     NA    NA
#>     4  310608 1108250  776189 1562400 272482 352053 206286     NA     NA    NA
#>     5  443160  693190  991983  769488 504851 470639     NA     NA     NA    NA
#>     6  396132  937085  847498  805037 705960     NA     NA     NA     NA    NA
#>     7  440832  847631 1131398 1063269     NA     NA     NA     NA     NA    NA
#>     8  359480 1061648 1443370      NA     NA     NA     NA     NA     NA    NA
#>     9  376686  986608      NA      NA     NA     NA     NA     NA     NA    NA
#>     10 344014      NA      NA      NA     NA     NA     NA     NA     NA    NA

#original triangle
incr2cum(cum2incr(GenIns))
#>       dev
#> origin      1       2       3       4       5       6       7       8       9
#>     1  357848 1124788 1735330 2218270 2745596 3319994 3466336 3606286 3833515
#>     2  352118 1236139 2170033 3353322 3799067 4120063 4647867 4914039 5339085
#>     3  290507 1292306 2218525 3235179 3985995 4132918 4628910 4909315      NA
#>     4  310608 1418858 2195047 3757447 4029929 4381982 4588268      NA      NA
#>     5  443160 1136350 2128333 2897821 3402672 3873311      NA      NA      NA
#>     6  396132 1333217 2180715 2985752 3691712      NA      NA      NA      NA
#>     7  440832 1288463 2419861 3483130      NA      NA      NA      NA      NA
#>     8  359480 1421128 2864498      NA      NA      NA      NA      NA      NA
#>     9  376686 1363294      NA      NA      NA      NA      NA      NA      NA
#>     10 344014      NA      NA      NA      NA      NA      NA      NA      NA
#>       dev
#> origin      10
#>     1  3901463
#>     2       NA
#>     3       NA
#>     4       NA
#>     5       NA
#>     6       NA
#>     7       NA
#>     8       NA
#>     9       NA
#>     10      NA

# See the example in Mack's 1999 paper

#original triangle
Mortgage
#>       dev
#> origin     1      2       3       4       5       6       7       8       9
#>      1 58046 127970  476599 1027692 1360489 1647310 1819179 1906852 1950105
#>      2 24492 141767  984288 2142656 2961978 3683940 4048898 4115760      NA
#>      3 32848 274682 1522637 3203427 4445927 5158781 5342585      NA      NA
#>      4 21439 529828 2900301 4999019 6460112 6853904      NA      NA      NA
#>      5 40397 763394 2920745 4989572 5648563      NA      NA      NA      NA
#>      6 90748 951994 4210640 5866482      NA      NA      NA      NA      NA
#>      7 62096 868480 1954797      NA      NA      NA      NA      NA      NA
#>      8 24983 284441      NA      NA      NA      NA      NA      NA      NA
#>      9 13121     NA      NA      NA      NA      NA      NA      NA      NA
incMortgage <- cum2incr(Mortgage)
#add missing values
incMortgage[1,1] <- NA
incMortgage[2,1] <- NA
incMortgage[1,2] <- NA

#with missing values argument
incr2cum(incMortgage, na.rm=TRUE)
#>       dev
#> origin     1      2       3       4       5       6       7       8       9
#>      1     0      0  348629  899722 1232519 1519340 1691209 1778882 1822135
#>      2     0 117275  959796 2118164 2937486 3659448 4024406 4091268      NA
#>      3 32848 274682 1522637 3203427 4445927 5158781 5342585      NA      NA
#>      4 21439 529828 2900301 4999019 6460112 6853904      NA      NA      NA
#>      5 40397 763394 2920745 4989572 5648563      NA      NA      NA      NA
#>      6 90748 951994 4210640 5866482      NA      NA      NA      NA      NA
#>      7 62096 868480 1954797      NA      NA      NA      NA      NA      NA
#>      8 24983 284441      NA      NA      NA      NA      NA      NA      NA
#>      9 13121     NA      NA      NA      NA      NA      NA      NA      NA

#compared to 
incr2cum(Mortgage)
#>       dev
#> origin     1       2       3        4        5        6        7        8
#>      1 58046  186016  662615  1690307  3050796  4698106  6517285  8424137
#>      2 24492  166259 1150547  3293203  6255181  9939121 13988019 18103779
#>      3 32848  307530 1830167  5033594  9479521 14638302 19980887       NA
#>      4 21439  551267 3451568  8450587 14910699 21764603       NA       NA
#>      5 40397  803791 3724536  8714108 14362671       NA       NA       NA
#>      6 90748 1042742 5253382 11119864       NA       NA       NA       NA
#>      7 62096  930576 2885373       NA       NA       NA       NA       NA
#>      8 24983  309424      NA       NA       NA       NA       NA       NA
#>      9 13121      NA      NA       NA       NA       NA       NA       NA
#>       dev
#> origin        9
#>      1 10374242
#>      2       NA
#>      3       NA
#>      4       NA
#>      5       NA
#>      6       NA
#>      7       NA
#>      8       NA
#>      9       NA
```
