# Run off triangle of claims data.

Run off triangle of accumulated general insurance claims data.
`GenInsLong` provides the same data in a 'long' format.

## Usage

``` r
GenIns
```

## Format

A matrix with 10 accident years and 10 development years.

## Source

TAYLOR, G.C. and ASHE, F.R. (1983) Second Moments of Estimates of
Outstanding Claims. *Journal of Econometrics* **23**, 37-61.

## References

See table 1 in: Distribution-free Calculation of the Standard Error of
Chain Ladder Reserve Estimates, Thomas Mack, 1993, *ASTIN Bulletin*
**23**, 213 - 225

## Examples

``` r
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
plot(GenIns)


plot(GenIns, lattice=TRUE)



head(GenInsLong)
#>   accyear devyear incurred claims
#> 1       1       1          357848
#> 2       2       1          352118
#> 3       3       1          290507
#> 4       4       1          310608
#> 5       5       1          443160
#> 6       6       1          396132

## Convert long format into triangle
## Triangles are usually stored as 'long' tables in data bases
as.triangle(GenInsLong, origin="accyear", dev="devyear", "incurred claims")
#>        devyear
#> accyear      1       2       3       4       5       6       7       8       9
#>      1  357848 1124788 1735330 2218270 2745596 3319994 3466336 3606286 3833515
#>      2  352118 1236139 2170033 3353322 3799067 4120063 4647867 4914039 5339085
#>      3  290507 1292306 2218525 3235179 3985995 4132918 4628910 4909315      NA
#>      4  310608 1418858 2195047 3757447 4029929 4381982 4588268      NA      NA
#>      5  443160 1136350 2128333 2897821 3402672 3873311      NA      NA      NA
#>      6  396132 1333217 2180715 2985752 3691712      NA      NA      NA      NA
#>      7  440832 1288463 2419861 3483130      NA      NA      NA      NA      NA
#>      8  359480 1421128 2864498      NA      NA      NA      NA      NA      NA
#>      9  376686 1363294      NA      NA      NA      NA      NA      NA      NA
#>      10 344014      NA      NA      NA      NA      NA      NA      NA      NA
#>        devyear
#> accyear      10
#>      1  3901463
#>      2       NA
#>      3       NA
#>      4       NA
#>      5       NA
#>      6       NA
#>      7       NA
#>      8       NA
#>      9       NA
#>      10      NA
```
