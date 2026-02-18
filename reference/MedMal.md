# Run off triangles of accumulated claim data

Run-off triangles of Medical Malpractice Data insurance.

## Usage

``` r
data(MedMal)
```

## Format

U.S. medical malpractice insurance for an experience period of 1969 to
1976. Reported Claims, Paid Claims, Case Outstanding and Open Claims
(i.e. the number of outstanding claims) respectively

## Source

Berquist, J.R. and Sherman, R.E., Loss Reserve Adequacy Testing: A
Comprehensive, Systematic Approach, *Proceedings of the Casualty
Actuarial Society*, LXIV, 1977, pp.123-184.

## Examples

``` r
data(MedMal)
names(MedMal)
#> [1] "MedMalReported"    "MedMalPaid"        "MedMalOutstanding"
#> [4] "MedMalOpen"       
MedMal$MedMalReported
#>          [,1]     [,2]     [,3]     [,4]     [,5]     [,6]     [,7]     [,8]
#> [1,]  2897000  5160000 10714000 15228000 16611000 20899000 22892000 23506000
#> [2,]  4828000 10707000 16907000 22840000 26211000 31970000 32216000       NA
#> [3,]  5455000 11941000 20733000 30928000 42395000 48377000       NA       NA
#> [4,]  8732000 18633000 32143000 57196000 61163000       NA       NA       NA
#> [5,] 11228000 19967000 50143000 73733000       NA       NA       NA       NA
#> [6,]  8706000 33459000 63477000       NA       NA       NA       NA       NA
#> [7,] 12928000 48904000       NA       NA       NA       NA       NA       NA
#> [8,] 15791000       NA       NA       NA       NA       NA       NA       NA
MedMal$MedMalPaid
#>        [,1]    [,2]    [,3]     [,4]     [,5]     [,6]     [,7]     [,8]
#> [1,] 125000  406000 1443000  2986000  4467000  8179000 12638000 15815000
#> [2,]  43000  529000 2016000  3641000  7523000 14295000 18983000       NA
#> [3,] 295000 1147000 2479000  5071000 11399000 17707000       NA       NA
#> [4,]  50000  786000 3810000  9771000 18518000       NA       NA       NA
#> [5,] 213000  833000 3599000 11292000       NA       NA       NA       NA
#> [6,] 172000 1587000 6267000       NA       NA       NA       NA       NA
#> [7,] 210000 1565000      NA       NA       NA       NA       NA       NA
#> [8,] 209000      NA      NA       NA       NA       NA       NA       NA
MedMal$MedMalOutstanding
#>          [,1]     [,2]     [,3]     [,4]     [,5]     [,6]     [,7]    [,8]
#> [1,]  2772000  4754000  9271000 12242000 12144000 12720000 10254000 7691000
#> [2,]  4785000 10178000 14891000 19199000 18688000 17675000 13233000      NA
#> [3,]  5160000 10794000 18254000 25857000 30996000 30670000       NA      NA
#> [4,]  8682000 17847000 28333000 47425000 42645000       NA       NA      NA
#> [5,] 11015000 19134000 46544000 62441000       NA       NA       NA      NA
#> [6,]  8534000 31872000 57210000       NA       NA       NA       NA      NA
#> [7,] 12718000 47339000       NA       NA       NA       NA       NA      NA
#> [8,] 15582000       NA       NA       NA       NA       NA       NA      NA
MedMal$MedMalOpen
#>      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8]
#> [1,]  749  840 1001 1206 1034  765  533  359
#> [2,]  660  957 1149 1350 1095  755  539   NA
#> [3,]  878 1329 1720 1799 1428 1056   NA   NA
#> [4,] 1043 1561 1828 1894 1522   NA   NA   NA
#> [5,] 1088 1388 1540 1877   NA   NA   NA   NA
#> [6,] 1033 1418 1663   NA   NA   NA   NA   NA
#> [7,] 1138 1472   NA   NA   NA   NA   NA   NA
#> [8,] 1196   NA   NA   NA   NA   NA   NA   NA
```
