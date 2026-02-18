# Run off triangles of accumulated claim data

Run-off triangles of Automobile Bodily Injury Liability.

## Usage

``` r
data(AutoBI)
```

## Format

Portfolio of automobile bodily injury liability for an experience period
of 1969 to 1976. Paid Claims, Closed Claims and Reported Claim Counts
respectively

## Source

Berquist, J.R. and Sherman, R.E., Loss Reserve Adequacy Testing: A
Comprehensive, Systematic Approach, *Proceedings of the Casualty
Actuarial Society*, LXIV, 1977, pp.123-184.

## Examples

``` r
data(AutoBI)
names(AutoBI)
#> [1] "AutoBIPaid"           "AutoBIClosed"         "AutoBIReportedCounts"
AutoBI$AutoBIPaid
#>      [,1] [,2]  [,3]  [,4]  [,5]  [,6]  [,7]  [,8]
#> [1,] 1904 5398  7496  8882  9712 10071 10199 10256
#> [2,] 2235 6261  8691 10443 11346 11754 12031    NA
#> [3,] 2441 7348 10662 12655 13748 14235    NA    NA
#> [4,] 2503 8173 11810 14176 15383    NA    NA    NA
#> [5,] 2838 8712 12728 15278    NA    NA    NA    NA
#> [6,] 2405 7858 11771    NA    NA    NA    NA    NA
#> [7,] 2759 9182    NA    NA    NA    NA    NA    NA
#> [8,] 2801   NA    NA    NA    NA    NA    NA    NA
AutoBI$AutoBIClosed
#>      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8]
#> [1,] 4079 6616 7192 7494 7670 7749 7792 7806
#> [2,] 4429 7230 7899 8291 8494 8606 8647   NA
#> [3,] 4914 8174 9068 9518 9761 9855   NA   NA
#> [4,] 4497 7842 8747 9254 9469   NA   NA   NA
#> [5,] 4419 7665 8659 9093   NA   NA   NA   NA
#> [6,] 3486 6214 6916   NA   NA   NA   NA   NA
#> [7,] 3516 6226   NA   NA   NA   NA   NA   NA
#> [8,] 3230   NA   NA   NA   NA   NA   NA   NA
AutoBI$AutoBIReportedCounts
#>      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8]
#> [1,] 6553 7696 7770 7799 7814 7819 7820 7821
#> [2,] 7277 8537 8615 8661 8675 8679 8682   NA
#> [3,] 8259 9765 9884 9926 9940 9945   NA   NA
#> [4,] 7858 9474 9615 9664 9680   NA   NA   NA
#> [5,] 7808 9376 9513 9562   NA   NA   NA   NA
#> [6,] 6278 7614 7741   NA   NA   NA   NA   NA
#> [7,] 6446 7884   NA   NA   NA   NA   NA   NA
#> [8,] 6115   NA   NA   NA   NA   NA   NA   NA
```
