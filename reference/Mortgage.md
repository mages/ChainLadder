# Run off triangle of accumulated claims data

Development triangle of a mortgage guarantee business

## Usage

``` r
data(Mortgage)
```

## Format

A matrix with 9 accident years and 9 development years.

## Source

Competition Presented at a London Market Actuaries Dinner, D.E.A.
Sanders, 1990

## References

See table 4 in: Distribution-free Calculation of the Standard Error of
Chain Ladder Reserve Estimates, Thomas Mack, 1993, *ASTIN Bulletin*
**23**, 213 - 225

## Examples

``` r
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
plot(Mortgage)

plot(Mortgage, lattice=TRUE)
```
