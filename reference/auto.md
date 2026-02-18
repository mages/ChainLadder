# Run off triangle of accumulated claim data

Run-off triangles of Personal Auto and Commercial Auto insurance.

## Usage

``` r
data(auto)
```

## Format

A list of three matrices, paid Personal Auto, incurred Personal Auto and
paid Commercial Auto respectively.

## Source

Zhang (2010). A general multivariate chain ladder model. *Insurance:
Mathematics and Economics*, 46, pp. 588-599.

## Examples

``` r
data(auto)
names(auto)
#> [1] "PersonalAutoPaid"     "PersonalAutoIncurred" "CommercialAutoPaid"  
```
