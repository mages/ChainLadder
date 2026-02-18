# UK motor claims triangle

Triangle of cumulative claims payments for four origin (accident) years
over time (development years).

## Usage

``` r
data("UKMotor")
```

## Format

A matrix with 7 accident years and 7 development years.

## Source

<https://www.actuaries.org.uk/system/files/documents/pdf/crm2-D5.pdf>

## References

Stavros Christofides. Regression models based on log-incremental
payments. Claims Reserving Manual. Volume 2 D5. September 1997

## Examples

``` r
data(UKMotor)
plot(UKMotor)

MackChainLadder(UKMotor, est.sigma="Mack")
#> MackChainLadder(Triangle = UKMotor, est.sigma = "Mack")
#> 
#>      Latest Dev.To.Date Ultimate   IBNR Mack.S.E CV(IBNR)
#> 2007 12,690       1.000   12,690      0     0.00      NaN
#> 2008 12,746       0.973   13,097    351     3.62   0.0103
#> 2009 12,993       0.926   14,031  1,038    22.90   0.0221
#> 2010 11,093       0.844   13,138  2,045   141.98   0.0694
#> 2011 10,217       0.736   13,880  3,663   426.70   0.1165
#> 2012  9,650       0.574   16,812  7,162   692.39   0.0967
#> 2013  6,283       0.304   20,680 14,397   900.58   0.0626
#> 
#>               Totals
#> Latest:    75,672.00
#> Dev:            0.73
#> Ultimate: 104,327.77
#> IBNR:      28,655.77
#> Mack.S.E    1,417.27
#> CV(IBNR):       0.05
```
