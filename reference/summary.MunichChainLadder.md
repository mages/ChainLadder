# Summary and print function for Munich-chain-ladder

`summary` and `print` methods for a `MunichChainLadder` object

## Usage

``` r
# S3 method for class 'MunichChainLadder'
summary(object, ...)

# S3 method for class 'MunichChainLadder'
print(x, ...)
```

## Arguments

- x, object:

  object of class `"MunichChainLadder"`

- ...:

  optional arguments to `print` or `summary` methods

## Details

`print.MunichChainLadder` calls `summary.MunichChainLadder` and prints a
formatted version of the summary.

## Value

`summary.MunichChainLadder` gives a list of two elements back

- ByOrigin:

  data frame with *Latest Paid* (latest actual paid claims costs),
  *Latest Incurred* (latest actual incurred claims position), *Latest
  P/I Ratio* (ratio of latest paid/incurred claims), *Ult. Paid*
  (estimate ultimate claims cost based on the paid triangle), *Ult.
  Incurred* (estimate ultimate claims cost based on the incurred
  triangle),*Ult. P/I Ratio* (ratio of ultimate paid forecast / ultimate
  incurred forecast)

- Totals:

  data frame of totals over all origin periods. The items follow the
  same naming convention as in `ByOrigin` above

## Author

Markus Gesmann

## See also

See also
[`MunichChainLadder`](http://mages.github.io/ChainLadder/reference/MunichChainLadder.md),
[`plot.MunichChainLadder`](http://mages.github.io/ChainLadder/reference/plot.MunichChainLadder.md)

## Examples

``` r
M <- MunichChainLadder(MCLpaid, MCLincurred)
#> Warning: 'loglinear' model to estimate sigma_n doesn't appear appropriate. 
#> p-value > 5.
#>  est.sigma will be overwritten to 'Mack'.
#>  Mack's estimation method will be used instead.
#> Warning: 'loglinear' model to estimate sigma_n doesn't appear appropriate. 
#> p-value > 5.
#>  est.sigma will be overwritten to 'Mack'.
#>  Mack's estimation method will be used instead.
M
#> MunichChainLadder(Paid = MCLpaid, Incurred = MCLincurred)
#> 
#>   Latest Paid Latest Incurred Latest P/I Ratio Ult. Paid Ult. Incurred
#> 1       2,131           2,174            0.980     2,131         2,174
#> 2       2,348           2,454            0.957     2,385         2,443
#> 3       4,494           4,644            0.968     4,554         4,634
#> 4       5,850           6,142            0.952     6,070         6,182
#> 5       4,648           4,852            0.958     4,879         4,958
#> 6       4,010           4,406            0.910     4,599         4,672
#> 7       2,044           5,022            0.407     7,505         7,655
#>   Ult. P/I Ratio
#> 1          0.980
#> 2          0.976
#> 3          0.983
#> 4          0.982
#> 5          0.984
#> 6          0.984
#> 7          0.980
#> 
#> Totals
#>             Paid Incurred P/I Ratio
#> Latest:   25,525   29,694      0.86
#> Ultimate: 32,121   32,720      0.98
summary(M)
#> $ByOrigin
#>   Latest Paid Latest Incurred Latest P/I Ratio Ult. Paid Ult. Incurred
#> 1        2131            2174        0.9802208  2131.000      2174.000
#> 2        2348            2454        0.9568052  2384.842      2443.222
#> 3        4494            4644        0.9677003  4553.624      4634.358
#> 4        5850            6142        0.9524585  6069.509      6182.347
#> 5        4648            4852        0.9579555  4878.950      4957.805
#> 6        4010            4406        0.9101226  4598.996      4672.402
#> 7        2044            5022        0.4070092  7504.576      7655.378
#>   Ult. P/I Ratio
#> 1      0.9802208
#> 2      0.9761052
#> 3      0.9825792
#> 4      0.9817483
#> 5      0.9840948
#> 6      0.9842894
#> 7      0.9803012
#> 
#> $Totals
#>              Paid Incurred P/I Ratio
#> Latest:   25525.0 29694.00 0.8596013
#> Ultimate: 32121.5 32719.51 0.9817230
#> 
summary(M)$ByOrigin
#>   Latest Paid Latest Incurred Latest P/I Ratio Ult. Paid Ult. Incurred
#> 1        2131            2174        0.9802208  2131.000      2174.000
#> 2        2348            2454        0.9568052  2384.842      2443.222
#> 3        4494            4644        0.9677003  4553.624      4634.358
#> 4        5850            6142        0.9524585  6069.509      6182.347
#> 5        4648            4852        0.9579555  4878.950      4957.805
#> 6        4010            4406        0.9101226  4598.996      4672.402
#> 7        2044            5022        0.4070092  7504.576      7655.378
#>   Ult. P/I Ratio
#> 1      0.9802208
#> 2      0.9761052
#> 3      0.9825792
#> 4      0.9817483
#> 5      0.9840948
#> 6      0.9842894
#> 7      0.9803012
```
