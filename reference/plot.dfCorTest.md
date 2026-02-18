# Plot method for a dfCorTest object

`plot.dfCorTest`, a method to plot the output of
[`dfCorTest`](http://mages.github.io/ChainLadder/reference/dfCorTest.md).
It is designed to give a quick overview of a `dfCorTest` object and to
check the assumption of independece between development factors.

## Usage

``` r
# S3 method for class 'dfCorTest'
plot(x, type = "l", xlab = "T", ylab = "Density", 
                          main = "Development Factor Correlation", col.area = "gray",
                          border = NA, ...)
```

## Arguments

- x:

  output from `dfCorTest`

- type:

  Default: "l". What type of plot should be drawn

- xlab:

  Default: "Z". X axis label

- ylab:

  Default: "Density". Y axis label

- main:

  Default: "Development Factor Correlation". Plot title

- col.area:

  Default: "gray". Color of the shaded area.

- border:

  Default: NULL, the color to draw the border. Use `border = NA` to omit
  borders.

- ...:

  optional arguments. See
  [`plot.default`](https://rdrr.io/r/graphics/plot.default.html) for
  more details.

## Details

`plot.dfCorTest` shows the underlying distribution, the test statistic
\\Z\\ and the relative Confidence Interval. If the test statistic \\Z\\
is within the highlighted region the hypothesis of correlation between
development factors could be rejected.

## Author

Marco De Virgilis <devirgilis.marco@gmail.com>

## See also

See Also
[`dfCorTest`](http://mages.github.io/ChainLadder/reference/dfCorTest.md),
[`summary.dfCorTest`](http://mages.github.io/ChainLadder/reference/summary.dfCorTest.md)

## Examples

``` r
plot(dfCorTest(RAA))
```
