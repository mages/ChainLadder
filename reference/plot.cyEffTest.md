# Plot method for a cyEffTest object

`plot.cyEffTest`, a method to plot the output of
[`cyEffTest`](http://mages.github.io/ChainLadder/reference/cyEffTest.md).
It is designed to give a quick overview of a `cyEffTest` object and to
check the assumption of independece between calendar years.

## Usage

``` r
# S3 method for class 'cyEffTest'
plot(x, type = "l", xlab = "Z", ylab = "Density", 
                          main = "Calendar Year Effect", col.area = "gray", 
                          border = NA, ...)
```

## Arguments

- x:

  output from `cyEffTest`

- type:

  Default: "l". What type of plot should be drawn

- xlab:

  Default: "Z". X axis label

- ylab:

  Default: "Density". Y axis label

- main:

  Default: "Calendar Year Effect". Plot title

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

`plot.cyEffTest` shows the underlying distribution, the test statistic
\\Z\\ and the relative Confidence Interval. If the test statistic \\Z\\
is within the highlighted region the hypothesis of dependence between
calendar years could be rejected.

## Author

Marco De Virgilis <devirgilis.marco@gmail.com>

## See also

See Also
[`cyEffTest`](http://mages.github.io/ChainLadder/reference/cyEffTest.md),
[`summary.cyEffTest`](http://mages.github.io/ChainLadder/reference/summary.cyEffTest.md)

## Examples

``` r
plot(cyEffTest(RAA))
```
