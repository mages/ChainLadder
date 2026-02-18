# Methods for Function plot

Methods for function `plot` to produce different diagonostic plots for
an object of class "MultiChainLadder".

## Usage

``` r
# S4 method for class 'MultiChainLadder,missing'
plot(x, y, which.plot=1:4, 
            which.triangle=NULL, 
            main=NULL,  
            portfolio=NULL,
            lowess=TRUE, 
            legend.cex=0.75,...)
```

## Arguments

- x:

  An object of class "MultiChainLadder".

- y:

  "missing"

- which.plot:

  This specifies which type of plot is desired. Its range is 1:5, but
  defaults to 1:4. "1" is the barplot of observed losses and predicted
  IBNR stacked and MSE predictions as error bars; "2" is a trajectory
  plot of the development pattern; "3" is the residual plot of
  standardized residuals against the fitted values; "4" is the Normal-QQ
  plot of the standardized residuals. "5" is the "xyplot" of development
  with confidence intervals for each accident year. Note that "3" and
  "4" are not available for portfolio.

- which.triangle:

  This specifies which triangles are to be plotted. Default value is
  NULL, where all triangles plus the portfolio result will be plotted.

- main:

  It should be a list of titles for each plot. If not supplied, use
  default titles.

- portfolio:

  It specifies which triangles are to be summed as the portfolio, to be
  passed on to `summary`.

- lowess:

  Logical. If `TRUE`, smoothing lines will be added on residual plots.

- legend.cex:

  plotting parameter to be passes on to `cex` in `legend` if
  `which.plot=1`.

- ...:

  optional graphical arguments.

## See also

See also
[`MultiChainLadder`](http://mages.github.io/ChainLadder/reference/MultiChainLadder.md)

## Examples

``` r
if (FALSE) { # \dontrun{
data(liab)
fit.liab <- MultiChainLadder(liab)

# generate diagonostic plots
par(mfcol=(c(3,2)))
plot(fit.liab,which.plot=1:2)

par(mfrow=(c(2,2)))
plot(fit.liab,which.plot=3:4)

plot(fit.liab,which.triangle=1,which.plot=5)
graphics.off()
} # }
```
