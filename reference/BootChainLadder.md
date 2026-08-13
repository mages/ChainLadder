# Bootstrap-Chain-Ladder Model

The `BootChainLadder` procedure provides a predictive distribution of
reserves or IBNRs for a cumulative claims development triangle.

## Usage

``` r
BootChainLadder(Triangle, R = 999, process.distr=c("gamma", "od.pois"), seed = NULL)
```

## Arguments

- Triangle:

  cumulative claims triangle. Assume columns are the development period,
  use transpose otherwise. A (mxn)-matrix \\C\_{ik}\\ which is filled
  for \\k \le n+1-i; i=1,\ldots,m; m\ge n \\. See
  [`qpaid`](http://mages.github.io/ChainLadder/reference/qpaid.md) for
  how to use (mxn)-development triangles with m\<n, say higher
  development period frequency (e.g quarterly) than origin period
  frequency (e.g accident years).

- R:

  the number of bootstrap replicates.

- process.distr:

  character string indicating which process distribution to be assumed.
  One of "gamma" (default), or "od.pois" (over-dispersed Poisson), can
  be abbreviated

- seed:

  optional seed for the random generator

## Details

The `BootChainLadder` function uses a two-stage bootstrapping/simulation
approach. In the first stage an ordinary chain-ladder methods is applied
to the cumulative claims triangle. From this we calculate the scaled
Pearson residuals which we bootstrap R times to forecast future
incremental claims payments via the standard chain-ladder method. In the
second stage we simulate the process error with the bootstrap value as
the mean and using the process distribution assumed. The set of reserves
obtained in this way forms the predictive distribution, from which
summary statistics such as mean, prediction error or quantiles can be
derived.

## Value

BootChainLadder gives a list with the following elements back:

- call:

  matched call

- Triangle:

  input triangle

- f:

  chain-ladder factors

- simClaims:

  array of dimension `c(m,n,R)` with the simulated claims

- IBNR.ByOrigin:

  array of dimension `c(m,1,R)` with the modeled IBNRs by origin period

- IBNR.Triangles:

  array of dimension `c(m,n,R)` with the modeled IBNR development
  triangles

- IBNR.Totals:

  vector of R samples of the total IBNRs

- ChainLadder.Residuals:

  adjusted Pearson chain-ladder residuals

- process.distr:

  assumed process distribution

- R:

  the number of bootstrap replicates

## References

England, PD and Verrall, RJ. Stochastic Claims Reserving in General
Insurance (with discussion), *British Actuarial Journal* 8, III. 2002

Barnett and Zehnwirth. The need for diagnostic assessment of bootstrap
predictive models, *Insureware technical report*. 2007

## Author

Markus Gesmann, <markus.gesmann@gmail.com>

## Note

The implementation of `BootChainLadder` follows closely the discussion
of the bootstrap model in section 8 and appendix 3 of the paper by
England and Verrall (2002).

## See also

See also
[`summary.BootChainLadder`](http://mages.github.io/ChainLadder/reference/summary.BootChainLadder.md),
[`plot.BootChainLadder`](http://mages.github.io/ChainLadder/reference/plot.BootChainLadder.md)
displaying results and finally
[`CDR.BootChainLadder`](http://mages.github.io/ChainLadder/reference/CDR.md)
for the one year claims development result.

## Examples

``` r
# See also the example in section 8 of England & Verrall (2002) on page 55.

B <- BootChainLadder(RAA, R=999, process.distr="gamma")
B
#> BootChainLadder(Triangle = RAA, R = 999, process.distr = "gamma")
#> 
#>      Latest Mean Ultimate Mean IBNR IBNR.S.E IBNR 75% IBNR 95%
#> 1981 18,834        18,834         0        0        0        0
#> 1982 16,704        16,874       170      751      213    1,435
#> 1983 23,466        24,088       622    1,240    1,039    3,089
#> 1984 27,067        28,785     1,718    1,915    2,758    5,461
#> 1985 26,180        29,085     2,905    2,387    4,148    7,509
#> 1986 15,852        19,561     3,709    2,476    5,189    8,102
#> 1987 12,314        17,835     5,521    3,089    7,197   11,348
#> 1988 13,112        24,170    11,058    4,920   13,952   19,626
#> 1989  5,395        16,271    10,876    5,920   14,529   21,221
#> 1990  2,063        19,929    17,866   14,195   25,820   43,213
#> 
#>                  Totals
#> Latest:         160,987
#> Mean Ultimate:  215,431
#> Mean IBNR:       54,444
#> IBNR.S.E         18,711
#> Total IBNR 75%:  65,615
#> Total IBNR 95%:  87,033
plot(B)

# Compare to MackChainLadder
MackChainLadder(RAA)
#> MackChainLadder(Triangle = RAA)
#> 
#>      Latest Dev.To.Date Ultimate   IBNR Mack.S.E CV(IBNR)
#> 1981 18,834       1.000   18,834      0        0      NaN
#> 1982 16,704       0.991   16,858    154      143    0.928
#> 1983 23,466       0.974   24,083    617      592    0.959
#> 1984 27,067       0.943   28,703  1,636      713    0.436
#> 1985 26,180       0.905   28,927  2,747    1,452    0.529
#> 1986 15,852       0.813   19,501  3,649    1,995    0.547
#> 1987 12,314       0.694   17,749  5,435    2,204    0.405
#> 1988 13,112       0.546   24,019 10,907    5,354    0.491
#> 1989  5,395       0.336   16,045 10,650    6,332    0.595
#> 1990  2,063       0.112   18,402 16,339   24,566    1.503
#> 
#>               Totals
#> Latest:   160,987.00
#> Dev:            0.76
#> Ultimate: 213,122.23
#> IBNR:      52,135.23
#> Mack.S.E   26,880.74
#> CV(IBNR):       0.52
quantile(B, c(0.75,0.95,0.99, 0.995))
#> $ByOrigin
#>        IBNR 75%  IBNR 95%  IBNR 99% IBNR 99.5%
#> 1981     0.0000     0.000     0.000      0.000
#> 1982   213.3995  1435.471  2841.149   3733.411
#> 1983  1039.0556  3089.224  4612.046   4945.226
#> 1984  2758.0100  5461.188  7326.144   8210.354
#> 1985  4147.8889  7508.974  9486.665  10451.706
#> 1986  5188.7194  8101.653 11425.500  12991.492
#> 1987  7196.7695 11348.087 14662.543  15553.875
#> 1988 13952.0263 19625.821 25384.471  27311.402
#> 1989 14529.3956 21220.849 27990.618  30039.953
#> 1990 25819.5193 43212.773 59326.863  69688.238
#> 
#> $Totals
#>                Totals
#> IBNR 75%:    65614.56
#> IBNR 95%:    87033.25
#> IBNR 99%:   105598.78
#> IBNR 99.5%: 113236.29
#> 

# fit a distribution to the IBNR
library(MASS)
plot(ecdf(B$IBNR.Totals))
# fit a log-normal distribution 
fit <- fitdistr(B$IBNR.Totals[B$IBNR.Totals>0], "lognormal")
fit
#>      meanlog         sdlog    
#>   10.843118752    0.361345937 
#>  ( 0.011432480) ( 0.008083984)
curve(plnorm(x,fit$estimate["meanlog"], fit$estimate["sdlog"]), col="red", add=TRUE)


# See also the ABC example in  Barnett and Zehnwirth (2007) 
A <- BootChainLadder(ABC, R=999, process.distr="gamma")
A
#> BootChainLadder(Triangle = ABC, R = 999, process.distr = "gamma")
#> 
#>         Latest Mean Ultimate Mean IBNR IBNR.S.E  IBNR 75%  IBNR 95%
#> 1977   762,544       762,544         0        0         0         0
#> 1978   889,022       903,785    14,763    5,294    17,870    25,088
#> 1979 1,019,932     1,057,485    37,553    8,255    42,584    52,947
#> 1980 1,002,134     1,066,375    64,241   10,251    70,679    81,667
#> 1981 1,002,194     1,102,771   100,577   11,906   108,005   121,005
#> 1982   944,614     1,088,751   144,137   15,090   153,149   169,439
#> 1983   895,700     1,108,749   213,049   17,306   225,902   241,604
#> 1984 1,024,228     1,410,477   386,249   24,510   402,307   427,184
#> 1985 1,173,448     1,939,010   765,562   37,797   790,815   829,805
#> 1986 1,011,178     2,374,197 1,363,019   62,695 1,402,463 1,468,146
#> 1987   496,200     2,695,177 2,198,977  115,211 2,271,713 2,406,393
#> 
#>                     Totals
#> Latest:         10,221,194
#> Mean Ultimate:  15,509,321
#> Mean IBNR:       5,288,127
#> IBNR.S.E           185,304
#> Total IBNR 75%:  5,410,706
#> Total IBNR 95%:  5,601,874
plot(A, log=TRUE)


## One year claims development result
CDR(A)
#>             IBNR   IBNR.S.E  CDR(1)S.E  CDR(1)75%  CDR(1)95%
#> 1977        0.00      0.000      0.000       0.00       0.00
#> 1978    14763.08   5293.531   5293.531   17869.54   25088.26
#> 1979    37553.24   8254.537   6670.831   41693.24   49736.98
#> 1980    64241.28  10251.176   7124.744   68509.36   77242.89
#> 1981   100576.54  11906.450   7873.762  105120.26  114376.66
#> 1982   144136.73  15090.436   9068.888  149663.92  160039.16
#> 1983   213049.04  17306.429  10684.855  219417.74  229909.23
#> 1984   386248.57  24509.999  15084.542  396311.31  412522.67
#> 1985   765562.32  37796.824  24428.579  782617.15  805892.09
#> 1986  1363019.41  62694.798  43786.860 1390328.35 1435838.51
#> 1987  2198976.82 115211.158  95813.485 2260987.08 2356162.85
#> Total 5288127.03 185304.355 138056.793 5369950.29 5522206.57
```
