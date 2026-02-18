# Tweedie Stochastic Reserving Model

This function implements loss reserving models within the generalized
linear model framework in order to generate the full predictive
distribution for loss reserves. Besides, it generates also the one year
risk view useful to derive the reserve risk capital in a Solvency II
framework. Finally, it allows the user to validate the model error while
changing different model parameters, as the regression structure and
diagnostics on the Tweedie p parameter.

## Usage

``` r
tweedieReserve(triangle, var.power = 1, 
                    link.power = 0, design.type = c(1, 1, 0), 
                    rereserving = FALSE, cum = TRUE, exposure = FALSE, 
                    bootstrap = 1, boot.adj = 0, nsim = 1000, 
                    proc.err = TRUE, p.optim = FALSE,
                    p.check = c(0, seq(1.1, 2.1, by = 0.1), 3),
                    progressBar = TRUE, ...)
```

## Arguments

- triangle:

  An object of class
  [`triangle`](http://mages.github.io/ChainLadder/reference/Triangles.md).

- var.power:

  The index (p) of the power variance function \\V(\mu)=\mu^p\\. Default
  to `p = 1`, which is the over-dispersed Poisson model. If `NULL`, it
  will be assumed to be in `(1, 2)` and estimated using the `cplm`
  package. See
  [`tweedie`](https://rdrr.io/pkg/statmod/man/tweedie.html).

- link.power:

  The index of power link function. The default `link.power = 0`
  produces a log link. See
  [`tweedie`](https://rdrr.io/pkg/statmod/man/tweedie.html).

- design.type:

  It's a 3 dimension array that specifies the design matrix underlying
  the GLM. The dimensions represent respectively: origin period,
  development and calendar period. Accepted values are: `0` (not
  modelled), `1` (modelled as factor) and `2` (modelled as variable).
  Default to `c(1,1,0)`, which is the common specification in actuarial
  literature (origin and development period as factors, calendar period
  not modelled). If a parameter for the calendar period is specified, a
  linear regression on the log CY parameter is fitted to estimate future
  values, thus is recommended to validate them running a plot of the
  gamma values (see output `gamma_y`) .

- rereserving:

  Boolean, if `TRUE` the one year risk view loss reserve distribution is
  derived. Default to `FALSE`. Note, the runtime can materially increase
  if set to `TRUE`.

- cum:

  Boolean, indicating whether the input triangle is cumulative or
  incremental along the development period. If `TRUE`, then `triangle`
  is assumed to be on the cumulative scale, and it will be converted to
  incremental losses internally before a GLM is fitted.

- exposure:

  Boolean, if `TRUE` the `exposure` defined in the
  [`triangle`](http://mages.github.io/ChainLadder/reference/Triangles.md)
  object is specified as `offset` in the GLM model specification.
  Default to `FALSE`.

- bootstrap:

  Integer, it specifies the type of bootstrap for parameter error.
  Accepted values are: `0` (disabled), `1` (parametric), `2`
  (semi-parametric). Default to `1`.

- boot.adj:

  Integer, it specified the methodology when using semi-parametric
  bootstrapping. Accepted values are: `0` (cycles until all the values
  of the pseudo-triangle are \>= 0), `1` (overwrite negative values to
  0.01). Default to `0`. Note, runtime can materially increase when set
  to `0`, as it could struggle to find pseudo-triangles \>= 0)

- nsim:

  Integer, number of simulations to derive the loss reserve
  distribution. Default to `1000`. Note, high num of simulations could
  materially increase runtime, in particular if a re-reserving algorithm
  is used as well.

- proc.err:

  Boolean, if `TRUE` a process error (coherent with the specified model)
  is added to the forecasted distribution. Default to `TRUE`.

- p.optim:

  Boolean, if `TRUE` the model estimates the MLE for the Tweedie's `p`
  parameter. Default to `FALSE`. Recommended to use to validate the
  Tweedie's `p` parameter.

- p.check:

  If `p.optim=TRUE`, a vector of `p` values for consideration. The
  values must all be larger than one (if the response variable has exact
  zeros, the values must all be between one and two). Default to
  `c(0,seq(1.1,2.1,by=0.1),3)`. As fitting the Tweedie p-value isn't a
  straightforward process, please refer to
  [`tweedie.profile`](https://rdrr.io/pkg/tweedie/man/tweedie_profile.html),
  `p.vec` argument.

- progressBar:

  Boolean, if `TRUE` a progress bar will be shown in the console to give
  an indication of bootstrap progress.

- ...:

  Arguments to be passed onto the function
  [`glm`](https://rdrr.io/r/stats/glm.html) or `cpglm` such as
  `contrasts` or `control`. It is important that `offset` and `weight`
  should not be specified. Otherwise, an error will be reported and the
  program will quit.

## Value

The output is an object of class `"glm"` that has the following
components:

- call:

  the matched call.

- summary:

  A data frame containing the predicted loss reserve statistics. The
  following items are displayed:

  - `Latest`: Latest paid

  - `Det.Reserve`: Deterministic reserve, i.e. the MLE GLM estimate of
    the Reserve

  - `Ultimate`: Ultimate cost, defined as `Latest+Det.Reserve`

  - `Dev.To.Date`: Development to date, defined as `Latest/Ultimate`

  The following items are available if `bootstrap>0`

  - `Expected.Reserve`: The expected reserve, defined as the average of
    the reserve simulations. Should be roughly as `Det.Reserve`.

  - `Prediction.Error`: The prediction error of the reserve, defined as
    sqrt of the simulations. Please note that if `proc.err=FALSE`, this
    field contains only the parameter error given by the bootstrap.

  - `CoV`: Coefficient of Variation, defined as
    `Prediction. Error/Expected.Reserve`.

  - `Expected Ultimate`: The expected ultimate, defined as
    `Expected.Reserve+Latest`.

  The following items are availbale if `bootstrap>0 & reserving=TRUE`

  - `Expected.Reserve_1yr`: The reserve derived as sum of next year
    payment and the expected value of the re-reserve at the end of the
    year. It should be similar to both `Expected.Reserve` and
    `Det.Reserve`. If it isn't, it's recommended to change regression
    structure and parameters.

  - `Prediction.Error_1yr`: The prediction error of the prospective
    Claims Development Result (CDR), as defined by Wüthrich
    (CDR=R(0)-X-R(1)).

  - `Emergence.Pattern`: It's the emergence pattern defined as
    `Prediction.Error_1yr/Prediction.Error`.

- Triangle:

  The input triangle.

- FullTriangle:

  The completed triangle, where empty cells in the original triangle are
  filled with model predictions.

- model:

  The fitted GLM, a class of `glm` or `cpglm`. It is most convenient to
  work with this component when model fit information is wanted.

- scale:

  The dispersion parameter phi

- bias:

  The model bias, defined as `bias<-sqrt(n/d.f)`

- GLMReserve:

  Deterministic reserve, i.e. the MLE GLM estimate of the Reserve

- gamma_y:

  When the calendar year is used, it displays the observed and fitted
  calendar year (usually called "gamma"") factors.

- res.diag:

  It's a data frame for residual diagnostics. It contains:

  - `unscaled`: The GLM Pearson residuals.

  - `unscaled.biasadj`: The GLM Person residuals adjusted by the bias,
    i.e. `unscaled.biasadj=unscaled*bias`.

  - `scaled`: The GLM Person scaled residuals, i.e.
    `scaled=unscaled/sqrt(phi)`.

  - `scaled, biasadj`: The GLM Person scaled residuals adjusted by the
    bias, i.e. `scaled.biasadj=scaled*bias`.

  - `dev`: Development year.

  - `origin`: Origin year.

  - `cy`: Calendar year.

\[If `boostrap>1`\]

- distr.res_ult:

  The full distribution "Ultimate View"

\[If `rereserve=TRUE`\]

- distr.res_1yr:

  The full distribution "1yr View"

## References

Gigante, Sigalotti. *Model risk in claims reserving with generalized
linear models*. Giornale dell'Istituto Italiano degli Attuari, Volume
LXVIII. 55-87. 2005

England, Verrall. *Stochastic claims reserving in general insurance*.
B.A.J. 8, III. 443-544. 2002

England, Verrall. *Predictive distributions of outstanding liabilities
in general insurance*. A.A.S. 1, II. 221-270. 2006

Peters, Shevchenko, Wüthrich, *Model uncertainty in claims reserving
within Tweedie's compound poisson models*. Astin Bulletin 39(1). 1-33.
2009

Renshaw, Verrall. *A stochastic model underlying the chain-ladder
technique*. B.A.J. 4, IV. 903-923. 1998

## Author

Alessandro Carrato MSc FIA OA <alessandro.carrato@gmail.com>

## Note

This function was born initially as a fork of the `glmReserve` by Wayne
Zhang. I would like to thank him for his work that permitted me to speed
up my coding.

## Warning

Note that the runtime can materially increase for certain parameter
setting. See above for more details.

## See also

See also
[`summary.tweedieReserve`](http://mages.github.io/ChainLadder/reference/tweedieMethods.md).

## Examples

``` r
if (FALSE) { # \dontrun{
## Verrall's ODP Model is a Tweedie with p=1, log link and 
## origin/development periods as factors, thus c(1,1,0)
res1 <- tweedieReserve(MW2008, var.power=1, link.power=0, 
                           design.type=c(1,1,0), rereserving=TRUE,
                           progressBar=TRUE)

## To get directly ultimate view and respective one year view 
## at selected percentiles
summary(res1) 

#To get other interesting statistics
res1$summary

## In order to validate the Tweedie parameter 'p', it is interesting to 
## review its loglikelihood profile. Please note that, given the nature 
## of our data, it is expected that we may have some fitting issues for 
## given 'p' parameters, thus any results/errors should be considered 
## only indicatively. Considering different regression structures is anyway 
## recommended. Different 'p' values can be defined via the p.check array 
## as input of the function. 
## See help(tweedie.profile), p.vec parameter, for further information.
## Note: The parameters rereserving and bootstrap can be set to 0 to speed up 
## the process, as they aren't needed. 

## Runs a 'p' loglikelihood profile on the parameters 
## p=c(0,1.1,1.2,1.3,1.4,1.5,2,3)
res2 <- tweedieReserve(MW2008, p.optim=TRUE, 
                       p.check=c(0,1.1,1.2,1.3,1.4,1.5,2,3), 
                       design.type=c(1,1,0), 
                        rereserving=FALSE, bootstrap=0, 
                        progressBar=FALSE)

## As it is possible to see in this example, the MLE of p (or xi) results 
## between 0 and 1, which is not possible as Tweedie models aren't 
## defined for 0 < p < 1, thus the Error message. 
## But, despite this, we can conclude that overall the value p=1 could be 
## reasonable for this dataset, as anyway it seems to be near the MLE. 

## In order to consider an inflation parameter across the origin period, 
## it may be interesting to change the regression structure to c(0,1,1) 
## to get the same estimates of the Arithmetic Separation Method, as 
## referred in Gigante/Sigalotti. 
res3 <- tweedieReserve(MW2008, var.power=1, link.power=0, 
                           design.type=c(0,1,1), rereserving=TRUE,
                           progressBar=TRUE)
res3

## An assessment on future fitted calendar year values (usually defined 
## as "gamma") is recommended
plot(res3$gamma_y)

## Model residuals can be plotted using the res.diag output
plot(scaled.biasadj ~ dev, data=res3$res.diag) # Development year
plot(scaled.biasadj ~ cy, data=res3$res.diag) # Calendar year
plot(scaled.biasadj ~ origin, data=res3$res.diag) # Origin year
} # }
```
