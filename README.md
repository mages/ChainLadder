# ChainLadder 

<!-- badges: start -->
[![R-CMD-check](https://github.com/mages/ChainLadder/workflows/R-CMD-check/badge.svg)](https://github.com/mages/ChainLadder/actions)
<!-- badges: end --> 
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/ChainLadder)](https://cran.r-project.org/package=ChainLadder) [![downloads](https://cranlogs.r-pkg.org/badges/grand-total/ChainLadder)](https://cran.r-project.org/package=ChainLadder)


ChainLadder is an R package providing methods and models which are typically 
used in insurance claims reserving, including:

- Mack chain-ladder, Munich chain-ladder and Bootstrap models
- General multivariate chain ladder-models 
- Loss development factor fitting and Cape Cod models 
- Generalized linear models 
- One year claims development result functions
- Utility functions to:
  - convert tables into triangles and triangles into tables 
  - convert cumulative into incremental and incremental into cumulative triangles
  - visualise triangles

For a Python claims reserving package visit [chainladder-python](https://github.com/casact/chainladder-python#readme).


## Installation

You can install the stable version from
[CRAN](https://cran.r-project.org/package=ChainLadder):

```s
install.packages('ChainLadder', dependencies = TRUE)
```

You can also install the package via the Github repository:

```s
# install.package("remotes") # In case you have not installed it.
remotes::install_github("mages/ChainLadder", dependencies = TRUE)
```

## Get started

```s
library(ChainLadder)
?ChainLadder
demo(ChainLadder)
```

See the ChainLadder package [vignette](https://mages.github.io/ChainLadder/articles/ChainLadder.html) for more details. 

## Citation

To cite package 'ChainLadder' in publications see the output of:
```s
citation(package="ChainLadder")
```

See also:

  Markus Gesmann. Claims Reserving and IBNR. [Computational Actuarial Science
  with R](https://www.routledge.com/Computational-Actuarial-Science-with-R/Charpentier/p/book/9781466592599). 2014. Chapman and Hall/CRC



## License

This package is free and open source software, licensed under [GPL](https://www.gnu.org/copyleft/gpl.html).

<a rel="license" href="https://creativecommons.org/licenses/by-sa/4.0/deed.en"><img alt="Creative Commons Licence" class="c1" src="https://i.creativecommons.org/l/by-sa/4.0/80x15.png" /></a><br />
<span>ChainLadder documentation</span> is licensed under a <a rel="license" href="https://creativecommons.org/licenses/by-sa/4.0/deed.en">Creative Commons Attribution-ShareAlike 4.0 International License</a>. 
