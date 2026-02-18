# ChainLadder

ChainLadder is an R package providing methods and models which are
typically used in insurance claims reserving, including:

- Mack chain-ladder, Munich chain-ladder and Bootstrap models
- General multivariate chain ladder-models
- Loss development factor fitting and Cape Cod models
- Generalized linear models
- One year claims development result functions
- Utility functions to:
  - convert tables into triangles and triangles into tables
  - convert cumulative into incremental and incremental into cumulative
    triangles
  - visualise triangles

For a Python claims reserving package visit
[chainladder-python](https://github.com/casact/chainladder-python#readme).

## Installation

You can install the stable version from
[CRAN](https://cran.r-project.org/package=ChainLadder):

``` s
install.packages('ChainLadder', dependencies = TRUE)
```

You can also install the package via the Github repository:

``` s
# install.package("remotes") # In case you have not installed it.
remotes::install_github("mages/ChainLadder", dependencies = TRUE)
```

## Get started

``` s
library(ChainLadder)
?ChainLadder
demo(ChainLadder)
```

See the ChainLadder package
[vignette](https://mages.github.io/ChainLadder/articles/ChainLadder.html)
for more details.

## Citation

To cite package ‘ChainLadder’ in publications see the output of:

``` s
citation(package="ChainLadder")
```

See also:

Markus Gesmann. Claims Reserving and IBNR. [Computational Actuarial
Science with
R](https://www.routledge.com/Computational-Actuarial-Science-with-R/Charpentier/p/book/9781466592599).
2014. Chapman and Hall/CRC

## License

This package is free and open source software, licensed under
[GPL](https://www.gnu.org/copyleft/gpl.html).

[![Creative Commons
Licence](https://i.creativecommons.org/l/by-sa/4.0/80x15.png)](https://creativecommons.org/licenses/by-sa/4.0/deed.en)  
ChainLadder documentation is licensed under a [Creative Commons
Attribution-ShareAlike 4.0 International
License](https://creativecommons.org/licenses/by-sa/4.0/deed.en).
