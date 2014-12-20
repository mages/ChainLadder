# ChainLadder 

ChainLadder is an R package providing methods and models which are typically used in insurance claims reserving, including:

- Mack-, Munich- and Bootstrap-chain-ladder models
- General multivariate chain ladder models 
- Loss development factor fitting and Cape Cod models 
- Generalized linear models 
- Utility functions to:
  - convert quickly tables into triangles
  - triangles into tables 
  - cumulative into incremental 
  - incremental into cumulative triangles

## Installation

You can install the stable version from
[CRAN](http://cran.r-project.org/package=ChainLadder):

```s
install.packages('ChainLadder')
```

To install the current development version from github you need the [devtools package](http://cran.r-project.org/web/packages/devtools/index.html) and the other packages on which ChainLadder depends:

```s
install.packages(c("systemfit", "actuar", "Hmisc", "statmod", "tweedie", "cplm"))
```

To install ChainLadder run:
```s
library(devtools)
install_github("mages/ChainLadder")
```

## Usage

```s
library(ChainLadder)
?ChainLadder
demo(ChainLadder)
```

See the ChainLadder package [vignettes](http://cran.r-project.org/web/packages/ChainLadder/) for more details. 

## Citation

To cite package 'ChainLadder' in publications use:

  Markus Gesmann, Daniel Murphy, Wayne Zhang and Alessandro Carrato (2015). 
  ChainLadder: Statistical methods and models for the calculation of 
  outstanding claims reserves in general insurance. R package version 0.1.9.
  
See also:

  Markus Gesmann. Claims Reserving and IBNR. Computational Actuarial Science
  with R. 2014. Chapman and Hall/CRC

## License

This package is free and open source software, licensed under [GPL](https://www.gnu.org/copyleft/gpl.html).

<a rel="license" href="http://creativecommons.org/licenses/by-sa/4.0/deed.en_GB"><img alt="Creative Commons Licence" class="c1" src="http://i.creativecommons.org/l/by-sa/4.0/80x15.png" /></a><br />
<span>ChainLadder documentation</span> by Markus Gesmann, Dan Murphy and Wayne Zhang is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by-sa/4.0/deed.en_GB">Creative Commons Attribution-ShareAlike 4.0 International License</a>. 