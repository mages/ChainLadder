# LDF METHOD
## by Daniel Murphy

test.LDFMethod.GenIns <- function() {
    ## Check function's various options
    x1 <- ClarkLDF(GenIns, maxage=20)
    checkEquals(tail(x1$Table65$TotalCV, 1), 16.8, tolerance=.1)
    x2 <- ClarkLDF(GenIns, maxage=Inf)
    checkEquals(tail(x2$Table65$TotalCV, 1), 19.0, tolerance=.1)
    x3 <- ClarkLDF(GenIns, maxage=20, adol=FALSE)
    checkEquals(tail(x3$Table65$TotalCV, 1), 15.6, tolerance=.1)
    x4 <- ClarkLDF(GenIns, maxage=Inf, G="weibull")
    checkEquals(tail(x4$Table65$TotalCV, 1), 18.3, tolerance=.1)
    x5 <- ClarkLDF(GenIns, maxage=20, G="weibull")
    checkEquals(tail(x5$Table65$TotalCV, 1), 18.2, tolerance=.1)
    ## Test adol.age option. 
    x6 <- ClarkLDF(GenIns, maxage=20, adol.age=0.5) ## average dol at midyear -- s/b same as default
    checkEquals(tail(x6$Table65$TotalCV, 1), 16.8, tolerance=.1)
    x7 <- ClarkLDF(GenIns, maxage=20, adol.age=0.75) ## average dol at end of Q3
    checkEquals(tail(x7$Table65$TotalCV, 1), 19.8, tolerance=.1)
    x8 <- ClarkLDF(GenIns, maxage=20, adol.age=0.25) ## average dol at end of Q1
    checkEquals(tail(x8$Table65$TotalCV, 1), 15.9, tolerance=.1)
    x9 <- ClarkLDF(GenIns, maxage=20, adol.age=0)
    checkEquals(tail(x9$Table65$TotalCV, 1), 15.6, tolerance=.1) ## should be same as when adol=FALSE
    }

test.LDFMethod.GenIns.Months <- function() {
    ## Check when ages measured in months
    X <- GenIns
    colnames(X) <- 12 * as.numeric(colnames(X))
    x <- ClarkLDF(X, maxage=240)
    checkEquals(tail(x$Table65$TotalCV, 1), 16.8, tolerance=.1)
    x <- ClarkLDF(X, maxage=Inf)
    checkEquals(tail(x$Table65$TotalCV, 1), 19.0, tolerance=.1)
    x <- ClarkLDF(X, maxage=240, adol=FALSE)
    checkEquals(tail(x$Table65$TotalCV, 1), 15.6, tolerance=.1)
    x <- ClarkLDF(X, maxage=Inf, G="weibull")
    checkEquals(tail(x$Table65$TotalCV, 1), 18.3, tolerance=.1)
    x <- ClarkLDF(X, maxage=240, G="weibull")
    checkEquals(tail(x$Table65$TotalCV, 1), 18.2, tolerance=.1)
    ## Test adol.age option. 
    x <- ClarkLDF(X, maxage=240, adol.age=6) ## average dol at midyear -- same as default
    checkEquals(tail(x$Table65$TotalCV, 1), 16.8, tolerance=.1)
    x <- ClarkLDF(X, maxage=240, adol.age=9) ## average dol at end of Q3
    checkEquals(tail(x$Table65$TotalCV, 1), 19.8, tolerance=.1)
    x <- ClarkLDF(X, maxage=240, adol.age=3) ## average dol at end of Q1
    checkEquals(tail(x$Table65$TotalCV, 1), 15.8, tolerance=.1)
    x <- ClarkLDF(X, maxage=240, adol.age=0)
    checkEquals(tail(x$Table65$TotalCV, 1), 15.6, tolerance=.1) ## should be same as when adol=FALSE
    }

test.LDFMethod.GenIns.1stEvalMidYear <- function() {
    ## Check when ages measured in months
    X <- GenIns
    colnames(X) <- 12 * as.numeric(colnames(X)) - 9
    x <- ClarkLDF(X, maxage=240)
    checkEquals(tail(x$Table65$TotalCV, 1), 18.1, tolerance=.1)
    checkEquals(x$Table64$AgeUsed[1], 234) # was 234.25
    x <- ClarkLDF(X, maxage=Inf)
    checkEquals(tail(x$Table65$TotalCV, 1), 30.8, tolerance=.1)
    x <- ClarkLDF(X, maxage=240, adol=FALSE)
    checkEquals(tail(x$Table65$TotalCV, 1), 19.8, tolerance=.1)
    x <- ClarkLDF(X, maxage=Inf, G="weibull")
    checkEquals(tail(x$Table65$TotalCV, 1), 22.3, tolerance=.1)
    x <- ClarkLDF(X, maxage=240, G="weibull")
    checkEquals(tail(x$Table65$TotalCV, 1), 19.9, tolerance=.1)
    ## Test adol.age option. 
    x <- ClarkLDF(X, maxage=240, adol.age=9) ## average dol at end of Q3
    checkEquals(tail(x$Table65$TotalCV, 1), 18.7, tolerance=.1)
    x <- ClarkLDF(X, maxage=240, adol.age=3) ## average dol at end of Q1
    checkEquals(tail(x$Table65$TotalCV, 1), 18.7, tolerance=.1)
    x <- ClarkLDF(X, maxage=240, adol.age=0)
    checkEquals(tail(x$Table65$TotalCV, 1), 19.8, tolerance=.1) ## should be same as when adol=FALSE
    }

test.LDFMethod.incrementalTriangle <- function() {
    X <- cum2incr(GenIns)
    x <- ClarkLDF(X, cumulative=FALSE, maxage=20)
    checkEquals(tail(x$Table65$TotalCV, 1), 16.8, tolerance=.1) ## should be same as base case
    }

test.LDFMethod.WideOriginPeriod <- function() {
    X <- GenIns
    colnames(X) <- 4*(1:10)
    x <- ClarkLDF(X, maxage=80, origin.width=4) ## e.g., 4 quarters
    checkEquals(tail(x$Table65$TotalCV, 1), 16.8, tolerance=.1) ## should be same as base case
    }

test.LDFMethod.quarterly_observations_of_annual_periods <- function() {
    X <- qincurred
    x <- ClarkLDF(X)
    checkEquals(tail(x$Table65$TotalCV, 1), 133.1, tolerance=.1)
    }

# CAPE COD

test.CCMethod.GenIns <- function() {
    ## Check function's various options
    X <- GenIns
    colnames(X) <- 12*as.numeric(colnames(X))
    x  <- ClarkCapeCod(X, Premium=10000000+400000*0:9, maxage=240)
    checkEquals(tail(x$Table65$TotalCV, 1), 11.6, tolerance=.1)
    }

test.CCMethod.qincurred <- function() {
    x  <- ClarkCapeCod(qincurred, Premium=1250+150*0:11, G="loglogistic")
    checkEquals(tail(x$Table65$TotalCV, 1), 12.3, tolerance=.1)
    }

test.CCMethod.PremiumRepeated <- function() {
    x <- ClarkCapeCod(GenIns, Premium=1000000, maxage=20)
    checkEquals(tail(x$Table65$TotalCV, 1), 12.0, tolerance=.1)
    checkEquals(any(is.na(x$Table68$Premium[-1L])), FALSE)
    }

test.CCMethod.RecyclePremium <- function() {
    x <- ClarkCapeCod(GenIns, Premium=1000000*1:3, maxage=20) ## warning issued
    checkEquals(tail(x$Table65$TotalCV, 1), 23.1, tolerance=.1)
    # Table68$Premium should be 1:3 recycled 3 times, then 1:
    checkEquals(x$Table68$Premium[2:11], c(rep(1000000*1:3, 3), 1000000))
    }

test.CCMethod.RepeatPremium <- function() {
    x <- ClarkCapeCod(GenIns, Premium=1000000, maxage=20) ## warning issued
    checkEquals(tail(x$Table65$TotalCV, 1), 12.0, tolerance=.1)
    # Prior to fix, Premium column contained NAs
    checkEquals(any(is.na(x$Table68$Premium[-1L])), FALSE)
    }

# ONE-ROW "TRIANGLES"

test.LDFMethod.OneRowTriangle <- function() {
    x <- ClarkLDF(GenIns[1,,drop=FALSE], maxage=20)
    checkEquals(tail(x$Table65$TotalCV, 1), 48.5, tolerance=.1)
    }

test.CCMethod.OneRowTriangle <- function() {
    x <- ClarkCapeCod(GenIns[1,,drop=FALSE], Premium=1000000, maxage=20)
    checkEquals(tail(x$Table65$TotalCV, 1), 48.5, tolerance=.1)
    }

# TEST EXCEPTIONS = Catch-able errors

test.LDFMethod.LastAgeBeyondmaxage <- function() {
    checkException(ClarkLDF(GenIns, maxage=8))
    }

test.LDFMethod.NonIncreasingAges <- function() {
    X <- GenIns
    colnames(X) <- rev(as.numeric(colnames(X)))
    checkException(ClarkLDF(X, maxage=20))
    }

test.LDFMethod.NonNumericAges <- function() {
    X <- GenIns
    colnames(X) <- LETTERS[1:10]
    checkException(ClarkLDF(X, maxage=20))
    }

test.LDFMethod.Vector <- function() {
    X <- c(GenIns)
    checkException(ClarkLDF(X, maxage=20))
    }

test.LDFMethod.BadGrowthFcn <- function() {
    checkException(ClarkLDF(GenIns, G="Gesmann"))
    }

test.LDFMethod.NonCharacterGrowthFcn <- function() {
    checkException(ClarkLDF(GenIns, G=23))
    }

test.LDFMethod.TooManyGrowthFcns <- function() {
    checkException(ClarkLDF(GenIns, G=c("weibull", "loglogistic")))
    }

test.LDFMethod.SmallMatrix <- function() {
    checkException(ClarkLDF(GenIns[1:3,1:3]))
    }

test.LDFMethod.Negative_adol <- function() {
    checkException(ClarkLDF(GenIns, adol.age=-2))
    }

test.LDFMethod.Negative_adol <- function() {
    checkException(ClarkLDF(GenIns, adol.age=1))
    }

test.LDFMethod.adol <- function() {
    checkException(ClarkLDF(GenIns, adol="maybe"))
    }

test.LDFMethod.adol <- function() {
    checkException(ClarkLDF(GenIns, adol="maybe"))
    }

test.LDFMethod.adol_FALSE_ageSpecified <- function() {
    checkException(ClarkLDF(GenIns, adol=FALSE, adol.age=.5))
    }

test.LDFMethod.adol_FALSE_origin.widthSpecified <- function() {
    checkException(ClarkLDF(GenIns, adol=FALSE, origin.width=1))
    }
    
test.dR_works_as_designed_LDF <- function() {
    y <- ClarkLDF(GenIns, maxage=20)
    # Make sure vcov works, dim(dR) is conformable, and matrix multiplication
    #   yields the correct result
    checkEquals(sqrt(diag(t(y$dR) %*% vcov(y) %*% y$dR)), y$Table65$ParameterSE[1:10])
    }
    
test.dR_works_as_designed_CapeCod <- function() {
    y <- ClarkCapeCod(GenIns, Premium=1000000, maxage=20)
    # Make sure vcov works, dim(dR) is conformable, and matrix multiplication
    #   yields the correct result
    checkEquals(sqrt(diag(t(y$dR) %*% vcov(y) %*% y$dR)), y$Table65$ParameterSE[1:10])
    }

