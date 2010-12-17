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
#    x6 <- ClarkLDF(GenIns, maxage=20, adol.age=0.75) ## average dol at end of Q3
#    checkEquals(tail(x6$Table65$TotalCV, 1), 19.6, tolerance=.1)
#    x7 <- ClarkLDF(GenIns, maxage=20, adol.age=0.25) ## average dol at end of Q1
#    checkEquals(tail(x7$Table65$TotalCV, 1), 15.9, tolerance=.1)
#    x8 <- ClarkLDF(GenIns, maxage=20, adol.age=0)
#    checkEquals(tail(x8$Table65$TotalCV, 1), 15.6, tolerance=.1) ## should be same as when adol=FALSE
    }

test.LDFMethod.GenIns.Months <- function() {
    ## Check when ages measured in months
    X <- GenIns
    colnames(X) <- 12 * as.numeric(colnames(X))
    x <- ClarkLDF(X, maxage=240)
    checkEquals(tail(x$Table65$TotalCV, 1), 16.7, tolerance=.1)
    x <- ClarkLDF(X, maxage=Inf)
    checkEquals(tail(x$Table65$TotalCV, 1), 18.9, tolerance=.1)
    x <- ClarkLDF(X, maxage=240, adol=FALSE)
    checkEquals(tail(x$Table65$TotalCV, 1), 15.6, tolerance=.1)
    x <- ClarkLDF(X, maxage=Inf, G="weibull")
    checkEquals(tail(x$Table65$TotalCV, 1), 18.3, tolerance=.1)
    x <- ClarkLDF(X, maxage=240, G="weibull")
    checkEquals(tail(x$Table65$TotalCV, 1), 18.2, tolerance=.1)
    ## Test adol.age option. 
#    x <- ClarkLDF(X, maxage=240, adol.age=9) ## average dol at end of Q3
#    checkEquals(tail(x$Table65$TotalCV, 1), 19.7, tolerance=.1)
#    x <- ClarkLDF(X, maxage=240, adol.age=3) ## average dol at end of Q1
#    checkEquals(tail(x$Table65$TotalCV, 1), 15.9, tolerance=.1)
#    x <- ClarkLDF(X, maxage=240, adol.age=0)
#    checkEquals(tail(x$Table65$TotalCV, 1), 15.6, tolerance=.1) ## should be same as when adol=FALSE
    }

test.LDFMethod.GenIns.1stEvalMidYear <- function() {
    ## Check when ages measured in months
    X <- GenIns
    colnames(X) <- 12 * as.numeric(colnames(X)) - 9
    x <- ClarkLDF(X, maxage=240)
#    checkEquals(tail(x$Table65$TotalCV, 1), 17.1, tolerance=.1)
    checkEquals(tail(x$Table65$TotalCV, 1), 18.0, tolerance=.1)
    x <- ClarkLDF(X, maxage=Inf)
#    checkEquals(tail(x$Table65$TotalCV, 1), 25.9, tolerance=.1)
    checkEquals(tail(x$Table65$TotalCV, 1), 30.9, tolerance=.1)
    x <- ClarkLDF(X, maxage=240, adol=FALSE)
    checkEquals(tail(x$Table65$TotalCV, 1), 19.8, tolerance=.1)
    x <- ClarkLDF(X, maxage=Inf, G="weibull")
    checkEquals(tail(x$Table65$TotalCV, 1), 26.4, tolerance=.1)
    x <- ClarkLDF(X, maxage=240, G="weibull")
    checkEquals(tail(x$Table65$TotalCV, 1), 21.1, tolerance=.1)
    ## Test adol.age option. 
#    x <- ClarkLDF(X, maxage=240, adol.age=9) ## average dol at end of Q3
#    checkEquals(tail(x$Table65$TotalCV, 1), 18.5, tolerance=.1)
#    x <- ClarkLDF(X, maxage=240, adol.age=3) ## average dol at end of Q1
#    checkEquals(tail(x$Table65$TotalCV, 1), 18.8, tolerance=.1)
#    x <- ClarkLDF(X, maxage=240, adol.age=0)
#    checkEquals(tail(x$Table65$TotalCV, 1), 19.8, tolerance=.1) ## should be same as when adol=FALSE
    }

test.LDFMethod.incrementalTriangle <- function() {
    X <- cum2incr(GenIns)
    x <- ClarkLDF(X, cumulative=FALSE, maxage=20)
    checkEquals(tail(x$Table65$TotalCV, 1), 16.8, tolerance=.1) ## should be same as base case
    }

#test.LDFMethod.WideOriginPeriod <- function() {
#    X <- GenIns
#    colnames(X) <- 4*(1:10)
#    x <- ClarkLDF(X, maxage=80, origin.width=4) ## e.g., 4 quarters
#    checkEquals(tail(x$Table65$TotalCV, 1), 16.8, tolerance=.1) ## should be same as base case
#    }

test.LDFMethod.quarterly_observations_of_annual_periods <- function() {
    X <- qincurred
    x <- ClarkLDF(X)
    checkEquals(tail(x$Table65$TotalCV, 1), 28.2, tolerance=.1)
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
    }

#test.CCMethod.RecyclePremium <- function() {
#    x <- ClarkCapeCod(GenIns, Premium=1000000*1:3, maxage=20) ## warning issued
#    checkEquals(tail(x$Table65$TotalCV, 1), 23.0, tolerance=.1)
#    }

# ONE-ROW "TRIANGLES"

test.LDFMethod.OneRowTriangle <- function() {
    x <- ClarkLDF(GenIns[1,,drop=FALSE], maxage=20)
    checkEquals(tail(x$Table65$TotalCV, 1), 48.4, tolerance=.1)
    }

test.CCMethod.OneRowTriangle <- function() {
    x <- ClarkCapeCod(GenIns[1,,drop=FALSE], Premium=1000000, maxage=20)
    checkEquals(tail(x$Table65$TotalCV, 1), 48.4, tolerance=.1)
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

#test.LDFMethod.adol_FALSE_ageSpecified <- function() {
#    checkException(ClarkLDF(GenIns, adol=FALSE, adol.age=.5))
#    }

#test.LDFMethod.adol_FALSE_origin.widthSpecified <- function() {
#    checkException(ClarkLDF(GenIns, adol=FALSE, origin.width=1))
#    }

