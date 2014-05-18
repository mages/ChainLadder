# runit.clark unit tests
## by Daniel Murphy
##
##  NOTE: For some reason which I can't understand, running Clark methods
##  from the source 'clark.r' yields different results -- par, value, 
##  counts, etc. -- than when the package has already been built.
##  Therefore, the test values below are set after the ChainLadder
##  package was successfully built.

test.LDFMethod.GenIns <- function() {
    ## Check function's various options
    x1 <- ClarkLDF(GenIns, maxage=20)
    checkEquals(tail(Table65(x1)$TotalCV, 1), 16.8, tolerance=.1)
    x2 <- ClarkLDF(GenIns, maxage=Inf)
    checkEquals(tail(Table65(x2)$TotalCV, 1), 19.0, tolerance=.1)
    x3 <- ClarkLDF(GenIns, maxage=20, adol=FALSE)
    checkEquals(tail(Table65(x3)$TotalCV, 1), 15.6, tolerance=.1)
    x4 <- ClarkLDF(GenIns, maxage=Inf, G="weibull")
    checkEquals(tail(Table65(x4)$TotalCV, 1), 18.3, tolerance=.1)
    x5 <- ClarkLDF(GenIns, maxage=20, G="weibull")
    checkEquals(tail(Table65(x5)$TotalCV, 1), 18.2, tolerance=.1)
    ## Test adol.age option. 
    x6 <- ClarkLDF(GenIns, maxage=20, adol.age=0.5) ## average dol at midyear -- s/b same as default
    checkEquals(tail(Table65(x6)$TotalCV, 1), 16.8, tolerance=.1)
    x7 <- ClarkLDF(GenIns, maxage=20, adol.age=0.75) ## average dol at end of Q3
    checkEquals(tail(Table65(x7)$TotalCV, 1), 19.7, tolerance=.1)
    x8 <- ClarkLDF(GenIns, maxage=20, adol.age=0.25) ## average dol at end of Q1
    checkEquals(tail(Table65(x8)$TotalCV, 1), 15.8, tolerance=.1)
    x9 <- ClarkLDF(GenIns, maxage=20, adol.age=0)
    checkEquals(tail(Table65(x9)$TotalCV, 1), 15.6, tolerance=.1) ## should be same as when adol=FALSE
    }

test.LDFMethod.GenIns.Months <- function() {
    ## Check when ages measured in months
    X <- GenIns
    colnames(X) <- 12 * as.numeric(colnames(X))
    x <- ClarkLDF(X, maxage=240)
    checkEquals(tail(Table65(x)$TotalCV, 1), 16.8, tolerance=.1)
    x <- ClarkLDF(X, maxage=Inf)
    checkEquals(tail(Table65(x)$TotalCV, 1), 19.0, tolerance=.1)
    x <- ClarkLDF(X, maxage=240, adol=FALSE)
    checkEquals(tail(Table65(x)$TotalCV, 1), 15.6, tolerance=.1)
    x <- ClarkLDF(X, maxage=Inf, G="weibull")
    checkEquals(tail(Table65(x)$TotalCV, 1), 18.3, tolerance=.1)
    x <- ClarkLDF(X, maxage=240, G="weibull")
    checkEquals(tail(Table65(x)$TotalCV, 1), 18.2, tolerance=.1)
    ## Test adol.age option. 
    x <- ClarkLDF(X, maxage=240, adol.age=6) ## average dol at midyear -- same as default
    checkEquals(tail(Table65(x)$TotalCV, 1), 16.8, tolerance=.1)
    x <- ClarkLDF(X, maxage=240, adol.age=9) ## average dol at end of Q3
    checkEquals(tail(Table65(x)$TotalCV, 1), 19.6, tolerance=.1)
    x <- ClarkLDF(X, maxage=240, adol.age=3) ## average dol at end of Q1
    checkEquals(tail(Table65(x)$TotalCV, 1), 15.8, tolerance=.1)
    x <- ClarkLDF(X, maxage=240, adol.age=0)
    checkEquals(tail(Table65(x)$TotalCV, 1), 15.6, tolerance=.1) ## should be same as when adol=FALSE
    }

test.LDFMethod.GenIns.1stEvalMidYear <- function() {
    ## Check when ages measured in months
    X <- GenIns
    colnames(X) <- 12 * as.numeric(colnames(X)) - 9
    x <- ClarkLDF(X, maxage=240)
    checkEquals(tail(Table65(x)$TotalCV, 1), 18.1, tolerance=.1)
    checkEquals(Table64(x)$AgeUsed[1], 234) # was 234.25
    x <- ClarkLDF(X, maxage=Inf)
    checkEquals(tail(Table65(x)$TotalCV, 1), 31.4, tolerance=.1)
    x <- ClarkLDF(X, maxage=240, adol=FALSE)
    checkEquals(tail(Table65(x)$TotalCV, 1), 19.8, tolerance=.1)
    x <- ClarkLDF(X, maxage=Inf, G="weibull")
    checkEquals(tail(Table65(x)$TotalCV, 1), 26.7, tolerance=.1)
    x <- ClarkLDF(X, maxage=240, G="weibull")
    checkEquals(tail(Table65(x)$TotalCV, 1), 21.3, tolerance=.1)
    ## Test adol.age option. 
    x <- ClarkLDF(X, maxage=240, adol.age=9) ## average dol at end of Q3
    checkEquals(tail(Table65(x)$TotalCV, 1), 18.9, tolerance=.1)
    x <- ClarkLDF(X, maxage=240, adol.age=3) ## average dol at end of Q1
    checkEquals(tail(Table65(x)$TotalCV, 1), 18.6, tolerance=.1)
    x <- ClarkLDF(X, maxage=240, adol.age=0)
    checkEquals(tail(Table65(x)$TotalCV, 1), 19.8, tolerance=.1) ## should be same as when adol=FALSE
    }

test.LDFMethod.incrementalTriangle <- function() {
    X <- cum2incr(GenIns)
    x <- ClarkLDF(X, cumulative=FALSE, maxage=20)
    checkEquals(tail(Table65(x)$TotalCV, 1), 16.8, tolerance=.1) ## should be same as base case
    }

test.LDFMethod.WideOriginPeriod <- function() {
    X <- GenIns
    colnames(X) <- 4*(1:10)
    x <- ClarkLDF(X, maxage=80, origin.width=4) ## e.g., 4 quarters
    checkEquals(tail(Table65(x)$TotalCV, 1), 16.8, tolerance=.1) ## should be same as base case
    }

test.LDFMethod.quarterly_observations_of_annual_periods <- function() {
    X <- qincurred
    x <- ClarkLDF(X)
    checkEquals(tail(Table65(x)$TotalCV, 1), 131.9, tolerance=.1)
    }

# CAPE COD

test.CCMethod.GenIns <- function() {
    ## Check function's various options
    X <- GenIns
    colnames(X) <- 12*as.numeric(colnames(X))
    x  <- ClarkCapeCod(X, Premium=10000000+400000*0:9, maxage=240)
    checkEquals(tail(Table65(x)$TotalCV, 1), 11.5, tolerance=.005) # tightened tolerance 2011/1/25 DMM
    }

test.CCMethod.qincurred <- function() {
    x  <- ClarkCapeCod(qincurred, Premium=1250+150*0:11, G="loglogistic")
    checkEquals(tail(Table65(x)$TotalCV, 1), 12.3, tolerance=.1)
    }

test.CCMethod.PremiumRepeated <- function() {
    x <- ClarkCapeCod(GenIns, Premium=1000000, maxage=20)
    checkEquals(tail(Table65(x)$TotalCV, 1), 11.8, tolerance=.1)
    checkEquals(any(is.na(Table68(x)$Premium[-1L])), FALSE)
    }

test.CCMethod.RecyclePremium <- function() {
    x <- ClarkCapeCod(GenIns, Premium=1000000*1:3, maxage=20) ## warning issued
    checkEquals(tail(Table65(x)$TotalCV, 1), 22.7, tolerance=.1)
    # Table68$Premium should be 1:3 recycled 3 times, then 1:
    checkEquals(Table68(x)$Premium[2:11], c(rep(1000000*1:3, 3), 1000000))
    }

test.CCMethod.RepeatPremium <- function() {
    x <- ClarkCapeCod(GenIns, Premium=1000000, maxage=20)
    checkEquals(tail(Table65(x)$TotalCV, 1), 11.8, tolerance=.1)
    # Prior to fix, Premium column contained NAs
    checkEquals(any(is.na(Table68(x)$Premium[-1L])), FALSE)
    }

test.Table68_FutureValue_Equal <- function() {
    X <- GenIns
    colnames(X) <- 12*as.numeric(colnames(X))
    x  <- ClarkCapeCod(X, Premium=10000000+400000*0:9, maxage=240)
    checkTrue(all(x$FutureValue==Table68(x)$FutureValue[2:11]))
    }

# ONE-ROW "TRIANGLES"

test.LDFMethod.OneRowTriangle <- function() {
    x <- ClarkLDF(GenIns[1,,drop=FALSE], maxage=20)
    checkEquals(tail(Table65(x)$TotalCV, 1), 48.6, tolerance=.1)
    }

test.CCMethod.OneRowTriangle <- function() {
    x <- ClarkCapeCod(GenIns[1,,drop=FALSE], Premium=1000000, maxage=20)
    checkEquals(tail(Table65(x)$TotalCV, 1), 47.4, tolerance=.1)
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
    
test.FutureValueGradient_works_as_designed_LDF <- function() {
    y <- ClarkLDF(GenIns, maxage=20)
    # Make sure vcov works, dim(FutureValueGradient) is conformable, and matrix multiplication
    #   yields the correct result
    checkEquals(sqrt(diag(t(y$FutureValueGradient) %*% vcov(y) %*% y$FutureValueGradient)), Table65(y)$ParameterSE[1:10])
    }
    
test.FutureValueGradient_works_as_designed_CapeCod <- function() {
    y <- ClarkCapeCod(GenIns, Premium=1000000, maxage=20)
    # Make sure vcov works, dim(dR) is conformable, and matrix multiplication
    #   yields the correct result
    checkEquals(sqrt(diag(t(y$FutureValueGradient) %*% vcov(y) %*% y$FutureValueGradient)), Table65(y)$ParameterSE[1:10])
    }

test.correct_SummaryLDF_comparison <- function() {
    y20 <- ClarkLDF(RAA, maxage=20)
    z20 <- summary(y20)
    yinf<-ClarkLDF(RAA)
    zinf <- summary(yinf)
    #   yields the correct result
    checkTrue(all((zinf[["Ldf"]] > z20[["Ldf"]])[1:10]))
    }

test.correct_SummaryCapeCod_comparison <- function() {
    X <- GenIns
    colnames(X) <- 12*as.numeric(colnames(X))
    y240 <- ClarkCapeCod(X, Premium=10000000+400000*0:9, maxage=240)
    z240 <- summary(y240)
    yInf <- ClarkCapeCod(X, Premium=10000000+400000*0:9, maxage=Inf)
    zInf <- summary(yInf)
    checkTrue(all((zInf[["FutureGrowthFactor"]] > z240[["FutureGrowthFactor"]])[1:10]))
    }

test.class_returned_by_print.ClarkLDF <- function() {
    x <- ClarkLDF(RAA)
    z <- print(x)
    y <- sapply(z, class)
    checkTrue(all(y == "character"))
    }

test.class_returned_by_print.ClarkCapeCod <- function() {
    x <- ClarkCapeCod(RAA, Premium=25000)
    z <- print(x)
    y <- sapply(z, class)
    checkTrue(all(y == "character"))
    }

test.zero_expected_value <- function() {
    # Create a new environment, store ages we know will 
    #   evaluate to 100% under a weibull growth function
    #   with parameters THETAG=(1,1).
    env <- new.env()
    env$Age.from <- 108
    env$Age.to <- 120
    # pretend there's one origin year
    env$origin <- 1
    env$value <- 1
    x <- ChainLadder:::LL.ODP(c(1,1,1), ChainLadder:::MU.LDF, ChainLadder:::weibull, env) # should not blow up
    checkTrue(x < 0)
    }
