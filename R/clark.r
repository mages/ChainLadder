# Clark method from 2003 eForum paper
# Author: Daniel Murphy
# Date: October 31, 2010 - 2011
# Includes:
#   LDF and Cape Cod methods
#   Two growth functions -- loglogistic and weibull
#   print method: displays the table on p. 65 of the paper
#   plot method: displays 3 residual plots, QQ-plot with 
#       results of Shapiro-Wilk normality test.

# Organization:
#   "LDF" and "CapeCod" scripts that accept the data (a matrix)
#       and arguments to flag:
#           Does the matrix hold cumulative or incremental data?
#           In the case of the Cape Code method, a premium vector
#               whose length = nrow(data)
#           Should the analysis be conducted based on the average
#               date of loss within the origin period?
#           What is the maximum age to which losses should be projected?
#           What growth function should be utilized?
#   print, plot, summary, and other methods
#   Functions
#       Definition of "function class with derivatives"
#           Generics to access the derivatives of a function
#       Definition of growth function class with derivatives with respect to
#           the second, parameter argument
#       Loglogistic growth function
#       Weibull growth function
#       Loglikelihood function under Clark's ODP assumption
#       Function to calculate SIGMA2
#       Expected value (MU) functions
#           LDF Method
#           Cape Cod Method
#       Reserve Functions
#           LDF Method
#           Cape Cod Method
#require(actuar) # for its fast loglogistic function!

ClarkLDF <- function(Triangle,
        cumulative = TRUE,
        maxage = Inf,
        adol = TRUE,
        adol.age = NULL,
        origin.width = NULL,
        G = "loglogistic"
        ) {
    # maxage represents the age to which losses are projected at Ultimate
    # adol.age is the age within an origin period of the 
    #   average date of loss (adol.age); only relevant if adol=TRUE
    # origin.width is the width of an origin period; only relevant if adol=TRUE
    
    if (!is.character(G))
        stop("Growth function G must be the character name of a growth function")
    if (length(G)>1L)
        stop("Only one growth function can be specified")
    G <- switch(G,
         loglogistic = loglogistic,
         weibull = weibull,
         stop(paste("Growth function '", G, "' unrecognized", sep=""))
         )
        
    if (!is.matrix(Triangle)) stop("ClarkLDF expects Triangle in matrix format")
    nr <- nrow(Triangle)
    if (ncol(Triangle) < 4L) stop("matrix must have at least 4 columns")

    dev <- as.numeric(colnames(Triangle))
    if (length(dev)<1 | any(is.na(dev))) stop("non-'age' column name(s)")
    if (any(dev[-1L]<=head(dev, -1L))) stop("ages must be strictly increasing")
    if (tail(dev, 1L) > maxage[1L]) stop("'maxage' must not be less than last age in triangle")
    
    # 'workarea' stores intermediate calculations
#    workarea <- new.env()
    workarea <<- new.env()

    if (!inherits(Triangle, "triangle")) Triangle <- as.triangle(Triangle)

    # Save the origin, dev names
    origins <- rownames(Triangle)
    devs <- colnames(Triangle)
    # Save user's names for 'origin' (row) and 'dev' (column), if any
    dimnms <- c("origin", "dev")
    if (!is.null(nm<-names(dimnames(Triangle)))) 
        # If only one name specified by user, other will be NA
        dimnms[!is.na(nm)] <- nm[!is.na(nm)]

    # Calculate the age.from/to's and maxage based on adol setting.
    Age.to <- dev
    if (adol) {
        if (is.null(origin.width)) {
            agediff <- diff(Age.to)
            if (!all(abs(agediff-agediff[1L])<sqrt(.Machine$double.eps))) 
                warning("origin.width unspecified, but varying age differences; check reasonableness of 'Table64$AgeUsed'")
            origin.width <-  mean(agediff)
            }
        if (is.null(adol.age)) # default is half width of origin period
            adol.age <- origin.width / 2
        # rudimentary reasonableness checks of adol.age
        if (adol.age < 0) 
            stop("age of average date of loss cannot be negative")
        if (adol.age >= origin.width) 
            stop("age of average date of loss must be < origin.width (ie, within origin period)")
        ## For all those ages that are before the end of the origin period,
        ## we will assume that the average date of loss of the
        ## partial period is proportional to the age
        early.age <- Age.to < origin.width
        Age.to[!early.age] <- Age.to[!early.age] - adol.age
        Age.to[early.age] <- Age.to[early.age] * (1 - adol.age / origin.width)
        colnames(Triangle) <- Age.to
        maxage.used <- maxage - adol.age
        }
    else {
        if (!is.null(adol.age))
            stop("adol.age is specified but adol is FALSE")
        if (!is.null(origin.width))
            stop("origin.width is specified but adol is FALSE")
        maxage.used <- maxage
        }
    Age.from <- c(0, head(Age.to, -1L))

    # Let's scale the Triangle asap.
    # Just as Clark uses SIGMA2 to scale to model with ODP, we will scale
    #   losses by a large amount so that the scaled losses and the growth 
    #   function  parameters are in a closer relative magnitude. Otherwise, 
    #   the Fisher Information matrix may not invert.
    CurrentValue <- getLatestCumulative(if (cumulative) Triangle else incr2cum(Triangle))
    Uinit <- if (is.logical(tryCatch(checkTriangle(Triangle), error = function(e) FALSE))) CurrentValue
        else 
        if (cumulative) predict(chainladder(Triangle))[,ncol(Triangle)] 
        else predict(chainladder(incr2cum(Triangle)))[,ncol(Triangle)]
    workarea$magscale <- magscale <- max(Uinit)
    Triangle <- Triangle / magscale
    CurrentValue <- CurrentValue / magscale
    Uinit <- Uinit / magscale

    # Save age from/to's of current diagonal
    CurrentAge <- getLatestCumulative({ # as labeled, prior to adol adjustment
        z <- col(Triangle)
        z[is.na(Triangle)]<-NA
        array(dev[z], dim(Triangle))
        })
    CurrentAge.from <- getLatestCumulative(array(Age.from[z], dim(Triangle)))
    CurrentAge.used <- CurrentAge.to <- getLatestCumulative(array(Age.to[z], dim(Triangle)))

    # Turn loss matrix into incremental losses, if not already
    if (cumulative) Triangle <-cum2incr(Triangle)

    # Create the "long format" data.frame as in Table 1.1 of paper.
    Table1.1 <- as.data.frame(as.triangle(Triangle))
    Table1.1$origin <- seq.int(nr)
    Table1.1$dev <- rep(seq.int(ncol(Triangle)), each=nr)
    Table1.1$Age.from <- rep(Age.from, each=nr)
    Table1.1$Age.to <- rep(Age.to, each=nr)
    Table1.1 <- Table1.1[!is.na(Table1.1[[3L]]),]
    
    # "prime" 'workarea' with initial data
    workarea$origin   <- Table1.1$origin # origin year (index) of the observation
    workarea$io <- outer(workarea$origin, 1:nr, `==`) # T/F flag: is obs in origin year=column
    workarea$value    <- as.numeric(Table1.1$value)
    workarea$Age.from <- Table1.1$Age.from
    workarea$Age.to   <- Table1.1$Age.to
    workarea$dev      <- devs[Table1.1$dev]
    workarea$nobs     <- nobs <- nrow(Table1.1)
    workarea$np       <- np   <- nr + G@np
    rm(Table1.1)

    # Calc starting values for the parameters, call optim
    theta <- c(structure(Uinit, names=origins), G@initialGuess(workarea))
    
    # optim returns the maximum (optimal) value of LL.ODP in the 
    #   list value 'value'; call that value valopt.
    # Any value of theta that gives a value of LL.ODP(theta) that is
    #   less than valopt is a "suboptimal" solution.
    S <- optim(
        par = theta,
        LL.ODP, # function to be maximized (fmscale=-1)
        gr = dLL.ODPdt,     ## much faster with gradient function 
        MU.LDF,
        G,              
        workarea,
        method="L-BFGS-B",
        lower = c(rep(sqrt(.Machine$double.eps), nr), G@LBFGSB.lower(workarea)),
        upper = c(rep(Inf, nr), G@LBFGSB.upper(workarea)),
        control = list(
            fnscale=-1,
            parscale=c(Uinit, 1, 1),
            factr=.Machine$double.eps^-.5,
            maxit=100000
            ),
        hessian=FALSE
        )

    if (S$convergence>0) {
        msg <- "Maximum likelihood solution not found."
        if (S$convergence == 1)
            msg<-paste(msg,"Max interations (100000) reached.")
        else msg<-paste(msg,"'convergence' code=",S$convergence)
        warning(msg)
        return(NULL)
        }

    # Pull the parameters out of the solution list
    theta <- S$par
    K  <- np - G@np
    K1 <- seq.int(K)
    U <- thetaU <- theta[K1]
    thetaG <- tail(theta, G@np)
    if (any(G@LBFGSB.lower(workarea) == thetaG | G@LBFGSB.upper(workarea) == thetaG)) {
        .prn(G@LBFGSB.lower(workarea))
        .prn(thetaG)
        .prn(G@LBFGSB.upper(workarea))
        .prn(thetaG)
        warning("Solution constrained at growth function boundary! Use results with caution!\n\n")
        }
    
    # Calculate the SIGMA2 "scaling parameter"
    SIGMA2 <- workarea$SIGMA2 <- LL.ODP.SIGMA2(workarea)

    # AY DETAIL LEVEL

    # Expected value of reserves, all origin years
    R <- R.LDF(theta, G, CurrentAge.to, maxage, oy=1:nr)
    # Alternatively, by developing current diagonal
    #   For LDF formula, see table at top of p. 64
    g <- G(CurrentAge.used, thetaG)
    g.maxage <- G(maxage.used, thetaG)
    ldf <- 1 / g
    ldf.truncated <- g.maxage / g
    R.ldf.truncated <- (ldf.truncated - 1) * CurrentValue

    # PROCESS RISK OF RESERVES
    gammar2 <- R * SIGMA2
    gammar <- sqrt(gammar2)

    # PARAMETER RISK OF RESERVES

    # Calculate the Fisher Information matrix = matrix of
    #   2nd partial derivatives of the LL fcn w.r.t. all parameters
    workarea$FI <- FI <- structure(
        d2LL.ODPdt2(S$par, MU.LDF, G, workarea),
        dimnames = list(names(S$par), names(S$par))
        )

    # array to "unscale" FI
    FImult <- array(
        c(rep(c(rep(1/magscale, K), rep(1, G@np)), K), 
          rep(c(rep(1, K), rep(magscale, G@np)), G@np)),
        c(np, np)
        ) 

    # Let's see if FI will invert
    if (rcond(FI) < .Machine$double.eps) { # No
        message("Fisher Information matrix is computationally singular (condition number = ",
                format(1/rcond(FI), digits=3, scientific=TRUE), 
                ")\nParameter risk estimates not available"
                )
        # Calculate the gradient matrix, dR = matrix of 1st partial derivatives
        #   for every origin year w.r.t. every parameter
        dR <- NA
        
        # Delta Method => approx var/cov matrix of reserves
        VCOV.R <- NA
    
        # Origin year parameter risk estimates come from diagonal entries
        # Parameter risk for sum over all origin years = sum  over
        #   entire matrix (see below).
        deltar2 <- rep(NA, nr)
        deltar  <- rep(NA, nr)
    
        # Total Risk = process risk + parameter risk
        totalr2 <- rep(NA, nr)
        totalr  <- rep(NA, nr)
    
        # AY Total row
        CurrentValue.sum <- sum(CurrentValue)
        R.sum <- sum(R)
        R.ldf.truncated.sum <- sum(R.ldf.truncated)
        gammar2.sum <- sum(gammar2)
        gammar.sum = sqrt(gammar2.sum)
        deltar2.sum <- NA
        deltar.sum <- NA
        totalr2.sum <- NA
        totalr.sum = NA
        }
    else {
        # Calculate the gradient matrix, dR = matrix of 1st partial derivatives
        #   for every origin year w.r.t. every parameter
        dR <- dfdx(R.LDF, theta, G, CurrentAge.to, maxage.used, oy=1:nr)
        
        # Delta Method => approx var/cov matrix of reserves
        VCOV.R <- -workarea$SIGMA2 * t(dR) %*% solve(FI, dR)

        # Origin year parameter risk estimates come from diagonal entries
        # Parameter risk for sum over all origin years = sum  over
        #   entire matrix (see below).
        deltar2 <- diag(VCOV.R)
        # Hessian only approximates the covariance matrix; no guarantee
        #   that the matrix is positive (semi)definite.
        # If find negative diagonal variances, set them to zero, 
        #   issue a warning.
        ndx <- deltar2<0
        if (any(ndx)) {
            if (sum(ndx)>1L) msg <- "The parameter risk approximation produced 'negative variances' for the following origin years (values set to zero):\n"
            else msg <- "The parameter risk approximation produced a 'negative variance' for the following origin year (value set to zero):\n"
            df2 <- data.frame(
                Origin = origins[ndx], 
                Reserve = R.ldf.truncated[ndx] * magscale, 
                ApproxVar = deltar2[ndx] * magscale^2, 
                RelativeVar = deltar2[ndx] * magscale / R.ldf.truncated[ndx]
                )
            df2 <- format(
                rbind(
                    data.frame(Origin="Origin",Reserve="Reserve", ApproxVar="ApproxVar", RelativeVar="RelativeVar"),
                    format(df2, big.mark=",", digits=3)
                    ),
                justify="right")
            msg <- c(msg, apply(df2, 1, function(x) paste(paste(x, collapse=" "), "\n")))
            warning(msg)
            deltar2[ndx] <- 0 
            }
        deltar  <- sqrt(deltar2)
    
        # Total Risk = process risk + parameter risk
        totalr2 <- gammar2 + deltar2
        totalr  <- sqrt(totalr2)
    
        # AY Total row
        CurrentValue.sum <- sum(CurrentValue)
        R.sum <- sum(R)
        R.ldf.truncated.sum <- sum(R.ldf.truncated)
        gammar2.sum <- sum(gammar2)
        gammar.sum = sqrt(gammar2.sum)
        deltar2.sum <- sum(VCOV.R)
        if (deltar2.sum<0) {
            msg <- "The parameter risk approximation produced a 'negative variance' for the Total row (value set to zero):\n"
            df2 <- data.frame(
                Reserve = R.ldf.truncated.sum * magscale, 
                ApproxVar = deltar2.sum * magscale^2, 
                RelativeVar = deltar2.sum * magscale / R.ldf.truncated.sum
                )
            df2 <- format(
                rbind(
                    data.frame(Reserve="Reserve", ApproxVar="ApproxVar", RelativeVar="RelativeVar"),
                    format(df2, big.mark=",", digits=3)
                    ),
                justify="right")
            msg <- c(msg, apply(df2, 1, function(x) paste(paste(x, collapse=" "), "\n")))
            warning(msg)
            deltar2.sum <- 0 
            }
        deltar.sum <- sqrt(deltar2.sum)
        totalr2.sum <- gammar2.sum + deltar2.sum
        totalr.sum = sqrt(totalr2.sum)
        }

    # KEY: Mixed case: origin year (row level) amounts
    #      all-lower-case: workarea (observation level) amounts
    #      all-upper-case: model parameters
    structure(
        list(
            method = "LDF",
            growthFunctionName = G@name,
            Origin = origins,
            CurrentValue = CurrentValue * magscale,
            CurrentAge = CurrentAge,
            CurrentAge.used = CurrentAge.used,
            MAXAGE = maxage,
            MAXAGE.USED = maxage.used,
            FutureValue = R.ldf.truncated * magscale,
            UltimateValue = (CurrentValue + R.ldf.truncated) * magscale,
            ProcessSE = gammar * magscale,
            ParameterSE = deltar * magscale,
            StdError = totalr * magscale,
            Total = list(
                Origin = "Total",
                CurrentValue = CurrentValue.sum * magscale,
                FutureValue = R.ldf.truncated.sum * magscale,
                UltimateValue = (CurrentValue.sum + R.ldf.truncated.sum) * magscale,
                ProcessSE = gammar.sum * magscale,
                ParameterSE = deltar.sum * magscale,
                StdError = totalr.sum * magscale
                ),
            PAR = c(unclass(S$par)) * c(rep(magscale, K), rep(1, G@np)),
            THETAU = thetaU * magscale,
            THETAG = thetaG,
            GrowthFunction = g,
            GrowthFunctionMAXAGE = g.maxage,
            SIGMA2 = c(unclass(SIGMA2)) * magscale,
            Ldf = ldf,
            LdfMAXAGE = 1/g.maxage,
            TruncatedLdf = ldf.truncated,
            FutureValueGradient = dR * c(rep(1, K), rep(magscale, G@np)),
            origin = workarea$origin,
            age = workarea$dev,
            fitted = workarea$mu * magscale,
            residuals = workarea$residuals * magscale,
            stdresid = workarea$residuals/sqrt(SIGMA2*workarea$mu),
            FI = FI * FImult,
            value = S$value,
            counts = S$counts
            ),
        class=c("ClarkLDF", "clark", "list")
        )
    }

ClarkCapeCod <- function(Triangle,
        Premium,
        cumulative = TRUE,
        maxage = Inf,
        adol = TRUE,
        adol.age = NULL,
        origin.width = NULL,
        G = "loglogistic"
        ) {
    # maxage represents the age to which losses are projected at Ultimate
    # adol.age is the age within an origin period of the 
    #   average date of loss (adol.age); only relevant if adol=TRUE
    # origin.width is the width of an origin period; only relevant if adol=TRUE
        
    if (!is.character(G))
        stop("Growth function G must be the character name of a growth function")
    if (length(G)>1L)
        stop("Only one growth function can be specified")
    G <- switch(G,
         loglogistic = loglogistic,
         weibull = weibull,
         stop(paste("Growth function '", G, "' unrecognized", sep=""))
         )
        
    if (!is.matrix(Triangle)) stop("ClarkCapeCod expects Triangle in matrix format")
    nr <- nrow(Triangle)
    if (ncol(Triangle) < 4L) stop("matrix must have at least 4 columns")

    # Recycle Premium, limit length, as necessary
    Premium <- c(Premium)
    if (length(Premium) == 1L) Premium <- rep(Premium, nr)
    else
    if (length(Premium)!=nr) {
        warning('Mismatch between length(Premium)=', length(Premium), ' and nrow(Triangle)=', nrow(Triangle), '. Check results!')
        if (length(Premium) < nr) Premium <- rep(Premium, nr)
        if (length(Premium) > nr) Premium <- Premium[seq.int(nr)]
        }

    dev <- as.numeric(colnames(Triangle))
    if (length(dev)<1 | any(is.na(dev))) stop("non-'age' column name(s)")
    if (any(dev[-1L]<=head(dev, -1L))) stop("ages must be strictly increasing")
    if (tail(dev, 1L) > maxage[1L]) stop("'maxage' must not be less than last age in triangle")
    
    # 'workarea' stores intermediate calculations
#    workarea <- new.env()
    workarea <<- new.env()

    if (!inherits(Triangle, "triangle")) Triangle <- as.triangle(Triangle)

    # Save the origin, dev names
    origins <- rownames(Triangle)
    devs <- colnames(Triangle)
    # Save user's names for 'origin' (row) and 'dev' (column), if any
    dimnms <- c("origin", "dev")
    if (!is.null(nm<-names(dimnames(Triangle)))) 
        # If only one name specified by user, other will be NA
        dimnms[!is.na(nm)] <- nm[!is.na(nm)]

    # Calculate the age.from/to's and maxage based on adol setting.
    Age.to <- dev
    if (adol) {
        if (is.null(origin.width)) {
            agediff <- diff(Age.to)
            if (!all(abs(agediff-agediff[1L])<sqrt(.Machine$double.eps))) 
                warning("origin.width unspecified, but varying age differences; check reasonableness of 'Table64$AgeUsed'")
            origin.width <-  mean(agediff)
            }
        if (is.null(adol.age)) # default is half width of origin period
            adol.age <- origin.width / 2
        # rudimentary reasonableness checks of adol.age
        if (adol.age < 0) 
            stop("age of average date of loss cannot be negative")
        if (adol.age >= origin.width) 
            stop("age of average date of loss must be < origin.width (ie, within origin period)")
        ## For all those ages that are before the end of the origin period,
        ## we will assume that the average date of loss of the
        ## partial period is proportional to the age
        early.age <- Age.to < origin.width
        Age.to[!early.age] <- Age.to[!early.age] - adol.age
        Age.to[early.age] <- Age.to[early.age] * (1 - adol.age / origin.width)
        colnames(Triangle) <- Age.to
        maxage.used <- maxage - adol.age
        }
    else {
        if (!is.null(adol.age))
            stop("adol.age is specified but adol is FALSE")
        if (!is.null(origin.width))
            stop("origin.width is specified but adol is FALSE")
        maxage.used <- maxage
        }
    Age.from <- c(0, head(Age.to, -1L))

    # Triangle does not need to be scaled for Cape Cod method.
    CurrentValue <- getLatestCumulative(if (cumulative) Triangle else incr2cum(Triangle))

    ELRinit <- if (is.logical(tryCatch(checkTriangle(Triangle), error = function(e) FALSE))) 1.000
        else sum(if (cumulative) predict(chainladder(Triangle))[,ncol(Triangle)] 
                 else predict(chainladder(incr2cum(Triangle)))[,ncol(Triangle)]) / sum(Premium)



    # Save age from/to's of current diagonal
    CurrentAge <- getLatestCumulative({
        z <- col(Triangle)
        z[is.na(Triangle)]<-NA
        array(dev[z], dim(Triangle))
        })
    CurrentAge.from <- getLatestCumulative(array(Age.from[z], dim(Triangle)))
    CurrentAge.used <- CurrentAge.to <- getLatestCumulative(array(Age.to[z], dim(Triangle)))

    # Turn loss matrix into incremental losses, if not already
    if (cumulative) Triangle <-cum2incr(Triangle)

    # Create the "long format" data.frame as in Table 1.1 of paper.
    Table1.1 <- as.data.frame(as.triangle(Triangle))
    Table1.1$origin <- seq.int(nr)
    Table1.1$P   <- rep(Premium, ncol(Triangle))
    Table1.1$dev <- rep(seq.int(ncol(Triangle)), each=nr)
    Table1.1$Age.from <- rep(Age.from, each=nr)
    Table1.1$Age.to <- rep(Age.to, each=nr)
    Table1.1 <- Table1.1[!is.na(Table1.1[[3L]]),]
    
    # "prime" workarea with initial data
    workarea$origin   <- Table1.1$origin # origin year (index) of the observation
#    workarea$io <- outer(workarea$origin, 1:nr, `==`) # not used for CC, but leave in anyway
    workarea$value    <- as.numeric(Table1.1$value)
    workarea$P        <- Table1.1$P
    workarea$Age.from <- Table1.1$Age.from
    workarea$Age.to   <- Table1.1$Age.to
    workarea$dev      <- devs[Table1.1$dev]
    workarea$nobs     <- nobs <- nrow(Table1.1)
    workarea$np       <- np   <- 1L + G@np
    rm(Table1.1)

    # Calc starting values for the parameters, call optim
    theta <- c(ELR=ELRinit, G@initialGuess(workarea))
    S <- optim(
        par = theta,
        LL.ODP, # function to be maximized (fmscale=-1)
        gr = dLL.ODPdt,     ## much faster with gradient function 
        MU.CapeCod,
        G,              
        workarea,
        method="L-BFGS-B",
        lower = c(sqrt(.Machine$double.eps), G@LBFGSB.lower(workarea)),
        upper = c(10, G@LBFGSB.upper(workarea)),
        control = list(
            fnscale=-1,
            factr=.Machine$double.eps^-.5,
            maxit=100000
            ),
        hessian=FALSE
        )
    if (S$convergence>0) {
        msg <- "Maximum likelihood solution not found."
        if (S$convergence == 1)
            msg<-paste(msg,"Max interations (100000) reached.")
        else msg<-paste(msg,"'convergence' code=",S$convergence)
        warning(msg)
        return(NULL)
        }

    # Pull the parameters out of the solution list
    theta <- S$par
    K  <- np - G@np
    K1 <- seq.int(K)
    ELR <- theta[1L]
    thetaG <- tail(theta, G@np)
    if (any(G@LBFGSB.lower(workarea) == thetaG | G@LBFGSB.upper(workarea) == thetaG)) 
        warning("Solution constrained at growth function boundary! Use results with caution!\n\n")
    
    # Calculate the SIGMA2 "scaling parameter"
    SIGMA2 <- workarea$SIGMA2 <- LL.ODP.SIGMA2(workarea)

    # AY DETAIL LEVEL

    # Expected value of reserves
    R <- R.CapeCod(theta, Premium, G, CurrentAge.used, maxage.used)#, workarea)
    g <- G(CurrentAge.used, thetaG)
    g.maxage <- G(maxage.used, thetaG)
    ldf <- 1 / g
    ldf.truncated <- g.maxage / g

    # PROCESS RISK OF RESERVES
    gammar2 <- R * SIGMA2
    gammar <- sqrt(gammar2)

    # PARAMETER RISK OF RESERVES

    # Calculate the Fisher Information matrix = matrix of
    #   2nd partial derivatives of the LL fcn w.r.t. all parameters
    workarea$FI <- FI <- structure(
        d2LL.ODPdt2(S$par, MU.CapeCod, G, workarea),
        dimnames = list(names(S$par), names(S$par))
        )

    # Let's see if FI will invert
    if (rcond(FI) < .Machine$double.eps) { # No
        message("Fisher Information matrix is computationally singular (condition number = ",
                format(1/rcond(FI), digits=3, scientific=TRUE), 
                ")\nParameter risk estimates not available"
                )
        # Calculate the gradient matrix, dR = matrix of 1st partial derivatives
        #   for every origin year w.r.t. every parameter
        dR <- NA
        
        # Delta Method => approx var/cov matrix of reserves
        VCOV.R <- NA
    
        # Origin year parameter risk estimates come from diagonal entries
        # Parameter risk for sum over all origin years = sum  over
        #   entire matrix (see below).
        deltar2 <- rep(NA, nr)
        deltar  <- rep(NA, nr)
    
        # Total Risk = process risk + parameter risk
        totalr2 <- rep(NA, nr)
        totalr  <- rep(NA, nr)
    
        # AY Total row
        CurrentValue.sum <- sum(CurrentValue)
        Premium.sum <- sum(Premium)
        R.sum <- sum(R)
        gammar2.sum <- sum(gammar2)
        gammar.sum = sqrt(gammar2.sum)
        deltar2.sum <- NA
        deltar.sum <- NA
        totalr2.sum <- NA
        totalr.sum = NA
        }
    else {
        # Calculate the gradient matrix, dR = matrix of 1st partial derivatives
        #   for every origin year w.r.t. every parameter
        dR <- dfdx(R.CapeCod, theta, Premium, G, CurrentAge.to, maxage.used)#, workarea)
        
        # Delta Method => approx var/cov matrix of reserves
        VCOV.R <- -workarea$SIGMA2 * t(dR) %*% solve(FI, dR)
    
        # Origin year parameter risk estimates come from diagonal entries
        # Parameter risk for sum over all origin years = sum  over
        #   entire matrix (see below).
        deltar2 <- diag(VCOV.R)
        # Hessian only approximates the covariance matrix; no guarantee
        #   that the matrix is positive (semi)definite.
        # If find negative diagonal variances, set them to zero, 
        #   issue a warning.
        ndx <- deltar2<0
        if (any(ndx)) {
            if (sum(ndx)>1L) msg <- "The parameter risk approximation produced 'negative variances' for the following origin years (values set to zero):\n"
            else msg <- "The parameter risk approximation produced a 'negative variance' for the following origin year (value set to zero):\n"
            df2 <- data.frame(
                Origin = origins[ndx], 
                Reserve = R[ndx],# * magscale, 
                ApproxVar = deltar2[ndx],# * magscale^2, 
                RelativeVar = deltar2[ndx] / R[ndx]# * magscale / R[ndx]
                )
            df2 <- format(
                rbind(
                    data.frame(Origin="Origin",Reserve="Reserve", ApproxVar="ApproxVar", RelativeVar="RelativeVar"),
                    format(df2, big.mark=",", digits=3)
                    ),
                justify="right")
            msg <- c(msg, apply(df2, 1, function(x) paste(paste(x, collapse=" "), "\n")))
            warning(msg)
            deltar2[ndx] <- 0 
            }
        deltar  <- sqrt(deltar2)
    
        # Total Risk = process risk + parameter risk
        totalr2 <- gammar2 + deltar2
        totalr  <- sqrt(totalr2)
    
        # AY Total row
        CurrentValue.sum <- sum(CurrentValue)
        Premium.sum <- sum(Premium)
        R.sum <- sum(R)
        gammar2.sum <- sum(gammar2)
        gammar.sum = sqrt(gammar2.sum)
        deltar2.sum <- sum(VCOV.R)
        if (deltar2.sum<0) {
            msg <- "The parameter risk approximation produced a 'negative variance' for the Total row (value set to zero):\n"
            df2 <- data.frame(
                Reserve = R.sum,# * magscale, 
                ApproxVar = deltar2.sum,# * magscale^2, 
                RelativeVar = deltar2.sum / R.sum# * magscale / R.sum
                )
            df2 <- format(
                rbind(
                    data.frame(Reserve="Reserve", ApproxVar="ApproxVar", RelativeVar="RelativeVar"),
                    format(df2, big.mark=",", digits=3)
                    ),
                justify="right")
            msg <- c(msg, apply(df2, 1, function(x) paste(paste(x, collapse=" "), "\n")))
            warning(msg)
            deltar2.sum <- 0 
            }
        deltar.sum <- sqrt(deltar2.sum)
        totalr2.sum <- gammar2.sum + deltar2.sum
        totalr.sum = sqrt(totalr2.sum)
        }

    # KEY: Mixed case: origin year (row level) amounts
    #      all-lower-case: workarea (observation level) amounts
    structure(
        list(
            method = "CapeCod",
            growthFunctionName = G@name,
            Origin = origins,
            Premium = Premium,
            CurrentValue = CurrentValue,
            CurrentAge = CurrentAge,
            CurrentAge.used = CurrentAge.used,
            MAXAGE = maxage,
            MAXAGE.USED = maxage.used,
            FutureValue = R,
            UltimateValue = CurrentValue + R,
            ProcessSE = gammar,
            ParameterSE = deltar,
            StdError = totalr,
            Total = list(
                Origin = "Total",
                Premium = Premium.sum,
                CurrentValue = CurrentValue.sum,
                FutureValue = R.sum,
                UltimateValue = CurrentValue.sum + R.sum,
                ProcessSE = gammar.sum,
                ParameterSE = deltar.sum,
                StdError = totalr.sum
                ),
            PAR=c(unclass(S$par)),
            ELR = ELR,
            THETAG = thetaG,
            GrowthFunction = g,
            GrowthFunctionMAXAGE = g.maxage,
            FutureGrowthFactor = g.maxage - g,
            SIGMA2=c(unclass(SIGMA2)),# * magscale,
            Ldf = ldf,
            LdfMAXAGE = 1/g.maxage,
            TruncatedLdf = ldf.truncated,
            FutureValueGradient = dR,
            origin = workarea$origin,
            age = workarea$dev,
            fitted = workarea$mu,# * magscale,
            residuals = workarea$residuals,# * magscale,
            stdresid = workarea$residuals/sqrt(SIGMA2*workarea$mu),
            FI = FI,
            value = S$value,
            counts = S$counts
            ),
        class=c("ClarkCapeCod", "clark", "list")
        )

    }

summary.ClarkLDF <- function(object, ...) {
    origin <- c(object$Origin, object$Total$Origin)
    data.frame(
        Origin = origin,
        CurrentValue = c(object$CurrentValue, object$Total$CurrentValue),
        Ldf = c(object$TruncatedLdf, NA),
        UltimateValue = c(object$UltimateValue, object$Total$UltimateValue),
        FutureValue = c(object$FutureValue, object$Total$FutureValue),
        StdError = c(object$StdError, object$Total$StdError),
        CV = c(object$StdError, object$Total$StdError) / c(object$FutureValue, object$Total$FutureValue),
        row.names = origin,
        stringsAsFactors = FALSE
        )
    }

print.ClarkLDF <- function(x, Amountdigits=0, LDFdigits=3, CVdigits=3, row.names=FALSE, ...) {
    y <- summary(x)
    z <- structure(data.frame(y[1], 
        format(round(y[[2]], Amountdigits), big.mark=","),
        Ldf = c(head(format(round(y[[3]], LDFdigits)), -1), ""), 
        format(round(y[[4]], Amountdigits), big.mark=","), 
        format(round(y[[5]], Amountdigits), big.mark=","), 
        format(round(y[[6]], Amountdigits), big.mark=","), 
        formatC(100*round(y[[7]], CVdigits), format="f", digits=CVdigits-2),
        stringsAsFactors = FALSE),
        names = c(names(y)[1:6], "CV%")
        )
    print(z, row.names = row.names, ...)
    }

summary.ClarkCapeCod <- function(object, ...) {
    origin <- c(object$Origin, object$Total$Origin)
    data.frame(
        Origin = origin,
        CurrentValue = c(object$CurrentValue, object$Total$CurrentValue),
        Premium = c(object$Premium, object$Total$Premium),
        ELR = c(rep(object$ELR, length(object$Premium)), NA),
        FutureGrowthFactor = c(object$FutureGrowthFactor, NA),
        FutureValue = c(object$FutureValue, object$Total$FutureValue),
        UltimateValue = c(object$UltimateValue, object$Total$UltimateValue),
        StdError = c(object$StdError, object$Total$StdError),
        CV = c(object$StdError, object$Total$StdError) / c(object$FutureValue, object$Total$FutureValue),
        row.names = origin,
        stringsAsFactors = FALSE
        )
    }

print.ClarkCapeCod <- function(x, Amountdigits=0, ELRdigits=3, Gdigits=4, CVdigits=3, row.names=FALSE, ...) {
    y <- summary(x)
    z <- structure(data.frame(y[1], 
        format(round(y[[2]], Amountdigits), big.mark=",", scientific=FALSE), 
        format(round(y[[3]], Amountdigits), big.mark=",", scientific=FALSE), 
        c(head(formatC(round(y[[4]], ELRdigits), digits=ELRdigits, format="f"), -1), ""), 
        c(head(formatC(round(y[[5]],   Gdigits), digits=  Gdigits, format="f"), -1), ""), 
        format(round(y[[6]], Amountdigits), big.mark=","), 
        format(round(y[[7]], Amountdigits), big.mark=","), 
        format(round(y[[8]], Amountdigits), big.mark=","), 
        formatC(100*round(y[[9]], CVdigits), format="f", digits=CVdigits-2),
        stringsAsFactors = FALSE),
        names = c(names(y)[1:8], "CV%")
        )
    print(z, row.names = FALSE, ...)
    }

plot.clark <- function(x, ...) {
    # We will plot the residuals as functions of
    #   1. origin
    #   2. age (at end of development period)
    #   3. fitted value (observe heteroscedasticity)
    # The y-values of the plots are the x$stdresid's
    # 4th plot shows results of normality test
    op <- par(mfrow=c(2,2),         # 4 plots on one page
              oma = c(0, 0, 5, 0))  # outer margins necessary for page title
    #
    plot(x$origin,
        x$stdresid,ylab="standardized residuals",
        xlab="Origin",
        main="By Origin")
    z <- lm(x$stdresid~x$origin)
    abline(z, col="blue")
    #
    plot(x$age,
        x$stdresid, 
        xlab="Age",
        ylab="standardized residuals",
        main="By Projected Age")
    age <- as.numeric(x$age)
    z <- lm(x$stdresid~age)
    abline(z, col="blue")
    #
    plot(x$fitted,
        x$stdresid,ylab="standardized residuals",
        xlab="Expected Value",
        main="By Fitted Value")
    z <- lm(x$stdresid~x$fitted)
    abline(z, col="blue")
    # Normality test
    ymin <- min(x$stdresid)
    ymax <- max(x$stdresid)
    yrange <- ymax - ymin
    sprd <- yrange/10
    xmin<-min(qqnorm(x$stdresid)$x)
    qqline(x$stdresid, col="blue")
    N <- min(length(x$stdresid),5000) # 5000=shapiro.test max sample size
    shap.p <- shapiro.test(sample(x$stdresid,N))
    shap.p.value <- round(shap.p$p.value,5)
    text(xmin, ymax - sprd, paste("Shapiro-Wilk p.value = ", shap.p.value, ".", sep=""), cex=.7, adj=c(0,0))
#    text(xmin, ymax - 2*sprd, paste(ifelse(shap.p.value<.05,"Should reject",
#                                              "Cannot reject"),
#                      "ODP hypothesis"), cex=.7, adj=c(0,0))
#    text(xmin, ymax - 3*sprd,expression(paste("at ",alpha," = .05 level.",sep="")), cex=.7, adj=c(0,0))

    # Finally, the overall title of the page of plots    
    mtext(
        paste(
            "Standardized Residuals\nMethod: ", 
            paste("Clark", x$method, sep=""), 
            "; Growth function: ", 
            x$growthFunction,
            sep=""), 
        outer = TRUE, 
        cex = 1.5
        )
    par(op)
    }

vcov.clark <- function(object, ...) {
    if (rcond(object$FI)<.Machine$double.eps) { # Fisher Information matrix will not invert
        message("Fisher Information matrix is computationally singular (condition number = ",
                format(1/rcond(object$FI), digits=3, scientific=TRUE), 
                ")\nCovariance matrix is not available."
                )
        return(NA)
        }
    -object$SIGMA2*solve(object$FI)
    }

Table65 <- function(x) { # Form "report table" as on p. 65 of paper
    data.frame(
        Origin = c(x$Origin, x$Total$Origin),
        CurrentValue = c(x$CurrentValue, x$Total$CurrentValue),
        FutureValue = c(x$FutureValue, x$Total$FutureValue),
        ProcessSE = c(x$ProcessSE, x$Total$ProcessSE),
        ProcessCV = 100 * round(c(x$ProcessSE, x$Total$ProcessSE) / c(x$FutureValue, x$Total$FutureValue), 3),
        ParameterSE = c(x$ParameterSE, x$Total$ParameterSE),
        ParameterCV = 100 * round(c(x$ParameterSE, x$Total$ParameterSE) / c(x$FutureValue, x$Total$FutureValue), 3),
        StdError = c(x$StdError, x$Total$StdError),
        TotalCV = 100 * round(c(x$StdError, x$Total$StdError) / c(x$FutureValue, x$Total$FutureValue), 3),
        stringsAsFactors = FALSE
        )
    }
    
Table64 <- function(x) {
    if (!inherits(x, "ClarkLDF")) {
        warning(sQuote(deparse(substitute(x))), " is not a ClarkLDF model.")
        return(NULL)
        }
    data.frame(
        Origin = c("", x$Origin, x$Total$Origin),
        CurrentValue = c(NA, x$CurrentValue, x$Total$CurrentValue),
        CurrentAge = c(x$MAXAGE, x$CurrentAge, NA),
        AgeUsed = c(x$MAXAGE.USED, x$CurrentAge.used, NA),
        GrowthFunction = c(x$GrowthFunctionMAXAGE, x$GrowthFunction, NA),
        Ldf = c(x$LdfMAXAGE, x$Ldf, NA),
        TruncatedLdf = c(1.0, x$TruncatedLdf, NA),
        LossesAtMaxage = c(NA, x$CurrentValue + x$FutureValue, x$Total$CurrentValue + x$Total$FutureValue),
        FutureValue = c(NA, x$FutureValue, x$Total$FutureValue),
        stringsAsFactors = FALSE
        )
    }

Table68 <- function(x) {
    if (!inherits(x, "ClarkCapeCod")) {
        warning(sQuote(deparse(substitute(x))), " is not a ClarkCapeCod model.")
        return(NULL)
        }
    data.frame(
        Origin = c("", x$Origin, x$Total$Origin),
        Premium = c(NA, x$Premium, x$Total$Premium),
        CurrentAge = c(x$MAXAGE, x$CurrentAge, NA),
        AgeUsed = c(x$MAXAGE.USED, x$CurrentAge.used, NA),
        GrowthFunction = c(x$GrowthFunctionMAXAGE, x$GrowthFunction, NA),
        FutureGrowthFactor = x$GrowthFunctionMAXAGE - c(x$GrowthFunctionMAXAGE, x$GrowthFunction, NA),
        PremiumxELR = x$ELR * c(NA, x$Premium, x$Total$Premium),
        FutureValue = c(NA, x$FutureValue, x$Total$FutureValue),
        stringsAsFactors = FALSE
        )
    }

# FUNCTIONS AND FUNCTION CLASSES

# Function Classes

# FUNCTION CLASS WITH DERIVATIVES

# First, a function-or-NULL virtual class
setClassUnion("funcNull", c("function","NULL"))

# Functions stemming from this class have one argument, x
setClass("dfunction", 
    # By issuing the name of the instance of this class, you will get
    #   the function as defined when the instance was created.
    contains = "function",
    representation = representation(
        # partial derivative function, vector of length = length(x)
        dfdx = "funcNull",
        # 2nd partial derivative function, vector of length = length(x)
        d2fdx2 = "funcNull"
        )
    )

# Generics to return the 1st & 2nd partial derivatives 
setGeneric("dfdx", function(f, ...) standardGeneric("dfdx"))
setMethod("dfdx", "dfunction", function(f, ...) f@dfdx(...))
setGeneric("d2fdx2", function(f, ...) standardGeneric("d2fdx2"))
setMethod("d2fdx2", "dfunction", function(f, ...) f@d2fdx2(...))

# Generic "change-in-function" method
setGeneric("del", function(f, from, to, ...) standardGeneric("del"))
setMethod("del", "function", function(f, from, to, ...) f(to, ...) - f(from, ...))

# GROWTH FUNCTION CLASS

# Functions stemming from this class have two arguments:
#   x = age/lag/time at which the function will be evaluated
#   theta = vector of parameters
# Growth function's derivatives will be with respect to 2nd argument --
#   't' stands for 'theta' = vector of parameters -- which is different
#   from dfunction class whose derivatives are w.r.t. 1st argument
setClass("GrowthFunction", 
    # By issuing the name of the instance of this class, you will get
    #   the function as defined when the instance was created.
    contains = "function",
    representation = representation(
        name = "character", 
        # number of parameters (length of theta)
        np = "integer", 
        # function to return initial parameters before running optim
        initialGuess = "funcNull", 
        # function to return lower "L-BFGS-B" constraint
        LBFGSB.lower = "funcNull", 
        # function to return upper "L-BFGS-B" constraint
        LBFGSB.upper = "funcNull", 
        # partial derivative function, vector of length np
        dGdt = "funcNull",
        # 2nd partial derivative function, np x np matrix
        d2Gdt2 = "funcNull"
        )
    )

# LOGLOGISTIC FUNCTION

G.loglogistic <- function(x, theta) {
    if (any(theta <= 0)) return(rep(0, length(x)))
    pllogis(x, shape = theta[1], scale = theta[2])
    }

dG.loglogisticdtheta <- function(x, theta) {
    if (any(theta <= 0)) return(
        if (length(x) > 1L) array(0, dim = c(2L, length(x)))
        else numeric(2L)
        )
    y <- pllogis(x, shape = theta[1], scale = theta[2])
    y1y <- y * (1-y)
    logxtheta <- log(x/theta[2])
    dy <- structure(
        cbind(
            y1y * logxtheta,
            -y1y * theta[1] / theta[2]
            ),
        dimnames = list(names(x), names(theta))
        )
    dy[is.na(dy)] <- 0
    dy
    }

d2G.loglogisticdtheta2 <- function(x, theta) {
    if (any(theta <= 0)) return(
        array(0, dim = if (length(x) > 1L) c(length(x), 4L) else c(2L, 2L))
        )
    y <- pllogis(x, shape = theta[1], scale = theta[2])
    y1y <- y * (1-y)
    oneMinus2y <- 1 - 2 * y
    logxtheta <- log(x/theta[2])
    dyomega <- y1y * logxtheta
    A <- dyomega * logxtheta * oneMinus2y
    B <- -y1y / theta[2] * (1 + theta[1] * logxtheta * oneMinus2y)
    C <- y1y * theta[1] / theta[2]^2 * (1 + theta[1] * oneMinus2y)
    d2y <- if (length(x)>1L) structure(cbind(A, B, B, C), dimnames = list(
                names(x),
                outer(names(theta), names(theta), paste, sep=":")))
           else array(c(A, B, B, C), dim = c(2L, 2L), dimnames = list(
                names(theta), names(theta)))
    d2y[is.na(d2y)] <- 0
    d2y
    }

loglogistic <- new("GrowthFunction", 
    G.loglogistic,
    name = "loglogistic",
    np = 2L,
    initialGuess = function(env) c(
        omega = 2, 
        theta = median(env$Age.to, na.rm=TRUE)
        ),
    LBFGSB.lower = function(env) c(
        omega = .01,
        theta = min(c(.5, env$Age.to))
        ),
    LBFGSB.upper = function(env) c(
        omega = Inf,
        theta = Inf
        ),
    dGdt = dG.loglogisticdtheta,
    d2Gdt2 = d2G.loglogisticdtheta2
    )

# WEIBULL FUNCTION

G.weibull <- function(x, theta) {
    if (any(theta <= 0)) return(rep(0, length(x)))
    om <- unname(theta[1L])
    th <- unname(theta[2L])
    y <- 1 - exp(-(x/th)^om)
    y[is.na(y)] <- 0
    y
    }

dG.weibulldtheta <- function(x, theta) {
    if (any(theta <= 0)) return(
        if (length(x) > 1L) array(0, dim = c(2L, length(x)))
        else numeric(2L)
        )
    om <- theta[1L]
    th <- theta[2L]
    xth <- x / th
    u   <- xth^om
    # dydom <- exp(-u) * u * log(u) / om
    # dydth <- -exp(-u) * u * om / th
    v <- exp(-u) * u
    dydom <- v * log(xth)
    dydth <- -v * om / th
    # Sandwich columns together.
    dtheta <- cbind(dydom, dydth)
    # If x <= 0, Inf, derivative = 0 by definition  (log returns NA)
    dtheta[is.na(dtheta)] <- 0
    dtheta
    }

d2G.weibulldtheta2 <- function(x, theta) {
    if (any(theta <= 0)) return(
        array(0, dim = if (length(x) > 1L) c(length(x), 4L) else c(2L, 2L))
        )
    om <- theta[1L]
    th <- theta[2L]
    xth  <- x/th
    u    <- xth^om
    logu <- log(u)
    u1   <- 1 - u
    v    <- exp(-u) * u
    dydom <- v * log(xth)
    dydthpositive <- v * om / th

    d2ydom2 <- 2 * dydom * u1
    d2ydth2 <- dydthpositive * (1 + om * u1) / th
    d2ydomdth <- -v * (1 + logu * u1) / th

    ndx <- x<=0 | is.infinite(x)
    d2ydom2[ndx] <- 0
    d2ydth2[ndx] <- 0
    d2ydomdth[ndx] <- 0
    
    if (length(x)>1L)
        # Create a matrix where each column holds an observation's
        #   d2 matrix "stretched out" into a vector of length 4
        cbind(d2ydom2, d2ydomdth, d2ydomdth, d2ydth2)
    else array(
        c(d2ydom2, d2ydomdth, d2ydomdth, d2ydth2),
        dim = c(2L, 2L),
        dimnames=list(names(theta), names(theta))
        )
    }

weibull <- new("GrowthFunction", 
    G.weibull,
    name = "weibull",
    np = 2L,
    initialGuess = function(env) c(
        omega = (om<-1.5),
        theta = max(env$Age.to, na.rm=TRUE) * (log(1/.05))^(-1/om) # 95% developed at current max age
        ),
    LBFGSB.lower = function(env) c(
        omega = .01,                     # a small number -- must be positive
        theta = sqrt(.Machine$double.eps)# a small number -- must be positive
        ),
    LBFGSB.upper = function(env) c(
        omega = 2,# 8 -- too high an exponent can lead to overflows
        theta = 2 * max(env$Age.to)
        ),
    dGdt = dG.weibulldtheta,
    d2Gdt2 = d2G.weibulldtheta2
    )

# LOGLIKELIHOOD FUNCTION UNDER ODP ASSUMPTION

LL.ODP <- function(theta, MU, G, workarea) {
    # Calculate the expected value of all observations, store in workarea.
    MU(theta, G, workarea)
    if (any(workarea$mu < 0)) { # should not happen if G's formed correctly
        #.prn(theta)
        #.prn(workarea$mu)
        msg <- c("Maximum likelihood search failure!\n",
                "Search area bounds need to be tailored to this growth function and data.\n",
                paste("Growth function parameters at point of failure: ",
                    paste(tail(theta, G@np), collapse=", "),
                    ".\n",
                    sep=""
                    ),
                paste("Ages: ",
                    paste(unique(workarea$Age.to), collapse=", "),
                    ".\n",
                    sep=""
                    ),
                "Contact package author."
                )
        stop(msg)
        }
    # It would not be unreasonable for a G to generate zero expected losses.
    # Since log(0)=Inf but optim must have finite values to work with, set
    #   mu to be a very small number. 
    # Also, a faster way to test for zero would be on the unique ages
    #   rather than on all the ages in workarea.
    workarea$mu[workarea$mu < .Machine$double.eps] <- .Machine$double.eps
    # Do ODP-model calc for all observations, add them up.
    sum(workarea$value * log(workarea$mu) - workarea$mu)
    }

dLL.ODPdt <- function(theta, MU, G, workarea) {
    # Calculate the gradient for all observations, store in workarea.
    # Create workarea$dmudt
    dfdx(MU, theta, G, workarea)
    # ... and an intermediate value used in second derivative
    workarea$cmuminus1 <- workarea$value/workarea$mu-1
    # Return a vector of column sums
    colSums(workarea$cmuminus1 * workarea$dmudt)  
    }

d2LL.ODPdt2 <- function(theta, MU, G, workarea) {
    # Calculate all the 2nd derivatives of MU
    d2fdx2(MU, theta, G, workarea)
    # Calculate the hessian matrix for every observation, store as a 
    #   row in a matrix with # rows = # obs, add up all the 
    #   matrices, and reshape.
    # For each obs, the hessian matrix is the matrix of second partial 
    #   derivatives of LL. The first term is the product of the 
    #   ("stretched out") matrix of second partial derivatives of the 
    #   MU function and the quantity cit/mu-1 (paper's notation).
    #   The second term is the outer product of (not the square of --
    #   we need a matrix not a vector) two first partial derivatives
    #   of the MU function times the quantity cit/mu^2.
    y <- colSums(
                 workarea$cmuminus1 * workarea$d2mudt2 -
                (workarea$value/(workarea$mu^2)) *
                    t(vapply(1:workarea$nobs, function(i)
                        c(outer(workarea$dmudt[i, ], workarea$dmudt[i, ])),
                        vector("double", length(theta)^2)
                        )))  
    dim(y) <- rep(length(theta), 2)
    y
    }

LL.ODP.SIGMA2 <- function(workarea) {
    workarea$residuals <- workarea$value-workarea$mu
    return(workarea$SIGMA2 <- 
        sum(workarea$residuals^2/workarea$mu) / (workarea$nobs-workarea$np)
        )
    }
    
# EXPECTED VALUE (MU) FUNCTIONS

# LDF METHOD

MU.LDF <- new("dfunction",
    # theta    = vector of parameters: U followed by omega and theta
    # G        = growth function (eg, loglogistic or weibull)
    # workarea = environment where intermediate values are stored
    function(theta, G, workarea) {
        lent <- length(theta)
        workarea$thetaU      <- theta[1L:(lenu<-lent-G@np)]
        workarea$thetaG      <- theta[(lenu+1):lent]
        workarea$u  <- workarea$thetaU[workarea$origin] ## or thetaU %*% workarea$io
        workarea$delG <- del(G, workarea$Age.from, workarea$Age.to, workarea$thetaG)
        workarea$mu <- workarea$u * workarea$delG
        },
    dfdx = function(theta, G, workarea) {
        workarea$deldG <- del(G@dGdt, workarea$Age.from, workarea$Age.to, workarea$thetaG)
        # Sandwich
        workarea$dmudt <- cbind(
            workarea$delG * workarea$io,
            workarea$u * workarea$deldG
            )
        },
    d2fdx2 = function(theta, G, workarea) {
        # Separately calculate the four submatrices for each obs:
        #   d2MUdthetaU2, d2MUdthetaUdthetaG, t(d2MUdthetaUdthetaG), and dtMUdthetaG2
        # Bind them into one hessian matrix for each obs.
        if (!exists("deldG", env=workarea)) stop("gr=NULL? Must run the derivatives.")
        
        # The result of the following will be a matrix with a row for 
        #   every observation. Each row is the hessian matrix for
        #   that observation, "stretched out" into a row vector.
        U2 <- array(0, rep(length(workarea$thetaU), 2))
        workarea$d2mudt2 <- t(vapply(1L:workarea$nobs, function(i) {
            # Cross partials of thetaU and thetaG: 10x1 %*% 1x2 = 10x2
            crossPartials.UG <- workarea$io[i, ] %*% t(workarea$deldG[i, ])
            # Cross partials of thetaG and thetaG
            crossPartials.GG <- workarea$u[i] * del(G@d2Gdt2, workarea$Age.from[i], workarea$Age.to[i], workarea$thetaG)
            c(rbind(cbind(U2, crossPartials.UG),
                  cbind(t(crossPartials.UG), crossPartials.GG)))
            }, vector("double", length(theta)^2)))
        }
    )

# CAPE COD METHOD

MU.CapeCod <- new("dfunction",
    # theta    = vector of parameters: U followed by omega and theta
    # G        = growth function (eg, loglogistic or weibull)
    # workarea = environment where intermediate values are stored
    function(theta, G, workarea) {
        workarea$ELR         <- theta[1L]
        workarea$thetaG      <- theta[2L:length(theta)]
        workarea$u  <- workarea$ELR * workarea$P
        workarea$delG <- del(G, workarea$Age.from, workarea$Age.to, workarea$thetaG)
        workarea$mu <- workarea$u * workarea$delG
        },
    dfdx = function(theta, G, workarea) {
        workarea$deldG <- del(G@dGdt, workarea$Age.from, workarea$Age.to, workarea$thetaG)
        # Sandwich
        workarea$dmudt <- cbind(
            workarea$P * workarea$delG,
            workarea$u * workarea$deldG
            )
        },
    d2fdx2 = function(theta, G, workarea) {
        # Separately calculate the four submatrices for each obs:
        #   d2MUdELR2, d2MUdELRdthetaG, t(d2MUdELRdthetaG), and dtMUdthetaG2
        # Bind them into one hessian matrix for each obs.
        if (!exists("deldG", env=workarea)) stop("gr=NULL? Must run the derivatives.")
        # The result of the following will be a matrix with a row for 
        #   every observation. Each row is the hessian matrix for
        #   that observation, "stretched out" into a column vector.
        ELR2 <- 0.0
        workarea$d2mudt2 <- t(vapply(seq.int(workarea$nobs), function(i) {
            # Cross partials of thetaU and thetaG: 10x1 %*% 1x2 = 10x2
            crossPartials.ELRG <- t(workarea$deldG[i, ])
            # Cross partials of thetaG and thetaG
            crossPartials.GG <- workarea$u[i] * del(G@d2Gdt2, workarea$Age.from[i], workarea$Age.to[i], workarea$thetaG)
            c(rbind(c(ELR2, crossPartials.ELRG),
                    cbind(t(crossPartials.ELRG), crossPartials.GG)))
            }, vector("double", length(theta)^2)))


        }
    )

# RESERVE FUNCTIONS
#   Functions to calculate the "reserve" (future development)
#       and partial derivatives under the two methods.

R.LDF <- new("dfunction",
    # theta    = vector of parameters: U followed by omega and theta
    # G        = growth function (eg, loglogistic or weibull)
    # from = age from after which losses are unpaid
    # to   = "ultimate" age. Could be a scalar
    # oy       = vector of origin years indices for selecting entries in thetaU
    function(theta, G, from, to, oy){
        lent   <- length(theta)
        thetaU <- theta[1L:(K <- lent - G@np)]
        thetaG <- theta[(K+1):lent]
        thetaU[oy] * del(G, from, to, thetaG)
        },
    dfdx = function(theta, G, from, to, oy){
        # R is a vector valued function (think "column matrix").
        # Taking the partials adds a new dimension ... put at beginning
        #   with rows corresponding to the parameters.
        # So dR is a matrix valued function where ncols=length(from)
        #   and nrow = length(theta)
        # 'to' can be a scalar (e.g., maxage)
        if (length(to)<2L) to <- rep(to, length.out=length(from))
        if (length(to)!=length(from)) stop("Unequal 'from', 'to'")
        lent   <- length(theta)
        thetaU <- theta[1L:(K <- lent - G@np)]
        thetaG <- theta[(K+1):lent]
        # dR
        # |A 0 0 . 0 | A: partial of R_ay wrt thetaU
        # |0 A 0 . . | B: partial of R_ay wrt thetaG
        # |0 0 .   . |
        # |. . . A 0 |
        # |0 0 . 0 A |
        # |B B ... B |
        ##  Number of rows (~ parameters) is fixed
        ##  Number of columns (~ origin years) can vary
        rbind(
            del(G, from, to, thetaG) * outer(oy, 1L:K, `==`),
            t(thetaU[oy] * del(G@dGdt, from, to, thetaG))
            )
        },
    d2fdx2 = NULL # not needed at this point
    )

R.CapeCod <- new("dfunction",
    # theta    = vector of parameters: U followed by omega and theta
    # G        = growth function (eg, loglogistic or weibull)
    # from = age from after which losses are unpaid
    # to   = "ultimate" age. Could be a scalar
    # oy       = vector of origin years indices for selecting entries in thetaU
    function(theta, Premium, G, from, to){
        ELR         <- theta[1L]
        thetaG      <- theta[2L:length(theta)]
        ELR * Premium * del(G, from, to, thetaG )
        },
    dfdx = function(theta, Premium, G, from, to){
        # R is a vector valued function (think "column matrix").
        # Taking the partials adds a new dimension ... put at beginning
        #   with rows corresponding to the parameters.
        # So dR is a matrix valued function where ncols=length(from)
        #   and nrow = length(theta)
        # 'to' can be a scalar (e.g., maxage)
        if (length(to)<2L) to <- rep(to, length.out=length(from))
        if (length(to)!=length(from)) stop("Unequal 'from', 'to'")
        ELR         <- theta[1L]
        thetaG      <- theta[2L:length(theta)]
        # dR
        # |A| A: partial of R_ay wrt ELR
        # |B| B: partial of R_ay wrt thetaG
        ##  Rows ~ origin years (variable), columns ~ parameters (fixed)
        rbind(
            ELR = Premium * del(G, from, to, thetaG),
            t(ELR * Premium * del(G@dGdt, from, to, thetaG))
            )
        },
    d2fdx2 = NULL # not needed at this point
    )

.prn <- function (
  x, txt, file = "") 
{
    # Based on code by Frank Harrell, Hmisc package, licence: GPL >= 2
  calltext <- as.character(sys.call())[2]
  if (file != "") 
    sink(file, append = TRUE)
  if (!missing(txt)) {
    if (nchar(txt) + nchar(calltext) + 3 > .Options$width) 
      calltext <- paste("\n\n  ", calltext, sep = "")
    else txt <- paste(txt, "   ", sep = "")
    cat("\n", txt, calltext, "\n\n", sep = "")
  }
  else cat("\n", calltext, "\n\n", sep = "")
  print(x)
  if (file != "") 
    sink()
  invisible()
}
