## Author: Markus Gesmann
## Copyright: Markus Gesmann, markus.gesmann@gmail.com
## Date:19/09/2008


chainladder <- function(Triangle, weights=1,
                        delta=1){

    Triangle <- checkTriangle(Triangle)
    n <- dim(Triangle)[2]


    ## Mack uses alpha between 0 and 2 to distinguish
    ## alpha = 0 ordinary regression with intercept 0
    ## alpha = 1 historical chain ladder age-to-age factors
    ## alpha = 2 straight averages

    ## However, in Zehnwirth & Barnett they use the notation of delta, whereby delta = 2 - alpha
    ## the delta is than used in a linear modelling context.

    weights <- checkWeights(weights, Triangle)
    delta <- rep(delta,(n-1))[1:(n-1)]

    lmCL <- function(i, Triangle){
      lm(y~x+0, weights=weights[,i]/Triangle[,i]^delta[i],
         data=data.frame(x=Triangle[,i], y=Triangle[,i+1]))
    } 
    myModel <- lapply(c(1:(n-1)), lmCL, Triangle)
    
    output <- list(Models=myModel, Triangle=Triangle, delta=delta, weights=weights)
    class(output) <- c("ChainLadder", "TriangleModel", class(output))
    return(output)
}

checkWeights <- function(weights, Triangle){

    if(is.null(dim(weights))){
        if(length(weights)==1){
            my.weights <- Triangle
            my.weights[!is.na(Triangle)] <- weights
            weights <- my.weights
        }
    }

return(weights)

}




###############################################################################
## predict
##
predict.TriangleModel <- function(object,...){

  n <- ncol(object[["Triangle"]])
  
  FullTriangle <- object[["Triangle"]]
  MF <- lapply(c(2:n), 
               function(j){
                 ii <- is.na(FullTriangle[,j])
                 FF <- predict(object[["Models"]][[j-1]], se.fit=TRUE,                    
                               newdata=data.frame(x=FullTriangle[ii, j-1]))
                 FullTriangle[ii,j] <<- FF$fit
                 return(FF)
               }             
               )
  
    return(list(FullTriangle=FullTriangle, Prediction=MF))
}



predict.ChainLadder <- function(object,...){
  res <- predict.TriangleModel(object,...)
  res[["FullTriangle"]]
}

################################################################################
## estimate tail factor, idea from Thomas Mack:
##       THE STANDARD ERROR OF CHAIN LADDER RESERVE ESTIMATES:
##       RECURSIVE CALCULATION AND INCLUSION OF A TAIL FACTOR
##
tailfactor <- function (clratios){
    f <- clratios
    n <- length(f)
    if (f[n - 2] * f[n - 1] > 1.0001) {
        fn <- which(clratios > 1)
        f <- clratios[fn]
        n <- length(f)
        tail.model <- lm(log(f - 1) ~ fn)
        co <- coef(tail.model)
        tail <- exp(co[1] + c((n+1):(n + 100)) * co[2]) + 1
        tail <- prod(tail)
        if (tail > 2){
            print("The estimate tail factor was bigger than 2 and has been reset to 1.")
            tail <- 1
        }
    }
    else {
        tail <- 1
        tail.model <- NULL
    }
    return(list(tail.factor=tail, tail.model=tail.model))
}


checkTriangle <- function(Triangle){

    ## if a triangle is an array with 3 dimension convert it into a matrix
    .dim <- dim(Triangle)
    if(length(.dim)>3){
      stop("Your array has too many dimensions.")
    }

    n <- .dim[2]
    m <- .dim[1]

    if(n>m){
#        print(.dim)
#        stop("Number of origin periods has to be equal or greater than the number of development periods.\n")
        stop("Number of origin periods, ", m, ", is less than the number of development periods, ", n, ".\n")
    }


    if(length(.dim)==3 & .dim[3]==1){
        dim(Triangle) <- c(m,n)
    }

    if("data.frame" %in% class(Triangle)){
        Triangle <- as.matrix(Triangle)
        Triangle <- as.triangle(Triangle)
        storage.mode(Triangle) <- "double"
    }

    tri.dimnames <- dimnames(Triangle)
    if(is.null(tri.dimnames[[1]])){
        .origin <- 1:m
    }else{
        .origin <- tri.dimnames[[1]]
    }
    if(is.null(tri.dimnames[[2]])){
        .dev <- 1:n
    }else{
        .dev <- tri.dimnames[[2]]
    }

    dimnames(Triangle) <- list(origin=.origin, dev=.dev)

    return(Triangle)
}
