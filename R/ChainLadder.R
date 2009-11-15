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
    delta <- checkDelta(delta,n)

    myModel <- vector("list", (n-1))
    for(i in c(1:(n-1))){
        ## weighted linear regression through origin
        dev.data <- data.frame(x=Triangle[,i], y=Triangle[,i+1])
  	myModel[[i]] <- lm(y~x+0, weights=weights[,i]/Triangle[,i]^delta[i], data=dev.data)
    }

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

checkDelta <- function(delta, n){

    if(! (all(delta %in% c(0,1,2))) )
        stop("Please specify alpha with integer values in {0;1;2}\n")

    delta.n <- length(delta)
    if(delta.n==1){
        delta <- rep(delta, n)
    }

    if(delta.n > 1 && delta.n <= (n-1)){
        print("delta=has more than one entry but less than n-1 entries. Therefore I will use the first entry only.")
        delta <- rep(delta[1], n)
    }
    return(delta)
}




###############################################################################
## predict
##
predict.TriangleModel <- function(object,...){
    n <- ncol(object[["Triangle"]])
    m <- nrow(object[["Triangle"]])
    FullTriangle <- object[["Triangle"]]

    for(j in c(1:(n-1))){
        i <- which(is.na(FullTriangle[, j+1]))
        ##        FullTriangle[c((m-j+1):m), j+1] <- predict(object[["Models"]][[j]],
        ##                                                  newdata=data.frame(x=FullTriangle[c((m-j+1):m), j]),...)
        FullTriangle[i, j+1] <- predict(object[["Models"]][[j]],
                                                   newdata=data.frame(x=FullTriangle[i, j]),...)

    }
    return(FullTriangle)
}

predict.ChainLadder <- function(object,...){
    predict.TriangleModel(object,...)
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
        tail <- exp(co[1] + c(n:(n + 100)) * co[2]) + 1
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
      stop("Your array has to many dimensions.")
    }

    n <- .dim[2]
    m <- .dim[1]

    if(n>m){
        print(.dim)
        stop("Number of origin periods has to be equal or greater than the number of development periods.\n")
    }


    if(length(.dim)==3 & .dim[3]==1){
        dim(Triangle) <- c(m,n)
    }

    if("data.frame" %in% class(Triangle)){
        Triangle <- as.matrix(Triangle)
        Triangle <- as.triangle(Triangle)
    }

    tri.dimnames <- dimnames(Triangle)
    if(is.null(tri.dimnames[[1]])){
        .origin <- 1:m
    }else{
        .origin <- tri.dimnames[[1]]
    }
    .dev <- 1:n

    dimnames(Triangle) <- list(origin=.origin, dev=.dev)

    return(Triangle)
}
