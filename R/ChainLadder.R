## Author: Markus Gesmann
## Copyright: Markus Gesmann, markus.gesmann@gmail.com
## Date:19/09/2008


ChainLadder <- function(Triangle, weights=1/Triangle){

    Triangle <- checkTriangle(Triangle)

    n <- dim(Triangle)[2]


    myModel <- vector("list", (n-1))
    for(i in c(1:(n-1))){
        ## weighted linear regression through origin
        dev.data <- data.frame(x=Triangle[,i], y=Triangle[,i+1])
  	myModel[[i]] <- lm(y~x+0, weights=weights[,i], data=dev.data)
    }

    output <- list(Models=myModel, Triangle=Triangle)
    class(output) <- c("ChainLadder", "TriangleModel", class(output))
    return(output)
}




###############################################################################
## predict
##
predict.TriangleModel <- function(object,...){
    n <- ncol(object[["Triangle"]])
    m <- nrow(object[["Triangle"]])
    FullTriangle <- object[["Triangle"]]

    for(j in c(1:(n-1))){
        FullTriangle[c((m-j+1):m), j+1] <- predict(object[["Models"]][[j]],
                                                   newdata=data.frame(x=FullTriangle[c((m-j+1):m), j]),...)
    }
    return(FullTriangle)
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
    if(class(Triangle)=="data.frame"){
        Triangle <- as.matrix(Triangle)
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
