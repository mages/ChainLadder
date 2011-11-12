## Author: Markus Gesmann
## Copyright: Markus Gesmann, markus.gesmann@gmail.com
## Date:10/11/2007; 17/09/2008; 16/11/2009

incr2cum <- function(Triangle, na.rm=FALSE){
    if(na.rm){
        upper <- col(Triangle) <= ncol(Triangle) + 1 - row(Triangle)
        upperna <- which(is.na(Triangle[upper]), arr.ind=TRUE)

        Triangle[upper][upperna] <- 0
    }
    cum <- t(apply(Triangle,1, cumsum))
    dimnames(cum) <- dimnames(Triangle)
    expos <- attr(Triangle,"exposure")
    if (!is.null(expos))
      attr(cum,"exposure") <- expos
    class(cum) <- c("triangle", "matrix")
    cum
}


cum2incr <- function(Triangle){
    incr <- cbind(Triangle[,1], t(apply(Triangle,1,diff)))
    dimnames(incr) <- dimnames(Triangle)
    expos <- attr(Triangle,"exposure")
    if (!is.null(expos))
      attr(incr,"exposure") <- expos    
    class(incr) <- c("triangle", "matrix")
    incr
}


as.triangle <- function(Triangle, origin="origin", dev="dev", value="value",...){
  UseMethod("as.triangle")
}

as.triangle.matrix <- function(Triangle, origin="origin", dev="dev", value="value",...){
 class(Triangle) <- c("triangle", "matrix")
 if(is.null(dimnames(Triangle))){
     dimnames(Triangle) <- list(origin=1:nrow(Triangle), dev=1:ncol(Triangle))
 }
 names(dimnames(Triangle)) <- c(origin, dev)

 if(is.null(dimnames(Triangle)$origin)){
     dimnames(Triangle)$origin <- 1:nrow(Triangle)
 }
 if(is.null(dimnames(Triangle)$dev)){
     dimnames(Triangle)$dev <- 1:col(Triangle)
 }


 return(Triangle)
}

as.triangle.data.frame <- function(Triangle, origin="origin", dev="dev", value="value", ...){
    d <- dim(Triangle)
    if(length(d) == 2 & d[1]==d[2]){
        matrixTriangle <- as.matrix(Triangle)
        matrixTriangle <- as.triangle(matrixTriangle)
    }else{
        matrixTriangle <- .as.MatrixTriangle(Triangle, origin, dev, value)
    }
    class(matrixTriangle) <- c("triangle", "matrix")
    return(matrixTriangle)
}

as.data.frame.triangle <- function(x, row.names=NULL, optional, lob=NULL, na.rm=FALSE,...){
    longTriangle <- .as.LongTriangle(x, na.rm)
    if(is.null(row.names))
        rownames(longTriangle) <- paste(longTriangle$origin, longTriangle$dev, sep="-")
    if(!is.null(lob))
        longTriangle$lob=lob

    class(longTriangle) <- c("long.triangle", "data.frame")
    return(longTriangle)
}

plot.triangle <- function(x,t="b",xlab="dev. period",ylab=NULL, lattice=FALSE,...){
    .x <- x
    class(.x) <- "matrix"
    if(!lattice){
        matplot(t(.x),type=t,
                xlab=xlab,
                ylab=ifelse(is.null(ylab), deparse(substitute(x)), ylab),...)
    }else{
        df <- as.data.frame(as.triangle(.x))
        xyplot(value ~ dev | factor(origin), data=df, t="l", as.table=TRUE,...)
    }
}

print.triangle <- function(x, ...) {
    class(x) <- tail(class(x), -1)
    NextMethod(x, ...)
    }

.as.MatrixTriangle <- function(x, origin="origin", dev="dev", value="value"){
    ## x has to be a data.frame with columns: origin, dev and value
    x <- x[,c(origin, dev, value)]
    x <- x[order(x[origin], x[dev]),]
    names(x) <- c("origin", "dev", "value")
    .names <- apply(x[,c("origin", "dev", "value")], 2, unique)
    if(class(.names) != "list"){
        .names <- as.list(as.data.frame(.names))
    }
    .namesOD <- .names[c("origin", "dev")]
    ## Expand to include entire array, in case don't have complete data
    .id <- paste(x$origin, x$dev,  sep='.')
    .grid <- expand.grid(.namesOD)
    .grid$id <- paste(.grid$origin, .grid$dev, sep='.')
    .grid$data <- x$value[match(.grid$id, .id)]
    ## Create data array
    .data <- array(.grid$data, dim=unlist(lapply(.namesOD, length)),
                   dimnames=.namesOD)
    return(.data)
}

.as.LongTriangle <- function(Triangle, na.rm=FALSE){
    x <- Triangle
    .origin <- try(as.numeric(dimnames(x)$origin))
    .dev <- try(as.numeric(dimnames(x)$dev))
    if(any(is.na(c(.origin, .dev)))){
      stop(paste("The origin and dev. period columns have to be of type numeric or a character",
                 "which can be converted into numeric.\n"))
    }
    lx <- expand.grid(origin=.origin, dev=.dev)
    ##    lx <- expand.grid(origin=dimnames(x)$origin, dev=dimnames(x)$dev)
    lx$value <- as.vector(x)
    if(na.rm){
        lx <- na.omit(lx)
    }
    return(lx)
}

# Idea: think about a class triangles, which stores an array of triangles, e.g. for several lines of business
