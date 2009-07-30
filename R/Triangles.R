as.ArrayTriangle <- function(x){

    ## x has to be a data.frame with columns: origin, dev and value
    .names <- apply(x[,c("origin", "dev", "value")], 2, unique)
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

as.LongTriangle <- function(x){

    lx <- expand.grid(origin=as.numeric(dimnames(x)$origin), dev=as.numeric(dimnames(x)$dev))
    lx$value <- as.vector(x)
    return(lx)
}


incr2cum <- function(Triangle){
    ## Christophe Dutang
    cum <- t(mapply(function(row)cumsum(Triangle[row,]), 1:NROW(Triangle)))
    dimnames(cum) <- dimnames(Triangle)
    cum
}


cum2incr <- function(Triangle){
    ## Christophe Dutang
    incr <- cbind(Triangle[,1], t(mapply(function(row)diff(Triangle[row,]), 1:NROW(Triangle))))
    dimnames(incr) <- dimnames(Triangle)
    incr
}
