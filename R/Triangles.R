## Author: Markus Gesmann
## Copyright: Markus Gesmann, markus.gesmann@gmail.com
## Date:10/11/2007; 17/09/2008; 16/11/2009

.allisnumeric <- function (x, 
                          what = c("test", "vector"), 
                          extras = c(".", "NA")) 
{
  # Based on code by Frank Harrell, Hmisc package, licence: GPL >= 2                          
  what <- match.arg(what)
  x <- sub("[[:space:]]+$", "", x)
  x <- sub("^[[:space:]]+", "", x)
  #xs <- x[x %nin% c("", extras)]
  xs <- x[match(x, extras, nomatch = 0) == 0]
  isnum <- suppressWarnings(!any(is.na(as.numeric(xs))))
  if (what == "test") 
    isnum
  else if (isnum) 
    as.numeric(x)
  else x
}

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
  
  if(is.null(dimnames(Triangle)[[origin]])){
    dimnames(Triangle)[[origin]] <- 1:nrow(Triangle)
  }
  if(is.null(dimnames(Triangle)[[dev]])){
    dimnames(Triangle)[[dev]] <- 1:ncol(Triangle)
  }
  
  storage.mode(Triangle) <- "double"
  return(Triangle)
}

as.triangle.data.frame <- function(Triangle, origin="origin", dev="dev", value="value", ...){
  #  d <- dim(Triangle)
  #  if(length(d) == 2 & d[1]==d[2]){
  #      matrixTriangle <- as.matrix(Triangle)
  #      matrixTriangle <- as.triangle(matrixTriangle)
  #  }else{
  
  fmla <- as.formula(paste(origin, "~", dev))
  matrixTriangle <- acast(Triangle, fmla, fun.aggregate = sum, 
                          value.var = value, fill = as.numeric(NA))
  names(dimnames(matrixTriangle)) <- c(origin, dev)
  #      matrixTriangle <- .as.MatrixTriangle(Triangle, origin, dev, value)
  #  }
  class(matrixTriangle) <- c("triangle", "matrix")
  return(matrixTriangle)
}

as.data.frame.triangle <- function(x, row.names=NULL, optional, lob=NULL, na.rm=FALSE,...){
  
  
  longTriangle <- .as.LongTriangle(x, na.rm)
  if(is.null(row.names))
    rownames(longTriangle) <- paste(longTriangle[,1], longTriangle[,2], sep="-")
  if(!is.null(lob))
    longTriangle$lob=lob
  
  class(longTriangle) <- c("long.triangle", "data.frame")
  return(longTriangle)
}

plot.triangle <- function(x,type="b",
                          xlab="dev. period",
                          ylab=NULL, lattice=FALSE,...){
  .x <- x
  class(.x) <- "matrix"
  if(!lattice){
    matplot(t(.x),type=type,
            xlab=xlab,
            ylab=ifelse(is.null(ylab), deparse(substitute(x)), ylab),...)
  }else{
    df <- as.data.frame(as.triangle(.x))
    xyplot(value ~ dev | factor(origin), data=df, type=type, 
           as.table=TRUE, xlab=xlab, ylab=ylab, ...)
  }
}

print.triangle <- function(x, ...) {
  class(x) <- tail(class(x), -1)
  NextMethod(x, ...)
}

.as.MatrixTriangle <- function(x, origin="origin", dev="dev", value="value"){
  ## x has to be a data.frame with columns: origin, dev and value
  x <- x[,c(origin, dev, value)]
  names(x) <- c("origin", "dev", "value")
  
  z <- reshape(x, timevar="dev", v.names="value", idvar="origin", direction="wide")
  
  z <- z[order(z$origin), ]
  
  .origin.names <- z$origin
  z <- z[,-1]
  
  names(z) <- gsub("value.", "",names(z))
  .dev.names <- as.numeric(as.character(names(z)))	
  z <- z[,order(.dev.names)]
  
  z<- as.matrix(z)
  dimnames(z) <- list(origin=.origin.names, dev=sort(.dev.names))
  
  names(dimnames(z)) <- c(origin, dev)
  return(z)
}

.as.LongTriangle <- function(Triangle, na.rm=FALSE){
  # 3/20/2013
  # Difference from old version: preserves names(dimnames) to be column names
  # in the date.frame rather than forcing 'origin' and 'dev'
  x <- Triangle
  nms <- names(dimnames(x))
  .origin <- try(as.numeric(dimnames(x)[[nms[1L]]]))
  if(class(dimnames(x)[['dev']]) %in% "character"){
    if(.allisnumeric(dimnames(x)[['dev']])){
      .dev <- try(as.numeric(dimnames(x)[[nms[2L]]]))
    }else{
      .dev <- seq(along=(dimnames(x)[['dev']]))
      warning(paste(
        c("Development period was a character and has been set to:\n",.dev), 
        collapse = " "))
    }
  }else{
    .dev <- try(as.numeric(dimnames(x)[[nms[2L]]]))
  }
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
  if (!is.null(nms)) {
    if (!is.na(nms[1L])) names(lx)[1L] <- nms[1L]
    if (!is.na(nms[2L])) names(lx)[2L] <- nms[2L]
  }
  return(lx)
}

# A coefficients method to quickly pull out the factors from a ChainLadder model
coef.ChainLadder <- function(object, ...) {
  structure(sapply(object$Models, coefficients)
            , names = head(colnames(object$Triangle), -1)
            , ...)
}

# Idea: think about a class triangles, which stores an array of triangles, e.g. for several lines of business
