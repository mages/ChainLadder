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

#  isDate <- inherits(Triangle[[origin]], "Date")
#
#  if (isDate) {
#    warning("Converting origin from Date to numeric")
#    Triangle[[origin]] <- as.numeric(Triangle[[origin]])
#  }

  fmla <- as.formula(paste(origin, "~", dev))
  matrixTriangle <- acast(Triangle, fmla, fun.aggregate = sum,
                          value.var = value, fill = as.numeric(NA))
  names(dimnames(matrixTriangle)) <- c(origin, dev)

  class(matrixTriangle) <- c("triangle", "matrix")
  return(matrixTriangle)
}


## Copyright notice for function 'triangle' only
## Author: Vincent Goulet
## Copyright: Vincent Goulet, vincent.goulet@act.ulaval.ca
## Date: 23/05/2018
triangle <- function(..., bycol = FALSE, origin = "origin", dev = "dev", value = "value"){
  x <- list(...)

  if (length(x) == 1L) {
    ## 'len' contains the number of development periods (when
    ## filling by row) or origin periods (when filling by column).
    ## Here it is the positive root of n(n + 1)/2 = k, where k is
    ## the number of data points provided.
    len <- (-1 + sqrt(1 + 8 * length(x[[1L]])))/2

    ## Error if 'len' is not an integer, otherwise it is just too
    ## complicated to try infer what user wants.
    if (abs(len - round(len)) > .Machine$double.eps^0.5)
        stop("invalid number of data points for a triangle")

    ## Rearrange the data vector in a list of vectors suitable to
    ## build a 'len' x 'len' triangle.
    s <- seq_len(len)
    x <- split(x[[1L]], rep(s, rev(s)))
  } else {
    ## If more than one data vector is provided in argument, the
    ## number of development or origin periods is derived from the
    ## *first* vector (this avoids looking at the length of each
    ## and every element to find the maximum).
    len <- length(x[[1L]])
  }

  ## Extend each data vector to length 'len' by filling with NAs and
  ## put into matrix form at the same time; dimension names will be in
  ## place thanks to 'sapply'.
  x <- sapply(x, function(x) { length(x) <- len; x })

  as.triangle.matrix(if (bycol) x else t(x),
                     origin = origin, dev = dev, value = value)
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

as.LongTriangle1 <- function(Triangle, varnames = names(dimnames(Triangle)), ...,
                           na.rm = TRUE, as.is = TRUE, value.name = "value") {
  if (!inherits(Triangle, "matrix")) stop("asLongTriangle only works for matrices")
  if (is.null(varnames)) varnames <- c("origin", "dev")
  else {
    if (is.na(varnames[1L])) varnames[1L] <- "origin"
    if (is.na(varnames[2L])) varnames[2L] <- "dev"
  }
  y <- reshape2::melt(Triangle, varnames = varnames, ..., na.rm = na.rm, as.is = as.is,
                      value.name = value.name)
  names(y)[1:2] <- varnames
  y
}

as.LongTriangle <- function (Triangle, varnames = names(dimnames(Triangle)),
                             value.name = "value", na.rm = TRUE) {
  if (!inherits(Triangle, "matrix")) stop("asLongTriangle only works for matrices")
  if (is.null(varnames)) varnames <- c("origin", "dev")
  else {
    if (is.na(varnames[1L])) varnames[1L] <- "origin"
    if (is.na(varnames[2L])) varnames[2L] <- "dev"
  }
  namecols <- setNames(expand.grid(dimnames(Triangle), KEEP.OUT.ATTRS = FALSE,
                                   stringsAsFactors = TRUE), varnames)
  if (na.rm) {
    isna <- is.na(Triangle)
    namecols <- namecols[!isna,]
    Triangle <- Triangle[!isna]
  }
  y <- cbind(namecols, setNames(data.frame(c(Triangle)), value.name))
  #   class(y) <- c("long.triangle", "data.frame")
  y
}

.as.LongTriangle <- function(Triangle, na.rm=FALSE){
  # 3/20/2013
  # Difference from old version: preserves names(dimnames) to be column names
  # in the data.frame rather than forcing 'origin' and 'dev'
  x <- Triangle
  nms <- names(dimnames(x))
  .dev <- try(as.numeric(dimnames(x)[[nms[2L]]]))
  #  .origin <- try(as.numeric(dimnames(x)[[nms[1L]]]))
  .origin <- dimnames(x)[[nms[1L]]]
  if(class(dimnames(x)[[nms[2L]]]) %in% "character"){
    if(.allisnumeric(dimnames(x)[[nms[2L]]])){
      .dev <- try(as.numeric(dimnames(x)[[nms[2L]]]))
    }else{
      .dev <- seq(along=(dimnames(x)[[nms[2L]]]))
      warning(paste(
        c("Development period was a character and has been set to:\n",.dev),
        collapse = " "))
    }
  }else{
    .dev <- try(as.numeric(dimnames(x)[[nms[2L]]]))
  }
#  if(any(is.na(c(.origin, .dev)))){
#    stop(paste("The origin and dev. period columns have to be of type numeric or a character",
#               "which can be converted into numeric.\n"))
#  }
  lx <- expand.grid(origin=.origin, dev=.dev, stringsAsFactors = FALSE)
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
