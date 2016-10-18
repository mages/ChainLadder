#' plotParms Plot the estimated parameters of a model
#' 
#' Methods to visualize the estimated parameters. 
#' S3 methods currently exist for objects of class
#' ChainLadder and MackChainLadder.
#' 
#' @param x object whose parameters are to be visualized
#' 
#' @param title optional; character holding title of the plot;
#' defaults to something the class author deems appropriate.
#library(ggplot2)
#library(grid)
#library(gridExtra)
plotParms <- function(x, ...) UseMethod("plotParms")
plotParms.default <- function(x, ...){
  stop("No 'plotParms' method exists for objects of class ",
       class(x))
}
plotParms.ChainLadder <- function(x, title, ...) {
  p1 <- plot.cl.f(x) + theme(axis.title.y=element_blank())
  p2 <- plot.cl.f.se(x) + theme(axis.title.y=element_blank())
  p3 <- plot.cl.f.cv(x) + theme(axis.title.y=element_blank()) 
  p4 <- plot.cl.sigma(x) + theme(axis.title.y=element_blank())
  if (missing(title)) title <- paste0("ChainLadder(",
                                      x$TriangleName, 
                                      ") parameter estimates")
    
  marrangeGrob(grobs=list(p1, p2, p3, p4), ncol = 2, nrow = 2,
               top = title)
}
plot.cl.f <- function(x) {
#  require(ggplot2)
  smmry <- suppressWarnings(lapply(x$Models, summary))
  f <- sapply(smmry, function(x) x$coef["x","Estimate"])
  n <- length(f)
  f.se <- sapply(smmry, function(x) x$coef["x","Std. Error"])
  sigma <- sapply(smmry, function(x) x$sigma)
  nobs <- as.character(sapply(x$Models, nobs))
  na_sigma <- is.na(sigma)
  xx <- factor(colnames(x$Triangle)[1:n], levels = colnames(x$Triangle)[1:n])
  df <- data.frame(xx, f, f.se, sigma, na_sigma, nobs, 
                   label = nobs,
                   stringsAsFactors = FALSE)
  df$f.se[na_sigma] <- 0
  # Need for aes_: see exchange at link (wrapped) below 
  #   and final solution
  # http://stackoverflow.com/questions/9439256/
  #   how-can-i-handle-r-cmd-check-no-visible-binding-for-
  #   global-variable-notes-when
  P <- ggplot(df, aes_(x=~xx, y=~f, label = ~label)) +  
    geom_errorbar(aes(ymin=f-f.se, ymax=f+f.se), colour="black", width=.1) +
    xlab(names(dimnames(x$Triangle))[1L]) +
    geom_line(aes(colour = na_sigma, group = 1)) +
    geom_point(aes(colour = na_sigma)) +
    ggtitle("f estimates")
  if (all(!na_sigma)) P <- P + theme(legend.position="none")
  P
}
plot.cl.f1 <- function(x) {
#  require(ggplot2)
  smmry <- suppressWarnings(lapply(x$Models, summary))
  f1 <- sapply(smmry, function(x) x$coef["x","Estimate"]) - 1
  n <- length(f1)
  f.se <- sapply(smmry, function(x) x$coef["x","Std. Error"])
  sigma <- sapply(smmry, function(x) x$sigma)
  nobs <- as.character(sapply(x$Models, nobs))
  na_sigma <- is.na(sigma)
  xx <- factor(colnames(x$Triangle)[1:n], levels = colnames(x$Triangle)[1:n])
  df <- data.frame(xx, f1, f.se, sigma, na_sigma, nobs, 
                   label = nobs,
                   stringsAsFactors = FALSE)
  df$f.se[na_sigma] <- 0
  P <- ggplot(df, aes_(x=~xx, y=~f1, label = ~label)) +  
    geom_errorbar(aes(ymin=f1-f.se, ymax=f1+f.se), colour="black", width=.1) +
    xlab(names(dimnames(x$Triangle))[1L]) +
    geom_line(aes(colour = na_sigma, group = 1)) +
    geom_point(aes(colour = na_sigma)) +
    ggtitle("f-1 estimates")
  if (all(!na_sigma)) P <- P + theme(legend.position="none")
  P
}
plot.cl.sigma <- function(x) {
#  require(ggplot2)
  smmry <- suppressWarnings(lapply(x$Models, summary))
  f <- sapply(smmry, function(x) x$coef["x","Estimate"])
  n <- length(f)
  f.se <- sapply(smmry, function(x) x$coef["x","Std. Error"])
  sigma <- sapply(smmry, function(x) x$sigma)
  nobs <- as.character(sapply(x$Models, nobs))
  na_sigma <- is.na(sigma)
  xx <- factor(colnames(x$Triangle)[1:n], levels = colnames(x$Triangle)[1:n])
  df <- data.frame(xx, f.se, sigma, na_sigma, nobs, 
                   sigmapoint = sigma,
                   label = nobs,
                   stringsAsFactors = FALSE)
  df$sigmapoint[na_sigma] <- 0
  P <- ggplot(df, aes_(x=~xx, y=~sigma, label = ~label)) +  
    xlab(names(dimnames(x$Triangle))[1L]) +
    geom_line(aes(colour = na_sigma, group = 1), na.rm = TRUE) +
    geom_point(aes_(y = ~sigmapoint, colour = ~na_sigma)) +
    ggtitle("sigma estimates")
  if (all(!na_sigma)) P <- P + theme(legend.position="none")
  P
}
plot.cl.f.se <- function(x) {
#  require(ggplot2)
  smmry <- suppressWarnings(lapply(x$Models, summary))
  f <- sapply(smmry, function(x) x$coef["x","Estimate"])
  n <- length(f)
  f.se <- sapply(smmry, function(x) x$coef["x","Std. Error"])
  sigma <- sapply(smmry, function(x) x$sigma)
  na_sigma <- is.na(sigma)
  #isna <- is.na(f.cv)
  #sigma[isna] <- 0
  xx <- factor(colnames(x$Triangle)[1:n], levels = colnames(x$Triangle)[1:n])
  df <- data.frame(xx, f, f.se, sigma, na_sigma,  
                   f.se.point = f.se,
                   stringsAsFactors = FALSE)
  df$f.se.point[na_sigma] <- 0
  P <- ggplot(df, aes(x=xx, y=f.se, colour = na_sigma)) +  
    xlab(names(dimnames(x$Triangle))[2L]) +
    geom_line(aes(colour = na_sigma, group = 1), na.rm = TRUE) +
    geom_point(aes_(y = ~f.se.point, colour = ~na_sigma), na.rm = TRUE) +
    ggtitle("f.se estimates") 
    if (all(!na_sigma)) P <- P + theme(legend.position="none")
  P
}
plot.cl.f.cv <- function(x) {
#  require(ggplot2)
  smmry <- suppressWarnings(lapply(x$Models, summary))
  f <- sapply(smmry, function(x) x$coef["x","Estimate"])
  n <- length(f)
  f.se <- sapply(smmry, function(x) x$coef["x","Std. Error"])
  f.cv <- f.se / (f-1)
  sigma <- sapply(smmry, function(x) x$sigma)
  na_sigma <- is.na(sigma)
  est_source <- rep("regr", n)
  est_source[na_sigma] <- "sigma=NA"
  est_source[f==1] <- "f=1"
  xx <- factor(colnames(x$Triangle)[1:n], levels = colnames(x$Triangle)[1:n])
  df <- data.frame(xx, f, f.se, sigma, na_sigma, f.cv, 
                   f.cv.point = f.cv,
                   est_source,
                   stringsAsFactors = FALSE)
  df$f.cv.point[est_source!="regr"] <- 0
  P <- ggplot(df, aes(x=xx, y=f.cv, colour = est_source)) +  
    xlab(names(dimnames(x$Triangle))[2L]) +
    ylab("cv(f-1)") +
    geom_line(aes(group = 1), na.rm = TRUE) +
    geom_point(aes_(y=~f.cv.point, colour = ~est_source), na.rm = TRUE) +
    ggtitle("cv(f-1) estimates") 
  P
}

# Now MackCL
plot.MackCL.f <- function(x) {
#  require(ggplot2)
  smmry <- suppressWarnings(lapply(x$Models, summary))
  f <- sapply(smmry, function(x) x$coef["x","Estimate"])
  n <- length(f)
  f.se <- x$f.se #sapply(smmry, function(x) x$coef["x","Std. Error"])
  sigma <- sapply(smmry, function(x) x$sigma)
  nobs <- as.character(sapply(x$Models, nobs))
  nasigma <- is.na(sigma)
  xx <- factor(colnames(x$Triangle)[1:n], levels = colnames(x$Triangle)[1:n])
  df <- data.frame(xx, f, f.se, sigma, nasigma, nobs, 
                   label = nobs,
                   stringsAsFactors = FALSE)
  P <- ggplot(df, aes_(x=~xx, y=~f, label = ~label)) +  
    geom_errorbar(aes(ymin=f-f.se, ymax=f+f.se), colour="black", width=.1) +
    xlab(names(dimnames(x$Triangle))[1L]) +
    geom_line(aes(colour = nasigma[1], group = 1)) +
    geom_point(aes(colour = nasigma)) +
    ggtitle("f estimates")
  P <- P + theme(legend.position="none")
  P
}
plot.MackCL.f1 <- function(x) {
#  require(ggplot2)
  smmry <- suppressWarnings(lapply(x$Models, summary))
  f1 <- sapply(smmry, function(x) x$coef["x","Estimate"]) - 1
  n <- length(f1)
  f.se <- x$f.se #sapply(smmry, function(x) x$coef["x","Std. Error"])
  sigma <- sapply(smmry, function(x) x$sigma)
  nobs <- as.character(sapply(x$Models, nobs))
  nasigma <- is.na(sigma)
  xx <- factor(colnames(x$Triangle)[1:n], levels = colnames(x$Triangle)[1:n])
  df <- data.frame(xx, f1, f.se, sigma, nasigma, nobs, 
                   label = nobs,
                   stringsAsFactors = FALSE)
  P <- ggplot(df, aes_(x=~xx, y=~f1, label = ~label)) +  
    geom_errorbar(aes(ymin=f1-f.se, ymax=f1+f.se), colour="black", width=.1) +
    xlab(names(dimnames(x$Triangle))[1L]) +
    geom_line(aes(colour = nasigma[1], group = 1)) +
    geom_point(aes(colour = nasigma)) +
    ggtitle("f-1 estimates")
  P <- P + theme(legend.position="none")
  P
}
plot.MackCL.sigma <- function(x) {
#  require(ggplot2)
  smmry <- suppressWarnings(lapply(x$Models, summary))
  #smmry <- lapply(x$Models, summary)
  f <- sapply(smmry, function(x) x$coef["x","Estimate"])
  n <- length(f)
  f.se <- sapply(smmry, function(x) x$coef["x","Std. Error"])
  sigma <- sapply(smmry, function(x) x$sigma)
  #nobs <- rep(">1", n)
  imputed <- is.na(sigma)
  nasigma <- is.na(sigma)
  #nobs[nasigma] <- "T"
  sigma[nasigma] <- x$sigma[imputed]
  #  df <- sapply(smmry, function(x) x$df[2L])
  xx <- factor(colnames(x$Triangle)[1:n], levels = colnames(x$Triangle)[1:n])
  df <- data.frame(xx, sigma, imputed, stringsAsFactors = FALSE)
  P <- ggplot(df, aes(x=xx, y=sigma, colour = imputed)) +  
    xlab(names(dimnames(x$Triangle))[2L]) +
    geom_line(aes(colour = imputed[1], group = 1)) +
    geom_point(aes(colour = imputed)) +
    ggtitle("sigma estimates")
  if (all(!nasigma)) P <- P + theme(legend.position="none")
  P
}
plot.MackCL.f.se <- function(x) {
#  require(ggplot2)
  smmry <- suppressWarnings(lapply(x$Models, summary))
  f <- sapply(smmry, function(x) x$coef["x","Estimate"])
  n <- length(f)
  f.se <- sapply(smmry, function(x) x$coef["x","Std. Error"])
  f.cv <- f.se / (f-1)
  sigma <- sapply(smmry, function(x) x$sigma)
  imputed <- is.na(sigma)
  sigma[imputed] <- x$sigma[imputed]
  f.se[imputed] <- x$f.se[imputed]
  #isna <- is.na(f.cv)
  #sigma[isna] <- 0
  xx <- factor(colnames(x$Triangle)[1:n], levels = colnames(x$Triangle)[1:n])
  df <- data.frame(xx, f, f.se, sigma, imputed, f.cv, stringsAsFactors = FALSE)
  P <- ggplot(df, aes(x=xx, y=f.se, colour = imputed)) +  
    xlab(names(dimnames(x$Triangle))[2L]) +
    geom_line(aes(colour = imputed[1], group = 1), na.rm = TRUE) +
    geom_point(aes(colour = imputed), na.rm = TRUE) +
    ggtitle("f.se estimates") 
  if (all(!imputed)) P <- P + theme(legend.position="none")
  P
}
plot.MackCL.f.cv <- function(x) {
#  require(ggplot2)
  smmry <- suppressWarnings(lapply(x$Models, summary))
  f <- sapply(smmry, function(x) x$coef["x","Estimate"])
  n <- length(f)
  f.se <- sapply(smmry, function(x) x$coef["x","Std. Error"])
  sigma <- sapply(smmry, function(x) x$sigma)
  imputed <- is.na(sigma)
  sigma[imputed] <- x$sigma[imputed]
  f.se[imputed] <- x$f.se[imputed]
  f.cv <- f.se / (f-1)
  xx <- factor(colnames(x$Triangle)[1:n], levels = colnames(x$Triangle)[1:n])
  df <- data.frame(xx, f, f.se, sigma, imputed, f.cv, stringsAsFactors = FALSE)
  P <- ggplot(df, aes(x=xx, y=f.cv, colour = imputed)) +  
    xlab(names(dimnames(x$Triangle))[2L]) +
    geom_line(aes(colour = imputed[1], group = 1), na.rm = TRUE) +
    geom_point(aes(colour = imputed), na.rm = TRUE) +
    ggtitle("f.cv estimates") 
  if (all(!imputed)) P <- P + theme(legend.position="none")
  P
}
