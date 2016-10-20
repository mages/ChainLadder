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
  p3 <- plot.cl.f1.cv(x) + theme(axis.title.y=element_blank()) 
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
    xlab(names(dimnames(x$Triangle))[2L]) +
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
    xlab(names(dimnames(x$Triangle))[2L]) +
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
  f.cv <- f.se / f
  sigma <- sapply(smmry, function(x) x$sigma)
  na_sigma <- is.na(sigma)
  est_source <- rep("regres", n)
  est_source[na_sigma] <- "sigma=NA"
  est_source[f==1] <- "f=1"
  xx <- factor(colnames(x$Triangle)[1:n], levels = colnames(x$Triangle)[1:n])
  df <- data.frame(xx, f, f.se, sigma, na_sigma, f.cv, 
                   f.cv.point = f.cv,
                   est_source,
                   stringsAsFactors = FALSE)
  df$f.cv.point[est_source!="regres"] <- 0
  P <- ggplot(df, aes(x=xx, y=f.cv, colour = est_source)) +  
    xlab(names(dimnames(x$Triangle))[2L]) +
    ylab("cv(f)") +
    geom_line(aes(group = 1), na.rm = TRUE) +
    geom_point(aes_(y=~f.cv.point, colour = ~est_source), na.rm = TRUE) +
    ggtitle("cv(f) estimates") 
  P
}
plot.cl.f1.cv <- function(x) {
#  require(ggplot2)
  smmry <- suppressWarnings(lapply(x$Models, summary))
  f <- sapply(smmry, function(x) x$coef["x","Estimate"])
  n <- length(f)
  f.se <- sapply(smmry, function(x) x$coef["x","Std. Error"])
  f.cv <- f.se / (f-1)
  sigma <- sapply(smmry, function(x) x$sigma)
  na_sigma <- is.na(sigma)
  est_source <- rep("regres", n)
  est_source[na_sigma] <- "sigma=NA"
  est_source[f==1] <- "f=1"
  xx <- factor(colnames(x$Triangle)[1:n], levels = colnames(x$Triangle)[1:n])
  df <- data.frame(xx, f, f.se, sigma, na_sigma, f.cv, 
                   f.cv.point = f.cv,
                   est_source,
                   stringsAsFactors = FALSE)
  df$f.cv.point[est_source!="regres"] <- 0
  P <- ggplot(df, aes(x=xx, y=f.cv, colour = est_source)) +  
    xlab(names(dimnames(x$Triangle))[2L]) +
    ylab("cv(f-1)") +
    geom_line(aes(group = 1), na.rm = TRUE) +
    geom_point(aes_(y=~f.cv.point, colour = ~est_source), na.rm = TRUE) +
    ggtitle("cv(f-1) estimates") 
  P
}

# Now MackCL
plotParms.MackChainLadder <- function(x, title, ...) {
  p1 <- plot.mackcl.f(x) + theme(axis.title.y=element_blank())
  p2 <- plot.mackcl.f.se(x) + theme(axis.title.y=element_blank())
  p3 <- plot.mackcl.f1.cv(x) + theme(axis.title.y=element_blank()) 
  p4 <- plot.mackcl.sigma(x) + theme(axis.title.y=element_blank())
  if (missing(title)) title <- paste0("MackChainLadder(",
                                      x$TriangleName, 
                                      ") parameter estimates")
    
  marrangeGrob(grobs=list(p1, p2, p3, p4), ncol = 2, nrow = 2,
               top = title)
}
plot.mackcl.f <- function(x) {
  smmry <- suppressWarnings(lapply(x$Models, summary))
  f <- x$f
  n <- length(f)
  #f.se <- sapply(smmry, function(x) x$coef["x","Std. Error"])
  f.se <- x$f.se
  if (length(f.se) < n) f.se <- c(f.se, NA)
  # set the display order
  src <- factor(c("regres", "est'd", "default", "input", "NA"), 
                levels = c("regres", "est'd", "default", "input", "NA"))
  source <- rep(src[1L], n)
  source[n] <- src[ifelse(is.logical(x$tail), 
                          ifelse(x$tail, 2L, 3L),
                          4L)]
  source.se <- rep(src[1L], n)
  source.se[is.na(f.se)] <- src[5L]
  f.se[is.na(f.se)] <- 0
  xx <- factor(colnames(x$Triangle)[1:n], levels = colnames(x$Triangle)[1:n])
  df <- data.frame(xx, f, f.se, # sigma, na_sigma, f.cv, 
                   source, source.se,
                   stringsAsFactors = FALSE)
  P <- ggplot(df, aes(x = xx, y = f, colour = source)) +  
    geom_errorbar(aes(ymin=f-f.se, ymax=f+f.se), colour="black", width=.1) +
    xlab(names(dimnames(x$Triangle))[2L]) +
    geom_line(aes(group = 1), na.rm = TRUE) +
    geom_point() +
    ggtitle("f estimates") 
  P
}
plot.mackcl.sigma <- function(x) {
  sigma <- x$sigma
  n <- length(x$f)
  if (length(sigma) < n) sigma <- c(sigma, NA)
  smmry <- suppressWarnings(lapply(x$Models, summary))
  sigmaregr <- sapply(smmry, function(x) x$sigma)
  if (length(sigmaregr) < n) sigmaregr <- 
    c(sigmaregr, rep(NA, n - length(sigmaregr)))
  ndx <- sigma != sigmaregr
  ndx[is.na(ndx)] <- TRUE
  src <- factor(c("regres", "log-linear", "Mack", "input", "NA"), 
                levels = c("regres", "log-linear", "Mack", "input", "NA"))
  source <- rep(src[1L], n)
  if (x$est.sigma[1] %in% "log-linear") source[ndx] <- src[2L] 
  else 
    if (x$est.sigma[1] %in% "Mack") source[ndx] <- src[3L]
    else source[ndx] <- src[4L]
  source[n] <- src[ifelse(is.null(x$tail.sigma.input), 5L, 2L)]
  xx <- factor(colnames(x$Triangle)[1:n], levels = colnames(x$Triangle)[1:n])
  sigmaNoNAs <- sigma
  
  sigmaNoNAs[is.na(sigma)] <- 0
  df <- data.frame(xx, sigma, sigmaNoNAs, 
                   source,
                   stringsAsFactors = FALSE)
  P <- ggplot(df, aes(x = xx, y = sigma, colour = source)) +  
    xlab(names(dimnames(x$Triangle))[2L]) +
    geom_line(aes(colour = source, group = 1), na.rm = TRUE) +
    geom_point(aes(y=sigmaNoNAs)) +
    ggtitle("sigma estimates") 
  P
}
plot.mackcl.f.se <- function(x) {
  f.se <- x$f.se
  n <- length(x$f)
  if (length(f.se) < n) f.se <- c(f.se, NA)
  smmry <- suppressWarnings(lapply(x$Models, summary))
  f.seregr <- sapply(smmry, function(x) x$coef["x","Std. Error"])
  if (length(f.seregr) < n) f.seregr <- 
    c(f.seregr, rep(NA, n - length(f.seregr)))
  ndx <- f.se != f.seregr
  ndx[is.na(ndx)] <- TRUE
  src <- factor(c("regres", "log-linear", "Mack", "input", "NA"), 
                levels = c("regres", "log-linear", "Mack", "input", "NA"))
  source <- rep(src[1L], n)
  if (x$est.sigma[1] %in% "log-linear") source[ndx] <- src[2L] 
  else 
    if (x$est.sigma[1] %in% "Mack") source[ndx] <- src[3L]
  else source[ndx] <- src[4L]
  source[n] <- src[ifelse(is.null(x$tail.se.input), 5L, 2L)]
  xx <- factor(colnames(x$Triangle)[1:n], levels = colnames(x$Triangle)[1:n])
  f.seNoNAs <- f.se
  
  f.seNoNAs[is.na(f.se)] <- 0
  df <- data.frame(xx, f.se, f.seNoNAs, 
                   source,
                   stringsAsFactors = FALSE)
  P <- ggplot(df, aes(x = xx, y = f.se, colour = source)) +  
    xlab(names(dimnames(x$Triangle))[2L]) +
    geom_line(aes(colour = source, group = 1), na.rm = TRUE) +
    geom_point(aes(y=f.seNoNAs)) +
    ggtitle("f.se estimates") 
  P
}
plot.mackcl.f1.cv <- function(x) {
  f <- x$f
  n <- length(f)
  f.se <- x$f.se
  if (length(f.se) < n) f.se <- c(f.se, NA)
  f1.cv <- f.se/ (f - 1)
  smmry <- suppressWarnings(lapply(x$Models, summary))
  f.seregr <- sapply(smmry, function(x) x$coef["x","Std. Error"])
  if (length(f.seregr) < n) f.seregr <- 
    c(f.seregr, rep(NA, n - length(f.seregr)))
  ndx <- f.se != f.seregr
  ndx[is.na(ndx)] <- TRUE
  src <- factor(c("regres", "log-linear", "Mack", "input", "NA", "f=1"), 
                levels = c("regres", "log-linear", "Mack", "input", "NA", "f=1"))
  source <- rep(src[1L], n)
  if (x$est.sigma[1] %in% "log-linear") source[ndx] <- src[2L] 
  else 
    if (x$est.sigma[1] %in% "Mack") source[ndx] <- src[3L]
  else source[ndx] <- src[4L]
  source[n] <- src[ifelse(is.null(x$tail.f.se), 5L, 2L)]
  xx <- factor(colnames(x$Triangle)[1:n], levels = colnames(x$Triangle)[1:n])
  f1.cvNoNAs <- f1.cv
  f1.cvNoNAs[is.na(f.se)] <- 0
  df <- data.frame(xx, f1.cv, f1.cvNoNAs, 
                   source,
                   stringsAsFactors = FALSE)
  P <- ggplot(df, aes(x = xx, y = f1.cv, colour = source)) +  
    xlab(names(dimnames(x$Triangle))[2L]) +
    ylab("cv(f-1)") +
    geom_line(aes(colour = source, group = 1), na.rm = TRUE) +
    geom_point(aes(y=f1.cvNoNAs)) +
    ggtitle("cv(f-1) estimates") 
  P
}
plot.mackcl.f.cv <- function(x) {
  f <- x$f
  n <- length(f)
  f.se <- x$f.se
  if (length(f.se) < n) f.se <- c(f.se, NA)
  f.cv <- f.se/ f
  smmry <- suppressWarnings(lapply(x$Models, summary))
  f.seregr <- sapply(smmry, function(x) x$coef["x","Std. Error"])
  if (length(f.seregr) < n) f.seregr <- 
    c(f.seregr, rep(NA, n - length(f.seregr)))
  ndx <- f.se != f.seregr
  ndx[is.na(ndx)] <- TRUE
  src <- factor(c("regres", "log-linear", "Mack", "input", "NA", "f=1"), 
                levels = c("regres", "log-linear", "Mack", "input", "NA", "f=1"))
  source <- rep(src[1L], n)
  if (x$est.sigma[1] %in% "log-linear") source[ndx] <- src[2L] 
  else 
    if (x$est.sigma[1] %in% "Mack") source[ndx] <- src[3L]
  else source[ndx] <- src[4L]
  source[n] <- src[ifelse(is.null(x$tail.f.se), 5L, 2L)]
  xx <- factor(colnames(x$Triangle)[1:n], levels = colnames(x$Triangle)[1:n])
  f.cvNoNAs <- f.cv
  f.cvNoNAs[is.na(f.se)] <- 0
  df <- data.frame(xx, f.cv, f.cvNoNAs, 
                   source,
                   stringsAsFactors = FALSE)
  P <- ggplot(df, aes(x = xx, y = f.cv, colour = source)) +  
    xlab(names(dimnames(x$Triangle))[2L]) +
    ylab("cv(f)") +
    geom_line(aes(colour = source, group = 1), na.rm = TRUE) +
    geom_point(aes(y=f.cvNoNAs)) +
    ggtitle("cv(f) estimates") 
  P
}
