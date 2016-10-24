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
# In case need for aes_: see exchange and final solution at link below 
# http://stackoverflow.com/questions/9439256/
#   how-can-i-handle-r-cmd-check-no-visible-binding-for-
#   global-variable-notes-when

plotParms <- function(x, which, ncol, nrow, title, ...) UseMethod("plotParms")
plotParms.default <- function(x, ...){
  stop("No 'plotParms' method exists for objects of class ",
       class(x))
}
plotParms.ChainLadder <- function(x, which = c(1L, 3L, 4L, 6L), 
                                  ncol = min(2L, length(which)),
                                  nrow = ceiling(length(which) / ncol),
                                  title, ...) {
  grobs <- lapply(which, function(i)
    switch(i,
           plot.cl.f(x) + theme(axis.title.y=element_blank()),
           plot.cl.f1(x) + theme(axis.title.y=element_blank()),
           plot.cl.sigma(x) + theme(axis.title.y=element_blank()),
           plot.cl.f.se(x) + theme(axis.title.y=element_blank()),
           plot.cl.f.cv(x) + theme(axis.title.y=element_blank()),
           plot.cl.f1.cv(x) + theme(axis.title.y=element_blank())
    )
  )
  if (missing(title)) title <- paste0("chainladder(",
                                      x$TriangleName, 
                                      ") parameter estimates")
  
  marrangeGrob(grobs = grobs, ncol = ncol, nrow = nrow, top = title, ...)
}
plot.cl.f <- function(x) {
  n <- length(x$Models)
  smmry <- suppressWarnings(lapply(x$Models, summary))
  f <- sapply(smmry, function(x) x$coef["x","Estimate"])
  f.se <- sapply(smmry, function(x) x$coef["x","Std. Error"])
  legend_captions <- c("lm", "NA")
  src <- factor(legend_captions, levels = legend_captions)
  source <- rep(src[1L], n)
  source[is.na(f.se)] <- src[2L]
  xx <- factor(colnames(x$Triangle)[1:n], levels = colnames(x$Triangle)[1:n])
  df <- data.frame(xx, f.se, 
                   source,
                   stringsAsFactors = FALSE)
  P <- ggplot(df, aes(x = xx, y = f, colour = source)) +  
    geom_errorbar(aes(ymin=f-f.se, ymax=f+f.se), colour="black", width=.1, na.rm = TRUE) +
    xlab(names(dimnames(x$Triangle))[2L]) +
    geom_line(aes(group = 1), na.rm = TRUE) +
    geom_point() +
    ggtitle("f estimates") 
  P
}
plot.cl.f1 <- function(x) {
  n <- length(x$Models)
  smmry <- suppressWarnings(lapply(x$Models, summary))
  f <- sapply(smmry, function(x) x$coef["x","Estimate"]) - 1
  f.se <- sapply(smmry, function(x) x$coef["x","Std. Error"])
  legend_captions <- c("lm", "NA")
  src <- factor(legend_captions, levels = legend_captions)
  source <- rep(src[1L], n)
  source[is.na(f.se)] <- src[2L]
  xx <- factor(colnames(x$Triangle)[1:n], levels = colnames(x$Triangle)[1:n])
  df <- data.frame(xx, f.se, 
                   source,
                   stringsAsFactors = FALSE)
  P <- ggplot(df, aes(x = xx, y = f, colour = source)) +  
    geom_errorbar(aes(ymin=f-f.se, ymax=f+f.se), colour="black", width=.1, na.rm = TRUE) +
    xlab(names(dimnames(x$Triangle))[2L]) +
    geom_line(aes(group = 1), na.rm = TRUE) +
    geom_point() +
    ggtitle("f-1 estimates") 
  P
}
plot.cl.sigma <- function(x) {
  #  require(ggplot2)
  n <- length(x$Models)
  smmry <- suppressWarnings(lapply(x$Models, summary))
  sigma <- sapply(smmry, function(x) x$sigma)
  legend_captions <- c("lm", "NA")
  src <- factor(legend_captions, levels = legend_captions)
  source <- rep(src[1L], n)
  source[is.na(sigma)] <- src[2L]
  sigmaNoNAs <- sigma
  sigmaNoNAs[is.na(sigma)] <- 0
  xx <- factor(colnames(x$Triangle)[1:n], levels = colnames(x$Triangle)[1:n])
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
plot.cl.f.se <- function(x) {
  #  require(ggplot2)
  n <- length(x$Models)
  smmry <- suppressWarnings(lapply(x$Models, summary))
  f.se <- sapply(smmry, function(x) x$coef["x","Std. Error"])
  legend_captions <- c("lm", "NA")
  src <- factor(legend_captions, levels = legend_captions)
  source <- rep(src[1L], n)
  source[is.na(f.se)] <- src[2L]
  f.seNoNAs <- f.se
  f.seNoNAs[is.na(f.se)] <- 0
  xx <- factor(colnames(x$Triangle)[1:n], levels = colnames(x$Triangle)[1:n])
  df <- data.frame(xx, f.se, f.seNoNAs, 
                   source,
                   stringsAsFactors = FALSE)
  P <- ggplot(df, aes(x = xx, y = f.se, colour = source)) +  
    xlab(names(dimnames(x$Triangle))[2L]) +
    geom_line(aes(colour = source, group = 1), na.rm = TRUE) +
    geom_point(aes(y = f.seNoNAs)) +
    ggtitle("f.se estimates") 
  P
}
plot.cl.f.cv <- function(x) {
  n <- length(x$Models)
  smmry <- suppressWarnings(lapply(x$Models, summary))
  f <- sapply(smmry, function(x) x$coef["x","Estimate"])
  f.se <- sapply(smmry, function(x) x$coef["x","Std. Error"])
  f.cv <- f.se / f
  legend_captions <- c("calc", "NA")
  src <- factor(legend_captions, levels = legend_captions)
  source <- rep(src[1L], n)
  source[is.na(f.cv)] <- src[2L]
  xx <- factor(colnames(x$Triangle)[1:n], levels = colnames(x$Triangle)[1:n])
  df <- data.frame(xx, f.cv, 
                   source,
                   stringsAsFactors = FALSE)
  P <- ggplot(df, aes(x = xx, y = f.cv, colour = source)) +  
    xlab(names(dimnames(x$Triangle))[2L]) +
    ylab("cv(f)") +
    geom_line(aes(group = 1), na.rm = TRUE) +
    geom_point(na.rm = TRUE) +
    ggtitle("cv(f) estimates") 
  P
}
plot.cl.f1.cv <- function(x) {
  n <- length(x$Models)
  smmry <- suppressWarnings(lapply(x$Models, summary))
  f <- sapply(smmry, function(x) x$coef["x","Estimate"]) - 1
  f.se <- sapply(smmry, function(x) x$coef["x","Std. Error"])
  f.cv <- f.se / f
  legend_captions <- c("calc", "NA")
  src <- factor(legend_captions, levels = legend_captions)
  source <- rep(src[1L], n)
  source[is.na(f.cv)] <- src[2L]
  xx <- factor(colnames(x$Triangle)[1:n], levels = colnames(x$Triangle)[1:n])
  df <- data.frame(xx, f.cv, 
                   source,
                   stringsAsFactors = FALSE)
  P <- ggplot(df, aes(x = xx, y = f.cv, colour = source)) +  
    xlab(names(dimnames(x$Triangle))[2L]) +
    ylab("cv(f)") +
    geom_line(aes(group = 1), na.rm = TRUE) +
    geom_point(na.rm = TRUE) +
    ggtitle("cv(f-1) estimates") 
  P
}

# Now MackCL
plotParms.MackChainLadder <- function(x, which = c(1L, 3L, 4L, 6L), 
                                      ncol = min(2L, length(which)),
                                      nrow = ceiling(length(which) / ncol),
                                      title, ...) {
  grobs <- lapply(which, function(i)
    switch(i,
           plot.mackcl.f(x) + theme(axis.title.y=element_blank()),
           plot.mackcl.f1(x) + theme(axis.title.y=element_blank()),
           plot.mackcl.sigma(x) + theme(axis.title.y=element_blank()),
           plot.mackcl.f.se(x) + theme(axis.title.y=element_blank()),
           plot.mackcl.f.cv(x) + theme(axis.title.y=element_blank()),
           plot.mackcl.f1.cv(x) + theme(axis.title.y=element_blank())
    )
  )
  if (missing(title)) title <- paste0("MackChainLadder(",
                                      x$TriangleName, 
                                      ") parameter estimates")
    
  marrangeGrob(grobs = grobs, ncol = ncol, nrow = nrow, top = title, ...)
}
plot.mackcl.f <- function(x) {
  smmry <- suppressWarnings(lapply(x$Models, summary))
  f <- x$f
  n <- length(f)
  f.se <- x$f.se
  if (length(f.se) < n) f.se <- c(f.se, NA)
  # set display order
  legend_captions <- c("lm", "log-lin", "default", "input", "NA")
  src <- factor(legend_captions, levels = legend_captions)
  source <- rep(src[1L], n)
  source[n] <- src[ifelse(is.logical(x$tail.input), 
                          ifelse(x$tail.input, 2L, 3L),
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
plot.mackcl.f1 <- function(x) {
  smmry <- suppressWarnings(lapply(x$Models, summary))
  f <- x$f - 1
  n <- length(f)
  f.se <- x$f.se
  if (length(f.se) < n) f.se <- c(f.se, NA)
  # set display order
  legend_captions <- c("lm", "log-lin", "default", "input", "NA")
  src <- factor(legend_captions, levels = legend_captions)
  source <- rep(src[1L], n)
  source[n] <- src[ifelse(is.logical(x$tail.input), 
                          ifelse(x$tail.input, 2L, 3L),
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
  notregr <- sigma != sigmaregr
  notregr[is.na(notregr)] <- TRUE
  # set display order
  legend_captions <- 
    c("lm", "log-lin", "Mack", "input", "input-x", "NA")
  src <- factor(legend_captions, levels = legend_captions)
  source <- rep(src[1L], n)
  # For periods with insufficient data to estimate sigma,
  #   MackChainLadder has ways to inpute a value
  if (x$est.sigma[1] %in% "log-linear") source[notregr] <- src[2L] 
  else 
    if (x$est.sigma[1] %in% "Mack") source[notregr] <- src[3L]
    else source[notregr] <- src[4L]
  # tail
  if (is.null(x$tail.sigma.input)){
    if (x$f[n] > 1) source[n] <- src[2L]
    else source[n] <- src[6L]
  } else if (!x$tail.input) source[n] <- src[5L]
  else source[n] <- src[4L]
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
  sigma <- x$sigma
  if (length(sigma) < n) sigma <- c(sigma, NA)
  smmry <- suppressWarnings(lapply(x$Models, summary))
  f.seregr <- sapply(smmry, function(x) x$coef["x","Std. Error"])
  if (length(f.seregr) < n) f.seregr <- 
    c(f.seregr, rep(NA, n - length(f.seregr)))
  notregr <- f.se != f.seregr
  notregr[is.na(notregr)] <- TRUE
  # set display order
  legend_captions <- 
    c("lm", "log-lin", "calc", "Mack", "input", "input-x", "NA")
  src <- factor(legend_captions, levels = legend_captions)
  source <- rep(src[1L], n)
  # Currently, f.se (o/t tail) can get a value in two ways:
  #   As a result of the regression (notregr = FALSE) or as a result of 
  #   a calculation based on an imputed sigma and f (notregr = TRUE).
  source[notregr] <- src[3L]
  # tail.se can get a value in two ways: input or estimated (log-linear)
  # input: If argument tail=FALSE, then the user does not want a tail
  #   in the model, so although tail.se was provided, it will
  #   be ignored -- "input-x"
  # estimated:
  #   It will be estimated when the tail > 1.000.
  #   Here, we cannot take the tail from x$tail b/c 
  #     in MackChainLadder when argument tail = TRUE the output value 
  #     named "tail" is the 'lm' object used to estimate it (the tail). 
  #     Therefore, take actual tail value from last f to see if > unity.
  if (is.null(x$tail.se.input)) { # tail.se not provided
    if (x$f[n] > 1) source[n] <- src[2L] # tail.se est'd via log-linear
    else source[n] <- src[7L]
  } else if (!x$tail.input) source[n] <- src[6L] # tail not est'd so 
  #                                                input tail.se ignored
  else source[n] <- src[5L] # tail estimated therefore input tail.se used
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
  sigma <- x$sigma
  if (length(sigma) < n) sigma <- c(sigma, NA)
  f1.cv <- f.se/ (f - 1)
  smmry <- suppressWarnings(lapply(x$Models, summary))
  f.seregr <- sapply(smmry, function(x) x$coef["x","Std. Error"])
  if (length(f.seregr) < n) f.seregr <- 
    c(f.seregr, rep(NA, n - length(f.seregr)))
  notregr <- f.se != f.seregr
  notregr[is.na(notregr)] <- TRUE
  # set display order
  legend_captions <- c("calc", "NA")
  src <- factor(legend_captions, levels = legend_captions)
  # Currently, the cv is always the result of a calculation.
  #   Only exception (NA) is in the tail when argument tail=FALSE
  source <- rep(src[1L], n)
  if (is.logical(x$tail.input)) if (!x$tail.input) source[n] <- src[2L]
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
  sigma <- x$sigma
  if (length(sigma) < n) sigma <- c(sigma, NA)
  f.cv <- f.se/ f
  smmry <- suppressWarnings(lapply(x$Models, summary))
  f.seregr <- sapply(smmry, function(x) x$coef["x","Std. Error"])
  if (length(f.seregr) < n) f.seregr <- 
    c(f.seregr, rep(NA, n - length(f.seregr)))
  notregr <- f.se != f.seregr
  notregr[is.na(notregr)] <- TRUE

  # set display order
  legend_captions <- c("calc", "NA")
  src <- factor(legend_captions, levels = legend_captions)
  # Currently, the cv is always the result of a calculation.
  #   Only exception (NA) is in the tail when argument tail=FALSE
  source <- rep(src[1L], n)
  if (is.logical(x$tail.input)) if (!x$tail.input) source[n] <- src[2L]
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
