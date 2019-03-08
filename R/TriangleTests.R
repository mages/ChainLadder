library(ChainLadder)

checkTriangle<-function (Triangle){
  .dim <- dim(Triangle)
  if (length(.dim) > 3) {
    stop("Your array has too many dimensions.")
  }
  n <- .dim[2]
  m <- .dim[1]
  if (n > m) {
    stop("Number of origin periods, ", m, ", is less than the number of development periods, ", 
         n, ".\n")
  }
  if (length(.dim) == 3 & .dim[3] == 1) {
    dim(Triangle) <- c(m, n)
  }
  if ("data.frame" %in% class(Triangle)) {
    Triangle <- as.matrix(Triangle)
    Triangle <- as.triangle(Triangle)
    storage.mode(Triangle) <- "double"
  }
  tri.dimnames <- dimnames(Triangle)
  if (is.null(tri.dimnames[[1]])) {
    .origin <- 1:m
  }
  else {
    .origin <- tri.dimnames[[1]]
  }
  if (is.null(tri.dimnames[[2]])) {
    .dev <- 1:n
  }
  else {
    .dev <- tri.dimnames[[2]]
  }
  dimnames(Triangle) <- list(origin = .origin, dev = .dev)
  return(Triangle)
}


# Calendar Year Effect ----------------------------------------------------

cyeff.test<-function(Triangle,ci=.95){
  
  if(ci==1){
    stop("Select a confidence level less than 1")
  }
  
  Triangle<-checkTriangle(Triangle)
  
  n<-dim(Triangle)[1]
  
  atatriangle<-ata(Triangle)[1:n-1,]
  
  S_L_Triangle<-apply(atatriangle, 2, function(x){ifelse(x<median(x, na.rm = T),"S",ifelse(x>median(x, na.rm = T),"L","*"))} )
  
  S_L_Diags<-lapply(2:(n-1),function(i){diag(S_L_Triangle[1:i,i:1])})
  
  df<-data.frame(j=(2:(n-1)),S_j=rep(NA,n-2),L_j=rep(NA,n-2),Z_j=rep(NA,n-2),n=rep(NA,n-2),m=rep(NA,n-2),E_Zj=rep(NA,n-2),Var_Zj=rep(NA,n-2))
  
  for (i in 1:(n-2)){
    df$S_j[i]<-sum(S_L_Diags[[i]]=="S")
    df$L_j[i]<-sum(S_L_Diags[[i]]=="L")
    df$Z_j[i]<-min(df$S_j[i],df$L_j[i])
    df$n[i]<-df$S_j[i]+df$L_j[i]
    df$m[i]<-floor((df$n[i]-1)/2)
    df$E_Zj[i]<-df$n[i]/2-choose(df$n[i]-1,df$m[i])*df$n[i]/2^(df$n[i])
    df$Var_Zj[i]<-df$n[i]*(df$n[i]-1)/4-choose(df$n[i]-1,df$m[i])*df$n[i]*(df$n[i]-1)/2^(df$n[i])+df$E_Zj[i]-df$E_Zj[i]^2
  }
  
  Z_j<-sum(df$Z_j)
  E_Zj<-sum(df$E_Zj)
  Var_Zj<-sum(df$Var_Zj)
  Range<-c(E_Zj-qnorm(ci+(1-ci)/2,0,1)*sqrt(Var_Zj),E_Zj+qnorm(ci+(1-ci)/2,0,1)*sqrt(Var_Zj))
  
  cat("Calendar Year Effect:", !(Z_j >= Range[1] & Z_j <= Range[2]) )
  
  output<-list(test_table=df, Z=Z_j, E=E_Zj, Var=Var_Zj, Range=Range, ci=ci)
  class(output) <- c("CYTest", class(output))
  return(output)
 
  }

plot.CYTest<-function(object, ...){
  
  x<-seq(object$E-qnorm(.9999+(1-.9999)/2,0,1)*sqrt(object$Var),object$E+qnorm(.9999+(1-.9999)/2,0,1)*sqrt(object$Var),.01)
  cord.x <- c(object$Range[1],seq(object$Range[1],object$Range[2],.01),object$Range[2])
  cord.y <- c(0,dnorm(seq(object$Range[1],object$Range[2],.01),object$E, sqrt(object$Var)),0)
  
  plot(x,dnorm(x, object$E, sqrt(object$Var)),type="l", xlab="Z", ylab="Density", main="Calendar Year Effect", ...)
  polygon(cord.x,cord.y,col='gray', border=NA)
  segments(object$Z,0,object$Z,dnorm(object$Z,object$E, sqrt(object$Var)), lwd=2)
  
  }

print.CYTest <- function(x, ...) {
  
  cat("Calendar Year Effect")
  cat("\n\n")
  cat("Z =", x$Z)
  cat("\n\n")
  cat(x$ci*100,"%-Range = ( ", x$Range[1]," ; ",x$Range[2]," )", sep = "")
  cat("\n\n")
  cat("Calendar Year Effect:", !(x$Z >= x$Range[1] & x$Z <= x$Range[2]) )
}

summary.CYTest <- function(x, ...) {
  
  table<-x$test_table
  
  totals<-as.data.frame(c(sum(x$test_table$Z_j),sum(x$test_table$E_Zj),sum(x$test_table$Var_Zj)))
  rownames(totals)<-c("Z","E[Z]","Var[Z]")
  colnames(totals)<-c("Totals")
  
  
  range<-as.data.frame(c(x$Range[1],x$Range[2]))
  rownames(range)<-c("Lower","Upper")
  colnames(range)<-c("Value")
  
  output <- list(Table=table, Totals=totals, Range=range)
  
  
  
  return(output)
  }


# DF Correlation ----------------------------------------------------

dfcor.test<-function(Triangle,ci=.5){
  
  if(ci==1){
    stop("Select a confidence level less than 1")
  }
  
  Triangle<-checkTriangle(Triangle)
  
  Triangle<-RAA
  
  Triangle<-checkTriangle(Triangle)
  
  n<-dim(Triangle)[1]
  
  atatriangle<-ata(Triangle)[1:n-1,]
  
  cor_fun<-function(i, Triangle){
    cor(Triangle[,i],Triangle[,i+1],method="spearman",use="pairwise.complete.obs")
  }
  
  T_k<-sapply(1:(n-3), cor_fun,atatriangle)
  
  T_final<-weighted.mean(T_k, (n-3):1)
  
  Var_T<-1/((n-2)*(n-3)/2)
  
  Range<-c(-qnorm(ci+(1-ci)/2,0,1)*sqrt(Var_T),qnorm(ci+(1-ci)/2,0,1)*sqrt(Var_T))
  
  cat("Correlation between development factors:", !(T_final >= Range[1] & T_final <= Range[2]) )
  
  output<-list(T_stat=T_final, Var=Var_T, Range=Range, ci=ci)
  class(output) <- c("DFCTest", class(output))
  return(output)
  
}

plot.DFCTest<-function(object, ...){
  
  x<-seq(-qnorm(.9999+(1-.9999)/2,0,1)*sqrt(object$Var),qnorm(.9999+(1-.9999)/2,0,1)*sqrt(object$Var),.01)
  cord.x <- c(object$Range[1],seq(object$Range[1],object$Range[2],.01),object$Range[2])
  cord.y <- c(0,dnorm(seq(object$Range[1],object$Range[2],.01),0, sqrt(object$Var)),0)
  
  plot(x,dnorm(x, 0, sqrt(object$Var)),type="l", xlab="T", ylab="Density", main="Development Factor Correlation", ...)
  polygon(cord.x,cord.y,col='gray', border=NA)
  segments(object$T_stat,0,object$T_stat,dnorm(object$T_stat,0, sqrt(object$Var)), lwd=2)
  
}

print.DFCTest <- function(x, ...) {
  
  cat("Development Factor Correlation")
  cat("\n\n")
  cat("T =", x$T_stat)
  cat("\n\n")
  cat(x$ci*100,"%-Range = ( ", x$Range[1]," ; ",x$Range[2]," )", sep = "")
  cat("\n\n")
  cat("Development Factor Correlation:", !(x$T_stat >= x$Range[1] & x$T_stat <= x$Range[2]) )
}

summary.DFCTest <- function(x, ...) {
  
  table<-x$test_table
  
  results<-as.data.frame(c(x$T_stat,0,x$Var))
  rownames(results)<-c("T","E[T]","Var[T]")
  colnames(results)<-c("Value")
  
  
  range<-as.data.frame(c(x$Range[1],x$Range[2]))
  rownames(range)<-c("Lower","Upper")
  colnames(range)<-c("Value")
  
  output <- list(Results=results, Range=range)
  
  
  return(output)
}


# Tests -------------------------------------------------------------------


test<-cyeff.test(RAA)

plot(test)

summary(test)

print(test)


test2<-dfcor.test(RAA)

plot(test2)

summary(test2)

print(test2)







