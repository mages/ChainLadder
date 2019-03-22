# Author: Marco De Virgilis
# Rationale: Set of functions to test different metrics of run off triangles.
# In particular:
# Calendar Year Effect
# Test Correlation between subsequent development factor
# Check level of infaltion year on year


# Calendar Year Effect ----------------------------------------------------

cyeff.test <- function(Triangle, ci = 0.95) {
    if (ci == 1) {
        stop("Select a confidence level less than 1")
    }
    
    Triangle <- checkTriangle(Triangle)
    
    n <- dim(Triangle)[1]
    
    # Calculate ata
    
    atatriangle <- ata(Triangle)[1:n - 1, ]
    
    # Check smaller or larger ata
    
    S_L_Triangle <- apply(atatriangle, 2, function(x) {
        ifelse(x < median(x, na.rm = T), "S", ifelse(x > median(x, na.rm = T), "L", "*"))
    })
    
    # collect the diagonals
    
    S_L_Diags <- lapply(2:(n - 1), function(i) {
        diag(S_L_Triangle[1:i, i:1])
    })
    
    # create final dataframe
    
    df <- data.frame(j = (2:(n - 1)), S_j = rep(NA, n - 2), L_j = rep(NA, n - 2), Z_j = rep(NA, n - 2), n = rep(NA, n - 2), m = rep(NA, n - 2), E_Zj = rep(NA, 
        n - 2), Var_Zj = rep(NA, n - 2))
    
    for (i in 1:(n - 2)) {
        df$S_j[i] <- sum(S_L_Diags[[i]] == "S")
        df$L_j[i] <- sum(S_L_Diags[[i]] == "L")
        df$Z_j[i] <- min(df$S_j[i], df$L_j[i])
        df$n[i] <- df$S_j[i] + df$L_j[i]
        df$m[i] <- floor((df$n[i] - 1)/2)
        df$E_Zj[i] <- df$n[i]/2 - choose(df$n[i] - 1, df$m[i]) * df$n[i]/2^(df$n[i])
        df$Var_Zj[i] <- df$n[i] * (df$n[i] - 1)/4 - choose(df$n[i] - 1, df$m[i]) * df$n[i] * (df$n[i] - 1)/2^(df$n[i]) + df$E_Zj[i] - df$E_Zj[i]^2
    }
    
    # calculate final metrics
    
    Z_j <- sum(df$Z_j)
    E_Zj <- sum(df$E_Zj)
    Var_Zj <- sum(df$Var_Zj)
    Range <- c(E_Zj - qnorm(ci + (1 - ci)/2, 0, 1) * sqrt(Var_Zj), E_Zj + qnorm(ci + (1 - ci)/2, 0, 1) * sqrt(Var_Zj))
    
    output <- list(test_table = df, Z = Z_j, E = E_Zj, Var = Var_Zj, Range = Range, ci = ci)
    class(output) <- c("cyeff.test", class(output))
    return(output)
    
}

# function to plot the ci of a CYTest class

plot.cyeff.test <- function(x, type = "l", xlab = "Z", ylab = "Density", main = "Calendar Year Effect", col.area ="gray", border = NA, ...) {
    x_seq <- seq(x$E - qnorm(0.9999 + (1 - 0.9999)/2, 0, 1) * sqrt(x$Var), x$E + qnorm(0.9999 + (1 - 0.9999)/2, 0, 1) * sqrt(x$Var), 0.01)
    cord.x <- c(x$Range[1], seq(x$Range[1], x$Range[2], 0.01), x$Range[2])
    cord.y <- c(0, dnorm(seq(x$Range[1], x$Range[2], 0.01), x$E, sqrt(x$Var)), 0)
    
    plot(x_seq, dnorm(x_seq, x$E, sqrt(x$Var)), type = type, xlab = xlab, ylab = ylab, main = main, ...)
    polygon(cord.x, cord.y, col = col.area, border = border)
    segments(x$Z, 0, x$Z, dnorm(x$Z, x$E, sqrt(x$Var)), lwd = 2)
    
}

# function to print the results of a CYTest class

print.cyeff.test <- function(x, ...) {
    cat("Calendar Year Effect")
    cat("\n\n")
    cat("Z =", x$Z)
    cat("\n\n")
    cat(x$ci * 100, "%-Range = ( ", x$Range[1], " ; ", x$Range[2], " )", sep = "")
    cat("\n\n")
    cat("Calendar Year Effect:", !(x$Z >= x$Range[1] & x$Z <= x$Range[2]))
}

# summary function of a CYTest class

summary.cyeff.test <- function(object, ...) {
    
  table <- object$test_table
    
  totals <- as.data.frame(c(sum(object$test_table$Z_j), sum(object$test_table$E_Zj), sum(object$test_table$Var_Zj)))
  rownames(totals) <- c("Z", "E[Z]", "Var[Z]")
  colnames(totals) <- c("Totals")
    
  range <- as.data.frame(c(object$Range[1], object$Range[2]))
  rownames(range) <- c("Lower", "Upper")
  colnames(range) <- c("Value")
  
  output <- list(Table = table, Totals = totals, Range = range)
    
  return(output)
}


# DF Correlation ----------------------------------------------------

dfcor.test <- function(Triangle, ci = .5) {
    if (ci == 1) {
        stop("Select a confidence level less than 1")
    }
    
    Triangle <- checkTriangle(Triangle)
    
    n <- dim(Triangle)[1]
    
    atatriangle <- ata(Triangle)[1:n - 1, ]
    
    # calculate rank correlation
    
    cor_fun <- function(i, Triangle) {
        cor(Triangle[, i], Triangle[, i + 1], method = "spearman", use = "pairwise.complete.obs")
    }
    
    T_k <- sapply(1:(n - 3), cor_fun, atatriangle)
    
    T_final <- weighted.mean(T_k, (n - 3):1)
    
    Var_T <- 1/((n - 2) * (n - 3)/2)
    
    Range <- c(-qnorm(ci + (1 - ci)/2, 0, 1) * sqrt(Var_T), qnorm(ci + (1 - ci)/2, 0, 1) * sqrt(Var_T))
    
    #return summary statistics
    
    output <- list(T_stat = T_final, Var = Var_T, Range = Range, ci = ci)
    class(output) <- c("dfcor.test", class(output))
    return(output)
    
}

# function to plot the ci of a dfcor.test class

plot.dfcor.test <- function(x, type = "l", xlab = "T", ylab = "Density", main = "Development Factor Correlation", col.area ="gray", border = NA, ...) {
    
    x_seq <- seq(-qnorm(0.9999 + (1 - 0.9999)/2, 0, 1) * sqrt(x$Var), qnorm(0.9999 + (1 - 0.9999)/2, 0, 1) * sqrt(x$Var), 0.01)
    cord.x <- c(x$Range[1], seq(x$Range[1], x$Range[2], 0.01), x$Range[2])
    cord.y <- c(0, dnorm(seq(x$Range[1], x$Range[2], 0.01), 0, sqrt(x$Var)), 0)
    
    plot(x_seq, dnorm(x_seq, 0, sqrt(x$Var)), type = type, xlab = xlab, ylab = ylab, main = main, ...)
    polygon(cord.x, cord.y, col = col.area, border = border)
    segments(x$T_stat, 0, x$T_stat, dnorm(x$T_stat, 0, sqrt(x$Var)), lwd = 2)
    
}

# function to print the results of a CYTest class

print.dfcor.test <- function(x, ...) {
    cat("Development Factor Correlation")
    cat("\n\n")
    cat("T =", x$T_stat)
    cat("\n\n")
    cat(x$ci * 100, "%-Range = ( ", x$Range[1], " ; ", x$Range[2], " )", sep = "")
    cat("\n\n")
    cat("Development Factor Correlation:", !(x$T_stat >= x$Range[1] & x$T_stat <= x$Range[2]))
}

# summary function of a CYTest class

summary.dfcor.test <- function(object, ...) {
    table <- object$test_table
    
    results <- as.data.frame(c(object$T_stat, 0, object$Var))
    rownames(results) <- c("T", "E[T]", "Var[T]")
    colnames(results) <- c("Value")
    
    
    range <- as.data.frame(c(object$Range[1], object$Range[2]))
    rownames(range) <- c("Lower", "Upper")
    colnames(range) <- c("Value")
    
    output <- list(Results = results, Range = range)
    
    
    return(output)
}

# AY Inflation ------------------------------------------------------------

# defining function that calculates rates considering an exponential model

exp.infl <- function(i, Triangle) {
  
  Triangle <- checkTriangle(Triangle)
  
  n <- dim(Triangle)[1]  
  
  model <- lm(log(y) ~ x, data = data.frame(y = Triangle[, i], x = 1:n))
  fl_rate <- exp(model$coefficients[2]) - 1
  
  r <- model$residuals
  
  f <- model$fitted.values
  
  mss <- sum((f - mean(f))^2)
  
  rss <- sum(r^2)
  
  final <- c(fl_rate, mss/(mss + rss), length(f))
  
  names(final) <- c("rate", "R2", "Points")
  
  return(final)
  
}

# apply the function to a triangle

check.tr.infl <- function(Triangle) {
  Triangle <- checkTriangle(Triangle)
  
  n <- dim(Triangle)[1]
  
  summ <- sapply(1:(n - 1), exp.infl, Triangle)
  
  output <- list(Triangle = Triangle, summ_table = summ)
  
  class(output) <- c("check.tr.infl", class(output))
  
  return(output)
  
}

plot.check.tr.infl <- function(x, col.line="black", type="b", xlab="dev. period", ylab=NULL, ...){
  
  dft<-as.data.frame(x$Triangle)
  
  n<-nrow(x$Triangle)  
  
  df<-dft[which(!is.na(dft$value) & dft$origin!=n), ]
  
  xyplot(
    value ~ dev | factor(origin),
    data = df,
    type = type,
    as.table = T,
    xlab = xlab,
    ylab = ylab,
    
    panel = function(x, y, ...) {
      panel.xyplot(x, y, ...)
      fm <- lm(log(y) ~ x)
      panel.lines(x, exp(fitted(fm)), col.line = col.line)
    },
    
    ylim = c(0,1.15*max(df$value,na.rm=T)), ...)
  
}

print.check.tr.infl <- function(x, ...){
  
  colnames(x$summ_table)<-as.character(1:ncol(x$summ_table))
  cat("Triangle Inflation Calculation")
  cat("\n\n")
  print(x$summ_table)
  
}

summary.check.tr.infl <- function(object, ...){

  summ_table <- object$summ_table
   
  colnames(summ_table)<-as.character(1:ncol(object$summ_table))
   
  return(summ_table)
  
}
