# Data Selection Method ----------------------------------------------------------
### function to inflate triangle from diagonal

inflate.triangle <- function(rate, Triangle) {
    Triangle <- checkTriangle(Triangle)
    
    n <- dim(Triangle)[1]
    
    infl_tr <- sapply(1:n, function(x) {
        diag(Triangle[, n:1])[n:1]/(1 + rate)^(n - x)
    })
    
    infl_final <- matrix(NA, ncol = n, nrow = n)
    
    
    for (i in 1:n) {
        infl_final[, i] <- c(t(infl_tr)[i:n, i], rep(NA, i - 1))
        
    }
    
    class(infl_final)<-c("triangle", class(infl_final))
    return(infl_final)
    
}

# Regression Method -------------------------------------------------------

### get triangular exponential regression coefficient

get_coeff_exp <- function(x, y) {
    model <- lm(log(y) ~ x, data = data.frame(x = x, y = y))
    
    a <- exp(model$coefficients[1])
    
    b <- model$coefficients[2]
    
    coeff <- c(a, b)
    
    return(coeff)
    
}

### get triangular linear regression coefficient

get_coeff_lin <- function(x, y) {
    model <- lm(y ~ x, data = data.frame(x = x, y = y))
    
    a <- model$coefficients[1]
    
    b <- model$coefficients[2]
    
    coeff <- c(a, b)
    
    return(coeff)
    
}

### Compute the paid adj triangle

BS.paid.adj <- function(Triangle_rep_counts = NULL, Triangle_closed, Triangle_paid, ult_counts = NULL, regression.type = "exponential") {
  
    if (is.null(Triangle_rep_counts) & is.null(ult_counts)) {
      stop("Provide a triangle of reported claim counts or a vector of Ultimate Claim Counts")
    }
    
    if (!is.null(Triangle_rep_counts) & !is.null(ult_counts)) {
      stop(
        "Provide either a triangle of reported claim counts or a vector of Ultimate Claim Counts"
      )
    }
    
    if (!(regression.type %in% c("exponential", "linear"))) {
      stop("Regression type admitted: exponential or linear")
    }
    
    if (!is.null(Triangle_rep_counts)) {
      Triangle_rep_counts <- checkTriangle(Triangle_rep_counts)
    }
    
    Triangle_closed <- checkTriangle(Triangle_closed)
    Triangle_paid <- checkTriangle(Triangle_paid)
    
    if (!is.null(Triangle_rep_counts)) {
      if (!all(
        dim(Triangle_rep_counts) == dim(Triangle_closed) &
        dim(Triangle_closed) == dim(Triangle_paid)
      )) {
        stop("Triangles of different dimensions")
      }
    } else {
      if (!all(dim(Triangle_closed) == dim(Triangle_paid))) {
        stop("Triangles of different dimensions")
      }
    }
    
    n <- dim(Triangle_paid)[1]
    
    if ((!is.null(ult_counts)) & (length(ult_counts) != n)) {
      stop("Ultimate Claim Counts provided do not match the triangles")
    }
    
    
    if (is.null(ult_counts)) {
      ult_counts <- MackChainLadder(Triangle_rep_counts)$FullTriangle[, n]
    } else {
      ult_counts <- ult_counts
    }
    
    disp_rate_tr <- sweep(Triangle_closed, 1, ult_counts, "/")
    
    disp_rate_sel <- diag(disp_rate_tr[, n:1])[n:1]
    
    full_adj_counts <-
      matrix(apply(expand.grid(disp_rate_sel, ult_counts), 1, prod),
             ncol = n,
             byrow = T)
    
    full_adj_counts[lower.tri(full_adj_counts)[, n:1]] <- NA
    
    
    a_tr <- matrix(NA, ncol = n, nrow = n)
    b_tr <- matrix(NA, ncol = n, nrow = n)
    
    for (i in (1:(n - 1))) {
      for (j in (1:(n - i))) {
        if (regression.type == "exponential") {
          a_tr[i, (j)] <-
            get_coeff_exp(x = Triangle_closed[i, j:(j + 1)], y = Triangle_paid[i, j:(j + 1)])[1]
          b_tr[i, (j)] <-
            get_coeff_exp(x = Triangle_closed[i, j:(j + 1)], y = Triangle_paid[i, j:(j + 1)])[2]
          
        } else {
          a_tr[i, (j)] <-
            get_coeff_lin(x = Triangle_closed[i, j:(j + 1)], y = Triangle_paid[i, j:(j + 1)])[1]
          b_tr[i, (j)] <-
            get_coeff_lin(x = Triangle_closed[i, j:(j + 1)], y = Triangle_paid[i, j:(j + 1)])[2]
        }
      }
    }
    
    
    in_range <- t(sapply(1:(n - 1), function(x) {
      c(findInterval(full_adj_counts[x, 1:(n - x + 1)], Triangle_closed[x, 1:(n - x + 1)]),
        rep(NA, x - 1))
    }))
    
    paid_adj <- matrix(NA, ncol = n, nrow = n)
    
    for (i in 1:(n - 1)) {
      for (j in 1:(n - i)) {
        if (in_range[i, j] != 0) {
          if (regression.type == "exponential") {
            paid_adj[i, j] <-
              a_tr[i, j] * exp(b_tr[i, j] * full_adj_counts[i, j])
          } else {
            paid_adj[i, j] <- a_tr[i, j] + b_tr[i, j] * full_adj_counts[i, j]
          }
          
        } else {
          if (regression.type == "exponential") {
            paid_adj[i, j] <-
              a_tr[i, 1] * exp(b_tr[i, 1] * full_adj_counts[i, j])
          } else {
            paid_adj[i, j] <- a_tr[i, 1] + b_tr[i, 1] * full_adj_counts[i, j]
          }
          
          
        }
      }
    }
    
    diag(paid_adj[, n:1]) <- diag(Triangle_paid[, n:1])
    
    
    return(as.triangle(paid_adj))
    
  }
