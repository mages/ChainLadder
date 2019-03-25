# Author: Marco De Virgilis
# Rationale: Set of functions to implement the two adjustments proposed by Berquist and Sherman.
# In particular:
# Inflate Triangle
# Paid Adjustment



# Data Selection Method ----------------------------------------------------------

### function to inflate triangle from diagonal

inflateTriangle <- function(Triangle, rate) {
    
    Triangle <- checkTriangle(Triangle)
    
    # Extract dimension from the triangle
    
    n <- dim(Triangle)[1]
    
    # deflate the latest diagonal in order to populate the triangle
    
    infl_tr <- sapply(1:n, function(x) {
        diag(Triangle[, n:1])[n:1]/(1 + rate)^(n - x)
    })
    
    # initialize the final output
    
    infl_final <- matrix(NA, ncol = n, nrow = n)
    
    # populate the final matrix
    
    for (i in 1:n) {
        infl_final[, i] <- c(t(infl_tr)[i:n, i], rep(NA, i - 1))
        
    }
    
    class(infl_final)<-c("triangle", class(infl_final))
    
    return(infl_final)
    
}

# Regression Method -------------------------------------------------------

### get triangular exponential regression coefficient

# define function that extract coefficients from a model Y = a * exp( b * x ) 

get_coeff_exp <- function(x, y) {
    
    # define model structure
  
    model <- lm(log(y) ~ x, data = data.frame(x = x, y = y))
    
    # save coefficients
    
    a <- exp(model$coefficients[1])
    
    b <- model$coefficients[2]
    
    # get final vector
    
    coeff <- c(a, b)
    
    return(coeff)
    
}

### get triangular linear regression coefficient

# define function that extract coefficients from a model Y = a + b * x

get_coeff_lin <- function(x, y) {
  
    # define the model structure  
  
    model <- lm(y ~ x, data = data.frame(x = x, y = y))
    
    # save coefficients
    
    a <- model$coefficients[1]
    
    b <- model$coefficients[2]
    
    # get final vector
    
    coeff <- c(a, b)
    
    return(coeff)
    
}

### Compute the paid adj triangle

BS.paid.adj <- function(Triangle.rep.counts = NULL, Triangle.closed, Triangle.paid, ult.counts = NULL, regression.type = "exponential") {
  
    # Initial checks on the provided inputs
  
    if (is.null(Triangle.rep.counts) & is.null(ult.counts)) {
      stop("Provide a triangle of reported claim counts or a vector of Ultimate Claim Counts")
    }
    
    if (!is.null(Triangle.rep.counts) & !is.null(ult.counts)) {
      stop(
        "Provide either a triangle of reported claim counts or a vector of Ultimate Claim Counts"
      )
    }
    
    if (!(regression.type %in% c("exponential", "linear"))) {
      stop("Regression type admitted: exponential or linear")
    }
    
    if (!is.null(Triangle.rep.counts)) {
      Triangle.rep.counts <- checkTriangle(Triangle.rep.counts)
    }
    
    Triangle.closed <- checkTriangle(Triangle.closed)
    Triangle.paid <- checkTriangle(Triangle.paid)
    
    if (!is.null(Triangle.rep.counts)) {
      if (!all(
        dim(Triangle.rep.counts) == dim(Triangle.closed) &
        dim(Triangle.closed) == dim(Triangle.paid)
      )) {
        stop("Triangles of different dimensions")
      }
    } else {
      if (!all(dim(Triangle.closed) == dim(Triangle.paid))) {
        stop("Triangles of different dimensions")
      }
    }
    
    # Define triangle dimensions
    
    n <- dim(Triangle.paid)[1]
    
    # check if the vector of ultimates provided matches the triangle
    
    if ((!is.null(ult.counts)) & (length(ult.counts) != n)) {
      stop("Ultimate Claim Counts provided do not match the triangles")
    }
    
    # assign / calculate the ultimates
    
    if (is.null(ult.counts)) {
      ult.counts <- MackChainLadder(Triangle.rep.counts)$FullTriangle[, n]
    } else {
      ult.counts <- ult.counts
    }
    
    # calculate disposal rates
    
    disp_rate_tr <- sweep(Triangle.closed, 1, ult.counts, "/")
    
    # retain the latest disposal rates, more reflective of the most recent experience
    
    disp_rate_sel <- diag(disp_rate_tr[, n:1])[n:1]
    
    # create the adjuste counts triangle based on the dsiposal rates just estimated
    
    full_adj_counts <-
      matrix(apply(expand.grid(disp_rate_sel, ult.counts), 1, prod),
             ncol = n,
             byrow = TRUE)
    
    # the lower triangle is not needed
    
    full_adj_counts[lower.tri(full_adj_counts)[, n:1]] <- NA
    
    
    # prepare two matrices, a and b, to allocate the regression coefficients
    
    a_tr <- matrix(NA, ncol = n, nrow = n)
    b_tr <- matrix(NA, ncol = n, nrow = n)
    
    # populate the matrices just created
    
    for (i in (1:(n - 1))) {
      for (j in (1:(n - i))) {
        if (regression.type == "exponential") {
          a_tr[i, (j)] <-
            get_coeff_exp(x = Triangle.closed[i, j:(j + 1)], y = Triangle.paid[i, j:(j + 1)])[1]
          b_tr[i, (j)] <-
            get_coeff_exp(x = Triangle.closed[i, j:(j + 1)], y = Triangle.paid[i, j:(j + 1)])[2]
          
        } else {
          a_tr[i, (j)] <-
            get_coeff_lin(x = Triangle.closed[i, j:(j + 1)], y = Triangle.paid[i, j:(j + 1)])[1]
          b_tr[i, (j)] <-
            get_coeff_lin(x = Triangle.closed[i, j:(j + 1)], y = Triangle.paid[i, j:(j + 1)])[2]
        }
      }
    }
    
    # define if the estimated values are within the range which the regression is based upon
    
    in_range <- t(sapply(1:(n - 1), function(x) {
      c(findInterval(full_adj_counts[x, 1:(n - x + 1)], Triangle.closed[x, 1:(n - x + 1)]),
        rep(NA, x - 1))
    }))
    
    # initializa adjusted paid triangle that will serve as output
    
    paid_adj <- matrix(NA, ncol = n, nrow = n)
    
    # populate the final matrix
    
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
    
    
    # get the diagonal from the initial trinagle, i.e. the digonal is not adjusted
    
    diag(paid_adj[, n:1]) <- diag(Triangle.paid[, n:1])
    
    # return the final output as an object of class triangle
    
    return(as.triangle(paid_adj))
    
  }
