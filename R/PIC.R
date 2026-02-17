#' PaidIncurredChain
#'
#' The Paid-incurred Chain model (Merz, Wuthrich (2010)) combines 
#' claims payments and incurred losses information to get 
#' a unified ultimate loss prediction.
#' 
#' The method uses some basic properties of multivariate Gaussian distributions
#' to obtain a mathematically rigorous and consistent model for the combination
#' of the two information channels.
#' 
#' @param triangleP Cumulative claims payments triangle
#' @param triangleI Incurred losses triangle.
#' @return The function returns:
#' \itemize{
#'   \item \strong{Ult.Loss.Origin} Ultimate losses for different origin years.
#'   \item \strong{Ult.Loss} Total ultimate loss.
#'   \item \strong{Res.Origin} Claims reserves for different origin years.
#'   \item \strong{Res.Tot} Total reserve.
#'   \item \strong{s.e.} Square root of mean square error of prediction 
#'   for the total ultimate loss.
#' }
#' @details
#' We assume as usual that I=J. 
#' The model assumptions for the Log-Normal PIC Model are the following:
#' \itemize{
#'   \item Conditionally, given \eqn{\Theta = (\Phi_0,...,\Phi_I,
#'   \Psi_0,...,\Psi_{I-1},\sigma_0,...,\sigma_{I-1},\tau_0,...,\tau_{I-1})}{\Theta = (\Phi[0],...,\Phi[I],
#'   \Psi[0],...,\Psi[I-1],\sigma[0],...,\sigma[I-1],\tau[0],...,\tau[I-1])}
#'   we have
#'   \itemize{
#'     \item the random vector \eqn{(\xi_{0,0},...,\xi_{I,I},
#'     \zeta_{0,0},...,\zeta_{I,I-1})}{(\xi[0,0],...,\xi[I,I],
#'     \zeta[0,0],...,\zeta[I,I-1])} has multivariate Gaussian distribution
#'     with uncorrelated components given by
#'     \deqn{\xi_{i,j} \sim N(\Phi_j,\sigma^2_j),}{\xi[i,j] distributed as N(\Phi[j],\sigma^2[j]),}
#'     \deqn{\zeta_{k,l} \sim N(\Psi_l,\tau^2_l);}{\zeta[k,l] distributed as N(\Psi[l],\tau^2[l]);}
#'     \item cumulative payments are given by the recursion
#'     \deqn{P_{i,j} = P_{i,j-1} \exp(\xi_{i,j}),}{P[i,j] = P[i,j-1] * exp(\xi[i,j]),}
#'     with initial value \eqn{P_{i,0} = \exp (\xi_{i,0})}{P[i,0] = * exp (\xi[i,0])};
#'     \item incurred losses \eqn{I_{i,j}}{I[i,j]} are given by the backwards
#'     recursion
#'     \deqn{I_{i,j-1} = I_{i,j} \exp(-\zeta_{i,j-1}),}{I[i,j-1] = I[i,j] * exp(-\zeta[i,j-1]),}
#'     with initial value \eqn{I_{i,I}=P_{i,I}}{I[i,I] = P[i,I]}.
#'   }
#'   \item The components of \eqn{\Theta}{\Theta} are independent and 
#'   \eqn{\sigma_j,\tau_j > 0}{\sigma[j],\tau[j] > 0} for all j.
#'  }
#'  
#' 
#' Parameters \eqn{\Theta}{\Theta} in the model are in general not known and need to be
#' estimated from observations. They are estimated in a Bayesian framework.
#' In the Bayesian PIC model they assume that the previous assumptions 
#' hold true with deterministic \eqn{\sigma_0,...,\sigma_J}{\sigma[0],...,\sigma[J]} and 
#' \eqn{\tau_0,...,\tau_{J-1}}{\tau[0],...,\tau[J-1]} and
#' \deqn{\Phi_m \sim N(\phi_m,s^2_m),}{\Phi[m] distributed as N(\phi[m],s^2[m]),}
#' \deqn{\Psi_n \sim N(\psi_n,t^2_n).}{\Psi[n] distributed as N(\psi[n],t^2[n]).}
#' This is not a full Bayesian approach but has the advantage to give
#' analytical expressions for the posterior distributions and the prediction
#' uncertainty.
#' 
#' @note The model is implemented in the special case of non-informative priors.
#' @author Fabio Concina, \email{fabio.concina@@gmail.com}
#' @seealso \code{\link{MackChainLadder}},\code{\link{MunichChainLadder}}
#' @references Merz, M., Wuthrich, M. (2010). Paid-incurred chain claims reserving method. 
#' Insurance: Mathematics and Economics, 46(3), 568-579.
#' @examples
#' PaidIncurredChain(USAApaid, USAAincurred)
#' @export
PaidIncurredChain <- function(triangleP, triangleI) {

  triangleP <- checkTriangle(triangleP)
  triangleI <- checkTriangle(triangleI)
  if (nrow(triangleP) != ncol(triangleP) || nrow(triangleI) != ncol(triangleI)) {
    stop("Triangles must be square (same number of origin and development years).")
  }
  if (!identical(dim(triangleP), dim(triangleI))) {
    stop("Paid and incurred triangles must have the same dimensions.")
  }

  J <- ncol(triangleP)
  diagP <- getLatestCumulative(triangleP)[2:J]

  # log(P_{i,j}/P_{i,j-1}) triangle
  logP <- log(triangleP)
  fP <- matrix(NA_real_, nrow = J, ncol = J)
  fP[, 1] <- logP[, 1]
  if (J > 1) fP[, 2:J] <- logP[, 2:J] - logP[, 1:(J-1)]
  fP <- as.triangle(fP)

  # log(I_{i,j} / I_{i,j+1}) triangle
  logI <- log(triangleI)
  fI <- matrix(NA_real_, nrow = J, ncol = J)
  if (J > 1) fI[, 1:(J-1)] <- logI[, 1:(J-1)] - logI[, 2:J]
  fI <- as.triangle(fI)

  # sigma_j estimates, j=1,...,J-1
  sigma2.hat <- c(apply(fP[, 1:(J-1), drop = FALSE], 2, var, na.rm = TRUE), NA)
  # sigma_J estimated through log-linear regression
  n <- length(sigma2.hat)
  dev <- 1:n
  my.dev <- dev[!is.na(sigma2.hat) & sigma2.hat > 0]
  my.model <- lm(log(sigma2.hat[my.dev]) ~ my.dev)
  sigma2.hat[is.na(sigma2.hat)] <- exp(predict(my.model, newdata=data.frame(my.dev=dev[is.na(sigma2.hat)])))

  # tau_j estimates, j=1,...,J-2
  tau2.hat <- c(apply(fI[, 1:(J-2), drop = FALSE], 2, var, na.rm = TRUE), NA)
  # tau_{J-1} estimated through log-linear regression
  n <- length(tau2.hat)
  dev <- 1:n
  my.dev <- dev[!is.na(tau2.hat) & tau2.hat > 0]
  my.model <- lm(log(tau2.hat[my.dev]) ~ my.dev)
  tau2.hat[is.na(tau2.hat)] <- exp(predict(my.model, newdata=data.frame(my.dev=dev[is.na(tau2.hat)])))

  # w2_j estimates, j=1,...,J
  w2 <- cumsum(sigma2.hat)

  # v2_j estimates, j=1,...,J (Proposition 2.2 in Merz & Wuthrich 2010)
  v2 <- sum(sigma2.hat) + c(rev(cumsum(rev(tau2.hat))), 0)

  # (c_1,...,c_J,b_1,...,b_{J-1}) parameters
  c_par <- numeric(J)
  for (j in 1:J) {
    if (j == 1) {
      c_par[j] <- (1/sigma2.hat[j]) * sum(fP[1:J, j])
    } else if (j == 2) {
      c_par[j] <- (1/sigma2.hat[j]) * sum(fP[1:(J+1-j), j]) +
        sum(1/(v2[1:(j-1)] - w2[1:(j-1)]) *
        log(triangleI[J:(J-j+2), 1:(j-1)] / triangleP[J:(J-j+2), 1:(j-1)]))
    } else {
      diag_I <- diag(triangleI[J:(J-j+2), 1:(j-1)])
      diag_P <- diag(triangleP[J:(J-j+2), 1:(j-1)])
      c_par[j] <- (1/sigma2.hat[j]) * sum(fP[1:(J+1-j), j]) +
        sum(1/(v2[1:(j-1)] - w2[1:(j-1)]) *
        log(diag_I[1:(j-1)] / diag_P[1:(j-1)]))
    }
  }

  b_par <- numeric(J - 1)
  for (j in 1:(J-1)) {
    if (j == 1) {
      b_par[j] <- -(1/tau2.hat[j]) * sum(fI[1:(J-j), j]) -
        sum(1/(v2[1:j] - w2[1:j]) *
        log(triangleI[J:(J-j+1), 1:j] / triangleP[J:(J-j+1), 1:j]))
    } else {
      diag_I <- diag(triangleI[J:(J-j+1), 1:j])
      diag_P <- diag(triangleP[J:(J-j+1), 1:j])
      b_par[j] <- -(1/tau2.hat[j]) * sum(fI[1:(J-j), j]) -
        sum(1/(v2[1:j] - w2[1:j]) *
        log(diag_I[1:j] / diag_P[1:j]))
    }
  }

  # inverse covariance matrix
  A <- matrix(NA, nrow=(J + J - 1), ncol=(J + J - 1))
  for (n in 0:(J-1)) {
    for (m in 0:(J-1)) {
      if(n==m) {
        if (n==0) {
          A[n+1,m+1] <- (J - n)/sigma2.hat[n+1]	
        } else {
          A[n+1,m+1] <- ((J - 1) - n + 1)/sigma2.hat[n+1] +
          sum(1/(v2[1:(min(n-1,m-1)+1)] -
          w2[1:(min(n-1,m-1)+1)]))
        }
      } else {
        if (n==0 | m==0) {
          A[n+1,m+1] <- 0	
        } else {
          A[n+1,m+1] <- sum(1/(v2[1:(min(n-1,m-1)+1)] -
          w2[1:(min(n-1,m-1)+1)]))	
        }
      }		
    }
  }

  for (n in 0:(J-2)) {
    for (m in 0:(J-2)) {
      if(n==m) {
        A[J+n+1,J+m+1] <- (J-n-1)/tau2.hat[n+1] +
        sum(1/(v2[1:min(n+1,m+1)] -
        w2[1:min(n+1,m+1)]))
      } else {
	    A[J+n+1,J+m+1] <- sum(1/(v2[1:min(n+1,m+1)] - w2[1:min(n+1,m+1)]))	
      }
    }
  }

  for (n in 0:(J-1)) {
    for (m in 0:(J-2)) {	
      if (n==0 | m==0) {
        A[n+1,J+m+1] <- 0	
      } else {
        A[n+1,J+m+1] <- -sum(1/(v2[1:(min(n-1,m)+1)] -
        w2[1:(min(n-1,m)+1)]))	
      }				
    }
  }

  for (n in 0:(J-2)) {
    for (m in 0:(J-1)) {	
      if (n==0 | m==0) {
        A[J+n+1,m+1] <- 0	
      } else {
        A[J+n+1,m+1] <- -sum(1/(v2[1:(min(n,m-1)+1)] -
        w2[1:(min(n,m-1)+1)]))	
      }
    }
  }
  
  # the inverse of the inverse is the posterior covariance matrix 
  Ainv <- solve(A)

  # posterior parameters
  cb <- c(c_par, b_par)
  theta.post <- Ainv %*% cb

  # beta and s2.post parameters
  beta <- numeric(J - 1)
  for (i in 1:(J-1)) {
    beta[i] <- (v2[J] - w2[i]) / (v2[i] - w2[i])
  }

  s2.post <- numeric(J - 1)
  E <- matrix(NA_real_, nrow = (J-1), ncol = (2*J-1))
  for (i in 2:J) {
    e <- rep(0,J+1-i)
    e <- c(e,rep(1 - beta[J+1-i],i-1))
    e <- c(e,rep(0,J-i))
    e <- c(e,rep(beta[J+1-i],i-1))
    E[i-1,] <- e
    s2.post[i-1] <- e %*% Ainv %*% e
  }
  
  # ultimate loss vector
  PIC.Ult <- numeric(J - 1)
  for (i in 2:J) {
    PIC.Ult[i-1] <- triangleP[i,J+1-i]^(1 - beta[J+1-i]) *
    triangleI[i,J+1-i]^(beta[J+1-i]) * exp((1 - beta[J+1-i]) *
    sum(theta.post[(J-i+2):J]) + beta[J+1-i] *
    sum(theta.post[(2*J-i+1):(2*J-1)])) * exp((1 - beta[J+1-i]) *
    (v2[J] - w2[J-i+1])/2 + s2.post[i-1]/2)
  }
  PIC.UltTot <- sum(PIC.Ult)

  # claims reserves
  PIC.Ris <- PIC.Ult - diagP
  PIC.RisTot <- sum(PIC.Ris)

  # prediction uncertainty
  msep <- 0
  for (i in 2:J) {
    for (k in 2:J) {
      if (i==k) {
        msep <- msep + (exp((1-beta[J+1-i]) * (v2[J] - w2[J+1-i]) +
        E[i-1, ]%*% Ainv %*% E[k-1, ]) - 1) * PIC.Ult[i-1] * PIC.Ult[k-1]
      } else {
        msep <- msep + (exp(E[i-1, ] %*% Ainv %*% E[k-1, ]) - 1) *
        PIC.Ult[i-1] * PIC.Ult[k-1]	
      }
    }
  }
  PIC.se <- sqrt(msep)

  list(
    Ult.Loss.Origin = as.matrix(PIC.Ult),
    Ult.Loss = as.numeric(PIC.UltTot),
    Res.Origin = as.matrix(PIC.Ris),
    Res.Tot = as.numeric(PIC.RisTot),
    s.e. = as.numeric(PIC.se)
  )
}
