## Author: Oskar Laverny
## Copyright: Oskar Laverny, oskar.laverny@gmail.com
## Date: 2018

#' @import mvtnorm
NULL

#' Boot Mack Chain Ladder model
#'
#' This function implement a simple bootstrap of the residuals from Mack model, potentially including a one-year risk view.
#'
#' @param Triangle Cumulative claims triangle. 
#' @param B The number of bootstrap resamples
#' @param distNy A parameter that sets the distribution of next-year incremental payments for the bootstrapping procedure. Either "normal" or "residuals", see details
#' @param threshold A value of NA (default) will prevent exclusion of residuals, but a numeric value (e.g 2) will exclude all residuals that have an absolution value greater than 2.
#' @param BF.premiums If a Bornhuetter-fergusson is needed, input a vector of Ultimates Premiums here. Otherwise, the BF code will not be triggered.
#' @param BF.param A vector of 2 integers that represent (respectively) the number of year of averaging Loss-ratios for the Bornhuetter Fergusson and then the number of year of applying the Bornhuetter Fergusson to.
#' @param stab A stabilisation parameter: forces developpements factors corresponding to developpements years greater than `stab` to be one.
#' @param clusters list of sequences of column numbers. See details.
#'
#' @details
#'
#' The bootstrap that is implemented here consist in a resampling of residuals obtained by the Mack model
#' (or simulated standard normal residuals if you choose so through the `distNy` parameter). On those resampled triangles, we construct a one-year
#' point of view of the Mack model, allowing us to bootstrap one-year quantities like the CDR or next year IBNRS.
#' Using this function properly, you could check that the proposed bootstrap is convergent with the Merz-Wuthrich formula
#' if you take standard normal residuals, but not otherwise.
#'
#' The residuals can also be clustered through the `clusters` argument, by providing clusters as a list of vectors of integers corresponding to each cluster:
#' `clusters=list(seq(1,10),seq(11,15))` for a triangle with 15 columns will cluster the residuals in two parts. The meaning of this parameter is that the residuals are resampled inside clusters only:
#' the underlying i.i.d hypothesis that is needed for the resampling can be tampered with through this parameterisation
#' 
#' For more details about the model, see the vignette.
#'
#' @return A BootMackChainLadder object with several results: bootstrapped ibnrs, ultimates, latests... 
#' You can `plot` it, `print` it and `str` it to extract information. 
#' Functions `mean`, `CDR` and `summary` can be used to extract informations as well.
#' @export
#'
#' @import magrittr
#'
#' @examples
#' data(ABC)
#' model <- BootMackChainLadder(Triangle = ABC, B = 100, distNy = "normal", threshold = 2)
#' mack <- MackChainLadder(ABC)
#' print(CDR(model))
#' print(CDR(mack)[, 1:2])
BootMackChainLadder <- function(Triangle, B = 100, distNy = "normal", threshold = NA, BF.premiums = NULL, BF.param = c(5, 5), stab = NA, clusters = NA) {
  if (!(distNy %in% c("normal", "residuals"))) {
    stop("DistNy Parameter must be 'normal' (classical MW) or 'residuals'")
  }


  if (!is.na(clusters)) {
    if (!(sort(unlist(clusters)) == seq_len(ncol(Triangle)))) {
      stop("The clusters should be provided as a list that unlist() to the set of integers 1:ncol(Triangle)")
    }
  }

  # Cf "One-year reserve risk including a tail factor : closed formula and bootstrapp aproach, 2011"

  # First step : Mack model.
  n <- dim(Triangle)[1]
  diag <- rev(.diag(Triangle))
  DF <- .DF(Triangle)
  DFIndiv <- .DFIndiv(Triangle)
  sigma <- .sigma(Triangle, DF, DFIndiv)
  residuals <- .residuals(Triangle, centered = TRUE, DF, DFIndiv, sigma)
  Ultimates <- .ultimates(Triangle, DF, stab = stab)
  IBNR <- .ibnr(Triangle, DF, stab = stab)

  # Step 2 : Resampling residuals.
  samples <- .sampling(residuals, B, threshold = threshold, stab = stab, clusters = clusters)
  sampledResiduals <- lapply(samples, .applysample, residuals)
  # Step3 : Calculation of booted estimators
  DFIndivBoot <- lapply(sampledResiduals, function(.x) {
    t(t(.x * sqrt(t(c(sigma^2, 0) / t(Triangle)))) + DF)
  })
  DFBoot <- lapply(DFIndivBoot, .DFPond, Triangle) # weighted by the original triangle !

  # Step 5 : Simulation of NY payments by a standard normal OR by the residuals... taking into account process error
  if (distNy == "normal") {
    NyCum <- lapply(DFBoot, function(.x) {
      rnorm(n = n - 1, mean = (diag * .x)[1:(n - 1)], sd = sqrt((diag * c(sigma^2, 0))[1:(n - 1)]))
    })
  }
  if (distNy == "residuals") {

    # To simulate the same thing with residuals assuming they are all i.i.d between developments years, we could do :
    if (!is.na(threshold)) { # FIrst, let's discard non-selected residuals.
      residuals[residuals[(!is.na(residuals))] > threshold] <- NA
    }

    # Stabilization :
    if (!is.na(stab)) {
      residuals[col(Triangle) > stab] <- NA
    }

    if (is.na(clusters)) {
      samples <- sample(residuals[!is.na(residuals)], size = (n - 1) * B, replace = TRUE)
      samples <- lapply(1:B, function(.x) {
        samples[((n - 1) * (.x - 1) + 1):((n - 1) * .x)]
      })
      NyCum <- mapply(function(.y, .x) {
        (.y * sqrt((diag * c(sigma^2, 0))[1:(n - 1)])) + (diag * .x)[1:(n - 1)]
      }, samples, DFBoot, SIMPLIFY = FALSE)
    } else {

      # e.g. clusters = list(c(1, 2), seq(3, 9), seq(10, 12), seq(13, n - 1))
      samples <- list()

      samples <- lapply(seq_len(length(clusters)), function(i) {
        x <- sample(residuals[, clusters[[i]]][!is.na(residuals[, clusters[[i]]])], size = length(clusters[[i]]) * B, replace = TRUE)
        x <- lapply(1:B, function(.x) {
          x[(length(clusters[[i]]) * (.x - 1) + 1):(length(clusters[[i]]) * .x)]
        })
        return(x)
      })

      samples2 <- lapply(1:B, function(i) {
        plop <- samples[[1]][[i]]
        for (j in 2:length(clusters)) {
          plop <- c(plop, samples[[j]][[i]])
        }
        return(plop)
      })


      NyCum <- mapply(function(.y, .x) {
        (.y * sqrt((diag * c(sigma^2, 0))[1:(n - 1)])) + (diag * .x)[1:(n - 1)]
      }, samples2, DFBoot, SIMPLIFY = FALSE)
    }
  }

  # Step 6 : Calculation of Ny estimators
  NyInc <- lapply(NyCum, function(.x) {
    .x - diag[1:(n - 1)]
  }) # Corresponding incremnts
  NyDFIndiv <- lapply(NyCum, function(.x) {
    .x / diag[1:(n - 1)]
  }) # Corresponding individual DF's
  NyDF <- lapply(NyDFIndiv, .newDFPond, DFIndiv, Triangle, stab) # DF
  NyUltimates <- mapply(function(.x, .y) {
    c(.x * rev(cumprod(rev(.y[2:n]))), Triangle[1, n])
  }, NyCum, NyDF, SIMPLIFY = FALSE)
  NyIBNR <- lapply(NyUltimates, function(.x) {
    .x - diag
  })



  # Stabilisation :
  if (!is.na(stab)) {

    # les DF :
    DF[(stab + 1):length(DF)] <- rep(1, length(DF) - stab)

    # et les DFBOOT :
    DFBoot <- lapply(DFBoot, function(rez) {
      rez[(stab + 1):length(rez)] <- rep(1, length(rez) - stab)
      return(rez)
    })
  }

  ######### Bf adjustement: takes the ultimate premiums as a parameter.
  if (!is.null(BF.premiums)) {
    Ultimates <- .BF.Ultimates(Ultimates, BF.premiums, DF, BF.param[1], BF.param[2])
    IBNR <- Ultimates - rev(diag)

    NyUltimates <- mapply(function(df, ultimate) {
      rev(.BF.Ultimates(rev(ultimate), BF.premiums, df, BF.param[1], BF.param[2]))
    }, NyDF, NyUltimates, SIMPLIFY = FALSE)

    NyIBNR <- lapply(NyUltimates, function(.x) {
      .x - diag
    })
  }
  # formatage et sortie :
  rez <- .formatOutput(n, Triangle, diag, DF, sigma, residuals, DFBoot, Ultimates, IBNR, NyCum, NyInc, NyDF, NyUltimates, NyIBNR)
  return(rez)
}
#' mean
#'
#' Calculate mean statiscics from the BootMackChainLadder object
#'
#' @param x A BootMackChainLadder Object
#' @param ... Nothing
#'
#' @return Three data.frames containing data per origin year ("ByOrigin"), by development year ("ByDev") and globally ("Totals")
#' @export
#'
#' @examples
#' data(ABC)
#' BMCL <- BootMackChainLadder(Triangle = ABC, B = 100, distNy = "residuals", threshold = 2)
#' mean(BMCL)
mean.BootMackChainLadder <- function(x, ...) {

  # the purpose of this function is to return means of everything BMCL returned.
  if (is.null(x$Latest)) { # so it's a light version of the model
    ByOrigin <- data.frame(
      Ultimates = x$Ultimates,
      IBNR = x$IBNR,
      NyUltimates = colMeans(x$NyUltimates),
      NyIBNR = colMeans(x$NyIBNR)
    )
    ByDev <- data.frame(
      NyDF = colMeans(x$NyDF)
    )
  } else {
    ByOrigin <- data.frame(
      Latest = x$Latest,
      Ultimates = x$Ultimates,
      IBNR = x$IBNR,
      NyInc = c(0, colMeans(x$NyInc)),
      NyCum = c(x$Ultimates[1], colMeans(x$NyCum)),
      NyUltimates = colMeans(x$NyUltimates),
      NyIBNR = colMeans(x$NyIBNR)
    )
    row.names(ByOrigin) <- rev(row.names(ByOrigin))
    ByDev <- data.frame(
      DFBoot = colMeans(x$DFBoot),
      NyDF = colMeans(x$NyDF)
    )
  }



  Totals <- colSums(ByOrigin)


  return(list(ByOrigin = ByOrigin, ByDev = ByDev, Totals = Totals))
}
#' CDR
#'
#' Calculate the one-year Claim developpement results from the BootMackChainLadder object
#'
#' @param x A BootMackChainLadder Object
#' @param ... other arguments for other methods.
#'
#' @return A data.Frame with one-year results from the bootstrap.
#' @export
#'
#' @examples
#' data(ABC)
#' BMCL <- BootMackChainLadder(Triangle = ABC, B = 100, distNy = "residuals", threshold = 2)
#' CDR(BMCL)
CDR.BootMackChainLadder <- function(x, ...) {
  Totals <- data.frame(IBNR = sum(x$IBNR), `CDR(1)S.E.` = sd(rowSums(x$NyIBNR)))
  row.names(Totals) <- "Total"
  names(Totals) <- c("IBNR", "CDR(1)S.E.")
  rbind(setNames(data.frame(IBNR = x$IBNR, `CDR(1)S.E.` = apply(x$NyIBNR, 2, sd)), names(Totals)), Totals)
}
#' @export
summary.BootMackChainLadder <- function(object, ...) {
  mean <- mean(object)
  cat("This is a BootMackChainLadder model \n\n")
  cat("Detailed results by Origin years : \n")
  print(format(mean$ByOrigin, format = "i", nsmall = 0, big.mark = ","))
  # print(mean$ByDev)
  cat("\n Totals across origin years : \n")
  xxx = t(t(mean$Totals))
  colnames(xxx) = 'Totals'
  print(xxx)
  return(invisible(mean))
}
#' print
#'
#' @param x the BootMackChainLadder object
#' @param ... nothing
#'
#' @return the BMCL object
#' @export
#'
#' @examples
#' data(ABC)
#' BMCL <- BootMackChainLadder(Triangle = ABC, B = 100, distNy = "normal", threshold = 2)
#' print(BMCL)
print.BootMackChainLadder <- function(x, ...) {
  summary(x)
}

#' MultiBootMackChainLadder
#'
#' The multi boot mack chain ladder algorythm computes todays and next-year estimates on a portefolio of several triangles, following closely a synchronised version of BootMackChainLadder.
#'
#' @param triangles A List of Triangles objects of the same size.
#' @param B The number of boostrap replicates
#' @param distNy The process distribution for next year increments. Either "normal" (default), "residuals.bycolumn","residuals.global" or "residuals". See details below.
#' @param names enventual names of the different triangles. IF set to NULL, the procedure will try to get names from the triangles list (first argument)
#' @param threshold Eventual exclusions limit for residuals. Set to NA (default) to avoid excluding anything.
#' @param stab Stabilisation parameter (number of CL coefs we keep). See the code.
#'
#' @details
#'
#' This function calculates, on a list of triangles, a synchronised bootstrap in the Mack model for the triangles.
#' The One-year point of view is also calculated.
#' It returns a specific S3 object, on wich you can use the standard functions like mean, CDR, NyIBNR, Corel provided by the package,
#' to produce usefull outputs. 
#'
#' This model uses the fact that the Mack model can be seen as a quasi-glm, and therefore provide resiuals.
#' Bootstrapping thoose residuals on the upper-left triangle allows to get bootstrap distribution of today's estimations (reserves, ultimates, ...).
#' Furthermore, if you simulate net year payments with a given process ditribution in each simulation, it gives 1 year results.
#'
#' The Multivariate dependence structure is induced by bootstrapping jointly the residueals of each triangles.
#'
#' If distNy = "normal", it follows *Boumezoued & al: One-year reserve risk including a tail factor : closed formula and bootstrapp aproach, 2011* and converges to the Merz-Wuthrich formula in the Braun model.
#' If distNy = "residuals", it also converges strongly but NOT to the Merz-Wuthrich formula, although each triangles is bootstrapped according to the Mack model, and has volatility parameters that matches the Mack model.
#' Notes that if you choose distNy = "residuals.bycolumn", the residuals will be resampled inside each column and not accross columns. 
#' Setting distNy = "residuals.global" or "residuals" triggers the same code.
#' 
#' For more details about the model, see the vignette: 
#' \code{vignette("BootMack", package = "ChainLadder")}
#'
#' @return a MBMCL object containing a list of BMCL objects and a little more.
#' @export
#'
#' @seealso BootMackChainLadder
#'
#' @examples
#' data(ABC)
#' triangles <- list(tri1 = ABC, tri2 = ABC, tri3 = ABC)
#' model <- MultiBootMackChainLadder(triangles, 100)
#' print(CDR(model))
#' print(mean(model))
MultiBootMackChainLadder <- function(triangles, B = 100, distNy = "normal", names = NULL, threshold = NA, stab = NA) {
  if (!(distNy %in% c("normal", "residuals.bycolumn", "residuals.global", "residuals"))) {
    stop("DistNy Parameter must be 'normal' (classical MW) or 'residuals'")
  }
  # Cf "One-year reserve risk including a tail factor : closed formula and bootstrapp aproach, 2011"

  # First step : deterministic statistics about the inputed triangles
  n <- dim(triangles[[1]])[1] # dimention des triangles
  N <- length(triangles) # nombre de triangles
  diag <- lapply(triangles, function(.x) {
    rev(.diag(.x))
  })
  DF <- lapply(triangles, .DF)
  DFIndiv <- lapply(triangles, .DFIndiv)
  sigma <- mapply(.sigma, triangles, DF, DFIndiv, SIMPLIFY = FALSE)
  residuals <- mapply(.residuals, triangles, center = TRUE, DF, DFIndiv, sigma, SIMPLIFY = FALSE)
  ibnr <- mapply(.ibnr, triangles, DF, stab, SIMPLIFY = FALSE)
  rho <- .rho(triangles, sigma, DFIndiv, DF)
  Ultimates <- mapply(.ultimates, triangles, DF, stab, SIMPLIFY = FALSE)

  # Second step : resampling and calculation of basic statistics on resampled triangles.


  # first step : tweaking residuals to exclude some of them.
  # the exclusions needs to be done synchronised. So our proposal is to replace positions that are excluded by positions from the same original column.

  # Let's calculate excluded positions :

  if (!is.na(threshold)) {
    # positions that pose problem :
    excluded_positions <- lapply(residuals, function(x) {
      which(abs(x) > threshold)
    })

    # on thoose positions, we have to replace the residuals that pose problem by one another from the same column.
    residuals <- mapply(function(res, pos) {
      for (i in seq_len(length(pos))) {
        col <- unique(floor(pos[i] / n) + 1)
        line <- which(abs(res[((col - 1) * n + 1):(col * n)]) > threshold)
        newrez <- sample(res[((col - 1) * n + 1):(col * n)][abs(res[((col - 1) * n + 1):(col * n)]) < threshold], size = length(line), replace = TRUE, prob = !is.na(res[((col - 1) * n + 1):(col * n)][abs(res[((col - 1) * n + 1):(col * n)]) < threshold]))
        for (j in seq_len(length(line))) {
          res[col, j] <- newrez[j]
        }
      }
      return(res)
    }, residuals, excluded_positions, SIMPLIFY = FALSE)
  }

  samples <- .sampling(Triangle = residuals[[1]], N = B, stab = stab) # same resampling for all N triangles

  sampledResiduals <- lapply(residuals, function(x) {
    lapply(samples, .applysample, x)
  })
  DFIndivBoot <- mapply(function(rez, DF, sigma, triangle) {
    lapply(rez, function(.x) {
      t(t(.x * sqrt(t(c(sigma^2, 0) / t(triangle)))) + DF)
    })
  }, sampledResiduals, DF, sigma, triangles, SIMPLIFY = FALSE) # individual development factors.

  DFBoot <- mapply(function(triangle, DFind) {
    lapply(DFind, .DFPond, triangle)
  }, triangles, DFIndivBoot, SIMPLIFY = FALSE) # DFs

  # simulation of correlated new residuals and formatting of those residuals.
  if (distNy == "normal") {
    # simu is a list of (n-1) B*N matrix
    simu <- lapply(1:(n - 1), function(i) {
      mvtnorm::rmvnorm(B, sigma = rho[i, , ])
    })
    simulation <- DFBoot
    for (c in 1:N) {
      for (b in 1:B) {
        simulation[[c]][[b]] <- sapply(simu, function(.x) {
          .x[b, c]
        })
      }
    }
  }
  if (distNy %in% c("residuals.global", "residuals")) {
    samples <- .sampling(residuals[[1]], B, stab = stab)
    samples <- lapply(samples, function(x) {
      as.vector(as.matrix(x)[!is.na(as.matrix(x))])[1:(n - 1)]
    })
    simulation <- lapply(residuals, function(r) {
      lapply(samples, function(s) {
        as.vector(as.matrix(r))[s]
      })
    })
  }
  if (distNy == "residuals.bycolumn") {
    samples <- lapply(1:(n - 1), function(i) {
      sample(1:(n - i), size = B, replace = TRUE)
    })
    simulation <- DFBoot
    for (a in 1:N) {
      for (b in 1:B) {
        simulation[[a]][[b]] <- sapply(1:(n - 1), function(c) {
          residuals[[a]][c, samples[[c]][b]]
        })
      }
    }
  }

  # Calculation of next year payments on thoose residuals.
  NyCum <- mapply(function(DFBoot, diag, sigma, simulation) {
    sd <- sqrt((diag * c(sigma^2, 0))[1:(n - 1)])
    mapply(function(.x, .y) {
      unname(.y * sd + (diag * .x)[1:(n - 1)])
    }, DFBoot, simulation, SIMPLIFY = FALSE)
  }, DFBoot, diag, sigma, simulation, SIMPLIFY = FALSE)


  # Corresponding incremental paymnts
  NyInc <- mapply(function(NyCum, diag) {
    lapply(NyCum, function(.x) {
      .x - diag[1:(n - 1)]
    })
  }, NyCum, diag, SIMPLIFY = FALSE)

  # Corresponding individual DF's
  NyDFIndiv <- mapply(function(NyCum, diag) {
    lapply(NyCum, function(.x) {
      rez <- .x / diag[1:(n - 1)]
      if (!is.na(stab)) {
        rez[(stab + 1):length(rez)] <- rep(1, length(rez) - stab)
      }
      return(rez)
    })
  }, NyCum, diag, SIMPLIFY = FALSE)

  # Corresponding Df's
  NyDF <- mapply(function(NyDFIndiv, DFIndiv, triangle) {
    # sabilisation:
    lapply(NyDFIndiv, .newDFPond, DFIndiv, triangle, stab)
  }, NyDFIndiv, DFIndiv, triangles, SIMPLIFY = FALSE)


  # Corresponding Ultimates
  NyUltimates <- mapply(function(triangle, NyCum, NyDF) {
    mapply(function(.x, .y) {
      c(.x * rev(cumprod(rev(.y[2:n]))), triangle[1, n])
    }, NyCum, NyDF, SIMPLIFY = FALSE)
  }, triangles, NyCum, NyDF, SIMPLIFY = FALSE)

  # corresponding IBNRs :
  NyIBNR <- mapply(function(y, z) {
    lapply(y, function(.x) {
      (.x - z)
    })
  }, NyUltimates, diag, SIMPLIFY = FALSE)


  # Formatting of outputs :
  # names of triangles :
  names <- names(triangles)
  if (is.null(names)) {
    names <- paste0("tri", 1:N)
  }

  # Stabilisation of developpement factors:
  if (!is.na(stab)) {
    DF <- lapply(DF, function(rez) {
      rez[(stab + 1):length(rez)] <- rep(1, length(rez) - stab)
      return(rez)
    })
    DFBoot <- lapply(DFBoot, function(DF) {
      lapply(DF, function(rez) {
        rez[(stab + 1):length(rez)] <- rep(1, length(rez) - stab)
        return(rez)
      })
    })
  }

  # the output will contains the marginals BootChainLadder models, retrieving the corelations will be done in external functions.
  rez <- mapply(.formatOutput, n, triangles, diag, DF, sigma, residuals, DFBoot, Ultimates, ibnr, NyCum, NyInc, NyDF, NyUltimates, NyIBNR, SIMPLIFY = FALSE)
  names(rez) <- names
  class(rez) <- c("MultiBootMackChainLadder", class(rez))
  return(rez)
}
#' mean
#'
#' @param x A MultiBootMackChainLadder Object
#' @param ... nothing
#'
#' @details
#'
#' Return mean informations about the MBMCL computed model. Gives the mean values of estimate quantities in a MBMCL bootstrap.
#'
#' @return Data frames containing mean informations.
#' @export
#'
#' @seealso MultiBootMackChainLadder, mean.BootMackChainLadder
#'
#' @examples
#' data(ABC)
#' triangles <- list(tri1 = ABC, tri2 = ABC, tri3 = ABC)
#' MBMCL <- MultiBootMackChainLadder(triangles, 100)
#' mean(MBMCL)
mean.MultiBootMackChainLadder <- function(x, ...) {
  margins <- lapply(x, mean)

  ByOrigin <- lapply(margins, `[[`, "ByOrigin")
  IBNR <- lapply(ByOrigin, `[[`, "IBNR")
  Ultimates <- lapply(ByOrigin, `[[`, "Ultimates")

  names(IBNR) <- paste0("IBNR", names(IBNR))
  names(Ultimates) <- paste0("Ultimates.", names(Ultimates))

  IBNR <- as.data.frame(IBNR)
  Ultimates <- as.data.frame(Ultimates)

  IBNR$IBNR.Tot <- rowSums(IBNR)
  Ultimates$Ultimates.Tot <- rowSums(Ultimates)

  ByOr <- cbind(IBNR, Ultimates)
  row.names(ByOr) <- row.names(ByOrigin[[1]])

  Totals <- as.data.frame(lapply(margins, `[[`, "Totals"))
  Totals$Tot <- rowSums(Totals)
  return(list(ByOrigin = ByOr, Totals = Totals))
}
#' CDR
#'
#' @param x A MultiBootMackChainLadder Object
#' @param ... other arguments for other methods.
#'
#' @details
#'
#' Give information about next year volatility for the MBMCL model.
#'
#' @return Informations about CDR and IBNRS.
#' @export
#'
#' @seealso MultiBootMackChainLadder, CDR.BootMackChainLadder
#'
#' @examples
#' data(ABC)
#' triangles <- list(tri1 = ABC, tri2 = ABC, tri3 = ABC)
#' MBMCL <- MultiBootMackChainLadder(triangles, 100)
#' CDR(MBMCL)
CDR.MultiBootMackChainLadder <- function(x, ...) {
  IBNR <- as.data.frame(lapply(x, `[[`, "IBNR"))
  names(IBNR) <- paste0("IBNR.", names(IBNR))
  IBNR$IBNR.Tot <- rowSums(IBNR)


  NyIBNR <- lapply(x, `[[`, "NyIBNR")
  NyIBNR$Tot <- Reduce("+", NyIBNR)
  CDR <- as.data.frame(lapply(NyIBNR, function(x) {
    apply(x, 2, sd)
  }))
  names(CDR) <- paste0("CDR(1).SE.", names(CDR))

  IBNR.tot <- as.vector(colSums(IBNR))
  CDR.tot <- lapply(NyIBNR, rowSums)
  CDR.tot <- as.data.frame(lapply(CDR.tot, sd))


  ByOrigin <- cbind(IBNR, CDR)
  Totals <- data.frame(IBNR = IBNR.tot, `CDR(1)S.E.` = t(CDR.tot))

  return(list(ByOrigin = ByOrigin, Total = Totals))
}
#' Next year IBNR
#'
#' @param x A MultiBootMackChainLadder Object
#' @param ByOrigin If Set to TRUE, Next year IBNRS will be rgiveng with per-origin details.
#'
#' @details
#'
#' Gives a data.frame with next-year payments informations found by the MBMCL model.
#'
#' @return Data frame with next year informations.
#' @export
#'
#' @examples
#' data(ABC)
#' triangles <- list(tri1 = ABC, tri2 = ABC, tri3 = ABC)
#' MBMCL <- MultiBootMackChainLadder(triangles, 100)
#' NyIBNR(MBMCL)
NyIBNR <- function(x, ByOrigin = FALSE) {
  NyIBNR <- lapply(x, `[[`, "NyIBNR")
  NyIBNR$Tot <- Reduce("+", NyIBNR)
  if (!ByOrigin) {
    NyIBNR <- lapply(NyIBNR, rowSums)
  }
  return(as.data.frame(NyIBNR))
}
#' Corel
#'
#' @param x A MultiBootMackChainLadder Object
#' @param ... Elements to be passed to the cor function
#'
#' @details
#'
#' Gives the correlations between the next-year IBNRs of the several triangles under the MBMCL model.
#'
#' @return A corelation matrix of next-year IBNRs.
#' @export
#'
#' @examples
#' data(ABC)
#' triangles <- list(tri1 = ABC, tri2 = ABC, tri3 = ABC)
#' MBMCL <- MultiBootMackChainLadder(triangles, 100)
#' Corel(MBMCL)
Corel <- function(x, ...) {
  NyIBNR <- NyIBNR(x)
  NyIBNR$Tot <- NULL
  return(cor(NyIBNR, ...))
}


##### Utility functions                                                                #####
.diag <- function(Triangle) {
  unname(rev(diag(Triangle[rev(seq_len(nrow(Triangle))), ])))
}
.DF <- function(Triangle) {
  # Simply returns Cl developpements factors
  n <- dim(Triangle)[1]
  Triangle2 <- Triangle
  Triangle2[col(Triangle2) == n - row(Triangle2) + sum(!is.na(Triangle2[, n]))] <- NA
  return(unname(c(colSums(Triangle[, 2:n], na.rm = TRUE) / colSums(Triangle2[, 1:(n - 1)], na.rm = TRUE), 1)))
}
.percent_dev <- function(DF, cumulative = TRUE) {
  if (cumulative) {
    1 / rev(cumprod(rev(DF)))
  } else {
    .firstdiff(1 / rev(cumprod(rev(DF))))
  }
}
.firstdiff <- function(x) {
  shifted <- c(0, x[1:(length(x) - 1)])
  x - shifted
}
.ultimates <- function(Triangle, df = .DF(Triangle), stab = NA) {
  # simply returns chain-ladder ultimates.
  if (!is.na(stab)) {
    df[(stab + 1):length(df)] <- rep(1, length(df) - stab)
  }
  .diag(Triangle) * cumprod(rev(df))
}
.ibnr <- function(Triangle, df = .DF(Triangle), stab = NA) {
  # simply returns the IBNR from classical chain-ladder model

  if (!is.na(stab)) {
    df[(stab + 1):length(df)] <- rep(1, length(df) - stab)
  }
  .diag(Triangle) * (cumprod(rev(df)) - 1)
}
.DFIndiv <- function(Triangle) {
  # Simply returns the Triangle of individual developpements factors.
  unname((cbind(Triangle, NA) / cbind(NA, Triangle))[, 2:(dim(Triangle)[[1]] + 1)])
}
.sigma <- function(Triangle, df = .DF(Triangle), DFIndiv = .DFIndiv(Triangle)) {
  # Returns Mack's estimate for the variances parameters sigma_j
  # Returns a line vector, indexed by Triangle columns.
  n <- dim(Triangle)[[1]]
  ecart <- DFIndiv - matrix(df, nrow = n, ncol = n, byrow = TRUE)
  rez <- Triangle * ecart^2
  sigma <- colSums(rez, na.rm = TRUE) / ((n - 2):(-1))

  # the last value is approximated by Mack's suggestion :
  sigma <- c(
    sigma[1:(n - 2)],
    min(
      sigma[n - 2]^2 / sigma[n - 3],
      sigma[n - 2],
      sigma[n - 3]
    )
  )
  if (is.nan(sigma[n - 1])) {
    sigma[n - 1] <- 0
  }
  # we return not sigma^2 but just sigma
  return(unname(sqrt(sigma)))
}
.rho <- function(triangles, sigma = lapply(triangles, .sigma), indivF = lapply(triangles, .DFIndiv), f = lapply(triangles, .DF)) {

  # Estimates the correlation matrix by collumn between different triangles:
  # Returns an array [N - 1 , NbTriangles,Nbtriangles] that contains correlations matrices between coumns of inputted triangles.
  # the `trianlges` arguments waits for a list of triangles.

  # Construt the output array:
  I <- dim(triangles[[1]])[1]
  rho <- array(NA, dim = c(I - 1, length(triangles), length(triangles)))

  # Construct rho:
  for (n in seq_along(triangles)) {
    for (m in seq_along(triangles)) {
      if (n == m) {
        # if n = m, this is the same triangle
        rho[, n, m] <- 1
      } else {
        # if n != m, then we construct a correlation matrix:

        for (j in 1:(I - 2)) {
          # we start by w_j^2
          omegaup <- 0
          omegadown <- 0
          for (i in 1:(I - (j - 1) - 1)) {
            # numerator
            omegaup <- omegaup + sqrt(triangles[[n]][i, j] * triangles[[m]][i, j])

            # denominator
            somme <- 0
            for (k in 1:(I - (j - 1) - 1)) {
              somme <- somme + triangles[[m]][k, j]
            }
            omegadown <- omegadown + triangles[[n]][i, j] * somme
          }
          omegaj <- omegaup^2 / omegadown

          cj <- 1 / (I - (j - 1) - 2 + omegaj)

          # the sum part of rho_j_n_m :
          somme <- 0
          for (l in 1:(I - (j - 1) - 1)) {
            somme <- somme + sqrt(triangles[[n]][l, j] * triangles[[m]][l, j]) * (indivF[[n]][l, j] - f[[n]][j]) * (indivF[[m]][l, j] - f[[m]][j]) / (sigma[[n]][j] * sigma[[m]][j])
          }
          # aggregation
          rho[j, n, m] <- cj * somme
        }

        # We still need to estimate the last factor
        # by polynomial projection (MACK 1997).
        rho[I - 1, n, m] <- min(
          (rho[I - 2, n, m] * sigma[[n]][I - 2] * sigma[[m]][I - 2])^2 / (rho[I - 3, n, m] * sigma[[n]][I - 3] * sigma[[m]][I - 3]),
          abs(rho[I - 3, n, m] * sigma[[n]][I - 3] * sigma[[m]][I - 3]),
          abs(rho[I - 2, n, m] * sigma[[n]][I - 2] * sigma[[m]][I - 2])
        )
      }
    }
  }
  # We return an array (j,n,m) with j the column index, n and m the triangles indices
  # If nan then 0:
  rho[is.nan(rho)] <- 0
  return(rho)
}
.residuals <- function(Triangle, centered = FALSE, DF = .DF(Triangle), DFIndiv = .DFIndiv(Triangle), sigma = .sigma(Triangle)) {
  # Estimation of Mack's residuals, based on England & Verral 2007
  # returns a Triangle of same size as the input, but with NA on the diagonal (no residuals for the diagonal)
  residus <- Triangle
  n <- dim(Triangle)[1]

  for (i in 1:n) {
    for (j in 1:n) {
      residus[i, j] <- sqrt((n - j) / (n - j - 1)) * sqrt(Triangle[i, j]) * (DFIndiv[i, j] - DF[j]) / sigma[j]
    }
  }
  residus[is.nan(residus)] <- 0
  if (centered) {
    residus <- apply(residus, 2, function(x) {
      return(x - mean(x, na.rm = TRUE))
    })
  }
  return(residus)
}
.sampling <- function(Triangle, N = 100, threshold = NA, stab = NA, clusters = NA) {
  # Bootstraps a triangle of positions
  # We build a list of triangles of bootstrapped positions :

  n <- length(Triangle)
  I <- dim(Triangle)[1]

  # Exclusion threshold for residuals
  Triangle2 <- Triangle
  if (!is.na(threshold)) {
    Triangle2[Triangle[(!is.na(Triangle))] > threshold] <- NA
  }


  # Stabilisation management:
  if (!is.na(stab)) {
    prob.stab <- col(Triangle) <= stab
  } else {
    prob.stab <- !is.na(Triangle2)
  }


  if (!is.na(clusters)) {


    # exemple clusters :
    # clusters <- list(c(1, 2), seq(3, 9), seq(10, 12), seq(13, I))

    samples <- lapply(seq_len(length(clusters)), function(i) {
      prob <- !is.na(Triangle2[, clusters[[i]]])
      positions <- matrix(1:n, nrow = I)
      return(lapply(1:N, function(x) {
        matrix(sample(positions[, clusters[[i]]],
          replace = TRUE,
          prob = prob * prob.stab[, clusters[[i]]],
          size = length(prob)
        ),
        nrow = I
        )
      }))
    })

    rez <- lapply(1:N, function(i) {
      return(do.call(cbind, lapply(samples, function(x) {
        x[[i]]
      })))
    })
  } else {
    prob <- !is.na(Triangle2)
    positions <- matrix(1:n, nrow = I)
    rez <- sample(positions, replace = TRUE, prob = prob * prob.stab, size = N * n)
    rez <- lapply(1:N, function(.x) {
      matrix(rez[(n * (.x - 1) + 1):(n * .x)], nrow = I)
    })
  }

  rez <- lapply(rez, function(x) {
    x[is.na(Triangle)] <- NA
    return(x)
  })
  return(rez)
}
.applysample <- function(sample, Triangle) {
  return(ChainLadder::as.triangle(
    matrix(as.vector(Triangle)[as.vector(sample)],
      nrow = dim(Triangle)
    )
  ))
}
.DFPond <- function(DFIndiv, ponderation) {
  n <- dim(ponderation)[1]
  ponderation[col(ponderation) == n - row(ponderation) + 1] <- NA
  rez <- colSums(ponderation * DFIndiv, na.rm = TRUE) / colSums(ponderation, na.rm = TRUE)
  rez[n] <- 1
  return(rez)
}
.newDFPond <- function(NyDFIndiv, DFIndiv, Triangle, stab = NA) {
  n <- dim(Triangle)[1]
  DFIndiv[col(DFIndiv) == n - row(DFIndiv) + 1] <- c(NyDFIndiv, 1)

  # stabilisation :
  if (!is.na(stab)) {
    DFIndiv[col(DFIndiv) > stab] <- 1
  }

  rez <- colSums(Triangle * DFIndiv, na.rm = TRUE) / colSums(Triangle, na.rm = TRUE)
  rez[n] <- 1
  return(rez)
}
.formatOutput <- function(n, Triangle, diag, DF, sigma, residuals, DFBoot, Ultimates, IBNR, NyCum, NyInc, NyDF, NyUltimates, NyIBNR) {
  # output formatting :
  NyCum <- do.call(rbind, lapply(NyCum, rev))
  NyInc <- do.call(rbind, lapply(NyInc, rev))
  NyUltimates <- do.call(rbind, lapply(NyUltimates, rev))
  NyIBNR <- do.call(rbind, lapply(NyIBNR, rev))
  NyDF <- do.call(rbind, NyDF)
  DFBoot <- do.call(rbind, DFBoot)

  names(Ultimates) <- rownames(Triangle)
  names(IBNR) <- rownames(Triangle)
  names(diag) <- rownames(Triangle)
  colnames(NyCum) <- rownames(Triangle)[2:(n)]
  colnames(NyInc) <- rownames(Triangle)[2:(n)]
  colnames(NyUltimates) <- rownames(Triangle)
  colnames(NyIBNR) <- rownames(Triangle)
  colnames(NyDF) <- colnames(Triangle)
  colnames(DFBoot) <- colnames(Triangle)

  result <- list(
    Latest = rev(diag),
    DF = DF,
    sigma = sigma,
    residuals = residuals,
    DFBoot = DFBoot,
    Ultimates = Ultimates,
    IBNR = IBNR,
    NyCum = NyCum,
    NyInc = NyInc,
    NyDF = NyDF,
    NyUltimates = NyUltimates,
    NyIBNR = NyIBNR
  )
  class(result) <- c("BootMackChainLadder", class(result))
  return(result)
}
.BF.Ultimates <- function(ultimes, primes, cadence, longeur_moyenne = 5, longeur_application = 5) {
  # la fonction de base est de prendre un BF 5ans 5ans. On peut le changer ici.
  n <- length(ultimes)
  a <- longeur_application
  b <- longeur_moyenne
  Poids <- c(rep(0, n - a - b), rep(1, b), rep(0, a))
  LR <- weighted.mean(x = ultimes / primes, w = Poids)
  Percent_dev <- rev(.percent_dev(cadence))
  Rez <- c(ultimes[1:(n - a)], (LR * primes * (1 - Percent_dev) + ultimes * (Percent_dev))[(n - a + 1):n])

  return(Rez)
}
