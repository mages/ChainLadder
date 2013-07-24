## Author: Dan Murphy
## Copyright: Daniel Murphy, chiefmurphy@gmail.com
## Date: 10/9/2008
##       3/20/2013
#

LRfcnPair <- function(a, B, E) sum(B ^ (1 - a) * E) / sum(B ^ (2 - a))
LRfcnPairD <- function(a, B, E, s) LRfcnPair(a, B, E) - s
LRdist <- function(a, B, E, s) abs(LRfcnPair(a, B, E) - s)
.approxeq <- function (x, y, tolerance = .Machine$double.eps^0.5) abs(x - y) < tolerance

CLFMdelta <- function(Triangle, selected, tolerance = .0005, ...){ #step.d = 1, ...){
  step.d <- 1
  # Find the value of delta such that coef(Triangle, delta=delta) = selected
  # per the CLFM paper by Bardis, Majidi, & Murphy (Variance, 2013)
  # Note that the paper refered to the parameter as alpha. The name is changed
  # to 'delta' in this package to conform to already-existing parameterization
  # of the chainladder() function. 
    #   Calculate the "major" averages: VW (delta=1), SA (2), RA (0)
    #   If selected factor is within tolerance of a major average *in that order*
    #       set delta=indicated integer and we're done.
    
  # Algorithm originally written with development period named "age" (U.S. custom).
  # Also, algoritm originally written for a Triangle in long format and 
  #   a more involved "selected" object derived from the Triangle. 
  #   It will be easier to align "selected" and "Triangle" if latter is a matrix. 
  Triangle <- as.triangle(Triangle, dev = "age")
  if (length(selected) != (ncol(Triangle) - 1)) stop("length(selected) must equal ncol(Triangle)-1")
#  if (!all.equal(names(selected), head(colnames(Triangle), -1))) 
#    warning("selected does not have the same names as columns of Triangle")

  # Calculate the optimal alpha's per the FFM paper
  # for Triangle tri and the selected link ratios.
  # tdf is the original triangle in df format
  # tdfBegin is the original triangle where the beginning values of 
  #   each development period may have been adjusted for 
  #   missing values or zero values.
  #   beginvalue's can still be zero if (end)value was zero.
  #   We must ignore those (0,0) observations.
  # Wish we could use optimize, but we define optimal to be the least
  #   optimal value (in absolute value), and there's no guarantee that,
  #   in case of a tie, optimize won't return the greater value.
  # Return vector of optimal alphas.
  
  Ka <- length(selected) # number of ata's excluding tail
  K  <- ncol(Triangle) # number of columns in triangle
  ata <- selected
  ages <- head(colnames(Triangle), -1)

  # a is the vector of alpha's to search over for optimality
  # maxa = right endpoint of search interval
  # So search interval = [0,10] on positive side, [-10,0] on negative side
# Change 8/19/11 different positive and negative widths, coincide with PSItable
  maxaPos <- 8
  maxaNeg <- 4
  
  # Start off w/ NAs. NA's at end will be informative
  oavec <- structure(rep(as.numeric(NA), Ka), names = ages)
  # We'll flag any index where no solution exists.
  attr(oavec, "foundSolution") <- rep(TRUE, Ka)

  # If ata's are within tolerance of the "major" averages, set oa 
  #     accordingly and can ignore those ages in search for optimal value.
  #     Often, the major averages are close together, so prioritize:
  #     volume weighted, simple average, regression average
  #       Higher priortized method overrides lower prioritized method
  RAata.Triangle <- function(Triangle) coefficients(chainladder(Triangle, delta=0))
  VWata.Triangle <- function(Triangle) coefficients(chainladder(Triangle, delta=1))
  SAata.Triangle <- function(Triangle) coefficients(chainladder(Triangle, delta=2))
  oavec[names(tata <- RAata.Triangle(Triangle))][.approxeq(tata, selected, tolerance = tolerance)] <- 0
  oavec[names(tata <- SAata.Triangle(Triangle))][.approxeq(tata, selected, tolerance = tolerance)] <- 2
  oavec[names(tata <- VWata.Triangle(Triangle))][.approxeq(tata, selected, tolerance = tolerance)] <- 1
  
  # Original algorithm works on "long triangles" with value.next in same row as value, where
  #   value.next is the value in the same row and age = next age in "selected".
  #   Here we assume Triangle is a matrix, and selected aligns with columns of Triangle.
  Triangle <- cbind(.as.LongTriangle(Triangle[, -K, drop = FALSE]), value.next = .as.LongTriangle(Triangle[, -1, drop = FALSE])[, 3])
  
  # For each ata age ...
  for (k in (1:Ka)[is.na(oavec[1:Ka])]) {
    # After all is said and done, if oa is still NA, it will mean that
    #   no optimal alpha was found.
    oa <- NA 
    # value      = value at age k, 
    # value.next = value at endage k
    # Ignore pairs w/ NA's in one or more cells
    rn <- which(Triangle$age == ages[k] 
                & !is.na(Triangle$value) & !is.na(Triangle$value.next)
                )
    # If no row numbers identified, move along. Shouldn't happen if 
    # selected and Triangle from same triangle.
    if (length(rn)>0) {
      B <- Triangle$value[rn]
      E <- Triangle$value.next[rn]

      # If more than one observation at age k, do the calculations.
      # Otherwise (when only one link ratio observation) oa=1 by default.
      if (length(rn)>1) {

        # We will look through a three times until found,
        # each time increasing the search resolution of a by factor of ten.
        # 'width' = search resolution
        width <- step.d
        loopcntr <- 1

        while (loopcntr <= 3 & is.na(oa)) {

#          a=seq(0,maxa,by=width)

          # positive side first
# Change 8/19/11 different positive and negative widths, coincide with PSItable
          a <- seq(0, maxaPos, by = width)
        
          # Calc (absolute) distance between the selected ata and the average 
          # link ratio for Begin value, End value, and exponents in 'a' ...
          tmp <- sapply(a, LRdist, B, E, ata[k])
          # ... and find the minimum.
          vp <- which.min(tmp)
          posroot <- a[vp]
          # See if uniroot will run within one step of vp
          # Note: it has occurred that the curvature of the function is such
          # that posroot could be at the optimal solution but interpolation
          # between the values one step away moves beyond tolerance.
          # Therefore, if the current root yields an ata within tolerance of
          # the selected ata, then we'll take that value as the solution.
          # Otherwise, we'll use uniroot.
          if (LRdist(posroot, B, E, ata[k]) > tolerance) {
              # "One step" handled differently within interval vs at endpoints
              if (vp>1 & vp<length(a)) { # vp within interval
                # Must be of opposite sign at endpoints for uniroot to run
                if (sign(LRfcnPairD(a[vp - 1], B, E, ata[k])) *
                    sign(LRfcnPairD(a[vp + 1], B, E, ata[k])) < 0) {
                  posroot<-uniroot(LRfcnPairD, c(a[vp - 1], a[vp + 1]), B, E, ata[k])$root
                  }
                }
            else {
                if (vp == 1) {# vp at left endpoint
                  # must be of opposite sign at endpoints
                  if (sign(LRfcnPairD(a[vp], B, E, ata[k])) *
                      sign(LRfcnPairD(a[vp + 1], B, E, ata[k])) < 0) {
                    posroot <- uniroot(LRfcnPairD, c(a[vp], a[vp + 1]), B, E, ata[k])$root
                    }
                  }
                else { # vp at right endpoint
                  # must be of opposite sign at endpoints
                  if (sign(LRfcnPairD(a[vp - 1], B, E, ata[k])) *
                      sign(LRfcnPairD(a[vp], B, E, ata[k])) < 0) {
                    posroot <- uniroot(LRfcnPairD, c(a[vp - 1], a[vp]), B, E, ata[k])$root
                    }
                  }
                }

              # if now not within tolerance, give positive root infinite value
              if (LRdist(posroot,B,E,ata[k]) > tolerance) posroot <- Inf
              }

          # negative side next
# Change 8/19/11 different positive and negative widths, coincide with PSItable
          a <- seq(0, maxaNeg, by = width)
          
          tmp <- sapply(-a, LRdist, B, E, ata[k])
          vn <- which.min(tmp)
          negroot <- -a[vn]
          if (LRdist(negroot, B, E, ata[k]) > tolerance) {
              # see if uniroot will run within a step of vn
              if (vn > 1 & vn < length(a)) { # within interval
                # must be of opposite sign at endpoints
                if (sign(LRfcnPairD(-a[vn + 1], B, E, ata[k])) *
                    sign(LRfcnPairD(-a[vn - 1], B, E, ata[k])) < 0) {
                  negroot <- uniroot(LRfcnPairD, c(-a[vn + 1], -a[vn - 1]), B, E, ata[k])$root
                  }
                }
              else {
                if (vn == 1) { # at 0, which for negative side is right endpoint
                  # must be of opposite sign at endpoints
                  if (sign(LRfcnPairD(-a[vn + 1], B, E, ata[k])) *
                      sign(LRfcnPairD(-a[vn], B, E, ata[k])) < 0) {
                    negroot <- uniroot(LRfcnPairD,c(-a[vn + 1], -a[vn]), B, E, ata[k])$root
                    }
                  }
                else { # vn at left endpoint
                  # must be of opposite sign at endpoints
                  if (sign(LRfcnPairD(-a[vn], B, E,ata[k])) *
                      sign(LRfcnPairD(-a[vn - 1], B, E,ata[k])) < 0) {
                    negroot <- uniroot(LRfcnPairD, c(-a[vn], -a[vn - 1]), B, E, ata[k])$root
                    }
                  }
                }
              # if now not within tolerance, give negative root infinite value
              if (LRdist(negroot, B, E, ata[k]) > tolerance) negroot <- -Inf
              }
          if (posroot < -negroot) oa <- posroot
          else if (is.finite(negroot)) oa <- negroot
          
          # get ready for next try
          loopcntr <- loopcntr + 1
          width <- width / 10
        
          } # end of three-tries loop
        } # end of 'if more than one observation at age k' logic
      
      # Only one ata observation at age k; oa=1 by default if 
      # selected factor = observed factor, otherwise oa=unity
      else oa <- ifelse(ata[k] == E / B, 1.0, NA)
      if (is.na(oa)) {
        warning("No optimal delta solution for age ",
            ages[k],". Returning NA.")
#        oa <- 1.0
        attr(oavec, "foundSolution")[k] <- FALSE
        }
      oavec[k] <- oa
      } # end of length(rn)>0 case
    } # end of k loop
    
  # Now that we have all the optimal alpha's, go through them one more time
  # and set them equal to the closest integer if they are within 'tolerance'
  # of the closest integer.
  roa <- round(oavec,0)
  i <- abs(oavec-roa) < tolerance & !is.na(oavec)
  oavec[i] <- roa[i]
  oavec
  }

LRfunction <- function(x, y, delta) {
  F <- y / x
  sapply(delta, function(a) {
    w <- x ^ (2 - a) / sum(x ^ (2 - a))
    sum(w * F)
    })
  }
