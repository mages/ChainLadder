ata <- function(Triangle, NArow.rm = TRUE, colname.sep = "-",
        colname.order=c("ascending","descending")) {

    if (!is.matrix(Triangle)) stop("Triangle must be a matrix.")
    if ((nc<-ncol(Triangle))<2L) stop("Triangle must have at least 2 columns.")
    if (!is.logical(NArow.rm)) stop("'NArow.rm' must be 'logical'")
    colname.order <- match.arg(colname.order)

    # Drop first and last columns
    to <- Triangle[, -1L, drop=FALSE]     
    from <- Triangle[, -nc, drop = FALSE]
    # Wherever one obs is unavailable so should be the corresponding obs
    to[is.na(from)] <- NA 
    from[is.na(to)] <- NA
    
    atamat <- to / from

    ## Keep only non all-NA rows?
    if (NArow.rm) atamat <- atamat[apply(atamat, 1L, function(x) any(!is.na(x))),,drop=FALSE]


    ## Simple averages
    #smpl <- colMeans(atamat, na.rm = TRUE)
    # Ignore link ratios with zeroes in the denominator
    smpl <- apply(atamat, 2, function(x) mean(x[is.finite(x)]))

    ## Volume weighted averages
    vwtd <- colSums(to, na.rm=TRUE) / colSums(from, na.rm=TRUE)
    # If no paired obs for some age, smpl will be NA but vwtd will be NaN.
    # Make it NA too.
    vwtd[is.na(smpl)] <- NA

    # Make up column labels
    names(smpl) <- names(vwtd) <- colnames(atamat) <- 
        if (colname.order == "ascending") 
            paste(colnames(Triangle)[-nc], colnames(Triangle)[-1L], sep=colname.sep)
        else
            paste(colnames(Triangle)[-1L], colnames(Triangle)[-nc], sep=colname.sep)
    names(dimnames(atamat)) <- names(dimnames(Triangle))

    structure(atamat,
        class=c("ata", class(Triangle)),
        smpl = smpl,
        vwtd = vwtd
        )
    }

summary.ata <- function(object, digits=3, ...) {
    # A matrix of ata factors with rows for the two
    # averages appended at the bottom.

    # Retain dimnames. Rownames appended with the two averages.
    dms <- dimnames(object)
    if (is.null(dms[[1]])) dms[[1]] <- paste("[", 1:nrow(object), ",]", sep="")
    dms[[1]] <- c(dms[[1]] , "smpl", "vwtd")
        
    smpl <- attr(object, "smpl")
    vwtd <- attr(object, "vwtd")

    if (!is.null(digits)) {
        digits <- suppressWarnings(as.numeric(digits[1L]))
        if (length(digits)==0 || is.na(digits)) stop("Non-numeric 'digits' specified.")
        object <- round(object, digits)
        smpl <- round(smpl, digits)
        vwtd <- round(vwtd, digits)
        }

    structure(
        rbind(
            object, 
            smpl = smpl, 
            vwtd = vwtd
            ), 
        dimnames = dms
        ) 
    }

print.ata <- function(x, ...) print(summary(x), ...)

