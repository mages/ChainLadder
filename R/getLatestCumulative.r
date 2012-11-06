getLatestCumulative <- function (Triangle, na.values = NULL) {
  # Returns the "current diagonal" from a triangle. This is defined to be
  #   the vector of entries from the triangle corresponding to the rightmost
  #   non-missing cell per row.
  # Features: 
  # 1. For the "lazy actuary" who leaves zeroes in triangle cells 
  #     corresponding to future observations, can specify 
  #     na.values=0 to find the rightmost non-zero and non-NA entry.
  # 2. Assigns a "latestcol" attribute that gives the column index
  #     of the rightmost entry per row.
  # 3. Adorns the returned vector with 'names' corresponding to the rownames
  #     of the triangle, if available, and with a 'rowsname' attribute holding
  #     the name of the row dimension of the matrix (e.g., 'origin' or 'ay'),
  #     if available.
  # 4. Assigns a 'colnames' attribute holding the column names of the matrix,
  #     if available, and with a "colsname" attribute holding 
  #     the name of the column dimension of the matrix 
  #     (e.g., 'dev' or 'age'), if available.
  # The attributes assigned in 3 & 4 can be useful when the current diagonal
  #   is utilized in a method that may be unrelated to the given Triangle 
  #   but that relies on the origin and development age of the observations
  #   in the diagonal.
  if (!is.matrix(Triangle)) stop("Triangle '", 
      deparse(substitute(Triangle)), "' must be a matrix")
  # Define a function that, depending on the value of na.values,
  #   finds the rightmost column per row.
  f <- if (is.null(na.values)) function(x) ifelse(length(w <- which(!is.na(x))) > 0L, tail(w, 1L), 1) else function(x) ifelse(length(w <- which(!is.na(x) & !(x %in% na.values))) > 0L, tail(w, 1L), 1)
  # Apply that function to each row.
  latestcol <- apply(Triangle, 1L, f)
  # Select the rightmost values
  latestdiag <- Triangle[cbind(seq_along(latestcol), latestcol)]
  # Set attributes ...
  attr(latestdiag, "latestcol") <- latestcol
  # Set names and colnames attributes
  nms <- names(dmnms <- dimnames(Triangle))
  nms1 <- nms[1L]
  nms2 <- nms[2L]
  if (!is.null(dmnms[[1L]])) { # 'rownames' are available
    names(latestdiag) <- dmnms[[1L]]
    # save the name of the row dimension
    attr(latestdiag, "rowsname") <- nms1
    }
  if (!is.null(dmnms[[2L]])) { # 'colnames' are available
    attr(latestdiag, "colnames") <- dmnms[[2L]][latestcol]
    # save the name of the column dimension
    attr(latestdiag, "colsname") <- nms2
    }
  return(latestdiag)
  }

