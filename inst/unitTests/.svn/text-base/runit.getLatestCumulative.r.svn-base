test.getLatestCumulative <- function() {
  g <- getLatestCumulative(GenIns)
  checkTrue(all(g == c(3901463, 5339085, 4909315, 4588268, 3873311, 3691712, 3483130, 2864498, 1363294, 344014)))
  checkTrue(identical(names(attributes(g)), c("latestcol", "names",     "rowsname",  "colnames",       "colsname")))
  checkTrue(all(attr(g, "latestcol") == 10:1))
  checkTrue(all(attr(g, "names") == as.character(1:10)))
  checkEquals(attr(g, "rowsname"), "origin")
  checkTrue(all(attr(g, "dev") == as.character(10:1)))
  checkEquals(attr(g, "colsname"), "dev")

  # Simple matrix, unadorned. Notice the NA's.
  X <- matrix(c(100, 200, 300, 150, 300, NA, 175, NA, NA), 3, 3)
  g <- getLatestCumulative(X)
  checkTrue(all(g == c(175, 300, 300))) # if all zero in row, latest is last column
  checkTrue(identical(names(attributes(g)), c("latestcol")))
  checkTrue(all(attr(g, "latestcol") == 3:1))

  # Give it rownames
  Y <- X
  rownames(Y) <- letters[1:3]
  g <- getLatestCumulative(Y)
  checkTrue(all(names(g) == c('a', 'b', 'c')))
  checkTrue(all(attr(g, "latestcol") == 3:1))
  checkTrue(all(names(attr(g, "latestcol")) == c('a', 'b', 'c')))

  # Name the row dimension of Y
  names(dimnames(Y))[1] <- "origin"
  g <- getLatestCumulative(Y)
  checkTrue(attr(g, "rowsname") == "origin")

  # Give it colnames
  colnames(Y) <- 1:3
  g <- getLatestCumulative(Y)
  checkTrue(all(attr(g, "colnames") == c('3', '2', '1')))
  checkTrue(is.na(attr(g, "colsname")))

  # Name the col dimension of Y
  names(dimnames(Y))[2] <- "age"
  g <- getLatestCumulative(Y)
  # "colnames" attribute becomes "age" attribute
  checkTrue(all(attr(g, "colnames") == c('3', '2', '1'))) #same as before
  checkTrue(attr(g, "colsname") == "age") # new

  # If all NA's in a row, latest = column 1  
  X <- matrix(c(100, NA, 300, 150, NA, NA, 175, NA, NA), 3, 3)
  X <- matrix(c(100, 200, 300, 
                 NA,  NA,  NA,
                250,  NA,  NA), 3, 3, byrow = TRUE)
  g <- getLatestCumulative(X)
  checkTrue(is.na(g[2]))

  # Put a zero in the middle of X -- NA's on both sides -- should find the zero
  X[2, 2] <- 0
  g <- getLatestCumulative(X)
  checkTrue(all(g == c(300, 0, 250))) # if all zero in row, latest is last column

  # test na.values
  Y <- matrix(c(1,  2,  3,
                4,  5,  0, 
                6, NA, NA), byrow=TRUE, nrow=3)
  g <- getLatestCumulative(Y) # c(3, 0, 6)
  checkTrue(all(g == c(3, 0, 6))) 
  g <- getLatestCumulative(Y, na.values = 0) # c(3, 5, 6) 
  checkTrue(all(g == c(3, 5, 6))) 
  # multiple NA values
  Y <- matrix(c(1,  2,  Inf,
                4,  5,  0, 
                6, NA, NA), byrow=TRUE, nrow=3)
  g <- getLatestCumulative(Y, na.values = c(0, Inf)) # c(2, 5, 6) 
  checkTrue(all(g == c(2, 5, 6))) 
     
  }