test.print.triangle <- function() {
  # Test that print(x) returns x; see ?print
  res <- triangle(c(100, 150, 175, 180, 200),
                  c(110, 168, 192, 205),
                  c(115, 169, 202),
                  c(125, 185),
                  c(150)
  )
  checkEquals(res, print(res))
}

test.as.triangle <- function(){
  long <- data.frame(OCC = c(2003, 2003, 2004, 2004, 2004, 2005),
                     DEV = c(2, 3, 1, 2, 3, 1),
                     VAL = c(100, 150, 300, 250, 250, 10))
  
  testTriangle <- structure(c(NA, 300, 10, 100, 250, NA, 150, 250, NA), 
                            .Dim = c(3L, 3L), 
                            .Dimnames = list(OCC = c("2003", "2004", "2005"), 
                                             DEV = c("1", "2", "3")), 
                            class = c("triangle", "matrix"))
  checkEquals(
    as.triangle(long, "OCC", "DEV", "VAL"),
    testTriangle
  )
  }