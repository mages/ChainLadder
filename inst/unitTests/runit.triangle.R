test.cyeff.test <- function() {
  
  
  
  res <- triangle(c(100, 150, 175, 180, 200),
                  c(110, 168, 192, 205),
                  c(115, 169, 202),
                  c(125, 185),
                  c(150)
  )
  checkEquals(res, print(res))
}
