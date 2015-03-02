test.tweedieReserve <- function() {
  set.seed(123)
  #ODP, PARAMETRIC BOOTSTRAP
  TR_1 <- tweedieReserve(MW2008,nsims=10000,rereserving=TRUE)
  rep_1<- summary(TR_1)
  
  ult_1 <- c(2239822.0, 127632.4, 2245462.4, 2327309.6, 2398481.2, 2448301.3, 2580004.2)
  one_1 <- c(2238888.5, 108079.5, 2238215.1, 2310797.3, 2367667.1, 2416964.8, 2520365.7)
  diag_1 <- c(2237826, 2239822, 2238889)
  checkEquals(rep_1$Prediction[['IBNR']], ult_1,tol=0.01, checkNames = FALSE)
  checkEquals(rep_1$Prediction[['CDR(1)']], one_1,tol=0.01, checkNames = FALSE)
  checkEquals(rep_1$Diagnostic,diag_1,tol=0.01, checkNames = FALSE)
  
  #Arithmetic separation method, PARAMETRIC BOOTSTRAP
  set.seed(123)
  TR_2 <- tweedieReserve(MW2008,nsims=10000,rereserving=TRUE,design.type=c(0,1,1))
  rep_2<- summary(TR_2)
  
  ult_2 <- c(2210254.6, 120120.2, 2206455.6, 2288314.0, 2366451.6, 2415008.5, 2504159.7)
  one_2 <- c(2209135.4, 109864.4, 2209053.0, 2280793.9, 2350931.7, 2384833.0, 2500526.3)
  diag_2 <- c(2213850, 2210255, 2209135 )
  
  checkEquals(rep_2$Prediction[['IBNR']], ult_2,tol=0.01, checkNames = FALSE)
  checkEquals(rep_2$Prediction[['CDR(1)']], one_2,tol=0.01, checkNames = FALSE)
  checkEquals(rep_2$Diagnostic,diag_2,tol=0.01, checkNames = FALSE)
  
}
