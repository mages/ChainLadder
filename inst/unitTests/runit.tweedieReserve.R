test.tweedieReserve <- function() {
  set.seed(123)
  #ODP, PARAMETRIC BOOTSTRAP
  TR_1 <- tweedieReserve(MW2008,nsims=10000,rereserving=TRUE)
  rep_1<-RC_report(TR_1)
  
  ult_1 <- c(2238807.8134,127632.394,0.0585,2206318.0016,2238345.1984,2309516.7468,2402395.6175,2458976.9985,2590679.9489,2611995.8277)
  one_1 <- c(2238824.0789,108079.4533,0.0479,2205981.4698,2234598.5035,2297152.4462,2375946.8734,2416920.3086,2509737.9688,2563451.1534)
  diag_1 <- c(2237826,2238808,2238824)
  
  checkEquals(rep_1$Ultimate_View, ult_1,tol=0.01, checkNames = FALSE)
  checkEquals(rep_1$'1yr_View', one_1,tol=0.01, checkNames = FALSE)
  checkEquals(rep_1$Diagnostic,diag_1,tol=0.01, checkNames = FALSE)
  
  #Arithmetic separation method, PARAMETRIC BOOTSTRAP
  set.seed(123)
  TR_2 <- tweedieReserve(MW2008,nsims=10000,rereserving=TRUE,design.type=c(0,1,1))
  rep_2<-RC_report(TR_2)
  
  ult_2 <- c(2210254.6134,120120.2424,0.0543,2180409.7893,2206455.6401,2269709.8490,2366451.5802,2415008.4877,2504159.7139,2614654.7706)
  one_2 <- c(2209135.3774,109864.4061,0.0497,2183977.6237,2209053.0242,2262391.5951,2350931.6847,2384833.0428,2500526.3293,2587117.8086)
  diag_2 <- c(2213850,2210255,2209135)
  
  checkEquals(rep_2$Ultimate_View, ult_2,tol=0.01, checkNames = FALSE)
  checkEquals(rep_2$'1yr_View', one_2,tol=0.01, checkNames = FALSE)
  checkEquals(rep_2$Diagnostic,diag_2,tol=0.01, checkNames = FALSE)
  
  
}