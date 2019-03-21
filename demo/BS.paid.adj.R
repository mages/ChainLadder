### Initial esttimate based on Paid Triangles
paid <- AutoBI$AutoBIPaid

paid_res <- MackChainLadder(paid)

plot(paid_res)

### Adjust the Paid Triangle based on the disposal rates
paid_adj <-
  BS.paid.adj(
    Triangle_rep_counts = AutoBI$AutoBIReportedCounts,
    Triangle_closed = AutoBI$AutoBIClosed,
    Triangle_paid = AutoBI$AutoBIPaid,
    regression.type = 'exponential'
  )

### Estimates based on the adjusted Triangle

paid_adj_res <- MackChainLadder(paid_adj)

plot(paid_adj_res)

### IBNR Differences

sum_paid<-summary(paid_res)

ibnr_paid<-sum_paid$Totals[4,1]

sum_paid_adj<-summary(paid_adj_res)

ibnr_paid_adj<-sum_paid_adj$Totals[4,1]

### Comparison Barplot

df <- c(ibnr_paid,ibnr_paid_adj)

barplot(height=df, main="IBNR",xlab="", names.arg = c("ibnr_paid","ibnr_paid_adj"))

