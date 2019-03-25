# Initial esttimate based on Paid Triangles

paid <- AutoBI$AutoBIPaid

# Get estimates based on std paid triangle data

paid_res <- MackChainLadder(paid)

# Plot initial results

plot(paid_res)

# Adjust the Paid Triangle based on the disposal rates

paid_adj <-
  BS.paid.adj(
    Triangle_rep_counts = AutoBI$AutoBIReportedCounts,
    Triangle_closed = AutoBI$AutoBIClosed,
    Triangle_paid = AutoBI$AutoBIPaid,
    regression.type = 'exponential'
  )

# Estimates based on the adjusted Triangle

paid_adj_res <- MackChainLadder(paid_adj)

# Plot Results

plot(paid_adj_res)

# IBNR Differences

# Get the IBNR considering the std Paid triangle

sum_paid<-summary(paid_res)

ibnr_paid<-sum_paid$Totals[4,1]

# Get the IBNR considering the adjusted Paid triangle

sum_paid_adj<-summary(paid_adj_res)

ibnr_paid_adj<-sum_paid_adj$Totals[4,1]

# Compare the differences

df <- c(ibnr_paid,ibnr_paid_adj)

# It is possible to appreciate the differences in the IBNR that such adjustment leads to

barplot(height=df, main="IBNR",xlab="", names.arg = c("ibnr_paid","ibnr_adj_paid"))
