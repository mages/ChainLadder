###################################################
##    Demo of glm-based insurance loss reserving 
###################################################

# Load required data
data(GenIns)
GenIns <- GenIns / 1000

# 1. Basic over-dispersed Poisson model (reproduces ChainLadder estimates)
fit1 <- ChainLadder::mlReserve(GenIns)
print("Basic over-dispersed Poisson model results:")
summary(fit1)
summary(fit1, type = "model")   # extract the underlying glm

# Visualize results
par(mfrow = c(2, 2))
# Original triangle
plot(fit1, which = 1, xlab = "dev year", ylab = "cum loss", 
     main = "Original Triangle")
# Residual plot
plot(fit1, which = 4, xlab = "fitted values", ylab = "residuals",
     main = "Residual Plot")
# QQ plot
plot(fit1, which = 5, main = "Normal Q-Q Plot")
# Full triangle
plot(fit1, which = 2, xlab = "dev year", ylab = "cum loss",
     main = "Full Triangle")
par(mfrow = c(1, 1))

# 2. Model with exposure offset
# Create exposure measure
expos <- (7 + 1:10 * 0.4) * 100
GenIns2 <- GenIns
attr(GenIns2, "exposure") <- expos

# Fit model with exposure
fit2 <- mlReserve(GenIns2)
print("\nModel results with exposure offset:")
summary(fit2)

# 3. Named exposures example
GenIns3 <- GenIns2
rownames(GenIns3) <- paste0(2007:2016, "-01-01")
names(expos) <- rownames(GenIns3)
attr(GenIns3, "exposure") <- expos

fit3 <- mlReserve(GenIns3)
print("\nModel results with named exposures:")
summary(fit3)

# 4. Negative binomial model
fit4 <- mlReserve(GenIns, nb = TRUE)
print("\nNegative binomial model results:")
summary(fit4)

# Optional: Bootstrap example (commented out as it takes longer)
# set.seed(11)
# fit5 <- mlReserve(GenIns, mse.method = "boot")
# print("\nBootstrap model results:")
# summary(fit5)
# 
# # Compute quantiles of predicted loss reserves
# print("\nReserve quantiles:")
# t(apply(fit5$sims.reserve.pred, 2, quantile, 
#         c(0.025, 0.25, 0.5, 0.75, 0.975)))
# 
# # Plot distribution of reserve
# plot(fit5, which = 3)