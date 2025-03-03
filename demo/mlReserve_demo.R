###################################################
##    Demo of glm-based insurance loss reserving 
###################################################

# Load required data
data(GenIns)
GenIns <- GenIns / 1000

# 0. GLM model (reproduces ChainLadder estimates)
fit1 <- ChainLadder::mlReserve(GenIns, fit_func = stats::glm)
print("GLM model results:")
summary(fit1)
summary(fit1, type = "model")   # extract the underlying glm

# Visualize results
par(mfrow = c(1, 2))
# Original triangle
plot(fit1, which = 1, xlab = "dev year", ylab = "cum loss", 
     main = "Original Triangle")
# Full triangle
plot(fit1, which = 2, xlab = "dev year", ylab = "cum loss",
     main = "Full Triangle")

# 1. SVM model (reproduces ChainLadder estimates)
fit1 <- ChainLadder::mlReserve(GenIns, fit_func = e1071::svm)
print("SVM model results:")
summary(fit1)
summary(fit1, type = "model")   # extract the underlying glm

# Visualize results
par(mfrow = c(1, 2))
# Original triangle
plot(fit1, which = 1, xlab = "dev year", ylab = "cum loss", 
     main = "Original Triangle")
# Full triangle
plot(fit1, which = 2, xlab = "dev year", ylab = "cum loss",
     main = "Full Triangle")


# 2. Random Forest model (reproduces ChainLadder estimates)
fit1 <- ChainLadder::mlReserve(GenIns, fit_func = randomForest::randomForest, 
                               predict_func = predict)
print("RF model results:")
summary(fit1)
summary(fit1, type = "model")   # extract the underlying glm

# Visualize results
par(mfrow = c(1, 2))
# Original triangle
plot(fit1, which = 1, xlab = "dev year", ylab = "cum loss", 
     main = "Original Triangle")
# Full triangle
plot(fit1, which = 2, xlab = "dev year", ylab = "cum loss",
     main = "Full Triangle")


# 3. GLMNET model (reproduces ChainLadder estimates)
fit1 <- ChainLadder::mlReserve(GenIns, fit_func = glmnet::cv.glmnet, 
                               predict_func = predict)
print("GLMNET model results:")
summary(fit1)
summary(fit1, type = "model")   # extract the underlying glm

# Visualize results
par(mfrow = c(1, 2))
# Original triangle
plot(fit1, which = 1, xlab = "dev year", ylab = "cum loss", 
     main = "Original Triangle")
# Full triangle
plot(fit1, which = 2, xlab = "dev year", ylab = "cum loss",
     main = "Full Triangle")

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