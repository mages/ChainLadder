# Before actually applying the Chain Ladder technique it is necessary to check
# whether the Development Factors are correlated

# Apply the function to the triangle and save the output into the variable test

test <- dfcor.test(RAA)

# Plot the confidence interval and the test metric

plot(test)

# The metric is within the confidence interval, therefore the Development Factors are nor correlated

# Print the summary table
summary(test)

# Print onny the major outcomes
print(test)
