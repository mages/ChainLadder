# Before actually applying the Chain Ladder technique it is necessary to check
# wether the triangle has Calendar Year Effect

# Apply the function to the triangle and save the output into the variable test

test <- cyeff.test(RAA)

# Plot the confidence interval and the test metric

plot(test)

# The metric is within the confidence interval, therefore the triangle doesn't
# have Calendar Year Effect

# Print the summary table
summary(test)

# Print onny the major outcomes
print(test)
