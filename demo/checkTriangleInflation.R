# Create a triangle of average open claims as the ratio between O/S Claims and Open Claims

avg <- MedMal$MedMalOutstanding / MedMal$MedMalOpen

# Check the level of average inflation Y-o-Y

test<-check.tr.infl(avg)

# Plot the results

# A model of exponential inflation fits quite well the level of average O/S claims
# It is particularly evident for DP 1,2,3

plot(test)

# Get the summary in an analytical way to observe the ratios and the number of points used

summary(test)

# Print the output

print(test) 
