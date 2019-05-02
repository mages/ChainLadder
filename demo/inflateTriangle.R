# Create a triangle of average open claims as the ratio between O/S Claims and Open Claims

avg <- MedMal$MedMalOutstanding / MedMal$MedMalOpen

# Check the inflation level

test<-check.tr.infl(avg)

# Select a rate of 15% based on the first development period

print(test)

# Inflate the triangle

avg.inflated <- inflate.triangle(Triangle = avg, rate = .15)

# Compared the two triangles, pre and post adjustment

plot(avg)

plot(avg.inflated)