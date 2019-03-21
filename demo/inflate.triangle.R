avg <- MedMal$MedMalOutstanding / MedMal$MedMalOpen
plot(avg)
avg.inflated <- inflate.triangle(rate = .15, avg)
plot(avg.inflated)