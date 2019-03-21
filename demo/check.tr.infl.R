MedMal$MedMalOutstanding
MedMal$MedMalOpen
avg <- MedMal$MedMalOutstanding / MedMal$MedMalOpen
test<-check.tr.infl(avg) 
plot(test) 
summary(test) 
print(test) 
