## Clark method demos
## Daniel Murphy, 2010-10-31

## Run the "LDF Method" on GenIns with the "inverse power" growth function,
## develop to only 20 years
LDF.loglogistic  <- ClarkLDF(GenIns, maxage=20, G="loglogistic")
LDF.loglogistic
plot(LDF.loglogistic)

## Run the "Cape Cod Method" with the weibull growth function,
## develop to Infinity
CC.weibull <- ClarkCapeCod(GenIns, Premium=10000000+400000*0:9, G="weibull")
CC.weibull
plot(CC.weibull)
