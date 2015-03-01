## MackChainLadder demos
## Author: Markus Gesmann, March 2010, 2015

# See the Taylor/Ashe example in Mack's 1993 paper
GenIns
plot(GenIns)
plot(GenIns, lattice=TRUE)
GNI <- MackChainLadder(GenIns, est.sigma="Mack")
GNI$f
GNI$sigma^2
GNI # compare to table 2 and 3 in Mack's 1993 paper
plot(GNI)
plot(GNI, lattice=TRUE)

# Different weights
# Using alpha=0 will use straight average age-to-age factors
MackChainLadder(GenIns, alpha=0)$f
# You get the same result via:
apply(GenIns[,-1]/GenIns[,-10],2, mean, na.rm=TRUE)

# See the example in Mack's 1999 paper
Mortgage
plot(Mortgage)
MRT <- MackChainLadder(Mortgage, tail=1.05, tail.sigma=71, tail.se=0.02, est.sigma="Mack")
MRT
plot(MRT, lattice=TRUE)
# Table 1 in the above paper
f <- c(11.10, 4.092, 1.708, 1.276, 1.139, 1.069, 1.026, 1.023, 1.05)
f.se <- c(2.24, 0.517, 0.122, 0.051, 0.042, 0.023, 0.015, 0.012, 0.02)
F.se3 <- c(7.38, 1.89, 0.357, 0.116, 0.078, 0.033, 0.015, 0.007, 0.03)
sig <- c(1337, 988.5, 440.1, 207, 164.2, 74.60, 35.49, 16.89,71)
# test output from MackChainLadder
MRT$f
MRT$f.se
MRT$F.se[3,]
MRT$sigma

plot(MRT) # We observe trends along calendar years.

# Table 2 in the above paper
MRT$FullTriangle[,9]/1000 ## C_{i9}
MRT$FullTriangle[,10]/1000 ## C_{i,ult}
MRT$Mack.S.E[,9]/1000 ## s.e.(C_{i9})

# Access process risk error
MRT$Mack.ProcessRisk

# Access parameter risk error
MRT$Mack.ParameterRisk

# Total risk
MRT$Mack.S.E

op <- par(mfrow=c(2,1))
plot(with(summary(MRT)$ByOrigin, Mack.S.E/Ultimate),t="l",
ylab="CV(Ultimate)", xlab="origin period")
plot(summary(MRT)$ByOrigin[["CV(IBNR)"]], t="l", ylab="CV(IBNR)",
xlab="origin period")
par(op)


# This data set is discussed in many papers, e.g. England and Verrall (2000),
# see Table 1 just there
RAA
plot(RAA)
R <- MackChainLadder(RAA)
R
plot(R)
plot(R, lattice=TRUE)
# Table 12 in England and Verrall (2000)
R$f
R$sigma^2
# Table 13 in England and Verrall (2000)
# Please note the different indexing of sigma
MackChainLadder(RAA, est.sigma=R$sigma[7])
# Table 14 in England and Verrall (2000)
MackChainLadder(RAA, est.sigma=R$sigma[8])

# Let's investigate the Mack model in more detail
R[["Models"]][[1]]   # Model for first development period
summary( R[["Models"]][[1]]) # Look at the model stats
op <- par(mfrow=c(2,2)) # plot residuals
  plot( R[["Models"]][[1]])
par(op)

# Let's include an intercept in our model
newModel <- update(R[["Models"]][[1]], y ~ x+1,
             weights=1/R[["Triangle"]][1:9,1],
             data=data.frame(x=R[["Triangle"]][1:9,1],
                             y=R[["Triangle"]][1:9,2])
              )

# View the new model
summary(newModel)
op <- par(mfrow=c(2,2))
  plot( newModel )
par(op)

# Change the model for dev. period one to the newModel
R2 <- R
R2[["Models"]][[1]] <- newModel
predict(R2) # predict the full triangle with the new model
#(only the last origin year will be affected)

R2[["FullTriangle"]] <-  predict(R2)$FullTriangle
R2[["FullTriangle"]]
R2   # Std. Errors have not been re-estimated!
# Plot the result

plot(R2, title="Changed R Model")

## Suppose you have a long table with claims development by line of
## business and would like to apply the MackChainLadder on all triangles
## in one go.

## First lets create a table similar to what you would get from a 'real' data base

myList <- list("General Liabilty" = RAA/1e3,
               "General Insurance" = GenIns/1e3,
               "Workers Comp"=ABC/1e3,
               "Mortgage Guarantee"=Mortgage/1e3)

myData <- do.call("rbind" , lapply(names(myList),
                            function(x) as.data.frame(myList[[x]],lob=x ,na.rm=TRUE)))

## Let's plot a nice summary, but first lets normalise the origin years
myData <- do.call("rbind",
    by(myData, list(lob=myData$lob),
    function(x) {org=as.numeric(as.character(x$origin))
      x$origin <- org-min(org)+2000;x}
    ))
rownames(myData) <- NULL

head(myData)  ## Does this look familiar? ;-)
xyplot(value ~ dev | lob, groups=factor(origin), data=myData, t="l",
      scales="free", auto.key=list(space="right", points=FALSE, lines=TRUE))

## Lets create triangles again and apply MackChainLadder for each lob:

myResults <- by(myData, list(lob=myData$lob), function(x)
                MackChainLadder(as.triangle(x), est.sigma="Mack"))
## That's it, lets look at the output
myResults

## Summarise all results by origin period in one data frame:
by.origin <- function(x){
              data.frame(lob=x,
              origin=dimnames(myResults[[x]]$Triangle)$origin,
              summary(myResults[[x]])$ByOrigin)
             }

ByOrigin <-do.call("rbind", lapply(names(myResults) , by.origin))
ByOrigin

## Similar for the totals
Totals <- do.call("rbind", lapply(names(myResults) ,
            function(x) data.frame(LOB=x, t(summary(myResults[[x]])$Totals))))
Totals

require(lattice)
barchart(Latest + IBNR ~ factor(origin) | lob, stack=TRUE, data=ByOrigin,
        scale="free", auto.key=TRUE, as.table=TRUE, xlab="origin")

## One year claims development result
# Example from the 2008 Merz & Wuthrich paper 
MW2008
M <- MackChainLadder(MW2008, est.sigma="Mack")
plot(M)
CDR(M)
# Return all run-off result developments
CDR(M, dev="all")

# Example from the 2014 Merz & Wuthrich paper
MW2014
W <- MackChainLadder(MW2014, est.sigma="Mack")
plot(W)
CDR(W)
