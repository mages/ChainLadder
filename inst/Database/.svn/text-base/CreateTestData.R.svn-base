list.tria <- list(ABC, RAA, GenIns, incr2cum(M3IR5), Mortgage, liab[[1]], liab[[2]], auto[[1]], auto[[2]], auto[[3]])
names(list.tria) <- c("ABC", "RAA", "GenIns", "M3IR5", "Mortgage", names(liab), names(auto))
# all triangles are cumulative, however often databases store incrementals

list.tria <- lapply(list.tria, as.triangle)

testData <- do.call("rbind",
                    lapply(names(list.tria),
                           function(x){
                               as.data.frame(list.tria[[x]], lob=x, na.rm=TRUE)
                           }
                           )
                    )

testDataDir <- system.file("Database", package="ChainLadder")
fn.csv <- paste(testDataDir,"/TestData.csv", sep="")
write.csv(testData, file=fn.csv, row.names=FALSE)

xyplot(value/1000 ~ dev | lob, groups=origin, data=testData, as.table=TRUE, scales="free", t="l", layout=c(5,2))


list.inc.tria <- lapply(list.tria, cum2incr)

testData <- do.call("rbind",
                    lapply(names(list.inc.tria),
                           function(x){
                               as.data.frame(list.inc.tria[[x]], lob=x, na.rm=TRUE)
                           }
                           )
                    )


   
testData <- getDatabaseExampleData()

list.triangles <- DataTableToTriangles(testData,
                                  origin="origin", dev="dev", value="value", lob="lob",
                                  inputCum=FALSE, outputCum=TRUE)

op <- par(mfrow=c(5,2), mar=rep(2,4))
lapply(list.triangles, plot)
par(op)

MackResults <- lapply(list.triangles, MackChainLadder, est.sigma="Mack")
MackResultsByOrigin <- ChainLadderSummaries(MackResults, summary.by="ByOrigin")

writeDatabaseExampleResults(MackResultsByOrigin,
                            tablename=paste('T_ChainLadderResults', format(Sys.time(), "%Y %B %d %H:%M:%S")))
