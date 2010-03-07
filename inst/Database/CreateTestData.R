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


xyplot(value/1000 ~ dev | lob, groups=origin, data=testData, as.table=TRUE, scales="free", t="l", layout=c(5,2))


list.inc.tria <- lapply(list.tria, cum2incr)

testData <- do.call("rbind",
                    lapply(names(list.inc.tria),
                           function(x){
                               as.data.frame(list.inc.tria[[x]], lob=x, na.rm=TRUE)
                           }
                           )
                    )


xyplot(cumsum(value)/1e6 ~ dev | lob, groups=origin, data=testData, as.table=TRUE, scales="free", t="l", layout=c(5,2))

DataTableToTriangles <- function(data, origin="origin", dev="dev", value="value", lob="lob",
                                  inputCum=FALSE, outputCum=TRUE){

    list.tria <- by(data, list(lob=data[[lob]]), function(x){
        tria <- as.triangle(x[c(origin, dev, value)], origin=origin, dev=dev, value=value)
        if(inputCum != outputCum){
            if(outputCum==FALSE){
                tria <- cum2incr(tria)
            }else{
                tria <- incr2cum(tria, na.rm=TRUE)
            }
        }
    }
                    )

    return(list.tria)
}


list.triangles <- DataTableToTriangles(testData)

MackResults <- lapply(list.triangles, MackChainLadder, est.sigma="Mack")

ChainLadderSummaries <- function(CL, summary.by="ByOrigin"){

    res <- do.call("rbind",
                   lapply(names(CL), function(x){
                       originResults <- summary(CL[[x]])[[summary.by]]
                       if(summary.by=="ByOrigin"){
                           originResults <- data.frame(lob=x, origin=rownames(originResults), originResults)
                       }else{
                           originResults <- data.frame(lob=x,origin="Totals", t(originResults))
                       }
                       originResults
                   }))
    return(res)
}

fn <- paste(system.file("Database", package="ChainLadder"),"/TestData.csv", sep="")
