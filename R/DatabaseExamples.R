
getDatabaseExampleData <- function(){
## Author: Markus Gesmann, March 2010
## Get test data, either via a CSV-file, or from a database
## The data data set are stored here
testDataDir <- system.file("Database", package="ChainLadder")


## Check operating system
## If OS is Windows we read the data from an Access data base,
## Otherwise we use read.csv
if(R.Version()$os == "mingw32"){

    require(RODBC)
    fn.mdb <- paste(testDataDir,"/ChainLadderTestData.mdb", sep="")
    channel <- odbcConnectAccess(access.file=fn.mdb)
    database.fn <- strsplit(attr(channel, "connection.string"), ";")[[1]][1]
    sqlstring <- "SELECT * FROM testData"
    cat(paste("Read data from:\n", database.fn,
                "\nWith SQL statement:\n", sqlstring, "\n\n"))

    testData <- sqlQuery(channel, sqlstring)
    odbcClose(channel)
  }else{
    ## Get data from CSV-file
    fn.csv <- paste(testDataDir,"/TestData.csv", sep="")
   testData <- read.csv(fn.csv)
  }
  return(testData)
}

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


writeDatabaseExampleResults <- function(data,tablename=NULL){

  if(is.null(tablename)){
    tablename <- paste('T_ChainLadderResults', format(Sys.time(), '%Y %B %d %H:%M:%S'))
  }
  testDataDir <- system.file("Database", package="ChainLadder")
  if(R.Version()$os == "mingw32"){
    require(RODBC)
    fn.mdb <- paste(testDataDir,"/ChainLadderTestData.mdb", sep="")
    channel <- odbcConnectAccess(access.file=fn.mdb)
    database.fn <- strsplit(attr(channel, "connection.string"), ";")[[1]][1]
    x <- sqlSave(channel, data, tablename, rownames=FALSE, nastring="")
    odbcClose(channel)
    my.message <- paste("Results stored in: ", database.fn)

  }else{
    ## Get data from CSV-file
    fn.csv <- paste(tempdir(),"/",tablename,".csv", sep="")
    write.csv(data, fn.csv)
    my.message <- paste("Data written into CSV-file:", fn.csv)
  }
  return(my.message)
}
