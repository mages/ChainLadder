## This demo demonstrates to the user how data can be extracted from databases,
## how the data can be reviewed, converted into a list of triangles,
## how a ChainLadder function can be applied to several triangles,
## and finally shows, how the results can be stored back into a database.

## Author: Markus Gesmann, March 2010


## Let's write a little function to get our test data  
getDatabaseExampleData <- function(){
## Get test data, either via a CSV-file, or from a database
## The data data set are stored here
testDataDir <- system.file("Database", package="ChainLadder")
## Check operating system
## If OS is Windows we read the data from an Access database:
## ChainLadderTestData.mdb, otherwise we use read.csv
if(.Platform$OS.type == "windows"){
    require(RODBC)
    fn.mdb <- paste(testDataDir,"/ChainLadderTestData.mdb", sep="")
    # Establish connection to the Access database
    channel <- odbcConnectAccess(access.file=fn.mdb)
    ## See the details in channel:
    database.fn <- strsplit(attr(channel, "connection.string"), ";")[[1]][1]
    ## Our SQL statement
    sqlstring <- "SELECT * FROM testData"
    cat(paste("Read data from:\n", database.fn,
                "\nWith SQL statement:\n", sqlstring, "\n\n"))
    ## Fetch the data from our database
    testData <- sqlQuery(channel, sqlstring)
    ## Close the database connection
    odbcClose(channel)
  }else{
    ## Get data from CSV-file
    fn.csv <- paste(testDataDir,"/TestData.csv", sep="")
    testData <- read.csv(fn.csv)
  }
  return(testData)
}


myTestData <- getDatabaseExampleData()

## Review our data
head(myTestData)
summary(myTestData)
str(myTestData)
require(lattice)
xyplot(value/1000 ~ dev | lob, groups=origin, data=myTestData,t="l", as.table=TRUE, scales="free")
## So we have received incremental data from our database.
## Hence we write a function to convert the data into a list of triangles

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

## Convert our test data.frame myTestData into a list of cumulative triangles
my.triangles <- DataTableToTriangles(myTestData, 
                            origin="origin", dev="dev", value="value", lob="lob",
                            inputCum=FALSE, outputCum=TRUE)
                            
## Review triangle list
my.triangles
## Let's plot all cumulative triangles into one window             
op=par(mfrow=c(5,2),mar=rep(2,4))                            
lapply(my.triangles, plot)
par(op)

## Apply the MackChainLadder function to all cumulative triangles
MackResults <- lapply(my.triangles, MackChainLadder, est.sigma="Mack")
MackResults

## To extract all the information from the MackChainLadder result
## we write a function 'ChainLadderSummaries' which does just that.
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

## Let's apply our new function ChainLadderSummaries

MackResultsByOrigin <- ChainLadderSummaries(MackResults, summary.by="ByOrigin")

## Finally we create a function which allows us to write the output back into a database 

writeDatabaseExampleResults <- function(data,tablename=NULL){

  if(is.null(tablename)){
    tablename <- paste('T_ChainLadderResults', format(Sys.time(), '%Y %B %d %H:%M:%S'))
  }
  testDataDir <- system.file("Database", package="ChainLadder")
  if(.Platform$OS.type == "windows"){
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

writeDatabaseExampleResults(MackResultsByOrigin,
                            tablename=paste('T_ChainLadderResults', format(Sys.time(), "%Y %B %d %H:%M:%S")))

## Now open the database where you will find the MackChainLadder output
