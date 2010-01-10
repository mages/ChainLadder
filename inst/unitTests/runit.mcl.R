test.mcl.triangles <- function(){

    myList <- list("General Liabilty" = RAA, "General Insurance" = GenIns,
                   "Workers Comp"=ABC, "Mortgage Guarantee"=Mortgage)

    ## Test should fail as triangles of different dimesnions have been combined
    checkException(mcl(myList))

}
