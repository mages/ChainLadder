
.onAttach <- function(lib, pkg,...){
  packageStartupMessage(chainladderWelcomeMessage())
}


chainladderWelcomeMessage <- function(){
  cit <- citation("ChainLadder")
  txt <- paste(c(format(cit,"citation")),collapse="\n\n")
  cit_txt <- strsplit(txt, "A BibTeX")[[1]][1]
  paste("\nWelcome to ChainLadder version ", packageDescription("ChainLadder")$Version,
        cit_txt,
        "To suppress this message use:\n",
        "suppressPackageStartupMessages(library(ChainLadder))\n",       
        sep='')
 }
