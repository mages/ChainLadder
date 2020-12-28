
.onAttach <- function(lib, pkg,...){
  packageStartupMessage(chainladderWelcomeMessage())
}


chainladderWelcomeMessage <- function(){
  cit <- utils::citation("ChainLadder")
  txt <- paste(c(format(cit,"citation")),collapse="\n\n")
  cit_txt <- base::strsplit(txt, "A BibTeX")[[1]][1]
  paste("\nWelcome to ChainLadder version ", utils::packageDescription("ChainLadder")$Version,
        cit_txt,
        "To suppress this message use:\n",
        "suppressPackageStartupMessages(library(ChainLadder))\n",       
        sep='')
 }
