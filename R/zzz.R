
.onAttach <- function(lib, pkg,...){
  packageStartupMessage(chainladderWelcomeMessage())
}


chainladderWelcomeMessage <- function(){

  paste("\nWelcome to ChainLadder version ", packageDescription("ChainLadder")$Version,
        "\n\n",
        "Type vignette('ChainLadder', package='ChainLadder') to access\n",
        "the overall package documentation.\n\n",
        "See demo(package='ChainLadder') for a list of demos.\n\n",
        "More information is available on the ChainLadder project web-site:\n",
        "https://github.com/mages/ChainLadder\n\n",
        "To suppress this message use:\n",
        "suppressPackageStartupMessages(library(ChainLadder))\n",       
        sep='')
 }
