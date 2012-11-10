.onLoad<- function(lib, pkg, ...)
{
    packageStartupMessage(chainladderWelcomeMessage())

    invisible()
}

chainladderWelcomeMessage <- function(){

  paste("\nChainLadder version ", packageDescription("ChainLadder")$Version,
                   " by:\nMarkus Gesmann <markus.gesmann@gmail.com>",
                   "\nWayne Zhang <actuary_zhang@hotmail.com>",
                   "\nDaniel Murphy <danielmarkmurphy@gmail.com>",
                   "\n\n",
        "Type ?ChainLadder to access overall documentation and\n",
        "vignette('ChainLadder') for the package vignette.\n\n",
        "Type demo(ChainLadder) to get an idea of the functionality of this package.\n",
        "See demo(package='ChainLadder') for a list of more demos.\n\n",
        "More information is available on the ChainLadder project web-site:\n",
        "http://code.google.com/p/chainladder/\n\n",
        "To suppress this message use the statement:\n",
        "suppressPackageStartupMessages(library(ChainLadder))\n",       
        sep='')
 }
