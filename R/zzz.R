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
        "Type library(help='ChainLadder') or ?ChainLadder\n",
        "to see overall documentation.\n\n",
        "Type demo(ChainLadder) to get an idea of the functionality of this package.\n\n",
        "See demo(package='ChainLadder') for a list of more demos.\n\n",
        "Feel free to send us an email if you would like to keep ",
        "informed of\nnew versions or if you have any feedback, ",
        "ideas, suggestions or ",
        "would\nlike to collaborate.\n\n",
        "More information is available on the ChainLadder project web-site:\n",
        "http://code.google.com/p/chainladder/\n\n",
        "To suppress this message use the statement:\n",
        "suppressPackageStartupMessages(library(ChainLadder))\n",       
        sep='')
 }
