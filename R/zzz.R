.onLoad<- function(lib, pkg, ...)
{
    require(utils)
    cat("\n",paste("ChainLadder version", packageDescription("ChainLadder")$Version,
                   "by Markus Gesmann <markus.gesmann@gmail.com>,\nYanwei (Wayne) Zhang <actuaryzhang@uchicago.edu>\n\n"),
        "Type library(help='ChainLadder') or ?ChainLadder\n",
        "to see overall documentation.\n\n",
        "Type example(ChainLadder) to get an idea of the functionality of this package.\n\n",
        "Feel free to send me an email if you would like to keep ",
        "informed of\nnew versions or if you have any feedback, ",
        "ideas, suggestions or ",
        "would\nlike to collaborate.\n\n",
        "More information is available on the ChainLadder project web-site:\n",
        "http://code.google.com/p/chainladder/\n\n",
        sep='')
  invisible()
}
