CDR <- function(x,...){
  UseMethod("CDR")
}

CDR.default <- function(x,...) {
  "No default claims development result function defined."
}