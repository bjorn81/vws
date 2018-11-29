#'@title Creates a matrix of multiple lags (0:-p)
#'
#'@description
#'\code{mlag} returns VIF-values of the independent variables in the lag/diff form 
#'used in regression
#'
#'@details
#'It's really quite simple
#'
#'@param variable any variable
#'@param lags specified as from:to
#'@export
#'@return the imput object is returned silently


mlag <- function(variable, lags){
  var.lags <- NULL
  for (lag.order in lags){
   var.lag <- lag(variable, lag.order)
   var.lags <- cbind(var.lags, var.lag)
  }
  if (length(lags) > 1){
    colnames(var.lags) <- paste0("lag(", deparse(substitute(variable)), ", ", lags, ")")
  }
  return(var.lags)
}