#'@title get the xreg from a vws regression
#'
#'@description
#'\code{getxreg} gets the xreg that was used in a vws regression 
#'
#'@details
#'Not much to add
#'
#'@param object an object of class \code{vws}
#'@param data the dataframe used in the regression that created the \code{vws} object
#'@export
#'@return the imput object is returned silently

getxreg <- function(object, data = NULL){
  if(is.null(data)){
    data <- eval(object$call$data)
  }
  eq<-Reduce(paste, deparse(object$call$formula))
  dep<-substr(eq, 1, regexpr('~', eq)-1)
  dep<-parse(text=dep)
  indep<-as.vector(strsplit(substr(eq, regexpr('~', eq)+1, nchar(eq)), "\\+"))[[1]]
  indep<-trimws(indep)
  varlist<-gsub("[, )*^].*", "", gsub(".*[(]", "", c(dep, indep)))
  varlist=unique(varlist)
  for (variable in varlist){assign(variable, ts(data[[variable]]))}
  y<-eval(dep)
  xreg<-y
  xnames<-NULL
  for (ind in indep){
    fa<-parse(text = ind)
    fa<-eval(fa)
    if(grepl("diff.+mlag", ind)){
      d_str <- gsub(" ", "", ind)
      d_str <- sub('diff\\(mlag\\([^//)]+', '', d_str)
      if(grepl("[0-9]+\\)", d_str)){
        n_diff <- gsub("[^0-9]", "", d_str)
        colnames(fa) <- paste0("diff(", colnames(fa), ", ", n_diff, ")")
      }else{
        colnames(fa) <- paste0("diff(", colnames(fa), ", ", "1", ")")}
    }
    if (all(class(fa)=="factor")){
      l<-length(levels(fa))
      nem<-levels(fa)
      fa<-model.matrix(~fa)[,2:l]
      for (j in colnames(fa)){
        xt<-ts(fa[, j])
        xreg<-cbind(xreg, xt)
        xnames<-c(xnames, j)
      }
      next
    } else {
      xreg<-cbind(xreg, fa)
      if (is.matrix(fa)){
        xnames <- c(xnames, colnames(fa))
      }else{
        xnames<-c(xnames, ind)
      }
    }
  }
  colnames(xreg)<-c(dep, trimws(xnames))
  xreg <- rbind(matrix(NA, nrow = attributes(xreg)$tsp[1] - 1, ncol = ncol(xreg)), xreg)
  xreg <- xreg[1:nrow(data), ]
  xreg <- xreg[, 2:NCOL(xreg), drop=FALSE]
  #xreg <- xreg[complete.cases(xreg), 2:NCOL(xreg), drop=FALSE]
  return(xreg)
}