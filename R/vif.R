#'@title get VIF-values of independent variables
#'
#'@description
#'\code{vif} returns VIF-values of the independent variables in the lag/diff form 
#'used in regression
#'
#'@details
#'It's really quite simple
#'
#'@param object an object of class \code{vws}
#'@param data the dataframe used in the regression that created the \code{vws} object
#'@export
#'@return the imput object is returned silently

vif<-function(object){
  data <- eval(object$call$data)
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
  xreg <- xreg[, 2:ncol(xreg)]
  vif.values<-data.frame(character(0), numeric(0))
  for (i in 1:ncol(xreg)){
    variable<-colnames(xreg)[i]
    reg.i<-lm(xreg[,i]~xreg[,-i])
    r2<-summary(reg.i)$r.squared
    vif.value<-round(1/(1-r2),2)
    vif.values<-rbind(vif.values, data.frame(variable, vif.value))
  }
  vif.values
  
}

