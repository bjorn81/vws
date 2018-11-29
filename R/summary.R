#'@title Summarizes vws estimate results
#'
#'@description
#'\code{summary.vws} summarizes the results of a regression with ARMA errors
#'
#'@details
#'Shows the most important information about the regression
#'
#'@param object an object of class \code{vws}
#'@method summary vws
#'@export
#'@return the imput object is returned silently
#'@examples
#'#'eu<-data.frame(EuStockMarkets)
#'eu$date<-seq.Date(from = as.Date('2014-01-30'), by = 'day', length.out = nrow(eu))
#'reg<-vws(diff(DAX, 1)~diff(CAC,1)+diff(SMI,1)+lag(diff(SMI,1),-5), ar=1, ma = 0, 
#'    data = subset(eu, subset = date>'2019-01-01'), datecolumn = "date")
#'summary(reg)


summary.vws <- function(object){
  if (!inherits(object, "vws"))
    stop("Object must be if class 'vws'")
  n<-object$nobs
    #length(na.omit(object$residuals))
  k<-length(object$coef)
  x<-cbind(object$fitted, object$fitted + object$residuals)
  rsq<-cor(x[,1],x[,2], use = "pairwise.complete.obs")^2
  adjrsq<-1-((1-rsq)*(n-1))/(n-k-1)
  std<-sqrt(diag(object$var.coef))
  t<-object$coef/sqrt(diag(object$var.coef))
  if (object$model$meth=="CSS"){
    p<-2*pt(abs(t), 
            df = n-k, 
            lower.tail = F)
    s<-mystars <- ifelse(p < .001, "***", 
                         ifelse(p < .01, "** ", ifelse(p < .05, "*  ", "   ")))
  } else {
    p<-2*(1-pnorm(abs(t)))
    s<-mystars <- ifelse(p < .001, "***", 
                         ifelse(p < .01, "** ", ifelse(p < .05, "*  ", "   ")))
  }
  ps<-paste(format(round(p,digits = 5), nsmall= 5), s)
  cat("call:\n")
  print(object$call)
  cat("\n")
  print(cbind(Estimates=format(round(object$coef,digits = 5) ,nsmall = 5),
              Std.Error=format(round(std, digits = 5), nsmall = 5),
              "t value"=format(round(t, digits = 3), nsmall = 3),
              "Pr(>|t|)"=ps), quote = FALSE ,right = TRUE)
  cat(paste0("\nR-squared: ", round(rsq,digits = 3),
             ", Adj.R-squared: ", round(adjrsq,digits = 3),
             ", AIC: ", round(object$aic,digits = 2),
             ", log likelihood: ", round(logLik(object)[1],
                                               digits = 2)))
  cat(paste0("\nsigma^2 estimated as: ", round(object$sigma2,digits = 3),
             ", Degrees of freedom: ", n-k))
  
  
}
