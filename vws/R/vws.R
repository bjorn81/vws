#' @title Eviews style regression specification
#'
#' @description
#'\code{vws} Allows the specification of a regression with lag(x, k), diff(x, p)
#'as well as ARMA errors.
#' structure
#'
#'@details
#'Returns an object of class "vws", which inherits from class "arima". If no ARMA
#'structure of errors is specified and "lm" = true , the returned object is of
#'class "lm". Generic functions \code{plot} and \code{summary} can be used.
#'
#'@param formula Regression formula to be evaluated
#'
#'@param ar number of autoregressive (AR) terms to be included in regression
#'
#'@param ma Number of moving average (MA) terms to be included in regression
#'
#'@param data The dataframe which contains the variables used
#'
#'@param method Estimation method (CSS-ML, ML, CSS)
#'
#'@param datecolumn Optional column of class Date
#'
#'@param lmclass if true and no ARMA errors specifed, object returned is of class lm
#'
#'@import forecast
#'
#'@export
#'
#'@return a list of x elements:
#'\item{coef}{a vector of AR, MA and regression coefficients, which can be extracted by the coef method.}
#'\item{sigma2}{the MLE of the innovations variance.}
#'\item{var.coef}{the estimated variance matrix of the coefficients coef, 'which can be extracted by the vcov method.}
#'\item{loglik}{the maximized log-likelihood (of the differenced data), or the approximation to it used.}
#'\item{aic}{}
#'\item{arma}{A compact form of the specification, as a vector giving the number of AR, MA, 'seasonal AR and seasonal MA coefficients, plus the period and the number of non-seasonal and seasonal differences.}
#'\item{residuals}{the fitted innovations.}
#'\item{fitted}{Fitted values of the formula}
#'\item{dates}{Optional dates for the fitted series}
#'@author Bjorn Backgard
#'@examples
#'#Generate some data: y0 follows an ARMA process conditional on x1, x2
#'error.model=function(n){rnorm(n, sd=.2)}
#'y0 <- arima.sim(model=list(ar=0.6, ma = -0.9), n=100,
#'                n.start=200, start.innov=rnorm(200, sd=.2),
#'                rand.gen=error.model )
#'x1 <- arima.sim(model=list(ar=0.95), n=100,
#'                n.start=200, start.innov=rnorm(200, sd=.2),
#'                rand.gen=error.model)
#'x2 <- arima.sim(model=list(ar=0.95), n=100,
#'                n.start=200, start.innov=rnorm(200, sd=.2),
#'                rand.gen=error.model)
#'y <- y0 + 0.5*x2 + 0.2 * x1
#'par(mfrow = c(2,2)) #Plot the variables
#'plot(y0)
#'plot(x1)
#'plot(x2)
#'plot(y)
#'dt <- data.frame(y,x1,x2)
#'par(mfrow = c(1,1))
#'library(vws)
#'vws.mod <- vws(y~x1+x2, ar = 0, ma = 0, data = dt) #Fit model
#'summary(vws.mod) #Get regression output
#'plot(vws.mod, 2) #Examine autocorrelation structure, ARMA terms needed
#'vws.mod <- vws(y~x1+x2, ar = 1, ma = 1, data = dt) #Fit new model
#'summary(vws.mod) #Get regression output
#'plot(vws.mod, 2) #Examine autocorrelation structure
#'plot(vws.mod, 3) #Check AR and MA roots
#'tsdiag(vws.mod) #No evidence of remaining autocorrelation in errors
#'plot(vws.mod, 1) #Check fit of final model
#'
vws<-function(formula, ar=0, ma=0, data, method="ML", subset = NULL,
              datecolumn=NULL, lmclass=FALSE, ...){
  cl <- match.call()
  eq<-Reduce(paste, deparse(eval.parent(cl$formula, n =1)))
  dep_var<-substr(eq, 1, regexpr('~', eq)-1)
  dep_var<-parse(text=dep_var)
  indep<-as.vector(strsplit(substr(eq, regexpr('~', eq)+1, nchar(eq)), "\\+"))[[1]]
  indep<-trimws(indep)
  varlist<-gsub("[, )*^].*", "", gsub(".*[(]", "", c(dep_var, indep)))
  varlist=unique(varlist)
  for (variable in varlist){assign(variable, ts(data[[variable]]))}
  y<-eval(dep_var)
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
  colnames(xreg)<-c(dep_var, trimws(xnames))
  xreg <- rbind(matrix(NA, nrow = attributes(xreg)$tsp[1] - 1, ncol = ncol(xreg)), xreg)
  xreg <- xreg[1:nrow(data), ]
  subselect <- eval(eval(quote(cl$subset)), data)
  if(is.null(subselect)) subselect <- TRUE
  xreg <- xreg[subselect, ]
  #colnames(xreg)<-c(dep_var, trimws(indep), nem)
  if (ar == 0 & ma == 0 & lmclass == TRUE){
    arma.mod<-lm(xreg[,1]~xreg[, 2:NCOL(xreg), drop=FALSE])
    names(arma.mod$coefficients)[2:length(arma.mod$coefficients)]<-trimws(xnames)
    arma.mod$call <- cl
    return(arma.mod)
  } else {
    arma.mod<-arima(xreg[,1], order =c(ar,0,ma),
                    xreg = xreg[, 2:NCOL(xreg), drop=FALSE],
                    method = method, include.mean = TRUE)

    #if (ar>0){
    #arma.mod$coef["intercept"]<-arma.mod$coef["intercept"]/
    #  (1-sum(coef(arma.mod)[paste0("ar", 1:ar)]))
    #}
    arma.mod$call <- cl
    arma.mod$model$meth<-method
    arma.mod$fitted<-fitted(arma.mod)
    arma.mod$xreg <- xreg
    # arma.mod$xreg <- xreg[, 2:NCOL(xreg), drop=FALSE]
    if (!is.null(datecolumn)){
      arma.mod$dates<-data[[datecolumn]][subselect]
      }else{
      is_date <- which(sapply(data, inherits, what = c("Date", "POSIXct")))
      if (length(is_date) > 0){
        first_date <- min(is_date)
        arma.mod$dates<-data[[first_date]][subselect]
      }
    }
    class(arma.mod)<-c("vws", "Arima")
    return(arma.mod)

  }
}
