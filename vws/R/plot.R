#'@title Plots vws estimate results
#'
#'@description
#'\code{plot.vws} plots fitted vs actual and residuals for regression with 
#'ARMA errors
#'
#'@details
#'By default 2 sets of plots are shown. First actual v.s. fitted and residuals.
#'Second two graphs showing the auto correaltion function (ACF) and 
#'the partial autocorrelation fuction.
#'
#'@param object an object of class \code{vws}
#'@param which as in plots to show
#'@param fromdate starting date to plot from, has to be readable as date
#'@param todate ending date in plot
#'@param interactive if true lets you click on data points in plot 1 (actual)
#'@method plot vws
#'@export
#'@return the imput object is returned silently
#'@examples
#'eu<-data.frame(EuStockMarkets)
#'eu$date<-seq.Date(from = as.Date('2014-01-30'), by = 'day', length.out = nrow(eu))
#'reg<-vws(diff(DAX, 1)~diff(CAC,1)+diff(SMI,1)+lag(diff(SMI,1),-5), ar=1, ma = 0, 
#'    data = subset(eu, subset = date>'2019-01-01'), datecolumn = "date")
#'plot(reg, which = 1, interactive=TRUE)
#'plot(reg, fromdate = "2019-02-15", todate = "2019-03-01")

plot.vws<-function(object, which=c(1,2,3), fromdate=NULL, todate=NULL, 
               interactive=FALSE){
  if (1 %in% which){
    par(mfrow=c(2,1))
    par(mar=c(2.4,4.1,0.55,1.1))
    if (!is.null(fromdate)){
      fromdate<-as.Date(fromdate)
    }
    if (!is.null(todate)){
      todate<-as.Date(todate)
    }
    if ('dates' %in% names(object)){
      fitted.and.actual<-zoo(cbind(object$fitted, object$fitted+object$residuals),
                             order.by = object$dates)
      fitted.and.actual<-window(fitted.and.actual, start = fromdate, end = todate)
      fitted.and.actual<-na.omit(fitted.and.actual)
      
      residuals<-zoo(object$residuals, object$dates)
      residuals<-window(residuals, start = fromdate, end = todate)
      residuals<-na.omit(residuals)
    } else{
      fitted.and.actual<-zoo(cbind(object$fitted, object$fitted+object$residuals))
      residuals<-zoo(object$residuals)
    }
    plot.zoo(fitted.and.actual, 
             col=c(rgb(1,0,0,alpha=0.5),rgb(0,0,1,alpha=0.55)),type="l", lwd=1.25,
             xlab="", ylab="", plot.type = "single")
    title(ylab=expression("actual" * phantom(" v.s. fitted")), 
          col.lab="blue3")
    title(ylab=expression(phantom("actual v.s. ") * "fitted"),
          col.lab="red3")
    title(ylab=expression(phantom("actual ") *"v.s. " * phantom("fitted"),
                          col.lab="black"))
    if (interactive==TRUE){
      identify(x = index(fitted.and.actual), y = fitted.and.actual[,2],
               labels = index(fitted.and.actual), cex = 0.75)}
    
    plot.zoo(residuals, col="gray25", type="h", lwd=1.5,
             ylab="", xlab="")
    abline(h=0, lty=3)
    title(ylab="residuals", col.lab="slateblue4")
    if (interactive==TRUE){
      identify(x = index(residuals), y = residuals,
               labels = index(residuals), cex = 0.75)}
  }
  if (2 %in% which){
    par(mfrow=c(2,1))
    par(mar=c(2.4,4.1,0.55,1.1))
    if (1 %in% which){
      invisible(readline(prompt = "Hit <Return> to see next plot: "))}
    Acf(object$residuals, col="blue4", lwd=1)
    Pacf(object$residuals, col="darkred", lwd=1)
  }
  par(mfrow=c(1,1))
  par(mar=c(5.1,4.1,4.1,2.1))
  if(sum(object$arma[1:2])>0 & 3 %in% which){
    if (1 %in% which | 2 %in% which){
      invisible(readline(prompt = "Hit <Return> to see next plot: "))}
    plot.Arima(object)
  }
}
