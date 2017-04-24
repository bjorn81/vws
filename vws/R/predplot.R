#'@title Plots the prediction of a vws model with confidence intervals
#'
#'@description
#'\code{predplot} plots fitted vs actual and residuals for regression with 
#'ARMA errors
#'
#'@details
#'Similar to plot(forecast())) in forecast library
#'@param object an object of class \code{vws}
#'@param prediction as returned from vws.predict
#'@export
#'@return the imput object is returned silently

predplot <- function(object = NULL, prediction = NULL){
  x <- attributes(prediction$pred)$tsp[1] : attributes(prediction$pred)$tsp[2]
  x <- c(min(x) - 1, x)
  y <- c(object$fitted, prediction$pred)
  y_last <- object$fitted[length(object$fitted)]
  ymin <- min(min(prediction$pred - 1.96 * prediction$se, na.rm = T), object$fitted, na.rm = T) * 0.95
  ymax <- max(max(prediction$pred + 1.96 * prediction$se, na.rm = T), object$fitted, na.rm = T) * 1.05
  

  plot(y, 
       type = "l", xlab = "period", ylab = "prediction, 80, 95 % interval", main = "prediction of model", col = "royalblue", lwd = 1.75,
       ylim = c(ymin, ymax))
  
  polygon(c(x,rev(x)),
          c(c(y_last, prediction$pred) - 1.96 * c(0, prediction$se), 
            rev(c(y_last, prediction$pred) + 1.96 * c(0, prediction$se))),
          col = "grey93", border = FALSE)
  polygon(c(x,rev(x)),
          c(c(y_last, prediction$pred) - 0.84 * c(0, prediction$se), 
            rev(c(y_last, prediction$pred) + 0.84 * c(0, prediction$se))),
          col = "gray82", border = FALSE)
  lines((length(y) - length(prediction$pred)):length(y), c(y[length(y) - length(prediction$pred)], prediction$pred), type = "l", col = "red", lwd = 2)
  
}
