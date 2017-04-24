#'@title Predict using a fitted vws model
#'
#'@description
#'\code{predict.vws} Returns predicted values and standard errors for new data 
#'ARMA errors
#'
#'@details
#'Supplied a vws model and a new dataset it predicts n periods ahead. Not that n must
#'equal the number of rows in the new dataset.
#'@param object an object of class \code{vws}
#'@param n.ahead number of periods ahead to predict
#'@param data the original data
#'@param se.fit return standard errors or not
#'@method predict vws
#'@export
#'@return the imput object is returned silently
#'@examples
#'eu<-data.frame(EuStockMarkets)
#'eu$date<-seq.Date(from = as.Date('2014-01-30'), by = 'day', length.out = nrow(eu))
#'reg<-vws(diff(DAX, 1)~diff(CAC,1)+diff(SMI,1)+lag(diff(SMI,1),-5), ar=1, ma = 0, 
#'    data = subset(eu, subset = date>'2019-01-01'), datecolumn = "date")
#'predict.vws(reg, n.ahead = 13, dataset = eu, newxreg = getxreg(reg, subset(eu, date>="2019-02-20")))
predict.vws <- function (object, newxreg = NULL, se.fit = TRUE, ...) 
{
myNCOL <- function(x) if (is.null(x)) 
  0
else NCOL(x)
rsd <- object$residuals
xtsp <- tsp(rsd)
n <- length(rsd)
n.ahead <- nrow(newxreg)
arma <- object$arma
coefs <- object$coef
narma <- sum(arma[1L:4L])
if (length(coefs) > narma) {
  if (names(coefs)[narma + 1L] == "intercept") {
    newxreg <- cbind(intercept = rep(1, n.ahead), newxreg)
    }
  xm <- if (narma == 0) 
    drop(as.matrix(newxreg) %*% coefs)
  else drop(as.matrix(newxreg) %*% coefs[-(1L:narma)])
}
else xm <- 0
if (arma[2L] > 0L) {
  ma <- coefs[arma[1L] + 1L:arma[2L]]
  if (any(Mod(polyroot(c(1, ma))) < 1)) 
    warning("MA part of model is not invertible")
}
if (arma[4L] > 0L) {
  ma <- coefs[sum(arma[1L:3L]) + 1L:arma[4L]]
  if (any(Mod(polyroot(c(1, ma))) < 1)) 
    warning("seasonal MA part of model is not invertible")
}
z <- KalmanForecast(n.ahead, object$model)
pred <- ts(z[[1L]] + xm, start = xtsp[2L] + deltat(rsd), 
           frequency = xtsp[3L])
if (se.fit) {
  se <- ts(sqrt(z[[2L]] * object$sigma2), start = xtsp[2L] + 
             deltat(rsd), frequency = xtsp[3L])
  list(pred = pred, se = se)
}
else pred
}