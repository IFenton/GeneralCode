# predict for sarlm
# 07 / 08 / 14
# Isabel Fenton

# The predict function for sarlm (predict.sarlm) cannot handle poly variables

# n.b. the same set of environmental variables will give slightly different predictions when based on the original data compared with new data. This is because the former values can measure the signal as well as the trend, whereas the latter just predicts the trend. 

## Errors:
# Error in X.old %*% B : non-conformable arguments - need to remove null factor levels

sar.predict <- function (object, newdata = NULL, olddata = NULL, listw = NULL, zero.policy = NULL, 
          legacy = TRUE, power = NULL, order = 250, tol = .Machine$double.eps^(3/5), 
          ...) # add term for olddata
{
  if (is.null(zero.policy)) 
    zero.policy <- get("zeroPolicy", envir = spdep:::.spdepOptions) # need to do this to specify the environment
  stopifnot(is.logical(zero.policy))
  if (object$type == "sac") 
    stop("no predict method for sac")
  if (is.null(power)) 
    power <- object$method != "eigen"
  stopifnot(is.logical(legacy))
  stopifnot(is.logical(power))
  if (is.null(newdata)) {
    res <- fitted.values(object)
    X <- object$X
    B <- object$coefficients
    y <- object$y
    tarX <- object$tarX
    tary <- object$tary
    if (object$type == "error") {
      attr(res, "trend") <- as.vector(X %*% B)
      attr(res, "signal") <- as.vector(-1 * (tary - y) - 
                                         -1 * (tarX - X) %*% B)
    }
    else {
      attr(res, "trend") <- as.vector(X %*% B)
      attr(res, "signal") <- as.vector(-1 * (tary - y))
    }
  }
  else {
    if (is.null(olddata)) stop("Requires olddata: the data object on which the model was originally run")
    if (object$type == "error") {
      if (object$etype == "error") {
        B <- object$coefficients
        frm <- formula(object$call)
        # obtained the predicted values using the old data
        mt.old <- delete.response(terms(frm, data = olddata))
        mf.old <- model.frame(mt.old, olddata)
        # check order of old and new (or it won't throw an error if wrong)
        X.old <- model.matrix(mt.old, mf.old)
        if (any(object$aliased)) 
          X.old <- X.old[, -which(object$aliased)]
        trend.old <- X.old %*% B
        if (any(as.vector(object$X %*% object$coefficients) != trend.old)) stop("Olddata doesn't match: order should be newdata, olddata")
        # run for the newdata        
        mt <- delete.response(terms(frm, data = newdata))
        # add the attributes to the terms so poly will predict
        attr(mt, "predvars") <- attr(attr(mf.old, "terms"), "predvars")
        attr(mt, "dataClasses") <- attr(attr(mf.old, "terms"), "dataClasses")
        mf <- model.frame(mt, newdata)
        X <- model.matrix(mt, mf)
        if (any(object$aliased)) 
          X <- X[, -which(object$aliased)]
        trend <- X %*% B
        signal <- rep(0, length(trend))
        res <- trend + signal
        attr(res, "trend") <- trend
        attr(res, "signal") <- signal
      }
      else if (object$etype == "emixed") {
        warning("Not tested for poly variables") # added
        if (is.null(listw) || !inherits(listw, "listw")) 
          stop("spatial weights list required")
        if (nrow(newdata) != length(listw$neighbours)) 
          stop("mismatch between newdata and spatial weights")
        B <- object$coefficients
        frm <- formula(object$call)
        mt <- delete.response(terms(frm, data = newdata))
        mf <- model.frame(mt, newdata)
        X <- model.matrix(mt, mf)
        K <- ifelse(colnames(X)[1] == "(Intercept)", 
                    2, 1)
        m <- ncol(X)
        if (m > 1) {
          WX <- matrix(nrow = nrow(X), ncol = (m - (K - 
                                                      1)))
          for (k in K:m) {
            wx <- lag.listw(listw, X[, k], zero.policy = zero.policy)
            if (any(is.na(wx))) 
              stop("NAs in lagged independent variable")
            WX[, (k - (K - 1))] <- wx
          }
        }
        if (K == 2) {
          if (!(listw$style == "W")) {
            intercept <- as.double(rep(1, nrow(X)))
            wx <- lag.listw(listw, intercept, zero.policy = zero.policy)
            if (m > 1) {
              WX <- cbind(wx, WX)
            }
            else {
              WX <- matrix(wx, nrow = nrow(X), ncol = 1)
            }
          }
        }
        X <- cbind(X, WX)
        if (any(object$aliased)) 
          X <- X[, -which(object$aliased)]
        trend <- X %*% B
        signal <- rep(0, length(trend))
        res <- trend + signal
        attr(res, "trend") <- trend
        attr(res, "signal") <- signal
      }
      else stop("unkown error model etype")
    }
    else if (object$type == "mixed") {
      warning("Not tested for poly variables") # added
      if (is.null(listw) || !inherits(listw, "listw")) 
        stop("spatial weights list required")
      if (nrow(newdata) != length(listw$neighbours)) 
        stop("mismatch between newdata and spatial weights")
      B <- object$coefficients
      frm <- formula(object$call)
      mt <- delete.response(terms(frm, data = newdata))
      mf <- model.frame(mt, newdata)
      X <- model.matrix(mt, mf)
      K <- ifelse(colnames(X)[1] == "(Intercept)", 2, 
                  1)
      m <- ncol(X)
      if (m > 1) {
        WX <- matrix(nrow = nrow(X), ncol = (m - (K - 
                                                    1)))
        for (k in K:m) {
          wx <- lag.listw(listw, X[, k], zero.policy = zero.policy)
          if (any(is.na(wx))) 
            stop("NAs in lagged independent variable")
          WX[, (k - (K - 1))] <- wx
        }
      }
      if (K == 2) {
        if (!(listw$style == "W")) {
          intercept <- as.double(rep(1, nrow(X)))
          wx <- lag.listw(listw, intercept, zero.policy = zero.policy)
          if (m > 1) {
            WX <- cbind(wx, WX)
          }
          else {
            WX <- matrix(wx, nrow = nrow(X), ncol = 1)
          }
        }
      }
      X <- cbind(X, WX)
      if (any(object$aliased)) 
        X <- X[, -which(object$aliased)]
      trend <- X %*% B
      if (power) {
        W <- as(as_dgRMatrix_listw(listw), "CsparseMatrix")
        res <- c(as(powerWeights(W, rho = object$rho, 
                                 X = trend, order = order, tol = tol), "matrix"))
      }
      else {
        res <- c(invIrW(listw, object$rho) %*% trend)
      }
      if (legacy) {
        signal <- object$rho * lag.listw(listw, res, 
                                         zero.policy = zero.policy)
        res <- c(trend + signal)
      }
      else {
        signal <- res - trend
      }
      attr(res, "trend") <- c(trend)
      attr(res, "signal") <- c(signal)
    }
    else {
      warning("Not tested for poly variables") # added
      if (is.null(listw) || !inherits(listw, "listw")) 
        stop("spatial weights list required")
      if (nrow(newdata) != length(listw$neighbours)) 
        stop("mismatch between newdata and spatial weights")
      B <- object$coefficients
      frm <- formula(object$call)
      mt <- delete.response(terms(frm, data = newdata))
      mf <- model.frame(mt, newdata)
      if (dim(mf)[1] != length(listw$neighbours)) 
        stop("missing values in newdata")
      X <- model.matrix(mt, mf)
      if (any(object$aliased)) 
        X <- X[, -which(object$aliased)]
      trend <- X %*% B
      if (power) {
        W <- as(as_dgRMatrix_listw(listw), "CsparseMatrix")
        res <- c(as(powerWeights(W, rho = object$rho, 
                                 X = trend, order = order, tol = tol), "matrix"))
      }
      else {
        res <- c(invIrW(listw, object$rho) %*% trend)
      }
      if (legacy) {
        signal <- object$rho * lag.listw(listw, res, 
                                         zero.policy = zero.policy)
        res <- c(trend + signal)
      }
      else {
        signal <- res - trend
      }
      attr(res, "trend") <- c(trend)
      attr(res, "signal") <- c(signal)
    }
  }
  class(res) <- "sarlm.pred"
  res
}

  
  
