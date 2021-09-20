
linreg <- function(formula, data){
  
  #' Performs linear regression on the input data
  #'
  #' @export linreg
  #' @param X A matrix of linear equations with respect to dependent variables
  #' @param Y A vector of dependent variables
  #' @param qrX A matrix with the same dimensions as X. The upper triangle contains the \bold{R} of the decomposition and the lower triangle contains information on the \bold{Q} of the decomposition (stored in compact form)
  #' @param Coeff Coefficients of the linear model
  #' @param Resid The residuals
  #' @param Fitted The fitted fitted values
  #' @param R The R matrix stored in qrX
  #' @param XprimXtinv Regression Diagnostics
  #' @param n The number of rows in X
  #' @param p The number of columns in X
  #' @param s2 The residual variance
  #' @param s2value s2 as a scalar
  #' @param nymatris X multiplied by regression diagnostics nultiplied by transposed X
  #' @param rri Diagonalised nymatris
  #' @param Stdres Standardised residuals
  #' @param covarBetahat The variance and covariance matrix
  #' @param varbetahat The variance of betahats
  #' @param tvalues The t-values of the model
  #' @param ptvalues The pt-values of the model
  #'
  #' @return Returns the results of the linear regression analysis stored in an object called linreg under an S3 class
  #' @examples
  #' linreg(speed ~ dist, cars)
  #' linreg(Employed ~ ., longley)
  #' @source \url{http://web.nchu.edu.tw/~numerical/course1012/ra/Applied_Regression_Analysis_A_Research_Tool.pdf}
  
  # Creates a X- and a Y-matrix
  
  X <- stats::model.matrix(formula, data)              # The matrix of linear equations with respect to dependent variables
  Y <- unlist(data[[all.vars(formula)[1]]])     # The vector of dependent variables
  
  qrX <- qr(X)                                  # QR-function in R.
  
  R <- qr.R(qrX)                                # The R-matrix, used below.
  
  XprimXtinv <- ginv(R) %*% ginv(t(R))          # Page 71, "Regression Diagnostics"
  
  
  # ------------------------------------------- Calculating a number of entities.
  # See 'Applied regression analysis', 2nd edition, page 144.
  
  Hatmatrix <- X %*% XprimXtinv %*% t(X)           # The Hat-matrix Page 16 and 71, "Regression Diagnostics"
  
  Fitted <- Hatmatrix %*% Y
  Resid  <- Y - Fitted
  Coeff  <- XprimXtinv %*% t(X) %*% Y            # Page 78 in 'Applied regression analysis', 2nd edition,
  
  rri <- diag(Hatmatrix)
  
  dimX <- dim(X)
  
  n <- dimX[1]                                  # Dimensions of X.
  p <- dimX[2]
  
  s2 <- t(Resid)%*%Resid/(n - p)                # Residual variance. 
  
  s2value <- s2[1,1]                            # s2 as a scalar.
  
  Stdres <- Resid /sqrt((1 - rri)*s2value)      # Standardised residuals.
  # ---------------------------------------------------------------------
  
  
  covarBetahat <- s2value * XprimXt             # Var and covar matrix
  
  varbetahat <- diag(covarBetahat)              # var of beta-hats.
  
  tvalues <- Coeff/sqrt(varbetahat)             # Calculating t-values.
  
  ptvalues <- (1 - pt(abs(tvalues), (n-p)))*2
  
  
  retur <- list("Coeff" = Coeff, "varB" = varbetahat, "Resid" = Resid, "Fitted" = Fitted, "Stdres" = Stdres, "s2" = s2, "tvalues" = tvalues, "pvalues" = ptvalues, "DoF" = DoF)
  
  #Save the object return under the class lineg
  class(retur) <- "linreg"
  
  #Create Methods print, plot, resid, pred, coef
  print1 <- function (obj, ...) {
    UseMethod("print1", obj)
  }
  
  plot <- function (obj, ...) {
    UseMethod("plot", obj)
  }
  
  resid <- function (obj, ...) {
    UseMethod("resid", obj)
  }
  
  pred <- function (obj, ...) {
    UseMethod("pred", obj)
  }
  
  coef <- function (obj, ...) {
    UseMethod("coef", obj)
  }
  
  summary <- function (obj, ...) {
    UseMethod("summary", obj)
  }
  
  return(retur)
  
}
