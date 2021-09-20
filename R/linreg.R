linreg <- function(){

  #X <- matrix(c(1, 1, 1, 1, 1, 9.3, 9.8, 7.7, 6.9, 7.3, 12.4, 18.8, 21.7, 22.9, 17.6, 18.7, 12.9, 14.5, 7.6, 5.8), ncol = 4, nrow = 5)
  #Y <- c(4.5, 6.6, 2.4, 4.9, 6.2)
  
  X <- data.matrix(cars$dist)                   # Makes matrices of variables from dataframe 'cars'.
  Y <- data.matrix(cars$speed)
  
  X0 <- rep(1, length(X))                       # Creates a column of '1':s to add to the X-matrix.
  X  <- cbind(X0, X)
  
  qrX <- qr(X)                                  # QR-function in R.
  
  #Coeff <- qr.coef(qrX, Y)                      # Coeffs of the linear model.
  #Resid <- qr.resid(qrX, Y)                     # The residuals.
  #Fitted <- qr.fitted(qrX, Y)                   # Fitted values.
  
  R <- qr.R(qrX)                                # The R-matrix, used below.
  
  #XprimXt <- solve(R) %*% solve(t(R))           # Page 71, "Regression Diagnostics"
  
  XprimXtinv <- ginv(R) %*% ginv(t(R))          # Page 71, "Regression Diagnostics"
  
  
  # ------------------------------------------- Calculating a number of entities.
  # See 'Applied regression analysis', 2nd edition, page 144.
  
  Hatmatrix <- X %*% XprimXtinv %*% t(X)        # The Hat-matrix Page 16 and 71, "Regression Diagnostics"
  
  
  Fitted <- Hatmatrix %*% Y
  Resid  <- Y - Fitted
  Coeff  <- XprimXtinv %*% t(X) %*% Y           # Page 78 in 'Applied regression analysis', 2nd edition,
  
  rri <- diag(Hatmatrix)
  
  dimX <- dim(X)
  
  n <- dimX[1]                                  # Dimensions of X.
  p <- dimX[2]
  
  s2 <- t(Resid)%*%Resid/(n - p)                # Residual variance. 
  
  s2value <- s2[1,1]                            # s2 as a scalar.
  
  Stdres <- Resid /sqrt((1 - rri)*s2value)      # Standardised residuals.
  # ---------------------------------------------------------------------
  
  # dimX <- dim(X)
  # 
  # n <- dimX[1]                                  # Dimensions of X.
  # p <- dimX[2]
  # 
  # s2 <- t(Resid)%*%Resid/(n - p)                # Residual variance. 
  # 
  # s2value <- s2[1,1]                            # s2 as a scalar.
  
  covarBetahat <- s2value * XprimXtinv          # Var and covar matrix
  
  varbetahat <- diag(covarBetahat)              # var of beta-hats.
  
  tvalues <- Coeff/sqrt(varbetahat)             # Calculating t-values.
  
  ptvalues <- (1 - pt(abs(tvalues), (n-p)))*2
retur <- list("Coeff" = Coeff, "varB" = varbetahat, "Resid" = Resid, "Fitted" = Fitted, "Stdres" = Stdres, "s2" = s2, "tvalues" = tvalues, "pvalues" = ptvalues) 
return(retur)

}
