# Creates a X- and a Y-matrix

X <- matrix(c(1, 1, 1, 1, 1, 9.3, 9.8, 7.7, 6.9, 7.3, 12.4, 18.8, 21.7, 22.9, 17.6, 18.7, 12.9, 14.5, 7.6, 5.8), ncol = 4, nrow = 5)

Y <- c(4.5, 6.6, 2.4, 4.9, 6.2)


qrX <- qr(X)                                  # QR-function in R.

Coeff <- qr.coef(qrX, Y)                      # Coeffs of the linear model.
Resid <- qr.resid(qrX, Y)                     # The residuals.

Fitted <- qr.fitted(qrX, Y)                   # Fitted values.

R <- qr.R(qrX)                                # The R-matrix, used below.

XprimXt <- solve(R) %*% solve(t(R))           # Page 71, "Regression Diagnostics"

dimX <- dim(X)

n <- dimX[1]                                  # Dimensions of X.
p <- dimX[2]

s2 <- t(Resid)%*%Resid/(n - p)                # Residual variance. 

s2value <- s2[1,1]                            # s2 as a scalar.

covarBetahat <- s2value * XprimXt             # Var and covar matrix

varbetahat <- diag(covarBetahat)              # var of beta-hats.

tvalues <- Coeff/sqrt(varbetahat)             # Calculating t-values.


ptvalues <- (1 - pt(abs(tvalues), (n-p)))*2

Coeff
Fitted
varbetahat
tvalues
ptvalues