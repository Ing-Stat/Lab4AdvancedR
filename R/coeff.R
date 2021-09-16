coeff <- function(Coeff) {
  
  Coeff <- round(Coeff, digits=3)
  return (c("Coefficients X0" = Coeff[1], "X1" = Coeff[2], "X2" = Coeff[3], "X3" = Coeff[4]))
}