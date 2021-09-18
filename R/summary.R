summary <- function(Coeff, varbeta, tvalues, pvalues) {
  
  
  Coeff   <- round(Coeff, digits = 3)
  varbeta <- round(sqrt(varbeta), digits = 3)
  tvalues <- round(tvalues, digits = 3)
  pvalues <- round(pvalues, digits = 3)
  
 minDF <- data.frame(Coeff, varbeta, tvalues, pvalues)

  return (minDF)
}

