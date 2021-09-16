plot <- function(Resid, Fitted, Stdres) {

library(ggplot2)
  
allaData <- data.frame(Resid, Fitted, Stdres)
ggplot(allaData, aes(x = Resid, y = Fitted)) + geom_point() + ylab('Residuals') + xlab('Fitted values')
ggplot(allaData, aes(x = Stdres, y = Fitted)) + geom_point() + ylab('Standardized resuduals') + xlab('Fitted values')

}