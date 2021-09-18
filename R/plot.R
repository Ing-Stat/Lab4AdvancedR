plot <- function(Resid, Fitted, Stdres) {

library(ggplot2)
library(dplyr)
  
allaData <- data.frame(Resid, Fitted, Stdres)
plot1 <- ggplot(allaData, aes(x = Fitted, y = Resid)) + geom_point() + ylab('Residuals') + xlab('Fitted values')

Stdres <- abs(Stdres)
allaData <- data.frame(Resid, Fitted, Stdres)
plot2 <- ggplot(allaData, aes(x = Fitted, y = Resid)) + geom_point() + ylab(expression(sqrt(abs('Standardized resuduals')))) + xlab('Fitted values')

retur <- list(plot1, plot2)
return(retur)
}