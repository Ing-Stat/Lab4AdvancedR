plot <- function(Resid, Fitted, stres) {

allaData <- data.frame(Resid, Fitted, stres)

ggplot(allaData, aes(x = Resid, y = Fitted) + geom_point() + ylab('Våra nya Y-värden') + xlab('Våra nya X-värden'))

ggplot(allaData, aes(x = stres, y = Fitted) + geom_point() + ylab('Våra nya Y-värden') + xlab('Våra nya X-värden'))

}