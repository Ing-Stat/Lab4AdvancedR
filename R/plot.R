plot <- function(Resid, Fitted, stres) {

allaData <- data.frame(Resid, Fitted, stres)

ggplot(allaData, aes(x = Resid, y = Fitted) + geom_point())

ggplot(allaData, aes(x = stres, y = Fitted) + geom_point())

}