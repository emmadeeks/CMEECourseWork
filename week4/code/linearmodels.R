
setwd("/Users/emmadeeks/Desktop/CMEECourseWork/week5/data")

library(repr) ; options(repr.plot.width=4, repr.plot.height= 4)
rm(list = ls())
graphics.off()
install.packages('repr')
library(repr)
install.packages
require("minpack.lm")
options(repr.plot.width=4, repr.plot.height=4)



powMod <- function(x, a, b) {
  return(a * x^b)
}


MyData <- read.csv("../data/GenomeSize.csv")
Data2Fit <- subset(MyData,Suborder == "Anisoptera")

Data2Fit <- Data2Fit[!is.na(Data2Fit$TotalLength),] # remove NA's
plot(Data2Fit$TotalLength, Data2Fit$BodyWeight)


library("ggplot2")

ggplot(Data2Fit, aes(x = TotalLength, y = BodyWeight)) + 
  geom_point(size = (3),color="red") + theme_bw() + 
  labs(y="Body mass (mg)", x = "Wing length (mm)")

PowFit <- nlsLM(BodyWeight ~ powMod(TotalLength, a, b), data = Data2Fit, start = list(a = .1, b = .1))

summary(PowFit)
Lengths <- seq(min(Data2Fit$TotalLength),max(Data2Fit$TotalLength),len=200)
coef(PowFit)["a"]
coef(PowFit)["b"]
Predic2PlotPow <- powMod(Lengths,coef(PowFit)["a"],coef(PowFit)["b"])
plot(Data2Fit$TotalLength, Data2Fit$BodyWeight)
lines(Lengths, Predic2PlotPow, col = 'blue', lwd = 2.5)
confint(PowFit)
















