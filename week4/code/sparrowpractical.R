setwd("/Users/emmadeeks/Desktop/CMEECourseWork/week5/sandbox/data")
d <- read.csv("rodents.csv")



d <- subset(d, d$sex!='P')
d <- subset(d, d$sex!='Z')
d <- subset(d, d$sex!='R')
d <- subset(d, d$sex!='')

require(ggplot2)
ggplot(data = d, aes(x=d$yr, y=d$precip, fill=d$sex)) + geom_bar(position = "fill", stat = "identity")


t.test(d$precip~d$sex, na.rm=TRUE)


male <- subset(d, sex=="M")
female <- subset(d, sex=="F")
a <- mean(female$precip)
b <- mean(male$precip)
barplot(a,b)
ggplot(data=d, aes(x=year, y=amount)) + geom_bar(stat="identity")