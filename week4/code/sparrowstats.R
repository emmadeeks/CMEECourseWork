

d<-read.table("SparrowSize.txt", header=TRUE)
d1<-subset(d, d$Wing!="NA")
summary(d1$Wing)
hist(d1$Wing)
model1 <- lm(Wing~Sex.1, data = d1)
summary(model1)

boxplot(d1$Wing~d1$Sex.1, ylab="Wing length (mm)")
anova(model1)

t.test(d1$Wing~d1$Sex.1, var.equal=TRUE)
boxplot(d1$Wing~d1$BirdID, ylab = "Wing length (mm)")
install.packages("dplyr")
require(dplyr)

tbl_df(d1)
glimpse(d1)
d$Mass %>% cor.test(d$Tarsus, na.rm=TRUE)
d1 %>%
  group_by(BirdID) %>%
  summarise (count=length(BirdID))


d1 %>%
  group_by(BirdID) %>%
  summarise (count=length(BirdID))


d1 %>%
  group_by(BirdID) %>%
  summarise (count=length(BirdID)) %>%
  count(count)

count(d1, d1$BirdID) %>%
  count(count)

model3<-lm(Wing~as.factor(BirdID), data=d1)
anova(model3)


boxplot(d$Mass~d$Year)
m2<-lm(d$Mass~as.factor(d$Year))
anova(m2)
summary(m2)

t(model.matrix(m2))








