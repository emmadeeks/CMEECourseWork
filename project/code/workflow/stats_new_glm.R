
rm(list=ls()) #Clear global environment 



library("lme4")
library("glmmADMB")      ## (not on CRAN)
library("glmmTMB")
library("MCMCglmm")
library("blme")
library("MASS")          ## for glmmPQL (base R)
library("nlme")          ## for intervals(), tundra example (base R)
## auxiliary
library("ggplot2")




setwd("/Users/emmadeeks/Desktop/CMEECourseWork/project/data") #go to the data directory 


year_merge <- read.csv("stats/mum_all_days_counts_sharks_stand.csv")
#every_5_days <- read.csv("stats/sharks_stand_stats.csv")
#year_merge <- read.csv("stats/5_days_count_detec.csv")
#year_merge <- read.csv("stats/year_merge_5_days_count_detec.csv")

#mum_5_days$X5day_use <- round(mum_5_days$X5day_use)
#mum_5_days$X5day_shark <- round(mum_5_days$X5day_shark)
#mum_5_days$X5day_use <- as.integer(mum_5_days$X5day_use)
#mum_5_days$X5day_shark <- as.integer(mum_5_days$X5day_shark)


#year_merge$X5day_use <- round(year_merge$X5day_use)
#year_merge$X5day_shark <- round(year_merge$X5day_shark)
#year_merge$X5day_use <- as.integer(year_merge$X5day_use)
#year_merge$X5day_shark <- as.integer(year_merge$X5day_shark)

year_merge$year <- substr(year_merge$monthyear, 0, 4)
year_merge$day <- substr(year_merge$Var1, 1, 2)

#year_merge$X5day_use <- year_merge$X5day_use * 10
year_merge$use <- as.integer(year_merge$use)
year_merge$shark_stand <- as.integer(year_merge$shark_stand)
year_merge$Boat <- as.factor(year_merge$Boat)
year_merge$month <- substr(year_merge$monthyear, 6, 7)

year_merge$Var1 <- strptime(as.character(year_merge$Var1), "%d/%m/%Y")



km_inner <- read.csv("../data/stats/km_inner.csv")
km_inner$Date <- as.character(km_inner$Date)
km_inner <- km_inner[-1,]
inner_nest <- km_inner %>%
  nest(data= -day)


inner_travel = as.data.frame(matrix(nrow = 1, ncol = 2))
rows <- c("day", "km_travelled")
colnames(inner_travel) <- rows
for (i in 1:length(inner_nest$day)){
  mydata <- inner_nest$data[[i]]
  mydata <- arrange(mydata, Date)
  mydata <- mutate(mydata, 
                   Distance = distHaversine(cbind(Longitude, Latitude),
                                            cbind(lag(Longitude), lag(Latitude))))
  
  mydata$Distance <- mydata$Distance / 1000
  sum_km <- sum(mydata$Distance, na.rm = T)
  date <- substr(mydata$Date[1], 0 , 10)
  toadd <- c(date, sum_km)
  inner_travel <- rbind(inner_travel, toadd)
}

inner_travel <- inner_travel[-1,]
names <- c("Var1", "km_travelled")
colnames(inner_travel) <- names

inner_travel$Var1 <- as.character(inner_travel$Var1)
year_merge$Var1 <- as.character(year_merge$Var1)


year_merge2 <- merge(year_merge, inner_travel, by = "Var1")

################## binomial or poisson 
#model.nb = glm.nb(use ~ month + shark_stand, Boat,  data = year_merge) 
#summary(model.nb)

#1 - pchisq(summary(model.nb)$deviance, summary(model.nb)$df.residual)

#model.nb = glm.nb(X5day_use ~ timepoint5 + X5day_shark, data = year_merge) 
#summary(model.nb)

#1 - pchisq(summary(model.nb)$deviance, summary(model.nb)$df.residual)

#model.nb = glm.nb(X5day_use ~ X5day_shark, data = year_merge) 
#summary(model.nb)

#1 - pchisq(summary(model.nb)$deviance, summary(model.nb)$df.residual)



#cmod_lme4_L <- glmmTMB(use ~ month + shark_stand + Boat +(1|day) +0,data=year_merge,
#                       family=nbinom2)

year_merge2$km_travelled <- as.integer(year_merge2$km_travelled)

one <- glmer.nb(use ~ month + shark_stand + Boat + km_travelled +(1|day) +0 , data=year_merge2,
                       family=nbinom2)


two <- glmer.nb(use ~ month + shark_stand +(1|day) +0,data=year_merge,
                        family=nbinom2)

three <- glmer.nb(use ~ month +(1|day) + 0,data=year_merge,
                family=nbinom2)

four <- glmer.nb(use ~ month + Boat +(1|day) + 0,data=year_merge,
                family=nbinom2)

five <- glmer.nb(use ~ shark_stand + Boat +(1|day) +0,data=year_merge,
                family=nbinom2)



AIC(one)
AIC(two)
AIC(three)
AIC(four)
AIC(five)

AIC(cmod_lme4_L)


cmod_lme4_L <- glmmTMB(use ~ shark_stand + Boat + month + km_travelled +(1|day) + 0,data=year_merge2,
                       family=poisson)

library(car)
qqPlot(resid(cmod_lme4_L)) 


round((negbinom$family$linkinv(-.47466)-1)*100)

round((cmod_lme4_L$modelInfo$family$linkinv(-0.7711273)-1)*100)

round((cmod_lme4_L$modelInfo$family$linkinv(309.72)-1)*100)


#shark_model_2 <- glmmTMB((X5day_use ~  month + timepoint5 +(1|Day_Gp)),
#                         data = year_merge,
#                         family = nbinom2)
#cmod_lme4_L <- glmmTMB(X5day_shark ~  month + year +(1|Day_Gp),data=year_merge,
#                       family=nbinom2)


summary(cmod_lme4_L)
AIC(cmod_lme4_L)


####################
sharks_plot <- read.csv("stats/year_merge_5_days_count_detec.csv")





sharks_plot$year <- substr(sharks_plot$fake_year, 0, 4)
sharks_plot$day <- substr(sharks_plot$day, 9, 10)

sharks_plot$X5day_use <- sharks_plot$X5day_use * 10
sharks_plot$X5day_use <- as.integer(sharks_plot$X5day_use)
sharks_plot$X5day_shark <- as.integer(sharks_plot$X5day_shark)
sharks_plot$month <- substr(sharks_plot$fake_year, 6, 7)

require("glmmTMB")
library(lme4)

model.nb = glm.nb(X5day_use ~ year + month, data = sharks_plot) 
summary(model.nb)

shark_model <- glmmTMB((X5day_use ~ year + month + (1|Day_Gp)),
                       data = sharks_plot,
                       family = nbinom2)


################################################


model <- glmmTMB((use ~ month + year + shark_stand + (1|day)),
                 data = detections,
                 ziformula = ~ 1,
                 family = nbinom2)


ff <- fixef(cmod_lme4_L)$zi
round(plogis(c(sppGP=unname(ff[1]),ff[-1]+ff[1])),3)


detections <- transform(mum_5_days, GP=as.numeric(month=="03"))
model2 = update(cmod_lme4_L, ziformula=~GP)

fixef(model2)[["zi"]]


model3 = update(model, ziformula=~(1|day.x))
fixef(model3)[["zi"]]
VarCorr(model3)

model4 = glmmTMB(use~month + (1|day.x), zi=~year, detections, family=nbinom2)
fixef(model4)[["zi"]]

cc = confint(model4,method="uniroot",parm=9, parm.range=c(-20,20))
print(cc)


######################################

cmod_lme4_L <- glmer(X5day_use ~ year +(1|Day_Gp),data=mum_5_days,
                     family=nbinom2)


cmod_lme4_L <- glmer(X5day_shark ~ year + month +(1|Day_Gp),data=mum_5_days,
                     family=nbinom2)


print(summary(cmod_lme4_L),correlation=FALSE)

cmod_lme4_block <- update(cmod_lme4_L,.~ year + month + X5day_shark +(year + month + X5day_shark + (1 | Day_Gp)))

fixef(cmod_lme4_block)

VarCorr(cmod_lme4_block)


AICtab(cmod_lme4_L,cmod_lme4_block,nobs=nrow(culcita_dat))


