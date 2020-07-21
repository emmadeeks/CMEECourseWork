

rm(list=ls()) #Clear global environment 
library(dplyr)
library(lmerTest)
#library(R2admb)
#library(glmmADMB)
library(glmmTMB)


setwd("/Users/emmadeeks/Desktop/CMEECourseWork/project/results/acoustic_GPS") #go to the data directory 


overlap <- read.csv("NO_REPEAT_Ac_GPS_all_10_overlap_no_DG.csv")


overlap$all <- substr(overlap$Date, 0,10)
overlap$all <- as.Date(overlap$all, "%Y-%m-%d")


count <- table(overlap$all)
count <- as.data.frame(count)




#count$month <- as.numeric(as.character(count$month))

########## Filling in the missing dates so the data is continuous by the day 


ts <- seq.POSIXt(as.POSIXct("2014-01-05",'%y-%m-%d'), as.POSIXct("2019-04-26",'%y-%m-%d'), by="days")
#ts <- seq.POSIXt(as.POSIXlt("2014-01-05"), as.POSIXlt("2019-04-26"), by="days")
ts <- format.POSIXct(ts,'%y-%m-%d')

df <- data.frame(Var1=ts)
df$Var1 <- substr(df$Var1, 0, 10)

df$Var1 <- as.Date(df$Var1, "%y-%m-%d")
#df$Freq <- 0



######### Removing missing data periods 
DATE1<-as.Date("2016-06-28")  
DATE2<-as.Date("2017-06-03")

daterange <- interval(DATE1, DATE2)


TEST <- df[which(df$Var1 %within% daterange),]

df <- df %>%
  filter(!(Var1 %in% TEST))

date3 <- as.Date("2018-07-01")
date4 <- as.Date("2018-07-31")

daterange <- interval(date3, date4)


TEST <- df[which(df$Var1 %within% daterange),]

df <- df %>%
  filter(!(Var1 %in% TEST))

date5 <- as.Date("2018-01-01")
date6 <- as.Date("2018-01-31")

daterange <- interval(date5, date6)


TEST <- df[which(df$Var1 %within% daterange),]

df <- df %>%
  filter(!(Var1 %in% TEST))


## Joining dataframes to get a more complete dataset 


count$Var1 <- as.Date(count$Var1, "%Y-%m-%d")

data_with_missing_times <- full_join(count,df, by = "Var1")


data_with_missing_times$Freq[is.na(data_with_missing_times$Freq)] <- 0



data_with_missing_times$day <- substr(data_with_missing_times$Var1, 9,10)
data_with_missing_times$month <- substr(data_with_missing_times$Var1, 6,7)
data_with_missing_times$year <- substr(data_with_missing_times$Var1, 1,4)
data_with_missing_times$monthyear <- substr(data_with_missing_times$Var1, 1, 7)

####### Standardising data 

summary_tags <- read.csv('AG_NR_summary_sharks_no_dg_NOREPEATS.csv')

standard <- cbind(as.character(summary_tags$monthyear), summary_tags$count_tag, summary_tags$actual_hours)
names <- c("monthyear", "count_tag", "actual_hours")
colnames(standard) <- names
standard <- as.data.frame(standard)


data <- merge(data_with_missing_times, standard, by = 'monthyear', all.x = T)

data <- data[order(as.Date(data$Var1, format="%Y-%m-%d")),]

data$Freq <- as.numeric(as.character(data$Freq))
data$actual_hours <- as.numeric(as.character(data$actual_hours))
data$count_tag <- as.numeric(as.character(data$count_tag))


#data$use <- (data$Freq / (data$count_tag * data$actual_hours))


#write.csv(data, "../../data/stats/figure_1_time_overlap_standard.csv")


data$day <- as.factor(data$day)

day_mod <- data
day_mod$use <- (day_mod$Freq / (day_mod$count_tag * day_mod$actual_hours))
day_mod$use <- day_mod$use * 10000
day_mod$use <- as.integer(day_mod$use)


sharks_stand <- read.csv("../../data/stats/all_days_sharks_stand_stats.csv")

colnames(sharks_stand)[3] <- "Var1"

merge <- cbind(as.character(sharks_stand$Var1), sharks_stand$shark_stand)
merge <- as.data.frame(merge)
cols <- c("Var1", "shark_stand")
colnames(merge) <- cols

merge$shark_stand <- as.integer(merge$shark_stand)
merge$Var1 <- as.Date(merge$Var1, format="%Y-%m-%d")
day_mod$Var1 <- as.Date(day_mod$Var1, format="%Y-%m-%d")
#day_mod <- data[order(as.Date(day_mod$Var1, format="%Y-%m-%d")),]

build <- merge(day_mod, merge, by = "Var1", all.x = T)

model <- glmmTMB((use ~ year + month + shark_stand +(1|day)),
                 data = build,
                 family = nbinom2)
summary(model)



model2 <- glmmTMB((shark_stand ~ year + month +(1|day)),
                  data = build,
                  family = nbinom2)

summary(model2)


write.csv(build, "../../data/stats/all_days_counts_sharks_stand.csv")

build <- build[,-3]
build <- build[,-6]
build <- build[,-6]


write.csv(build, "../../data/stats/mum_all_days_counts_sharks_stand.csv")

########
data_nest <- build %>%
  nest(data= -monthyear) #

detections = as.data.frame(matrix(nrow = 1, ncol = 10))
names <- colnames(data_month)
names <- c(names, "day_no")
colnames(detections) <- names 

for (i in 1:nrow(data_nest)) {
  data_month <- data_nest$data[[i]]
  data_month <- as.data.frame(data_month)
  month <- data_nest$monthyear[[i]]
  #adding <- data_month$Freq
  detec <- nrow(data_month)
  #null <- detections[1,]
  #null$Freq <- 0
  if (detec %% 5 == 0) {
  } else {
    data_month <- data_month[-1,]
  }
  detec <- nrow(data_month)
  if (detec %% 5 == 0) {
  } else {
    data_month <- data_month[-1,]
  }
  detec <- nrow(data_month)
  if (detec %% 5 == 0) {
  } else {
    data_month <- data_month[-1,]
  }
  detec <- nrow(data_month)
  if (detec %% 5 == 0) {
  } else {
    data_month <- data_month[-1,]
  }
  detec <- nrow(data_month)
  if (detec %% 5 == 0) {
  } else {
    data_month <- data_month[-1,]
  }
  #divide <- nrow(data_month)/6
  summed_overlap <- colSums(matrix(data_month$use, nrow=5))
  meaned_ac <- colSums(matrix(data_month$shark_stand, nrow=5))
  df.new = data_month[seq(1, nrow(data_month), 5), ]
  df.new$use <- summed_overlap
  df.new$shark_stand <- meaned_ac
  df.new$day_no <- 1:length(summed_overlap)
  df.new$Var1 <- as.character(df.new$Var1)
  
  detections <- rbind(detections, df.new)
  #toadd <- c(month, detec)
  #detections <- rbind(detections, toadd)
}

detections <- detections[-1,]
detections$shark_stand <- as.integer(detections$shark_stand)

model <- glmmTMB((use ~ month + year + shark_stand + (1|day_no)),
                 data = detections,
                 family = nbinom2)
summary(model)





















######## old part but probably still need to use 




####### making it every 5 days 

data_nest <- data %>%
  nest(data= -monthyear) #

detections = as.data.frame(matrix(nrow = 1, ncol = 8))
names <- colnames(data_month)
names <- c(names, "day_no")
colnames(detections) <- names 

for (i in 1:nrow(data_nest)) {
  data_month <- data_nest$data[[i]]
  data_month <- as.data.frame(data_month)
  month <- data_nest$monthyear[[i]]
  #adding <- data_month$Freq
  detec <- nrow(data_month)
  #null <- detections[1,]
  #null$Freq <- 0
  if (detec %% 5 == 0) {
  } else {
    data_month <- data_month[-1,]
  }
  detec <- nrow(data_month)
  if (detec %% 5 == 0) {
  } else {
    data_month <- data_month[-1,]
  }
  detec <- nrow(data_month)
  if (detec %% 5 == 0) {
  } else {
    data_month <- data_month[-1,]
  }
  detec <- nrow(data_month)
  if (detec %% 5 == 0) {
  } else {
    data_month <- data_month[-1,]
  }
  #divide <- nrow(data_month)/6
  summed <- colSums(matrix(data_month$Freq, nrow=5))
  df.new = data_month[seq(1, nrow(data_month), 5), ]
  df.new$Freq <- summed 
  df.new$day_no <- 1:length(summed)
  df.new$Var1 <- as.character(df.new$Var1)
  
  detections <- rbind(detections, df.new)
  #toadd <- c(month, detec)
  #detections <- rbind(detections, toadd)
}


detections$use <- (detections$Freq / (detections$count_tag * detections$actual_hours))
detections <- detections[-1,]
detections$use <- detections$use * 10000
detections$use <- as.integer(detections$use)

write.csv(detections, "../../data/stats/figure_1_time_overlap_standard.csv")
#detections <- read.csv("../../data/stats/figure_1_time_overlap_standard.csv")


model <- glmmTMB((use ~ month + year + (1|day_no)),
                 data = detections,
                 ziformula = ~ 1,
                 family = nbinom2)

model <- glmmTMB((use ~ month + year + (1|day_no)),
                 data = detections,
                 family = nbinom2)
summary(model)




######## Encorporating sharks into the model 


#detections <- read.csv("../../data/stats/figure_1_time_overlap_standard.csv")
#detections <- detections[-1, ]
detections$monthyear <- substr(detections$Var1, 0, 7)


sharks_stand <- read.csv("../../data/stats/sharks_stand_stats.csv")
sharks_stand <- sharks_stand[,-3]
sharks_stand <- sharks_stand[,-3]
sharks_stand <- sharks_stand[,-3]

detections <- merge(detections, sharks_stand, by = "monthyear")
detections$shark_stand <- as.integer(detections$shark_stand)


model <- glmmTMB((use ~ month + year + shark_stand + (1|day)),
                 data = detections,
                 family = nbinom2)

summary(model)

###### glm for sharks 


shark_model <- glmmTMB((sharks_stand ~ year + (1|day)),
                 data = detections,
                 family = nbinom2)



########## EXAMPLE 1
model <- glmmTMB((use ~ month + year + shark_stand + (1|day)),
                 data = detections,
                 ziformula = ~ 1,
                 family = nbinom2)




ff <- fixef(model)$zi
round(plogis(c(sppGP=unname(ff[1]),ff[-1]+ff[1])),3)


detections <- transform(detections, GP=as.numeric(month=="03"))
model2 = update(model, ziformula=~GP)

fixef(model2)[["zi"]]


model3 = update(model, ziformula=~(1|day.x))
fixef(model3)[["zi"]]
VarCorr(model3)

model4 = glmmTMB(use~month + (1|day.x), zi=~year, detections, family=nbinom2)
fixef(model4)[["zi"]]

cc = confint(model4,method="uniroot",parm=9, parm.range=c(-20,20))
print(cc)



############# EXAMPLE 2 
load(system.file("vignette_data","troubleshooting.rda",package="glmmTMB"))
summary(mod1)

summary(model2)

diagnose_vcov <- function(model, tol=1e-5, digits=2, analyze_hessian=FALSE) {
  vv <- vcov(model, full=TRUE)
  nn <- rownames(vv)
  if (!all(is.finite(vv))) {
    if (missing(analyze_hessian)) warning("analyzing Hessian, not vcov")
    if (!analyze_hessian) stop("can't analyze vcov")
    analyze_hessian <- TRUE
  }
  if (analyze_hessian) {
    par.fixed <- model$obj$env$last.par.best
    r <- model$obj$env$random
    if (!is.null(r)) par.fixed <- par.fixed[-r]
    vv <- optimHess(par.fixed, fn=model$obj$fn, gr=model$obj$gr)
    ## note vv is now HESSIAN, not vcov
  }
  ee <- eigen(vv)
  if (all(ee$values>tol)) {message("var-cov matrix OK"); return(invisible(NULL))}
  ## find negative or small-positive eigenvalues (flat/wrong curvature)
  bad_evals <- which(ee$values<tol)
  ## order worst to best
  bad_evals <- bad_evals[order(-ee$values[bad_evals])]
  ret <- lapply(bad_evals,
                function(i) {
                  ## extract loadings
                  v <- setNames(ee$vectors[,i], nn)
                  ## order in decreasing magnitude & round
                  list(val=ee$values[i],vec=round(v[order(-abs(v))],digits))
                })
  return(ret)
}


(d1 <- diagnose_vcov(model3))
mod2 <- update(model3, ziformula=~0)
mod3 <- update(mod2, family=poisson)
diagnose_vcov(mod3)
mod3$sdr$pdHess   





##########
emm_az <- emmeans(model4, ~ month ,lmer.df = "satterthwaite")


# this is our post-hoc
library("emmeans")
emm_az <- emmeans(model_lfinal, ~ loc_locs,lmer.df = "satterthwaite")
rbind(pairs(emm_az), adjust="bonferroni") # can change "Tukey" to "bonferroni"

library(car)
qqPlot(resid(model)) 




###### SALAMANDERS EXAMPLE 

data(Salamanders)

zinbm0 = glmmTMB(count~spp + (1|site), zi=~spp, Salamanders, family=nbinom2)
fixef(zinbm0)

ff <- fixef(zinbm0)$zi
round(plogis(c(sppGP=unname(ff[1]),ff[-1]+ff[1])),3)

Salamanders <- transform(Salamanders, GP=as.numeric(spp=="GP"))
zinbm0_A = update(zinbm0, ziformula=~GP)
fixef(zinbm0_A)[["zi"]]







model <-lmer(use ~ month + year + (1|day), data=detections)
model2 <-lmer(Freq ~ year + (1|day), data=data_with_missing_times)
step(model)

data_with_missing_times$day <- as.factor(data_with_missing_times$day)


glmmNB <- glmmadmb(use~month+(1|day), data = data, 
                   zeroInflation=TRUE, family="nbinom")


glmmNB <- glmmadmb(use~monthyear+(1|day), data = data, 
                   zeroInflation=TRUE, family="nbinom")










model_w <- lmer(word_corr~word_locs*lr_Ndirection*Sex+(1|Subject),data=df2)


model1a <-lmer(Mean_wordScore ~ lr_Ndirection + (1|Subject), data=df1)
step(model1a)
