rm(list=ls()) #Clear global environment 

library(tidyverse)
library(lubridate)
library(plyr)

library(ggplot2)


library(cowplot)
library(dplyr)


library(dplyr)
library(geosphere)

setwd("/Users/emmadeeks/Desktop/CMEECourseWork/project/data") #go to the data directory 
BPV <- read.csv("New_data_no_dg_hour/BPV_formatted_CORRECT_hour_INCLUDE_dg.csv")


#BPV$Longitude <- as.numeric(levels(BPV$Longitude))[BPV$Longitude]
#BPV$Latitude <- as.numeric(levels(BPV$Latitude))[BPV$Latitude]

BPV$Latitude <- as.numeric(as.character(BPV$Latitude))
BPV$Longitude <- as.numeric(as.character(BPV$Longitude))

BPV$day <- substr(BPV$Date, 0, 10)
BPV$NewDate <- substr(BPV$Date, 0, 7)


BPV_loop <- BPV %>%
  nest(data= -day)

mydata <- BPV_loop$data[[1]]


#mydata[(order(as.Date(mydata$Date, format = "%y-%m-%d %H:%M:%S"))),]
mydata <- arrange(mydata, Date)

mydata <- mutate(mydata, 
       Distance = distHaversine(cbind(Longitude, Latitude),
                                cbind(lag(Longitude), lag(Latitude))))

mydata$Distance <- mydata$Distance / 1000

priority = as.data.frame(matrix(nrow = 1, ncol = 2))
rows <- c("day", "km_travelled")
colnames(priority) <- rows
for (i in 1:length(BPV_loop$day)){
  mydata <- BPV_loop$data[[i]]
  mydata <- arrange(mydata, Date)
  mydata <- mutate(mydata, 
                   Distance = distHaversine(cbind(Longitude, Latitude),
                                            cbind(lag(Longitude), lag(Latitude))))
  
  mydata$Distance <- mydata$Distance / 1000
  sum_km <- sum(mydata$Distance, na.rm = T)
  date <- substr(mydata$Date[1], 0 , 10)
  toadd <- c(date, sum_km)
  priority <- rbind(priority, toadd)
}

priority <- priority[-1,]
priority$km_travelled <- as.numeric(as.character(priority$km_travelled))

priority$NewDate <- substr(priority$day, 0, 7)
BPV_AV <- priority %>%
  nest(data= -NewDate)

mean_table = as.data.frame(matrix(nrow = 1, ncol = 2))
rows <- c("monthyear", "average")
colnames(mean_table) <- rows
for (i in 2:length(BPV_AV$NewDate)){
  mydata <- BPV_AV$data[[i]]
  mydata <- as.data.frame(mydata)
  a <- as.numeric(mydata$km_travelled)
  date <- BPV_AV$NewDate[[i]]
  mean <- mean(a)
  toadd <- c(date, mean)
  mean_table <- rbind(mean_table, toadd)
}

mean_table <- mean_table[-1,]
mean_table <- na.omit(mean_table)

mean_table$average <- as.numeric(as.character(mean_table$average))


ggplot(mean_table, aes(x=monthyear, y=average, group = 1)) + 
  geom_point() +
  geom_line() +
  stat_smooth(method="lm", se=TRUE, fill=NA, formula=y ~ poly(x, 2, raw=TRUE),colour="red") +
  xlab("Month") +# for the x axis label
  ylab("Proportion of successful overlaps compared to 744 and sharks at liberty") +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1), panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
mean_table$X <- 1:nrow(mean_table)

lm(average~X, data = summary_tags)
summary(lm(mean_table$average ~ poly(mean_table$X, 1, raw = TRUE)))


p<- ggplot(priority, aes(x=NewDate, y=km_travelled))
p=p+geom_point()
p=p+geom_boxplot()
p

every_nth = function(n) {
  return(function(x) {x[c(TRUE, rep(FALSE, n - 1))]})
}


write.csv(priority, "stats/km_travelled_not_grouped.csv")



pdf("../results/Thesis_figures/figure_3_panel.pdf")
ggplot(priority, aes(x=NewDate, y=km_travelled)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4, colour = "black", fill = "lightblue") + 
  scale_x_discrete(breaks = every_nth(n = 3)) +
  ylab("Kilometers travelled by patrol vessel") + 
  xlab("Month") +
  coord_cartesian(ylim = c(0, 1000)) +  theme(axis.text.x = element_text(angle = 90, hjust = 1), panel.border = element_blank(),
                                              axis.line = element_line(colour = "black"), panel.background = element_rect(fill='grey96', colour='black')) 

dev.off()


a <- ggplot(priority, aes(x=NewDate, y=km_travelled)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4, colour = "black", fill = "lightblue") + 
  scale_x_discrete(breaks = every_nth(n = 3)) +
  ylab("Kilometers travelled") + 
  xlab("Month") +
  coord_cartesian(ylim = c(0, 1000)) +  theme(axis.text.x = element_text(angle = 90, hjust = 1), panel.border = element_blank(),
                                              axis.line = element_line(colour = "black"), panel.background = element_rect(fill='grey96', colour='black')) 






summed <- aggregate(km_travelled~NewDate, priority, FUN=sum) 



b <- ggplot(summed, aes(x=NewDate, y=km_travelled, group = 1)) + 
  geom_line(size = 0.9) +
  #geom_point() + 
  stat_smooth(method="lm", se=TRUE, fill=NA, formula=y ~ poly(x, 2, raw=TRUE),colour="blue2", size = 0.8) +
  xlab("Month") +# for the x axis label
  scale_x_discrete(breaks = every_nth(n = 3)) +
  ylab("Total number of kilometers travelled") +
  theme_bw() +  theme(axis.text.x = element_text(angle = 90, hjust = 1), panel.border = element_blank(),
                        axis.line = element_line(colour = "black"), panel.background = element_rect(fill='grey96', colour='black')) 




n <- plot_grid(a, b, ncol = 1)

pdf("../results/Thesis_figures/figure_3_panel_km.pdf")
n
dev.off()

summed$X <- 1:nrow(summed)

lm(km_travelled~X, data = summed)
summary(lm(summed$km_travelled ~ poly(summed$X, 2, raw = TRUE)))



############################ Making it every 5 days for stats 

data_nest <- priority %>%
  nest(data= -NewDate) #

data_nest <- data_nest[-32,]

average_km = as.data.frame(matrix(nrow = 1, ncol = 2))
names <- colnames(data_month)
colnames(average_km) <- names 
for (i in 1:nrow(data_nest)) {
  data_month <- data_nest$data[[i]]
  data_month <- as.data.frame(data_month)
  month <- data_nest$NewDate[[i]]
  #adding <- data_month$Freq
  detec <- nrow(data_month)
  null <- average_km[1,]
  null$km_travelled <- 0
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
  summed <- colMeans(matrix(data_month$km_travelled, nrow=5))
  df.new = data_month[seq(1, nrow(data_month), 5), ]
  df.new$km_travelled <- summed 
  df.new$day <- as.character(df.new$day)
  
  average_km <- rbind(average_km, df.new)
  #toadd <- c(month, detec)
  #detections <- rbind(detections, toadd)
}

average_km <- average_km[-1,]

average_km$year <- substr(average_km$day, 0, 4)

average_km$year[average_km$year == 2013] <- "Marlin"
average_km$year[average_km$year == 2014] <- "Marlin"
average_km$year[average_km$year == 2015] <- "Marlin"
average_km$year[average_km$year == 2016] <- "Marlin"

average_km$year[average_km$year == 2017] <- "Grampian"
average_km$year[average_km$year == 2018] <- "Grampian"
average_km$year[average_km$year == 2019] <- "Grampian"

average_km <- average_km %>% drop_na()



ggplot(average_km, aes(x=year, y=km_travelled)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4, colour = "black") + 
  #geom_point(size = 0.1) +
  scale_x_discrete(breaks = every_nth(n = 3)) +
  ylab("Kilometers travelled") + 
  xlab("Month") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                        panel.background = element_blank(), axis.line = element_line(colour = "black"))



plot(density(average_km$km_travelled))
shapiro.test(average_km$km_travelled)
qqnorm(average_km$km_travelled);qqline(average_km$km_travelled, col = 2)

hist(average_km$km_travelled, 
     main="Credit Score", 
     xlab="Credit Score", 
     border="light blue", 
     col="blue", 
     las=1, 
     breaks=5)



#############   BOXPLOTS FOR TRAVELLING 

km_inner <- read.csv("../data/stats/km_inner.csv")
km_outer <- read.csv("../data/stats/km_outer.csv")

km_outer$Date <- as.character(km_outer$Date)
km_outer <- km_outer[-1,]
outer_nest <- km_outer %>%
  nest(data= -day)


outer_travel = as.data.frame(matrix(nrow = 1, ncol = 2))
rows <- c("day", "km_travelled")
colnames(outer_travel) <- rows
for (i in 1:length(outer_nest$day)){
  mydata <- outer_nest$data[[i]]
  mydata <- arrange(mydata, Date)
  mydata <- mutate(mydata, 
                   Distance = distHaversine(cbind(Longitude, Latitude),
                                            cbind(lag(Longitude), lag(Latitude))))
  
  mydata$Distance <- mydata$Distance / 1000
  sum_km <- sum(mydata$Distance, na.rm = T)
  date <- substr(mydata$Date[1], 0 , 10)
  toadd <- c(date, sum_km)
  outer_travel <- rbind(outer_travel, toadd)
}



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



###################### sorting out boxplot for inner part 
inner_travel$NewDate <- substr(inner_travel$day, 0, 7)
inner_travel <- inner_travel[-1,]
inner_travel$km_travelled <- as.factor(as.character(inner_travel$km_travelled))

inner_date <- inner_travel %>%
  nest(data= -NewDate) #

average_km_inner = as.data.frame(matrix(nrow = 1, ncol = 2))
names <- colnames(data_month)
colnames(average_km_inner) <- names 
for (i in 1:nrow(inner_date)) {
  data_month <- inner_date$data[[i]]
  data_month <- as.data.frame(data_month)
  month <- inner_date$NewDate[[i]]
  #adding <- data_month$Freq
  detec <- nrow(data_month)
  null <- average_km_inner[1,]
  null$km_travelled <- 0
  if (detec %% 5 == 0) {
  } else {
    data_month <- rbind(data_month, null)
  }
  detec <- nrow(data_month)
  if (detec %% 5 == 0) {
  } else {
    data_month <- rbind(data_month, null)
  }
  detec <- nrow(data_month)
  if (detec %% 5 == 0) {
  } else {
    data_month <- rbind(data_month, null)
  }
  detec <- nrow(data_month)
  if (detec %% 5 == 0) {
  } else {
    data_month <- rbind(data_month, null)
  }
  #divide <- nrow(data_month)/6
  data_month$km_travelled = as.numeric(as.character(data_month$km_travelled))
  #data_month <- as.data.frame(data_month)
  summed <- colMeans(matrix(data_month$km_travelled, nrow=5))
  df.new = data_month[seq(1, nrow(data_month), 5), ]
  df.new$km_travelled <- summed 
  df.new$day <- as.character(df.new$day)
  
  average_km_inner <- rbind(average_km_inner, df.new)
  #toadd <- c(month, detec)
  #detections <- rbind(detections, toadd)
}

average_km_inner <- average_km_inner[-1,]

average_km_inner$year <- substr(average_km_inner$day, 0, 4)

average_km_inner$year[average_km_inner$year == 2013] <- "Marlin"
average_km_inner$year[average_km_inner$year == 2014] <- "Marlin"
average_km_inner$year[average_km_inner$year == 2015] <- "Marlin"
average_km_inner$year[average_km_inner$year == 2016] <- "Marlin"

average_km_inner$year[average_km_inner$year == 2017] <- "Grampian"
average_km_inner$year[average_km_inner$year == 2018] <- "Grampian"
average_km_inner$year[average_km_inner$year == 2019] <- "Grampian"

average_km_inner <- average_km_inner %>% drop_na()



####################################



ggplot(average_km_inner, aes(x=year, y=km_travelled)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4, colour = "black") + 
  #geom_point(size = 0.1) +
  scale_x_discrete(breaks = every_nth(n = 3)) +
  ylab("Kilometers travelled") + 
  xlab("Month") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                        panel.background = element_blank(), axis.line = element_line(colour = "black"))



plot(density(average_km$km_travelled))
shapiro.test(average_km$km_travelled)
qqnorm(average_km$km_travelled);qqline(average_km$km_travelled, col = 2)

hist(average_km$km_travelled, 
     main="Credit Score", 
     xlab="Credit Score", 
     border="light blue", 
     col="blue", 
     las=1, 
     breaks=5)



################################ sorting out boxplot for outer areas 

outer_travel$NewDate <- substr(outer_travel$day, 0, 7)
outer_travel <- outer_travel[-1,]
outer_travel$km_travelled <- as.factor(as.character(outer_travel$km_travelled))

outer_date <- outer_travel %>%
  nest(data= -NewDate) #

average_km_outer = as.data.frame(matrix(nrow = 1, ncol = 2))
names <- colnames(data_month)
colnames(average_km_outer) <- names 
for (i in 1:nrow(outer_date)) {
  data_month <- outer_date$data[[i]]
  data_month <- as.data.frame(data_month)
  month <- outer_date$NewDate[[i]]
  #adding <- data_month$Freq
  detec <- nrow(data_month)
  null <- average_km_outer[1,]
  null$km_travelled <- 0
  if (detec %% 5 == 0) {
  } else {
    data_month <- rbind(data_month, null)
  }
  detec <- nrow(data_month)
  if (detec %% 5 == 0) {
  } else {
    data_month <- rbind(data_month, null)
  }
  detec <- nrow(data_month)
  if (detec %% 5 == 0) {
  } else {
    data_month <- rbind(data_month, null)
  }
  detec <- nrow(data_month)
  if (detec %% 5 == 0) {
  } else {
    data_month <- rbind(data_month, null)
  }
  #divide <- nrow(data_month)/6
  data_month$km_travelled = as.numeric(as.character(data_month$km_travelled))
  #data_month <- as.data.frame(data_month)
  summed <- colMeans(matrix(data_month$km_travelled, nrow=5))
  df.new = data_month[seq(1, nrow(data_month), 5), ]
  df.new$km_travelled <- summed 
  df.new$day <- as.character(df.new$day)
  
  average_km_outer <- rbind(average_km_outer, df.new)
  #toadd <- c(month, detec)
  #detections <- rbind(detections, toadd)
}

average_km_outer <- average_km_outer[-1,]

average_km_outer$year <- substr(average_km_outer$day, 0, 4)

average_km_outer$year[average_km_outer$year == 2013] <- "Marlin"
average_km_outer$year[average_km_outer$year == 2014] <- "Marlin"
average_km_outer$year[average_km_outer$year == 2015] <- "Marlin"
average_km_outer$year[average_km_outer$year == 2016] <- "Marlin"

average_km_outer$year[average_km_outer$year == 2017] <- "Grampian"
average_km_outer$year[average_km_outer$year == 2018] <- "Grampian"
average_km_outer$year[average_km_outer$year == 2019] <- "Grampian"




average_km_outer <- average_km_outer %>% drop_na()

average_km_inner$group <- "Inside MPA"
average_km_outer$group <- "Outside MPA"

final_km <- rbind(average_km_inner, average_km_outer)
final_km$year <- factor(final_km$year , levels=c("Marlin", "Grampian"))
#final_km$group <- factor(final_km$year , levels=c("Outside MPA", "Inside MPA"))
write.csv(final_km, "../data/stats/grampian_marlin_averaged_boxplots.csv")



pdf("../results/Thesis_figures/km_travelled_in_out_MPA.pdf")

ggplot(final_km) + 
  geom_boxplot( aes(x=factor(year), y=km_travelled, fill=factor(group), color = group), outlier.colour="red", outlier.shape=8,
                outlier.size=4, alpha = 0.7, lwd=0.9) + 
  #geom_point(size = 0.1) +
  xlab("Boat names") + 
  ylab("Daily kilometers travelled") + 
  scale_fill_manual(values=c('red', '#208EA3'), name = "") +
  scale_color_manual(name = "", values = c('grey50', 'grey50')) +
  #scale_fill_discrete(name = "New Legend Title") + 
  theme_bw() +
  theme( 
        text = element_text(size=20, color = "black"), axis.text.x = element_text(size = 18), 
        axis.text.y = element_text(size = 18), legend.position= c(0.8,0.9),
        legend.text = element_text(size = 18),
        axis.line = element_line(colour = "black"), 
        panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.minor = element_blank(), panel.grid.major = element_blank())

dev.off()


ggplot(final_km) + 
  geom_boxplot( aes(x=factor(year), y=km_travelled, fill=factor(group)), outlier.colour="red", outlier.shape=8,
                outlier.size=4, alpha = 0.9, lwd=0.9) + 
  #geom_point(size = 0.1) +
  xlab("Boat names") + 
  ylab("Kilometers travelled") + 
  scale_fill_manual(values=c("#69b3a2", "grey"), name = "") +
  #scale_fill_discrete(name = "New Legend Title") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(size=14, color = "black"), axis.text.x = element_text(size = 14), 
        axis.text.y = element_text(size = 14), legend.position= c(0.8,0.9),
        legend.text = element_text(size = 14),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


will <- final_km[final_km$group == 'Inside MPA',]

wilcox.test(km_travelled ~ year, data = will) 

will_out <- final_km[final_km$group == 'Outside MPA',]

wilcox.test(km_travelled ~ year, data = will_out) 







########## rough 
boxplot(km_travelled~NewDate,
        data=priority,
        ylim = c(0, 1000), 
        xlab="Month",
        ylab="Kilometers travelled by patrol vessel",
        col="lightblue",
        border="black") 


boxplot(km_travelled~NewDate,
        data=priority,
        ylim = c(0, 1000), 
        xlab="Month",
        ylab="Kilometers travelled by patrol vessel",
        col="lightblue",
        border="black") 


