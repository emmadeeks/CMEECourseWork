rm(list=ls()) #Clear global environment 


library(dplyr)
library(ggplot2)

library(tidyverse)
library(lubridate)
library(plyr)




setwd("/Users/emmadeeks/Desktop/CMEECourseWork/project/data") #go to the data directory 
GPS_acoustic <- read.csv("New_data_no_dg_hour/acoustic_GPS_no_elas.csv")
#GPS_acoustic <- GPS_acoustic[!duplicated(GPS_acoustic[c('day', 'Code')]),] 



GPS_acoustic$NewDate <- substr(GPS_acoustic$Date, 0 , 10)
GPS_acoustic <- GPS_acoustic[!duplicated(GPS_acoustic[c('Date', 'Code')]),] 
all_combined <- GPS_acoustic %>%
  nest(data= -NewDate) #

detections = as.data.frame(matrix(nrow = 1, ncol = 2))
for (i in 1:nrow(all_combined)) {
  data <- all_combined$data[[i]]
  month <- all_combined$NewDate[[i]]
  detec <- nrow(data)
  toadd <- c(month, detec)
  detections <- rbind(detections, toadd)
}

#detections <- detections[-52,]
detections <- detections[-1,]


detections$V1 <- as.Date(detections$V1, format="%Y-%m-%d")
detections$V1 <- as.factor(as.character(detections$V1))


#plot(detections$V1, detections$V2)
names <- c("monthyear", "count")
colnames(detections) <- names

tags_at_liberty <- read.csv("../results/acoustic_GPS/AG_standardising_tags.csv")  


detections$count_tag <- 
  sapply(detections$monthyear, function(x)
    sum(as.Date(tags_at_liberty$min, "%Y-%m-%d") <= as.Date(x, "%Y-%m-%d") &
          as.Date(tags_at_liberty$max, "%Y-%m-%d") >= as.Date(x, "%Y-%m-%d")))


detections <- detections[order(as.Date(detections$monthyear, format="%Y-%m-%d")),]

detections$year <- substr(detections$monthyear, 0, 4)
detections <-detections[!(detections$year == "2013"),]

#summary_original <- read.csv('../results/acoustic_GPS/AG_NR_summary_sharks_no_dg_NOREPEATS.csv')


#detections$monthyear <- substr(detections$monthyear, 0 , 7)
#new_frame <- merge(summary_original, detections, by = "monthyear")
#new_frame$count_tag <- as.numeric(as.character(new_frame$count_tag.y))
#new_frame$count <- as.numeric(as.character(new_frame$count.y))


#summary_tags <- read.csv('../results/acoustic_GPS/AG_NR_summary_sharks_no_dg_NOREPEATS.csv')



#new_frame$plot1 <- (new_frame$count / new_frame$count_tag)
#new_frame$plot2 <- (new_frame$count / new_frame$number_sharks)
#new_frame <- new_frame[-30,]




detections$count_tag <- as.numeric(as.character(detections$count_tag))
detections$count <- as.numeric(as.character(detections$count))
detections$plot1 <- detections$count / detections$count_tag
detections$plot1 <- detections$plot1 * 100
#detections$monthyear <- substr(detections$monthyear, 0 , 7)
#detections$year <- substr(detections$monthyear, 0, 4)
detections$day <- substr(detections$monthyear, 9,10)
detections$plot1 <- as.integer(detections$plot1)

#d <- unstack(detections, form=plot1~year)
#d <- as.data.frame(d)

require("glmmTMB")

#detections <- detections[order(as.Date(detections$day, format="%Y-%m-%d")),]

shark_model <- glmmTMB((plot1 ~ year + (1|day)),
                       data = detections,
                       family = nbinom2)


plot <- aggregate(detections[, 5], list(detections$year), mean)

pdf("../results/Thesis_figures/acoustic_detections_years.pdf")
ggplot() + geom_bar(plot, mapping = aes(x=Group.1, y=x), stat="identity", colour = "blue", fill = "gray") +
  ylab("Standardised detection frequency") +
  xlab("Year") +
  #geom_line(summary_tags, mapping = aes(x=monthyear, y=standard2, group = 1)) +
  #geom_bar(alpha = 0.5) +
  #theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.text.y   = element_text(size=12),
        axis.text.x   = element_text(size=12, angle = 90, hjust = 1),
        axis.title.y  = element_text(size=14),
        axis.title.x  = element_text(size=14),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=2)
  )
dev.off()









########################





pdf("../results/acoustic/acoustic_detections_tags_at_lib.pdf")
ggplot() + geom_bar(detections, mapping = aes(x=monthyear, y=plot1), stat="identity", colour = "black", alpha = 0.3) +
  ylab("Number of detections of acoustically tagged sharks standardised by tags at liberty") +
  #geom_line(summary_tags, mapping = aes(x=monthyear, y=standard2, group = 1)) +
  #geom_bar(alpha = 0.5) +
  #theme(axis.text.x = element_text(angle = 90)) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), axis.text.x = element_text(angle = 90), 
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) 

dev.off()




#pdf("../results/Thesis_figures/figure_2_panel.pdf")
ggplot() + geom_bar(new_frame, mapping = aes(x=monthyear, y=plot2), stat="identity", colour = "black", alpha = 0.3) +
  ylab("Detection frequency (standardised by tags at liberty)") +
  #geom_line(summary_tags, mapping = aes(x=monthyear, y=standard2, group = 1)) +
  #geom_bar(alpha = 0.5) +
  #theme(axis.text.x = element_text(angle = 90)) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), axis.text.x = element_text(angle = 90), 
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) 
#dev.off()


##################### New stats figures #######

GPS_acoustic$day <- substr(GPS_acoustic$Date,0,10)

all_combined_days <- GPS_acoustic %>%
  nest(data= -day) #

detections_days = as.data.frame(matrix(nrow = 1, ncol = 3))
names <- c("day", "count", "shark_number")
colnames(detections_days) <- names 


for (i in 1:nrow(all_combined_days)) {
  data <- all_combined_days$data[[i]]
  month <- all_combined_days$day[[i]]
  detec <- nrow(data)
  sharks <- unique(data$Code)
  sharks <- length(sharks)
  toadd <- c(month, detec, sharks)
  detections_days <- rbind(detections_days, toadd)
}


detections_days <-  detections_days[-1,]

detections_days <- detections_days[order(as.Date(detections_days$day, format="%Y-%m-%d")),]

detections_days$NewDate <- substr (detections_days$day, 0, 7)


detections_days$count <- as.factor(as.character(detections_days$count))



summary_original <- read.csv('../results/acoustic_GPS/AG_NR_summary_sharks_no_dg_NOREPEATS.csv')


detections_days$monthyear <- substr(detections_days$day, 0 , 7)
counttag <- cbind(as.character(summary_original$monthyear), summary_original$count_tag)
counttag <- as.data.frame(counttag)
cols <- c("monthyear", "count_tag")
colnames(counttag) <- cols
new_frame_all <- merge(detections_days, counttag, by = "monthyear")
new_frame_all$count <-as.numeric(as.character(new_frame_all$count))
new_frame_all$count_tag <-as.numeric(as.character(new_frame_all$count_tag))
new_frame_all$shark_stand <- (new_frame_all$count / new_frame_all$count_tag)
new_frame_all$shark_stand <- new_frame_all$shark_stand * 100

write.csv(new_frame_all, "../data/stats/all_days_sharks_stand_stats.csv")

sharks_plot <- read.csv("../data/stats/all_days_sharks_stand_stats.csv")

sharks_plot$year <- substr(sharks_plot$monthyear, 0, 4)
sharks_plot$day <- substr(sharks_plot$day, 9, 10)

sharks_plot$shark_stand <- as.integer(sharks_plot$shark_stand)
sharks_plot$month <- substr(sharks_plot$monthyear, 6, 7)




detections_days_2 <-  detections_days %>%
  nest(data= -NewDate) #

detections_days_2 <- detections_days_2[-76,]

############################## LEFT AS YOU NEED TO FIGRE OUT WHAT TO DO ABOUT COUNTS 
every_5 = as.data.frame(matrix(nrow = 1, ncol = 3))
names <- c("day", "count", "rep_no")
colnames(every_5 ) <- names 


for (i in 1:nrow(detections_days_2)) {
  data_month <- detections_days_2$data[[i]]
  data_month <- as.data.frame(data_month)
  month <- data_month$day[[1]]
  #adding <- data_month$Freq
  detec <- nrow(data_month)
  #null <- data_month[1,]
  #null$count <- factor(0)
  #null$shark_number <- 0
  #null$day <- as.character(null$day)
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
  data_month$count = as.numeric(as.character(data_month$count))
  data_month$shark_number = as.numeric(as.character(data_month$shark_number))
  
  #data_month <- as.data.frame(data_month)
  summed <- colMeans(matrix(data_month$count, nrow=5))
  add <- 1:length(summed)
  df.new = data_month[seq(1, nrow(data_month), 5), ]
  df.new$count <- summed 
  df.new$day <- as.character(df.new$day)
  df.new <- df.new[,-3]
  df.new <- df.new[,-3]
  df.new$rep_no <- add
  
  every_5 <- rbind(every_5, df.new)
  #toadd <- c(month, detec)
  #detections <- rbind(detections, toadd)
}

every_5 <- every_5[-1,]


every_5 <- every_5[order(as.Date(every_5$day, format="%Y-%m-%d")),]




summary_original <- read.csv('../results/acoustic_GPS/AG_NR_summary_sharks_no_dg_NOREPEATS.csv')


every_5$monthyear <- substr(every_5$day, 0 , 7)
counttag <- cbind(as.character(summary_original$monthyear), summary_original$count_tag)
counttag <- as.data.frame(counttag)
cols <- c("monthyear", "count_tag")
colnames(counttag) <- cols
new_frame <- merge(every_5, counttag, by = "monthyear")

tags_at_liberty <- read.csv("../results/acoustic_GPS/AG_standardising_tags.csv")  

new_frame$count_tag <- 
  sapply(new_frame$day, function(x)
    sum(as.Date(tags_at_liberty$min, "%Y-%m-%d") <= as.Date(x, "%Y-%m-%d") &
          as.Date(tags_at_liberty$max, "%Y-%m-%d") >= as.Date(x, "%Y-%m-%d")))


new_frame$count <-as.numeric(as.character(new_frame$count))
new_frame$count_tag <-as.numeric(as.character(new_frame$count_tag))
new_frame$shark_stand <- (new_frame$count / new_frame$count_tag)
new_frame$shark_stand <- new_frame$shark_stand * 100





write.csv(new_frame, "../data/stats/sharks_stand_stats.csv")

sharks_plot <- read.csv("../data/stats/sharks_stand_stats.csv")





sharks_plot$year <- substr(sharks_plot$monthyear, 0, 4)
sharks_plot$day <- substr(sharks_plot$day, 9, 10)

sharks_plot$shark_stand <- as.integer(sharks_plot$shark_stand)
sharks_plot$month <- substr(sharks_plot$monthyear, 6, 7)

require("glmmTMB")
library(lme4)
shark_model <- glmmTMB((shark_stand ~ year + month + (1|rep_no)),
                       data = sharks_plot,
                       family = nbinom2)

shark_model_2 <- glmmTMB((shark_stand ~ month + year (1|rep_no)),
                       data = sharks_plot,
                       family = nbinom2)



every_nth = function(n) {
  return(function(x) {x[c(TRUE, rep(FALSE, n - 1))]})
}


pdf("../results/Thesis_figures/acoustic_variance.pdf")
ggplot(sharks_plot, aes(x=year, y=shark_stand)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=16,
               outlier.size=2, colour = "blue", fill = "gray") + 
  #scale_x_discrete(breaks = every_nth(n = 3)) +
  ylab("Standardised acoustic detections") + 
  xlab("Year") +
  coord_cartesian(ylim = c(0, 650)) + theme(axis.text.y   = element_text(size=12),
                                            axis.text.x   = element_text(size=12, angle = 90, hjust = 1),
                                            axis.title.y  = element_text(size=14),
                                            axis.title.x  = element_text(size=14),
                                            panel.background = element_blank(),
                                            panel.grid.major = element_blank(), 
                                            panel.grid.minor = element_blank(),
                                            axis.line = element_line(colour = "black"),
                                            panel.border = element_rect(colour = "black", fill=NA, size=2)
  )

dev.off()

plot <- aggregate(sharks_plot[, 7], list(sharks_plot$year), mean)

ggplot() + geom_bar(plot, mapping = aes(x=Group.1, y=x), stat="identity", colour = "blue", fill = "gray") +
  ylab("Detection frequency (standardised by tags at liberty)") +
  xlab("Year") +
  #geom_line(summary_tags, mapping = aes(x=monthyear, y=standard2, group = 1)) +
  #geom_bar(alpha = 0.5) +
  #theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.text.y   = element_text(size=12),
                                            axis.text.x   = element_text(size=12, angle = 90, hjust = 1),
                                            axis.title.y  = element_text(size=14),
                                            axis.title.x  = element_text(size=14),
                                            panel.background = element_blank(),
                                            panel.grid.major = element_blank(), 
                                            panel.grid.minor = element_blank(),
                                            axis.line = element_line(colour = "black"),
                                            panel.border = element_rect(colour = "black", fill=NA, size=2)
  )


#dev.off()

########### Detection days 
detections_days$monthyear <- detections_days$NewDate
detections_days <- detections_days[,-4]
day_boxplot <- merge(detections_days, counttag, by = "monthyear")

day_boxplot$count <-as.numeric(as.character(day_boxplot$count))
day_boxplot$count_tag <-as.numeric(as.character(day_boxplot$count_tag))
day_boxplot$shark_stand <- (day_boxplot$count / day_boxplot$count_tag)
day_boxplot$shark_stand <- day_boxplot$shark_stand * 10

day_boxplot$year <- substr(day_boxplot$monthyear, 0, 4)


ggplot(day_boxplot, aes(x=year, y=shark_stand)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=16,
               outlier.size=2, colour = "black", fill = "gray") + 
  #scale_x_discrete(breaks = every_nth(n = 3)) +
  ylab("Standardised acoustic detections") + 
  xlab("Month") +
  coord_cartesian(ylim = c(0, 100)) + theme(axis.text.y   = element_text(size=12),
                                            axis.text.x   = element_text(size=12, angle = 90, hjust = 1),
                                            axis.title.y  = element_text(size=14),
                                            axis.title.x  = element_text(size=14),
                                            panel.background = element_blank(),
                                            panel.grid.major = element_blank(), 
                                            panel.grid.minor = element_blank(),
                                            axis.line = element_line(colour = "black"),
                                            panel.border = element_rect(colour = "black", fill=NA, size=2)
  )


