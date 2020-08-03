
rm(list=ls()) #Clear global environment 
setwd("/Users/emmadeeks/Desktop/CMEECourseWork/project/data") #go to the data directory 
#Map of the world American

library(data.table)

library(tidyverse)
library(lubridate)
library(plyr)

library(ggplot2)

tags_at_liberty <- read.csv("../results/acoustic_GPS/AG_standardising_tags.csv")  


setwd("/Users/emmadeeks/Desktop/CMEECourseWork/project/data") #go to the data directory 


acoustic <- read.csv("New_data_no_dg_hour/acoustic_GPS_no_elas.csv")
BPV <- read.csv("New_data_no_dg_hour/BPV_formatted_CORRECT_hour_no_dg.csv")

acoustic$NewDate <- substr(acoustic$Date, 0, 10)

BPV$year <- substr(BPV$Date, 0, 4)
BPV$month <- substr(BPV$Date, 6, 7)

nest_BPV <- BPV %>%
  nest(data= -NewDate)
priority = as.data.frame(matrix(nrow = 1, ncol = 2))
rows <- c("BPV_date", "actual_hours")
colnames(priority) <- rows
for (i in 1:length(nest_BPV$NewDate)){
  monthdata <- nest_BPV$data[[i]]
  month <- nest_BPV$NewDate[[i]]
  rows <- nrow(monthdata)
  new <- c(as.character(month), rows)
  priority <- rbind(priority, new)
}

priority <- priority[-1,]


########## removing unneeded months from acoustic dataset 

acoustic <- acoustic[order(as.Date(acoustic$NewDate, format="%Y-%m-%d")),]

acoustic <- acoustic[-(1:102409),]
acoustic$NewDate <- substr(acoustic$Date, 0, 7)
missing <- c("2016-07", "2016-08", "2016-09", "2016-10", "2016-11", "2016-12", "2017-01", "2017-02", "2017-03", "2017-04", "2017-05", "2018-01", "2018-07")

for (i in 1:13) {
  to_remove = missing[i]
  acoustic = acoustic[!acoustic$NewDate == to_remove, ]
}

nest_acoustic <- acoustic %>%
  nest(data= -NewDate)

sharks_lib = as.data.frame(matrix(nrow = 1, ncol = 2))
rows <- c("monthyear", "sharks_study")
colnames(sharks_lib) <- rows
for (i in 1:length(nest_acoustic$NewDate)){
  monthdata <- nest_acoustic$data[[i]]
  month <- nest_acoustic$NewDate[[i]]
  sharks_site <- unique(monthdata$Code)
  sharks_site_total <- length(sharks_site)
  new <- c(as.character(month), sharks_site_total)
  sharks_lib <- rbind(sharks_lib, new)
}
sharks_lib <- sharks_lib[-1,]
############################# shuffle loop 
original_summary <- read.csv("../results/acoustic_GPS/updated_AG_NR_summary_sharks_no_dg_NOREPEATS.csv")  

average_table <- cbind(as.character(original_summary$monthyear), original_summary$standard2)
cols <- c("acoustic_date", "original")
colnames(average_table) <- cols 
average_table <- as.data.frame(average_table)

r_squared = as.data.frame(matrix(nrow = 1, ncol = 3))
rows <- c("permutation", "Multiple_R-squared", "adjusted_r_squared")
colnames(r_squared) <- rows


for (i in 1:100) {
  shuffle_acoustic <- acoustic[sample(nrow(acoustic)),] #shuffle rows 
  shuffle_BPV <- BPV[sample(nrow(BPV)),] #shuffle rows 
  shuffle_acoustic <- shuffle_acoustic[,-1]
  shuffle_BPV <- shuffle_BPV[,-1]
  
  shuffle_acoustic <- transform(shuffle_acoustic, id=match(NewDate, unique(NewDate))) ###### after shuffling rows you assign a unique id for each NewDate
  shuffle_BPV <- transform(shuffle_BPV, id=match(NewDate, unique(NewDate)))
  
  ######## create a pretend date with the time, day and the ID as the month and then merge like you do with the overlap analysis 
  shuffle_acoustic$new <- substr(shuffle_acoustic$Date, 8, 19)
  shuffle_acoustic$new <- paste(shuffle_acoustic$id,shuffle_acoustic$new, sep = "")
  
  shuffle_BPV$new <- substr(shuffle_BPV$Date, 8, 19)
  shuffle_BPV$new <- paste(shuffle_BPV$id,shuffle_BPV$new, sep = "")
  
  ####### merge and match the made up dates 
  m1 <- merge(shuffle_acoustic, shuffle_BPV, by = "new")
  
  all_overlap <- m1
  
  r.ft <- 6378137*3.28084             # radius of the earth, in feet
  r.km   <- r.ft*0.0003048
  sep.km   <- 20
  all_overlap$distance<-distHaversine(all_overlap[,3:4], all_overlap[,10:11], r=r.km)
  all_10_overlap <- all_overlap[all_overlap$distance<sep.km,]
  
  overlap <- all_10_overlap
  new_uniq <- overlap[!duplicated(overlap[c('new', 'Code')]),] 
  
  overlap <- new_uniq
  all_nestmonths <- overlap %>%
    nest(data= -id.x) #this is experimenting with nesting, by nesting the data i can suset the day by ID and go into that
  
  setwd("/Users/emmadeeks/Desktop/CMEECourseWork/project/data") #go to the data directory 
  
  summary_sharks = as.data.frame(matrix(nrow = 1, ncol = 5))
  for (j in 1:length(all_nestmonths$id.x)){
    monthdata <- all_nestmonths$data[[j]]
    month <- all_nestmonths$id.x[[j]]
    rows <- nrow(monthdata)
    date_BPV <- as.character(substr(monthdata$Date.y, 0, 7))
    date_BPV <- date_BPV[1]
    date_ac <- as.character(substr(monthdata$Date.x, 0, 10))
    date_ac <- date_ac[1]
    no_sharks <- unique(monthdata$Code)
    no_sharks <- length(no_sharks)
    toadd <- c(as.character(month), rows, no_sharks, date_ac, date_BPV)
    summary_sharks <- rbind(summary_sharks, toadd)
  }
  
  summary <- c("month", "count", "number_sharks", "acoustic_date", "BPV_date")
  colnames(summary_sharks) <- summary
  summary_sharks <- summary_sharks[-1,]
  
  
  
  summary_sharks$count_tag <- 
    sapply(summary_sharks$acoustic_date, function(x)
      sum(as.Date(tags_at_liberty$min, "%Y-%m-%d") <= as.Date(x, "%Y-%m-%d") &
            as.Date(tags_at_liberty$max, "%Y-%m-%d") >= as.Date(x, "%Y-%m-%d")))
  
  
  missing_dataframe = as.data.frame(matrix(nrow = 13, ncol = 9))
  missing <- c("2016-07", "2016-08", "2016-09", "2016-10", "2016-11", "2016-12", "2017-01", "2017-02", "2017-03", "2017-04", "2017-05", "2018-01", "2018-07")
 new_df <- cbind(missing, missing_dataframe)

  
  names <- c("monthyear", "month", "count", "number_sharks", "acoustic_date", "BPV_date", "count_tag", "year", "stations", "month2")
  colnames(new_df) <- names
  
  summary <- merge(summary_sharks, priority, by= 'BPV_date', all.x = T)
  summary$standard2 <- (as.numeric(summary$count) / (as.numeric(summary$actual_hours) * as.numeric(summary$count_tag)))
  
  summary$acoustic_date <- substr(summary$acoustic_date, 0, 7)
  new_dat <- cbind(as.character(summary$acoustic_date), summary$standard2)
  new_dat <- as.data.frame(new_dat)
  
  
  
  nam <- paste("A", i, sep = "")
  cols <- c("acoustic_date", nam)
  colnames(new_dat) <- cols 
  average_table <- merge(average_table, new_dat, by = 'acoustic_date', all.x = T)
  
  
  assign(nam, summary)
  
  summary <- na.omit(summary)
  summary$X <- 1:nrow(summary)
  stats <- summary(lm(summary$standard2 ~ poly(summary$X, 2, raw = TRUE)))
  add1 <- stats$r.squared
  add <- stats$adj.r.squared
  toadd <- c(nam, add1, add)
  r_squared <- rbind(r_squared, toadd)
  
}

View(average_table)
write.csv(r_squared, "stats/r_squared.csv")


######################################

summary(lm(summary_tags$standard3 ~ poly(summary_tags$X, 2, raw = TRUE)))
r_squared <- r_squared[-1,]
r_squared[order(r_squared$`Multiple_R-squared`),]  

#average_table <- as.data.frame(average_table)
library(data.table)
long <- melt(setDT(average_table), id.vars = c("acoustic_date"), variable.name = "Standard")

long$value <- as.numeric(as.character(long$value))
long$value <- round(long$value, digits = 3)

average_table[,3:102] <- lapply(average_table[,3:102], function(x) as.numeric(as.character(x)))


average_table$mean=rowMeans(average_table[,-c(1:2)], na.rm = TRUE)

average_table$mean <- as.numeric(as.character(average_table$mean))
average_table$acoustic_date <- as.character(average_table$acoustic_date)

write.csv(average_table, "../results/Thesis_figures/permutation_average.csv")
average_table <- read.csv("../results/Thesis_figures/permutation_average.csv" )



pdf("../results/acoustic_GPS/permute_test/compare_permute_mean_original.pdf")
ggplot() + 
  geom_line(average_table, mapping = aes(x=as.character(acoustic_date), y=as.numeric(as.character(mean)), group = 1), colour = "red", size = 1) +
  geom_line(average_table, mapping = aes(x=as.character(acoustic_date), y=as.numeric(as.character(original)), group = 1), colour="black", size = 1) +
  scale_x_discrete() +
  #geom_point() + 
  xlab("Acoustic date") +# for the x axis label
  ylab("Overlap scoree (Standardisation 2)") +
  #geom_line(summary,mapping =  aes(x=acoustic_date, y=standard2, group = 1), colour="#000099") +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1), panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

dev.off()




pdf("../results/acoustic_GPS/permute_test/permute_boxplot.pdf")
ggplot(long) + 
  geom_boxplot(aes(x=acoustic_date, y= value)) +
  xlab("Acoustic date") +# for the x axis label
  ylab("Overlap scoree (Standardisation 2)") +
  geom_line(average_table, mapping = aes(x=as.character(acoustic_date), y=as.numeric(as.character(original)), group = 1), colour="black", size = 1) +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1), panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

dev.off()

average_table$X <- 1:nrow(average_table)
#lm(standard~X, data = summary_tags)
summary(lm(average_table$mean ~ poly(average_table$X, 2, raw = TRUE)))
#original_summary$standard2 <- original_summary$standard2 / 50


ggplot(original_summary, aes(x=monthyear, y=standard2, group = 1)) + 
  geom_line(original_summary, mapping =aes(x=monthyear, y=log(standard2), group = 1), colour = "black", size = 2) +
  geom_line(A1, mapping =  aes(x=acoustic_date, y=log(standard2), group = 1), colour="grey", size = 1) +
  geom_line(A2, mapping = aes(x=acoustic_date, y=log(standard2), group = 1), colour="grey", size = 1) +
  geom_line(A3, mapping = aes(x=acoustic_date, y=log(standard2), group = 1), colour="grey", size = 1) +
  geom_line(A4, mapping = aes(x=acoustic_date, y=log(standard2), group = 1), colour="grey", size = 1) +
  geom_line(A5, mapping = aes(x=acoustic_date, y=log(standard2), group = 1), colour="grey", size = 1) +
  #geom_point() + 
  xlab("Month") +# for the x axis label
  ylab("Proportion of successful overlaps compared to 744 and sharks at liberty") +
  #geom_line(summary,mapping =  aes(x=acoustic_date, y=standard2, group = 1), colour="#000099") +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1), panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

pdf("../results/acoustic_GPS/permute_test/permute_fig_may.pdf")
ggplot(original_summary, aes(x=monthyear, y=standard2, group = 1), ylim(0, 0.040)) + 
  geom_line(A1, mapping =  aes(x=acoustic_date, y=standard2, group = 1), colour="grey", size = 0.8) +
  geom_line(A2, mapping = aes(x=acoustic_date, y=standard2, group = 1), colour="grey", size = 0.8) +
  geom_line(A3, mapping = aes(x=acoustic_date, y=standard2, group = 1), colour="grey", size = 0.8) +
  geom_line(A4, mapping = aes(x=acoustic_date, y=standard2, group = 1), colour="grey", size = 0.8) +
  geom_line(A5, mapping = aes(x=acoustic_date, y=standard2, group = 1), colour="grey", size = 0.8) +
  geom_line(A6, mapping =  aes(x=acoustic_date, y=standard2, group = 1), colour="grey", size = 0.8) +
  geom_line(A7, mapping = aes(x=acoustic_date, y=standard2, group = 1), colour="grey", size = 0.8) +
  geom_line(A8, mapping = aes(x=acoustic_date, y=standard2, group = 1), colour="grey", size = 0.8) +
  geom_line(A9, mapping = aes(x=acoustic_date, y=standard2, group = 1), colour="grey", size = 0.8) +
  geom_line(A10, mapping = aes(x=acoustic_date, y=standard2, group = 1), colour="grey", size = 0.8) +
  geom_line(original_summary, mapping =aes(x=monthyear, y=standard2, group = 1), colour = "black", size = 1.2) +
  #geom_point() + 
  xlab("Month") +# for the x axis label
  ylab("Proportion of successful overlaps") +
  #geom_line(summary,mapping =  aes(x=acoustic_date, y=standard2, group = 1), colour="#000099") +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1), panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

dev.off()



lm(standard~X, data = summary_tags)
summary(lm(original_summary$standard2 ~ poly(original_summary$X.2, 2, raw = TRUE)))

original_summary <- merge(original_summary, sharks_lib, by = "monthyear", all.x = T)

original_summary$standard2.2 <- (as.numeric(original_summary$count) / (as.numeric(original_summary$actual_hours) * as.numeric(original_summary$sharks_study)))

original_summary$standard1.2 <- (as.numeric(original_summary$count) / (as.numeric(original_summary$Boat_station_freq) * as.numeric(original_summary$sharks_study)))





ggplot(original_summary, aes(x=monthyear, y=standard1.2, group = 1)) + 
  geom_line(original_summary, mapping =  aes(x=monthyear, y=standard1.2, group = 1), colour="black", size = 0.8) +
  #geom_line(original_summary, mapping =aes(x=monthyear, y=standard2, group = 1), colour = "black", size = 1.2) +
  #geom_point() + 
  xlab("Month") +# for the x axis label
  ylab("Proportion of successful overlaps") +
  #geom_line(summary,mapping =  aes(x=acoustic_date, y=standard2, group = 1), colour="#000099") +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1), panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


summary(lm(original_summary$standard2.2 ~ poly(original_summary$X.2, 2, raw = TRUE)))


summary(lm(original_summary$standard1.2 ~ poly(original_summary$X.2, 2, raw = TRUE)))

write.csv(original_summary, "../results/acoustic_GPS/new_summary_study_site_sharks.csv")

#pdf("../results/acoustic/stat_each_month_BPV_plotted_compare.pdf")


############## Standardisation 1 ########
summary_tags$month2 <- substr(summary_tags$monthyear, 6, 7)
new_df <- new_df[,-12]
#summary_tags$standard1 <- (summary_tags$count / (summary_tags$Boat_station_freq * summary_tags$count_tag * summary_tags$stations))
trying <- rbind(new_df, summary_tags)
trying$monthyear <- paste0(trying$monthyear, "-01")

summary_tags <- trying[order(as.Date(trying$monthyear, format="%Y-%m-%d")),]
