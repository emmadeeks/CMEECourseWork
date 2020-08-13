rm(list=ls()) #Clear global environment 


library(dplyr)
library(ggplot2)

library(tidyverse)
library(lubridate)
library(plyr)
library(cowplot)



setwd("/Users/emmadeeks/Desktop/CMEECourseWork/project/data") #go to the data directory 



###################### KM plot 


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


inner_travel$NewDate <- substr(inner_travel$day, 0, 7)
BPV_AV <- inner_travel %>%
  nest(data= -NewDate)

mean_table = as.data.frame(matrix(nrow = 1, ncol = 3))
rows <- c("monthyear", "average", "SD")
colnames(mean_table) <- rows
for (i in 2:length(BPV_AV$NewDate)){
  mydata <- BPV_AV$data[[i]]
  mydata <- as.data.frame(mydata)
  a <- as.numeric(mydata$km_travelled)
  date <- BPV_AV$NewDate[[i]]
  mean <- mean(a)
  sd <- sd(a)
  toadd <- c(date, mean, sd)
  mean_table <- rbind(mean_table, toadd)
}

mean_table <- mean_table[-1,]
mean_table <- na.omit(mean_table)

mean_table$average <- as.numeric(as.character(mean_table$average))

mean_table$X <- 1:nrow(mean_table)
summary(lm(mean_table$average ~ poly(mean_table$X, 2, raw = TRUE)))

mean_table <- mean_table[-1,]

missing_dataframe = as.data.frame(matrix(nrow = 13, ncol = 4))

names <- colnames(mean_table)
colnames(missing_dataframe) <- names


missing <- c("2016-07", "2016-08", "2016-09", "2016-10", "2016-11", "2016-12", "2017-01", "2017-02", "2017-03", "2017-04", "2017-05", "2018-01", "2018-07")
missing_dataframe$monthyear <- missing


trying <- rbind(missing_dataframe, mean_table)

summary_tags <- trying
summary_tags$month <- substr(summary_tags$monthyear, 6,7)


summary_tags$SD <- as.numeric(summary_tags$SD)

every_nth = function(n) {
  return(function(x) {x[c(TRUE, rep(FALSE, n - 1))]})
}

summary_tags$year <- substr(summary_tags$monthyear, 0 ,4)

b<- ggplot(summary_tags, mapping = aes(x=monthyear, y=average, group = 1)) + 
  geom_ribbon(data = summary_tags, mapping = aes(ymin = average - SD, ymax = average + SD), fill = "#208EA3", alpha = 0.5) + 
  geom_line(size = 1) +
  #geom_line(aes(y = average))
  geom_point(size = 2, shape = 1) +
  #facet_wrap(~summary_tags$year, strip.position = "bottom", scales = "free_x") +
  #ggtitle("Standardisation 3: Potential overlap is station with highest sharks, tags at liberty and hours in station") +
  #scale_y_continuous(sec.axis = sec_axis(~. *200, "Number of interceptions (bars)")) +
  #geom_bar(final, mapping = aes(x=Month, y=(IUU_events/200)), stat="identity", alpha = 0.1, fill = "blue", colour = "black") +
  scale_x_discrete(breaks = every_nth(n = 3)) +
  stat_smooth(method="lm", se=TRUE, fill=NA, formula=y ~ poly(x, 2, raw=TRUE),colour="red", size = 0.9) +
  annotate("rect", xmin = '2016-06', xmax = '2017-06', ymin = -100, ymax = 650, alpha = 0.4) +
  xlab("") +# for the x axis label
  ylab("Daily km travelled in MPA") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), text = element_text(size=12, color = "black"), axis.line = element_line(colour = "black"), 
        panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
#axis.line.y.right = element_line(color = "blue4"), 
#axis.ticks.y.right = element_line(color = "blue4"))
b  





################# acoustic detections 



sharks_plot <- read.csv("../data/stats/correct_shark_stand.csv")




sharks_plot$NewDate <- substr(sharks_plot$monthyear, 0, 7)
sharks_plot <-sharks_plot[!(sharks_plot$NewDate == "2018-02"),]
sharks_plot <-sharks_plot[!(sharks_plot$NewDate == "2018-01"),]
shark_nest <- sharks_plot %>%
  nest(data= -NewDate)

mean_table_sharks = as.data.frame(matrix(nrow = 1, ncol = 3))
rows <- c("monthyear", "average", "SD")
colnames(mean_table_sharks) <- rows
for (i in 2:length(shark_nest$NewDate)){
  mydata <- shark_nest$data[[i]]
  mydata <- as.data.frame(mydata)
  a <- as.numeric(mydata$plot1)
  date <- shark_nest$NewDate[[i]]
  mean <- mean(a)
  sd <- sd(a)
  toadd <- c(date, mean, sd)
  mean_table_sharks <- rbind(mean_table_sharks, toadd)
}


mean_table_sharks <- mean_table_sharks[-1,]


mean_table_sharks$average <- as.numeric(mean_table_sharks$average)
mean_table_sharks$SD <- as.numeric(mean_table_sharks$SD)


c <- ggplot(mean_table_sharks, mapping =  aes(x=monthyear, y=average, group = 1)) + 
  
  geom_ribbon(data = mean_table_sharks, mapping = aes(ymin = average - SD, ymax = average + SD), fill = "#208EA3", alpha = 0.5) + 
  geom_line(size = 1) +
  #geom_line(aes(y = average))
  geom_point(size = 2, shape = 1) +
  #ggtitle("Standardisation 3: Potential overlap is station with highest sharks, tags at liberty and hours in station") +
  #scale_y_continuous(sec.axis = sec_axis(~. *200, "Number of interceptions (bars)")) +
  #geom_bar(final, mapping = aes(x=Month, y=(IUU_events/200)), stat="identity", alpha = 0.1, fill = "blue", colour = "black") +
  scale_x_discrete(breaks = every_nth(n = 3)) +
  stat_smooth(method="lm", se=TRUE, fill=NA, formula=y ~ poly(x, 2, raw=TRUE),colour="red", size = 0.9) +
  #annotate("rect", xmin = '2016-06', xmax = '2017-06', ymin = -100, ymax = 650, alpha = 0.3) +
  xlab("Date") +# for the x axis label
  ylab("Daily elasmobranch detections") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), text = element_text(size=12, color = "black"), axis.line = element_line(colour = "black"), 
        panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
#axis.line.y.right = element_line(color = "blue4"), 
c


mean_table_sharks$X <- 1:nrow(mean_table_sharks)
okay <- summary(lm(mean_table_sharks$average ~ poly(mean_table_sharks$X, 1, raw = TRUE)))


###################### sharks overlap 


summary_tags <- read.csv("../results/acoustic_GPS/updated_AG_NR_summary_sharks_no_dg_NOREPEATS.csv")  

missing_dataframe = as.data.frame(matrix(nrow = 13, ncol = 19))

names <- colnames(summary_tags)
colnames(missing_dataframe) <- names


missing <- c("2016-07", "2016-08", "2016-09", "2016-10", "2016-11", "2016-12", "2017-01", "2017-02", "2017-03", "2017-04", "2017-05", "2018-01", "2018-07")
missing_dataframe$monthyear <- missing


trying <- rbind(summary_tags, missing_dataframe)

summary_tags <- trying
summary_tags$month <- substr(summary_tags$monthyear, 6,7)
summary_tags$month <- as.factor(summary_tags$month)
summary_tags$month <- month.abb[summary_tags$month]
summary_tags$year <- substr(summary_tags$monthyear, 0, 4)
#################### plotting 


require("RColorBrewer")

a <- ggplot(summary_tags, aes(x= month, y=standard2, fill=factor(year), colour = factor(year), group=factor(year))) + 
  geom_line(size=1) + 
  geom_point(size = 2.5, shape = 1) +
  xlab("Month") +# for the x axis label
  ylab("Standardised overlap count") +
  scale_x_discrete(limits = month.abb) +
  #ggtitle("Standardisation 2: Hours recorded and tags at liberty") +
  #scale_shape_manual(values = c(0,1,3,0,1,3)) +
  scale_colour_manual(name = "Year", values = c('#E8384F', '#208EA3', '#EECC16', '#37A862', '#AA71FF', '#8D9F9B')) + 
  scale_fill_manual(name = "Year", values = c('#E8384F', '#208EA3', '#EECC16', '#37A862', '#AA71FF', '#8D9F9B')) +
  #theme(axis.text.x = element_text(angle = 180, hjust = 1)) 
  theme_bw() +
  theme(#legend.position= c(0.7,0.7), 
        legend.position= c(0.8,0.7),
        legend.key=element_blank(), text = element_text(size=12, color = "black"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.title = element_text(size = 11),
        legend.text = element_text(size = 10), 
        axis.line = element_line(colour = "black"),  panel.border = element_rect(colour = "black", fill=NA, size=1), legend.title.align=0.5)



a <- a + guides(col = guide_legend(nrow = 3))

every_nth = function(n) {
  return(function(x) {x[c(TRUE, rep(FALSE, n - 1))]})
}

final <- read.csv('../results/acoustic_GPS/IUU_POTENIAL_ORIGINAL_summary_sharks_no_dg_NOREPEATS.csv')
final[36,5] <- 0
final[38,5] <- 0

summary_tags <- read.csv("../results/acoustic_GPS/updated_AG_NR_summary_sharks_no_dg_NOREPEATS.csv") 

missing_dataframe = as.data.frame(matrix(nrow = 13, ncol = 19))

names <- colnames(summary_tags)
colnames(missing_dataframe) <- names


missing <- c("2016-07", "2016-08", "2016-09", "2016-10", "2016-11", "2016-12", "2017-01", "2017-02", "2017-03", "2017-04", "2017-05", "2018-01", "2018-07")
missing_dataframe$monthyear <- missing


trying <- rbind(missing_dataframe, summary_tags)

summary_tags <- trying
summary_tags$month <- substr(summary_tags$monthyear, 6,7)


d <-  ggplot(summary_tags, mapping =  aes(x=monthyear, y=standard2, group = 1)) + 
  geom_line(size = 1) +
  geom_point(size = 2.5, shape = 1) +
  #ggtitle("Standardisation 3: Potential overlap is station with highest sharks, tags at liberty and hours in station") +
  #scale_y_continuous(sec.axis = sec_axis(~. *200, "Number of interceptions (bars)")) +
  #geom_bar(final, mapping = aes(x=Month, y=(IUU_events/200)), stat="identity", alpha = 0.1, fill = "blue", colour = "black") +
  scale_x_discrete(breaks = every_nth(n = 3)) +
  stat_smooth(method="lm", se=TRUE, fill=NA, formula=y ~ poly(x, 2, raw=TRUE),colour="red", size = 0.9) +
  annotate("rect", xmin = '2016-06', xmax = '2017-06', ymin = 0, ymax = 0.025, alpha = 0.4) +
  xlab("Date") +# for the x axis label
  ylab("Standardised overlap count") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),   panel.grid.minor = element_blank(), text = element_text(size=12, color = "black"),
        axis.line = element_line(colour = "black"), panel.border = element_rect(colour = "black", fill=NA, size=1), 
        panel.grid.major = element_blank())
#axis.line.y.right = element_line(color = "blue4"), 
#axis.ticks.y.right = element_line(color = "blue4"))

d


n <- plot_grid(d,a, ncol = 1, labels = 'auto')
pdf("../results/Thesis_figures/panel_fig_2.pdf")
n
dev.off()

t <- plot_grid(c, b, ncol = 1, labels = 'auto')
t
pdf("../results/Thesis_figures/panel_fig_1.pdf")
t
dev.off()





##################### Figure 5

library(data.table)
Simulation  <- c(206)
Actual <- c(1221)

plot <- rbind(Simulation, Actual)
names <- "count"
colnames(plot) <- names
plot <- as.data.frame(plot)
plot <- setDT(plot, keep.rownames = TRUE)[]


ggplot(plot) +
  geom_bar(aes(x = rn, y = count), stat="identity", fill="skyblue", alpha=0.7) +
  theme_bw() +
  theme(
    text = element_text(size=14, color = "black"), axis.text.x = element_text(size = 14), 
    axis.text.y = element_text(size = 14), legend.position= c(0.8,0.9),
    legend.text = element_text(size = 14), axis.line = element_line(colour = "black"))


pdf("../results/Thesis_figures/random_walk.pdf")
ggplot(plot, mapping = aes(x = rn, y = count)) + geom_bar(stat="identity", colour = 'gray50', fill = c('red', '#208EA3'), alpha = 0.7) +
  ylab("Counts of overlap") +
  xlab("") +
  theme_bw() +
  #scale_fill_manual(values = c('red', '#208EA3')) +
  #geom_errorbar(aes(ymin=x.x-x.y, ymax=x.x+x.y), width=.2) +
  #geom_line(summary_tags, mapping = aes(x=monthyear, y=standard2, group = 1)) +
  #geom_bar(alpha = 0.5) +
  #theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.text.y   = element_text(size=18),
        axis.text.x   = element_text(size=18),
        axis.title.y  = element_text(size=19),
        axis.title.x  = element_text(size=20),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=2), panel.grid.major = element_blank(), panel.grid.minor = element_blank())

dev.off()


######################################### Re-making figure 1
year_merge <- read.csv("stats/mum_all_days_counts_sharks_stand.csv")


count_day <- year_merge %>%
  nest(data= -monthyear)

mean_table = as.data.frame(matrix(nrow = 1, ncol = 3))
rows <- c("monthyear", "average", "SD")
colnames(mean_table) <- rows
for (i in 2:length(count_day$monthyear)){
  mydata <- count_day$data[[i]]
  mydata <- as.data.frame(mydata)
  a <- as.numeric(mydata$use)
  date <- as.character(count_day$monthyear[[i]])
  mean <- mean(a)
  #sd <- sd(a)
  sd <- std.error(a)
  toadd <- c(date, mean, sd)
  mean_table <- rbind(mean_table, toadd)
}

mean_table <- mean_table[-1,]
#mean_table <- na.omit(mean_table)

mean_table$average <- as.numeric(as.character(mean_table$average))

mean_table$X <- 1:nrow(mean_table)
summary(lm(mean_table$average ~ poly(mean_table$X, 2, raw = TRUE)))

#mean_table <- mean_table[-1,]

missing_dataframe = as.data.frame(matrix(nrow = 13, ncol = 4))

names <- colnames(mean_table)
colnames(missing_dataframe) <- names


missing <- c("2016-07", "2016-08", "2016-09", "2016-10", "2016-11", "2016-12", "2017-01", "2017-02", "2017-03", "2017-04", "2017-05", "2018-01", "2018-07")
missing_dataframe$monthyear <- missing


trying <- rbind(missing_dataframe, mean_table)

summary_tags <- trying
summary_tags$month <- substr(summary_tags$monthyear, 6,7)


summary_tags$SD <- as.numeric(summary_tags$SD)

every_nth = function(n) {
  return(function(x) {x[c(TRUE, rep(FALSE, n - 1))]})
}

summary_tags$year <- substr(summary_tags$monthyear, 0 ,4)
pdf("../results/Thesis_figures/day_overlap_count.pdf")
 ggplot(summary_tags, mapping = aes(x=monthyear, y=average, group = 1)) + 
  geom_ribbon(data = summary_tags, mapping = aes(ymin = average - SD, ymax = average + SD), fill = "#208EA3", alpha = 0.5) + 
  geom_line(size = 1) +
  #geom_line(aes(y = average))
  geom_point(size = 2, shape = 1) +
  #facet_wrap(~summary_tags$year, strip.position = "bottom", scales = "free_x") +
  #ggtitle("Standardisation 3: Potential overlap is station with highest sharks, tags at liberty and hours in station") +
  #scale_y_continuous(sec.axis = sec_axis(~. *200, "Number of interceptions (bars)")) +
  #geom_bar(final, mapping = aes(x=Month, y=(IUU_events/200)), stat="identity", alpha = 0.1, fill = "blue", colour = "black") +
  scale_x_discrete(breaks = every_nth(n = 3)) +
  stat_smooth(method="lm", se=TRUE, fill=NA, formula=y ~ poly(x, 2, raw=TRUE),colour="red", size = 0.9) +
  annotate("rect", xmin = '2016-06', xmax = '2017-06', ymin = -7, ymax = 20, alpha = 0.4) +
  xlab("") +# for the x axis label
  ylab("Average Overlap score per day") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),   panel.grid.minor = element_blank(), text = element_text(size=12, color = "black"), axis.line = element_line(colour = "black"), 
        panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.major = element_blank())
#axis.line.y.right = element_line(color = "blue4"), 
#axis.ticks.y.right = element_line(color = "blue4"))
 
 dev.off()
 
b  

summary_tags$X <- 1:nrow(summary_tags)
summary(lm(summary_tags$average ~ poly(summary_tags$X, 2, raw = TRUE)))




x <- ggplot(summary_tags, mapping = aes(x=monthyear, y=average, group = 1)) + 
  geom_ribbon(data = summary_tags, mapping = aes(ymin = average - SD, ymax = average + SD), fill = "#208EA3", alpha = 0.5) + 
  geom_line(size = 1) +
  #geom_line(aes(y = average))
  geom_point(size = 2, shape = 1) +
  #facet_wrap(~summary_tags$year, strip.position = "bottom", scales = "free_x") +
  #ggtitle("Standardisation 3: Potential overlap is station with highest sharks, tags at liberty and hours in station") +
  #scale_y_continuous(sec.axis = sec_axis(~. *200, "Number of interceptions (bars)")) +
  #geom_bar(final, mapping = aes(x=Month, y=(IUU_events/200)), stat="identity", alpha = 0.1, fill = "blue", colour = "black") +
  scale_x_discrete(breaks = every_nth(n = 3)) +
  stat_smooth(method="lm", se=TRUE, fill=NA, formula=y ~ poly(x, 2, raw=TRUE),colour="red", size = 0.9) +
  annotate("rect", xmin = '2016-06', xmax = '2017-06', ymin = 0, ymax = 10, alpha = 0.4) +
  xlab("") +# for the x axis label
  ylab("Daily overlap score") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),   panel.grid.minor = element_blank(), text = element_text(size=12, color = "black"), axis.line = element_line(colour = "black"), 
        panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.major = element_blank())


xa <- plot_grid(b, c, ncol = 1, scale = 0.9, labels = 'auto', label_x = 0, label_y = 1)

xa <- plot_grid(b, c, ncol = 1, labels = 'auto')

xa <- plot_grid(x, b, c, ncol = 1, labels = 'auto')
pdf("../results/Thesis_figures/panel_fig_2_DAY.pdf")
xa
dev.off()
