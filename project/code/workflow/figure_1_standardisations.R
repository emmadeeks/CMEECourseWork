rm(list=ls()) #Clear global environment 
setwd("/Users/emmadeeks/Desktop/CMEECourseWork/project/data/shape_files/")
#Map of the world American
#Load the data 
#install.packages('raster') # Core raster GIS data package
#install.packages('sf') # Core vector GIS data package
#install.packages('rgeos') # Extends vector data functionality
#install.packages('lwgeom') # Extends vector data functionality
#install.packages('viridis') # Because we like the colour scheme!
#install.packages('ggmap')
#install.packages('rgdal')
#install.packages("hexbin")

library(raster) #Require these packages 
library(sf)     #Require 
library(viridis)
library('units')
library('rgdal')

require(ggmap)
require(rgdal)
require(sf)
library(data.table)
library(geosphere)
library(tidyverse)
library(lubridate)
library(plyr)
require(hexbin)
library(ggplot2)
library(tidyverse)
library("wesanderson")
library(cowplot)
library(dplyr)
library(ggsn)
#chagos_array <- read_sf(dsn = ".", layer = "Chagos_array")
chagos_v6 <- read_sf(dsn = ".", layer = "Chagos_v6") #read in the shapefiles
#chagosEEZ <- read_sf(dsn = ".", layer = "ChagosEEZ") # read in the shapefiles

#plot(st_geometry(chagos_v6), asp=1, axes=TRUE)
#plot(st_geometry(chagosEEZ), asp=1, axes=TRUE, main='Chagos outline') #plot the shapefiles

tracts <- readOGR(dsn = ".", layer = "ChagosEEZ") %>%
  spTransform("+proj=longlat +ellps=WGS84")
Chagos_try <- fortify(tracts)

chagos_v6 <- readOGR(dsn = ".", layer = "Chagos_v6") %>%
  spTransform("+proj=longlat +ellps=WGS84")
Chagos_island <- fortify(chagos_v6)


setwd("/Users/emmadeeks/Desktop/CMEECourseWork/project/data") #go to the data directory 

######### Overall plot 
IUU <- read.csv("IUU_Data_catches.csv", header = T)
IUU <- IUU[,1:4]

summary_tags <- read.csv('../results/acoustic_GPS/AG_NR_summary_sharks_no_dg_NOREPEATS.csv')
summary_tags[30,12] <- NA



#potential <- read.csv('../results/acoustic_GPS/POTENIAL_summary_sharks_no_dg_NOREPEATS.csv')

#summary_tags <- read.csv('../results/acoustic_GPS/AG_NR_summary_sharks_no_dg_NOREPEATS.csv')
final <- read.csv('../results/acoustic_GPS/IUU_POTENIAL_ORIGINAL_summary_sharks_no_dg_NOREPEATS.csv')
#summary_tags$standard3 <- (summary_tags$count / (744 * summary_tags$count_tag * summary_tags$stations))

summary_tags <- read.csv("../results/acoustic_GPS/updated_AG_NR_summary_sharks_no_dg_NOREPEATS.csv")  

pdf("../results/Thesis_figures/fig_1_std2.pdf")
ggplot(summary_tags, aes(x= as.factor(month), y=standard2, fill=factor(year), colour = factor(year), group=factor(year))) + geom_line(size=0.8) + 
  geom_point(size = 1) +
  xlab("Month") +# for the x axis label
  ylab("Overlap score (between 0 and 1)") +
  #ggtitle("Standardisation 2: Hours recorded and tags at liberty") +
  #scale_shape_manual(values = c(0,1,3,0,1,3)) +
  scale_colour_manual(name = "Year", values = c('#B40F20', '#D69C4E', '#046C9A', '#ABDDDE', '#00A08A', '#000000')) + 
  scale_fill_manual(name = "Year", values = c('#B40F20', '#046C9A', '#046C9A', '#ABDDDE', '#00A08A', '#000000')) +
  #theme(axis.text.x = element_text(angle = 180, hjust = 1))  
  theme(panel.border = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), panel.background = element_rect(fill='grey96', colour='black'))
dev.off()

################# SUMMARY TAGS ##############

pdf("../results/Thesis_figures/fig_2_std2.pdf")
ggplot(summary_tags, mapping =  aes(x=monthyear, y=(standard2*250), group = 1)) + 
  geom_line(size = 1) +
  #ggtitle("Standardisation 3: Potential overlap is station with highest sharks, tags at liberty and hours in station") +
  scale_y_continuous(sec.axis = sec_axis(~. / 9, "Overlap score")) +
  geom_bar(final, mapping = aes(x=Month, y=IUU_events), stat="identity", colour = "black", alpha = 0.3) +
  scale_x_discrete() +
  stat_smooth(method="lm", se=TRUE, fill=NA, formula=y ~ poly(x, 2, raw=TRUE),colour="red") +
  xlab("Date") +# for the x axis label
  ylab("Number of IUU interceptions") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), panel.border = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), panel.background = element_rect(fill='grey96', colour='black'))
dev.off()

summary(lm(summary_tags$standard2 ~ poly(summary_tags$X, 2, raw = TRUE)))



ggplot(final, mapping =  aes(x=Month, y=to_plot, group = 1)) + 
  geom_line(size = 1) +
  #ggtitle("Standardisation 3: Potential overlap is station with highest sharks, tags at liberty and hours in station") +
  scale_y_continuous(sec.axis = sec_axis(~. / 9, "Overlap score (between 0 and 1)")) +
  geom_bar(final, mapping = aes(x=Month, y=IUU_events), stat="identity", colour = "black", alpha = 0.3) +
  scale_x_discrete() +
  stat_smooth(method="lm", se=TRUE, fill=NA, formula=y ~ poly(x, 2, raw=TRUE),colour="red") +
  xlab("Date") +# for the x axis label
  ylab("Number of IUU interceptions") +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1), panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

summary(lm(final$to_plot ~ poly(final$X, 2, raw = TRUE)))

lm(standard~X, data = summary_tags)
summary(lm(summary_tags$standard2 ~ poly(summary_tags$X, 1, raw = TRUE)))




a <- ggplot(summary_tags, aes(x= as.factor(month), y=standard2, fill=factor(year), colour = factor(year), group=factor(year))) + geom_line(size=0.8) + 
  geom_point(size = 1) +
  xlab("Month") +# for the x axis label
  ylab("Standardised overlap score") +
  #ggtitle("Standardisation 2: Hours recorded and tags at liberty") +
  #scale_shape_manual(values = c(0,1,3,0,1,3)) +
  scale_colour_manual(name = "Year", values = c('#B40F20', '#D69C4E', '#046C9A', '#ABDDDE', '#00A08A', '#000000')) + 
  scale_fill_manual(name = "Year", values = c('#B40F20', '#046C9A', '#046C9A', '#ABDDDE', '#00A08A', '#000000')) +
  #theme(axis.text.x = element_text(angle = 180, hjust = 1))  
  theme(panel.border = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), panel.background = element_rect(fill='grey96', colour='black'))

every_nth = function(n) {
  return(function(x) {x[c(TRUE, rep(FALSE, n - 1))]})
}

b <- ggplot(summary_tags, mapping =  aes(x=monthyear, y=standard2, group = 1)) + 
  geom_line(size = 0.9) +
  #ggtitle("Standardisation 3: Potential overlap is station with highest sharks, tags at liberty and hours in station") +
  scale_y_continuous(sec.axis = sec_axis(~. *200, "Number of IUU interceptions (bars)")) +
  geom_bar(final, mapping = aes(x=Month, y=(IUU_events/200)), stat="identity", alpha = 0.3, colour = "black") +
  scale_x_discrete(breaks = every_nth(n = 3)) +
  stat_smooth(method="lm", se=TRUE, fill=NA, formula=y ~ poly(x, 2, raw=TRUE),colour="blue2", size = 0.8) +
  xlab("Date") +# for the x axis label
  ylab("Standardised overlap score (lines)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), panel.border = element_blank(),
        axis.line = element_line(colour = "black"), panel.background = element_rect(fill='grey96', colour='black')) 
#axis.line.y.right = element_line(color = "blue4"), 
#axis.ticks.y.right = element_line(color = "blue4"))




n <- plot_grid(a, b, ncol = 1)




pdf("../results/Thesis_figures/figure_1_panel.pdf")
n
dev.off()



####################### POTENTIAL ###################
potential$X <- 1:nrow(potential)

ggplot(potential, mapping =  aes(x=NewDate, y=(standard2*50000), group = 1)) + 
  geom_line(size = 1) +
  ggtitle("Standardisation 3: Potential overlap is station with highest sharks, tags at liberty and hours in station") +
  scale_y_continuous(sec.axis = sec_axis(~. / 9, "Overlap score (between 0 and 1), Potential being location with most sharks")) +
  geom_bar(potential, mapping = aes(x=NewDate, y=IUU_events), stat="identity", colour = "black", alpha = 0.3) +
  scale_x_discrete() +
  stat_smooth(method="lm", se=TRUE, fill=NA, formula=y ~ poly(x, 2, raw=TRUE),colour="red") +
  xlab("Date") +# for the x axis label
  ylab("Number of IUU interceptions") +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1), panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


lm(standard~X, data = summary_tags)
summary(lm(potential$standard2 ~ poly(potential$X, 2, raw = TRUE)))
