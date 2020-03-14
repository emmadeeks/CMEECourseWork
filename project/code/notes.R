################################################ NOTES ##########################################
cols <- c("Code","Longitude", "Latitude")
new_acoustic <- data.table(acoustic_birthday$code, acoustic_birthday$receiver_lon, acoustic_birthday$receiver_lat)
colnames(new_acoustic) = cols #assigning column names 

collating_BPV <- data.table(BPV_birthday$Degrees , BPV_birthday$Longitude, BPV_birthday$Latitude)
colnames(collating_BPV) = cols




sep.km   <- 10      # critical separation ((km))

#Preparing data 
### Creating a data frame with the code of the tag, long and latitude from data from feburary 2016

####### Function that finds datapoints within 10km  radius of eachother 

d <- function(x){                     # distance between station[i] and all subsequent stations
  r.ft <- 6378137*3.28084             # radius of the earth, in feet
  r.km   <- r.ft*0.0003048
  if (x[1]==nrow(new_acoustic)) return()  # don't process last row
  ref <- new_acoustic[(x[1]+1):nrow(new_acoustic),]
  z <- distHaversine(ref[,2:3,with=F],x[2:3], r=r.km)
  z <- data.table(BPV_time=x[1], tag.2=ref$Code, dist=z, long.1=x[2], lat.1=x[3], long.2=ref$Longitude, lat.2=ref$Latitude)
  return(z[z$dist<sep.km,])
}


splitframe_BPV <- cbind(trying$Date, trying$Longitude.x, trying$Latitude.x)
splitframe_acoustic <- cbind(trying$Date, trying$Longitude.y, trying$Latitude.y)
cols_function <- c("Date","Longitude", "Latitude")
colnames(splitframe_BPV) = cols_function
colnames(splitframe_acoustic) = cols_function








######## THIS FUNCTION IS NOT GOING TO WORK BECAUSE YOU NO LONGER WANT YOUR 
# FUNCTION TO GO THROUGH EVERY POSSIBLE OPTION AND CALCULATE DISTANCE 
# YOU NOW WANT YOUR FUNCTION TO GO THROUGH EACH OF THE COLUMNS AND CALCULATE DISTANCE 
# THIS IS ACTUALLY A LOT SIMPLER YOU JUST NEED TO UNDERSTAND WHAT THIS FUNCTION IS ACTUALLY DOING 
# THE COMPLICATED PART OF THIS FUNCTION IS THE SUBSETTING WHICH YOU DO NOT NEED TO BE DOING 
d_temporal <- function(trying){                     # distance between station[i] and all subsequent stations
  r.ft <- 6378137*3.28084             # radius of the earth, in feet
  r.km   <- r.ft*0.0003048
  #if (x[1]==nrow(splitframe_acoustic)) return()  # don't process last row
  p <- trying$distance<-distHaversine(trying[,2:3], trying[,4:5])
  #ref <- splitframe_acoustic
  #z <- distHaversine(ref[,3:2],x[,2:3], r=r.km)
  z <- data.table(BPV_time=trying[1], dist= p, long.1=trying[2], lat.1=trying[3], long.2=trying[4], lat.2=trying[5])
  return(z[z$dist<sep.km,])
}
############### PERHAPS IT IS NOW SIMPLER TO JUST PUT IT INTO A FORLOOP #############

##### Plotting 2014 ###########

BPV_2k14 <- BPV[grep("2014", BPV$Date),] #extract one year of the data
acoustic_2k14 <- acoustic[grep("2014-", acoustic$detect_date),] # extract the same year of the data 

map_2k14 <- ggplot() + geom_polygon(data=Chagos_try, aes(x=long, y=lat, group=group), color='black', fill = NA) + geom_point(data=BPV_2k14, aes(x= Longitude, y= Latitude, size =Value),size=2, pch = 21, colour = "Light Blue", fill = "Blue", alpha=I(0.2)) +
  geom_point(data=acoustic_2k14, aes(x= receiver_lon, y= receiver_lat, size = receiver_lat),size=2, pch = 21, colour = "Deep Pink", fill = "Pink", alpha=I(0.5))

#points(x = acoustic_2k14[,7], y = acoustic_2k14[,6], col="red", cex = 1, pch = 19)

#points(x = BPV_2k14[,3], y = BPV_2k14[,2], col="blue", cex = 1, pch = 19, asp = 1)

##### Plotting 2016 ###########

BPV_2k16 <- BPV[grep("2016", BPV$Date),] #extract one year of the data
acoustic_2k16 <- acoustic[grep("2016-", acoustic$detect_date),] # extract the same year of the data 

map_2k16 <- ggplot() + geom_polygon(data=Chagos_try, aes(x=long, y=lat, group=group), color='black', fill = NA) + geom_point(data=BPV_2k16, aes(x= Longitude, y= Latitude, size =Value),size=2, pch = 21, colour = "Light Blue", fill = "Blue", alpha=I(0.2)) +
  geom_point(data=acoustic_2k16, aes(x= receiver_lon, y= receiver_lat, size = receiver_lat),size=2, pch = 21, colour = "Deep Pink", fill = "Pink", alpha=I(0.5))

########### Plotting one day in 2016 #########

BPV_birthday <- BPV[grep("16/02/2016", BPV$Date),] #extract one year of the data
acoustic_birthday <- acoustic[grep("2016-02-16", acoustic$detect_date),] # extract the same year of the data 

map_birthday <- ggplot() + geom_polygon(data=Chagos_try, aes(x=long, y=lat, group=group), color='black', fill = NA) + geom_point(data=BPV_birthday, aes(x= Longitude, y= Latitude, size =Value),size=2, pch = 21, colour = "Light Blue", fill = "Blue", alpha=I(0.2)) +
  geom_point(data=acoustic_birthday, aes(x= receiver_lon, y= receiver_lat, size = receiver_lat),size=2, pch = 21, colour = "Deep Pink", fill = "Pink", alpha=I(0.5))







comparison_temporal = do.call(rbind,apply(splitframe_BPV,1,d_temporal))

comparison_t = do.call(rbind,apply(collating_BPV,1,d))


#######################

#coloc.2 = do.call(rbind,apply(acoustic,1,d))
comparison = do.call(rbind,apply(collating_BPV,1,d))

map_overlap <- ggplot() + geom_polygon(data=Chagos_try, aes(x=long, y=lat, group=group), color='black', fill = NA) + geom_point(data=comparison, aes(x= long.1, y= lat.1),size=2, pch = 21, colour = "Light Blue", fill = "Blue", alpha=I(0.2)) +
  geom_point(data=comparison, aes(x= long.2, y= lat.2),size=2, pch = 21, colour = "Deep Pink", fill = "Pink", alpha=I(0.5))

map_islands <- ggplot() + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) + geom_point(data=comparison, aes(x= long.1, y= lat.1),size=2, pch = 21, colour = "Light Blue", fill = "Blue", alpha=I(0.2)) +
  geom_point(data=comparison, aes(x= long.2, y= lat.2),size=2, pch = 21, colour = "Pink", fill = "Deep Pink", alpha=I(0.5))

map_islands <- ggplot() + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) + geom_point(data=less_than_10, aes(x= long.1, y= lat.1),size=2, pch = 21, colour = "Light Blue", fill = "Blue", alpha=I(0.2)) +
  geom_point(data=less_than_10, aes(x= long.2, y= lat.2),size=2, pch = 21, colour = "Pink", fill = "Deep Pink", alpha=I(0.5))






####################### birthday ##################

BPV_birthday <- BPV[grep("2016-02", BPV$Date),] #extract one year of the data
acoustic_birthday <- acoustic[grep("2016-02", acoustic$detect_date),] # extract the same year of the data 

map_birthday <- ggplot() + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) + geom_point(data=BPV_birthday, aes(x= Longitude, y= Latitude, size =Value),size=2, pch = 21, colour = "Light Blue", fill = "Blue", alpha=I(0.2)) +
  geom_point(data=acoustic_birthday, aes(x= receiver_lon, y= receiver_lat, size = receiver_lat),size=2, pch = 21, colour = "Deep Pink", fill = "Pink", alpha=I(0.5))


###### Formatting data to be processed by the function 
cols <- c("Date","Longitude", "Latitude")
new_acoustic_birthday <- data.table(acoustic_birthday$detect_date, acoustic_birthday$receiver_lon, acoustic_birthday$receiver_lat)
colnames(new_acoustic_birthday) = cols #assigning column names 

new_BPV_birthday <- data.table(BPV_birthday$Date, BPV_birthday$Longitude, BPV_birthday$Latitude)
colnames(new_BPV_birthday) = cols

birthday <- merge(new_BPV_birthday, new_acoustic_birthday, by = "Date", all.x = TRUE)

######## This actually works but you just need to make it less than 10km etc
######## Also need to make it so that it is in km not feet which i think this is in 
r.ft <- 6378137*3.28084             # radius of the earth, in feet
r.km   <- r.ft*0.0003048
sep.km   <- 10
birthday$distance<-distHaversine(birthday[,2:3], birthday[,4:5], r=r.km)
less_than_10 <- birthday[birthday$distance<sep.km,]


map_islands <- ggplot() + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) + geom_point(data=less_than_10, aes(x= Longitude.x, y= Latitude.x),size=2, pch = 21, colour = "Light Blue", fill = "Blue", alpha=I(0.2)) +
  geom_point(data=less_than_10, aes(x= Longitude.y, y= Latitude.y),size=2, pch = 21, colour = "Pink", fill = "Deep Pink", alpha=I(0.5))

################# March #################

BPV_march <- BPV[grep("2016-03", BPV$Date),] #extract one year of the data
acoustic_march <- acoustic[grep("2016-03", acoustic$detect_date),] # extract the same year of the data 

map_march <- ggplot() + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) + geom_point(data=BPV_march, aes(x= Longitude, y= Latitude, size =Value),size=2, pch = 21, colour = "Light Blue", fill = "Blue", alpha=I(0.2)) +
  geom_point(data=acoustic_march, aes(x= receiver_lon, y= receiver_lat, size = receiver_lat),size=2, pch = 21, colour = "Deep Pink", fill = "Pink", alpha=I(0.5))



###### Formatting data to be processed by the function 
cols <- c("Date","Longitude", "Latitude")
new_acoustic_march <- data.table(acoustic_march$detect_date, acoustic_march$receiver_lon, acoustic_march$receiver_lat)
colnames(new_acoustic_march) = cols #assigning column names 

new_BPV_march <- data.table(BPV_march$Date, BPV_march$Longitude, BPV_march$Latitude)
colnames(new_BPV_march) = cols


#longterm <- subset(count(collating_BPV$Date, new_acoustic$Date))
march_overlap <- merge(new_BPV_march, new_acoustic_march, by = "Date", all.x = TRUE)

######## This actually works but you just need to make it less than 10km etc
######## Also need to make it so that it is in km not feet which i think this is in 
r.ft <- 6378137*3.28084             # radius of the earth, in feet
r.km   <- r.ft*0.0003048
sep.km   <- 10
march_overlap$distance<-distHaversine(march_overlap[,2:3], march_overlap[,4:5], r=r.km)
march_10_overlap <- march_overlap[march_overlap$distance<sep.km,]


march_islands <- ggplot() + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) + geom_point(data=march_10_overlap, aes(x= Longitude.x, y= Latitude.x),size=2, pch = 21, colour = "Light Blue", fill = "Blue", alpha=I(0.2)) +
  geom_point(data=march_10_overlap, aes(x= Longitude.y, y= Latitude.y),size=2, pch = 21, colour = "Pink", fill = "Deep Pink", alpha=I(0.5))


################################ April BPV ########################

BPV_april <- BPV[grep("2016-04", BPV$Date),] #extract one year of the data
acoustic_april <- acoustic[grep("2016-04", acoustic$detect_date),] # extract the same year of the data 

map_april <- ggplot() + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) + geom_point(data=BPV_april, aes(x= Longitude, y= Latitude, size =Value),size=2, pch = 21, colour = "Light Blue", fill = "Blue", alpha=I(0.2)) +
  geom_point(data=acoustic_april, aes(x= receiver_lon, y= receiver_lat, size = receiver_lat),size=2, pch = 21, colour = "Deep Pink", fill = "Pink", alpha=I(0.5))



###### Formatting data to be processed by the function 
cols <- c("Date","Longitude", "Latitude")
new_acoustic_april <- data.table(acoustic_april$detect_date, acoustic_april$receiver_lon, acoustic_april$receiver_lat)
colnames(new_acoustic_april) = cols #assigning column names 

new_BPV_april <- data.table(BPV_april$Date, BPV_april$Longitude, BPV_april$Latitude)
colnames(new_BPV_april) = cols


#longterm <- subset(count(collating_BPV$Date, new_acoustic$Date))
april_overlap <- merge(new_BPV_april, new_acoustic_april, by = "Date", all.x = TRUE)

######## This actually works but you just need to make it less than 10km etc
######## Also need to make it so that it is in km not feet which i think this is in 
r.ft <- 6378137*3.28084             # radius of the earth, in feet
r.km   <- r.ft*0.0003048
sep.km   <- 10
april_overlap$distance<-distHaversine(april_overlap[,2:3], april_overlap[,4:5], r=r.km)
april_10_overlap <- april_overlap[april_overlap$distance<sep.km,]


april_islands <- ggplot() + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) + geom_point(data=april_10_overlap, aes(x= Longitude.x, y= Latitude.x),size=2, pch = 21, colour = "Light Blue", fill = "Blue", alpha=I(0.2)) +
  geom_point(data=april_10_overlap, aes(x= Longitude.y, y= Latitude.y),size=2, pch = 21, colour = "Pink", fill = "Deep Pink", alpha=I(0.5))

write.csv(april_10_overlap, "april_overlap.csv")

############################### August ########################

BPV_august <- new_BPV_2[grep("2017-06", BPV$Date),] #extract one year of the data
acoustic_august <- acoustic[grep("2017-06", acoustic$detect_date),] # extract the same year of the data 

map_august <- ggplot() + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) + geom_point(data=BPV_august, aes(x= V3, y= V4, size =Value),size=2, pch = 21, colour = "Light Blue", fill = "Blue", alpha=I(0.2)) +
  geom_point(data=acoustic_august, aes(x= receiver_lon, y= receiver_lat, size = receiver_lat),size=2, pch = 21, colour = "Deep Pink", fill = "Pink", alpha=I(0.5))

pdf("trying.pdf")
plot(map_august)
dev.off()

###### Formatting data to be processed by the function 
cols <- c("Date","Longitude", "Latitude")
new_acoustic_august <- data.table(acoustic_august$detect_date, acoustic_august$receiver_lon, acoustic_august$receiver_lat)
colnames(new_acoustic_august) = cols #assigning column names 

new_BPV_august <- data.table(BPV_august$Date, BPV_august$Longitude, BPV_august$Latitude)
colnames(new_BPV_august) = cols


#longterm <- subset(count(collating_BPV$Date, new_acoustic$Date))
august_overlap <- merge(new_BPV_august, new_acoustic_august, by = "Date", all.x = TRUE)

######## This actually works but you just need to make it less than 10km etc
######## Also need to make it so that it is in km not feet which i think this is in 
r.ft <- 6378137*3.28084             # radius of the earth, in feet
r.km   <- r.ft*0.0003048
sep.km   <- 10
august_overlap$distance<-distHaversine(august_overlap[,2:3], august_overlap[,4:5], r=r.km)
august_10_overlap <- august_overlap[august_overlap$distance<sep.km,]


august_islands <- ggplot() + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) + geom_point(data=august_10_overlap, aes(x= Longitude.x, y= Latitude.x),size=2, pch = 21, colour = "Light Blue", fill = "Blue", alpha=I(0.2)) +
  geom_point(data=august_10_overlap, aes(x= Longitude.y, y= Latitude.y),size=2, pch = 21, colour = "Pink", fill = "Deep Pink", alpha=I(0.5))






##################### HEAT MAPS ##############
april_islands <- ggplot(data=april_10_overlap, aes(x= Longitude.x, y= Latitude.x)) + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) +
  geom_hex(bins = 70) +
  scale_fill_continuous(type = "viridis") +
  theme_bw()


april_islands <- ggplot() + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) + geom_point(data=april_10_overlap, aes(x= Longitude.x, y= Latitude.x),size=2, pch = 21, colour = "Light Blue", fill = "Blue", alpha=I(0.2)) +
  geom_raster(aes(fill = density)) +
  scale_fill_gradientn(colours = terrain.colors(10))

april_islands <- ggplot(data=april_10_overlap, aes(x= Longitude.x, y= Latitude.x)) + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) +
  geom_hex(bins = 70) +
  scale_colour_gradientn(colours = terrain.colors(10)) +
  theme_bw()


########################################## YEAR 2016 ################################


BPV_year <- BPV[grep("2016-", BPV$Date),] #extract one year of the data
acoustic_year <- acoustic[grep("2016-", acoustic$detect_date),] # extract the same year of the data 

map_year <- ggplot() + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) + geom_point(data=BPV_year, aes(x= Longitude, y= Latitude, size =Value),size=2, pch = 21, colour = "Light Blue", fill = "Blue", alpha=I(0.2)) +
  geom_point(data=acoustic_year, aes(x= receiver_lon, y= receiver_lat, size = receiver_lat),size=2, pch = 21, colour = "Deep Pink", fill = "Pink", alpha=I(0.5))



###### Formatting data to be processed by the function 
cols <- c("Date","Longitude", "Latitude", "Code")
new_acoustic_year <- data.table(acoustic_year$detect_date, acoustic_year$receiver_lon, acoustic_year$receiver_lat, acoustic_year$code)
colnames(new_acoustic_year) = cols #assigning column names 

cols_BPV <- c("Date","Longitude", "Latitude")
new_BPV_year <- data.table(BPV_year$Date, BPV_year$Longitude, BPV_year$Latitude)
colnames(new_BPV_year) = cols_BPV


#longterm <- subset(count(collating_BPV$Date, new_acoustic$Date))
year_overlap <- merge(new_BPV_year, new_acoustic_year, by = "Date", all.x = TRUE)
cols_10 <- c("Date","Longitude_BPV", "Latitude_BPV", "Longitude_acoustic", "Latitude_acoustic", "Code")
colnames(year_overlap) = cols_10

######## This actually works but you just need to make it less than 10km etc
######## Also need to make it so that it is in km not feet which i think this is in 
r.ft <- 6378137*3.28084             # radius of the earth, in feet
r.km   <- r.ft*0.0003048
sep.km   <- 10
year_overlap$distance<-distHaversine(year_overlap[,2:3], year_overlap[,4:5], r=r.km)
year_10_overlap <- year_overlap[year_overlap$distance<sep.km,]


year_islands <- ggplot() + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) + geom_point(data=year_10_overlap, aes(x= Longitude.x, y= Latitude.x),size=2, pch = 21, colour = "Light Blue", fill = "Blue", alpha=I(0.2)) +
  geom_point(data=year_10_overlap, aes(x= Longitude.y, y= Latitude.y),size=2, pch = 21, colour = "Pink", fill = "Deep Pink", alpha=I(0.5))

year_islands <- ggplot(data=year_10_overlap, aes(x= Longitude.x, y= Latitude.x)) + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) +
  geom_hex(bins = 30) +
  scale_fill_continuous(type = "viridis") +
  theme_bw()


year_islands <- ggplot(data=year_10_overlap, aes(x= Longitude_BPV, y= Latitude_BPV)) + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) +
  geom_hex(bins = 30) +
  scale_fill_continuous(type = "viridis") +
  theme_bw()

# Trying to create a forloop to iterate 
year_10_overlap$NewDate <- substr(year_10_overlap$Date, 0, 7)

nestmonths <- year_10_overlap %>%
  nest(data= -NewDate) #this is experimenting with nesting, by nesting the data i can suset the day by ID and go into that

pdf("myOut.pdf")
summary_sharks = as.data.frame(matrix(nrow = 1, ncol = 3))
for (i in 1:length(nestmonths$NewDate)){
  monthdata <- nestmonths$data[[i]]
  month <- nestmonths$NewDate[[i]]
  rows <- nrow(monthdata)
  no_sharks <- unique(monthdata$Code)
  no_sharks <- length(no_sharks)
  year_islands_i <- ggplot(data=monthdata, aes(x= Longitude_BPV, y= Latitude_BPV)) + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) +
    geom_hex(bins = 50) +
    ggtitle(month) +
    scale_fill_continuous(type = "viridis") +
    theme_bw()
  toadd <- c(month, rows, no_sharks)
  summary_sharks <- rbind(summary_sharks, toadd)
  plot(year_islands_i)
}
dev.off()

geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) + geom_point()          

map_april + geom_density_2d() + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA)

##################


ggplot(datasaurus_dozen, aes(x=x, y=y))+
  geom_point()+
  theme_minimal() +
  transition_states(dataset, 3, 1) + 
  ease_aes('cubic-in-out')



p <- ggplot(datasaurus_dozen, aes(x=x,y=y)) +
  geom_point() +
  theme_minimal() +
  transition_states(dataset,3,1) + 
  ease_aes() 

animate(p, duration = 5, fps = 20, width = 200, height = 200, renderer = gifski_renderer())
anim_save("output.gif")
anim_save("myfilename.gif",p)


################## COMPARISON OF SHIP MOVEMENTS, THIS DOESNT REALLY NEED TO HAPPEN AS A FORLOOP BECAUSE WE JUST 
###### CANT REALLY GET MUCH FROM IT 
pdf("all_out_acoustic_LOG.pdf")
for (i in 1:length(all_nestmonths$NewDate)){
  monthdata <- all_nestmonths$data[[i]]
  month <- all_nestmonths$NewDate[[i]]
  
  map_month_i <- ggplot(BPV_april, aes(x= Longitude, y= Latitude)) + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) + xlim(70.7, 73) +
    ylim(-8, -4.5) + geom_hex(data = acoustic_april, aes(x= receiver_lon, y= receiver_lat, fill = stat(log(count)))) + scale_fill_continuous(type = "viridis")
  
  map_month_i_new <- map_april + geom_density_2d(aes(color = ..level..)) 
  
  plot(map_month_i_new)
}
dev.off()

############### Exoeriemnting with just one month for the sake for animations 

year_islands_16 <- ggplot(data=ex, aes(x= Longitude_acoustic, y= Latitude_acoustic)) + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) +
  geom_hex(aes(x=Longitude_acoustic,y=Latitude_acoustic)) +
  scale_fill_gradient2(limits = c(0, 4500), oob = scales::squish) +
  #scale_fill_continuous(type = "viridis") +
  theme_bw()

year_islands_16 <- ggplot(data=ex, aes(x= Longitude_acoustic, y= Latitude_acoustic)) + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) +
  geom_hex(aes(x=Longitude_acoustic,y=Latitude_acoustic)) +
  scale_fill_continuous(type = "viridis", limits = c(0, 3000), oob = scales::squish)
#scale_fill_continuous(type = "viridis") +


year_islands_16
year_islands_16 + geom_density_2d()
##############################


######################## USING STAT_DENSITY FOR ANIMATIONS ########

################################ April BPV ########################

BPV_april <- BPV[grep("2016-04", BPV$Date),] #extract one year of the data
acoustic_april <- acoustic[grep("2016-04", acoustic$detect_date),] # extract the same year of the data 

map_april <- ggplot() + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) +
  geom_point(data=acoustic_april, aes(x= receiver_lon, y= receiver_lat, size = receiver_lat),size=2, pch = 21, colour = "Deep Pink", fill = "Pink", alpha=I(0.5)) + geom_density_2d()

map_april <- ggplot(data=acoustic_april, aes(x= receiver_lon, y= receiver_lat)) + geom_point() +
  xlim(70, 73) +
  ylim(-9, -4)

map_april + geom_density_2d() + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA)


############## THIS FORMAT WORKS ##################

map_april <- ggplot(BPV_april, aes(x= Longitude, y= Latitude)) + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) + xlim(70.7, 73) +
  ylim(-8, -4.5) + geom_hex(data = acoustic_april, aes(x= receiver_lon, y= receiver_lat, fill = stat(log(count)))) + scale_fill_continuous(type = "viridis")

map_april + geom_density_2d(aes(color = ..level..)) 

############## ABOVE WORKS DO NOT CHANGE ########## also works when you just add density to end 
map_april <- ggplot(BPV_april, aes(x= Longitude, y= Latitude)) + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) + xlim(70.7, 73) +
  ylim(-8, -4.5) + geom_hex(data = acoustic_april, aes(x= receiver_lon, y= receiver_lat, fill = stat(log(count)))) + scale_fill_continuous(type = "viridis") + geom_density_2d(aes(color = ..level..)) 

map_april + geom_density_2d(aes(color = ..level..)) 


############## ANIMATING ####################

map_april <- ggplot(BPV_april, aes(x= Longitude, y= Latitude)) + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) + xlim(70.7, 73) +
  ylim(-8, -4.5) + geom_density_2d(aes(color = ..level..)) 

p <- ggplot(BPV_april, aes(x= Longitude, y= Latitude)) + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) + xlim(70.7, 73) +
  ylim(-8, -4.5) +
  geom_point()+
  theme_minimal() +
  transition_states(Date, 3, 1) + 
  ease_aes() 

animate(p, duration = 5, fps = 20, width = 200, height = 200, renderer = gifski_renderer())
anim_save("myfilename.gif",p)

p <- ggplot(BPV_april, aes(x= Longitude, y= Latitude)) + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) + xlim(70.7, 73) +
  ylim(-8, -4.5) + geom_hex(data = acoustic_april, aes(x= receiver_lon, y= receiver_lat, fill = stat(log(count)))) + scale_fill_continuous(type = "viridis") +
  geom_point()+
  theme_minimal() +
  transition_states(Date, 3, 1) + 
  ease_aes() 



march_islands <- ggplot(data=march_10_overlap, aes(x= Longitude.y, y= Latitude.y)) +
  geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) + 
  geom_hex(fill = stat(log(count))) + scale_fill_continuous(type = "viridis")

p <- ggplot(BPV_april, aes(x= Longitude, y= Latitude)) + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) + xlim(70.7, 73) +
  ylim(-8, -4.5) + geom_hex(data = acoustic_april, aes(x= receiver_lon, y= receiver_lat, fill = stat(log(count)))) + scale_fill_continuous(type = "viridis") +
  geom_point()+
  theme_minimal() +
  transition_states(Date, 3, 1) + 
  ease_aes()                                                                                                                                                                                              geom_hex(data = acoustic_april, aes(x= receiver_lon, y= receiver_lat, fill = stat(log(count)))) + scale_fill_continuous(type = "viridis")

year_islands <- ggplot(data=march_10_overlap, aes(x= Longitude.y, y= Latitude.y)) + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) +
  geom_hex(data = march_10_overlap, aes(x=Longitude.y, y=Latitude.y)) +
  scale_fill_continuous(type = "viridis", fill = stat(log(count))) +
  theme_bw()


map_april <- ggplot(data=BPV_april, aes(x= Longitude, y= Latitude)) + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA)
map_april + geom_density_2d() 


map_april <- ggplot() + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) + geom_point(data=BPV_april, aes(x= Longitude, y= Latitude, size =Value),size=2, pch = 21, colour = "Light Blue", fill = "Blue", alpha=I(0.2)) +
  geom_point(data=acoustic_april, aes(x= receiver_lon, y= receiver_lat, size = receiver_lat),size=2, pch = 21, colour = "Deep Pink", fill = "Pink", alpha=I(0.5))




geom_point(data=acoustic_april, aes(x= receiver_lon, y= receiver_lat, size = receiver_lat),size=2, pch = 21, colour = "Deep Pink", fill = "Pink", alpha=I(0.5))


map_april <- ggplot(BPV_april, aes(x= Longitude, y= Latitude)) + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) + xlim(70.7, 73) +
  ylim(-8, -4.5) + geom_hex(data = acoustic_april, aes(x= receiver_lon, y= receiver_lat, fill = stat(log(count))))
#+ geom_point(data=acoustic_april, aes(x= receiver_lon, y= receiver_lat))

#+ geom_hex(aes(fill = stat(log(count)))) +
#scale_fill_continuous(type = "viridis")




map_april + geom_density_2d(aes(color = ..level..)) 


map_april <- ggplot(acoustic_april, aes(x= receiver_lon, y= receiver_lat)) + geom_polygon(data=Chagos_island, aes(x=long, y=lat, group=group), color='black', fill = NA) + xlim(70.7, 73) +
  ylim(-8, -4.5) + ggplot(data= BPV_april, aes(x= Longitude, y= Latitude)) +
  geom_hex(aes(fill = stat(log(count)))) +
  scale_fill_continuous(type = "viridis")

map_april + geom_point(data= BPV_april, aes(x= Longitude, y= Latitude, size = receiver_lat),size=2, pch = 21, colour = "Deep Pink", fill = "Pink", alpha=I(0.5)) + stat_density2d()

install.packages('gganimate')
library(datasauRus)
library(ggplot2)
library(gganimate)
library(gapminder)
library(gganimate)
library(gifski)

numbersgif <- c(1:346)
BPV_april <- cbind(BPV_april, numbersgif)
p <- ggplot(data= BPV_april, aes(x= Longitude, y= Latitude))+
  geom_point()+
  theme_minimal() +
  transition_states(Date, 3, 1) + 
  ease_aes() 

animate(p, duration = 5, fps = 20, width = 200, height = 200, renderer = gifski_renderer())
anim_save("myfilename.gif",p)

