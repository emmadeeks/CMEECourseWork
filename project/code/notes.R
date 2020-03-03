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

