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


