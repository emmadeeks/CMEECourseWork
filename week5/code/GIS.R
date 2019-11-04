rm(list=ls()) #Clear global environment 
install.packages('raster') # Core raster GIS data package
install.packages('sf') # Core vector GIS data package
install.packages('rgeos') # Extends vector data functionality
install.packages('lwgeom') # Extends vector data functionality
install.packages('viridis') # Because we like the colour scheme!
install.packages('rgdal')

library(raster) #Require these packages 
library(sf)     #Require 
library(viridis)
library('units')
library('rgdal')

#Make a dataframe called population density with two columns named 
#n_km2 and country 
pop_dens <- data.frame(n_km2 = c(260, 67,151, 4500, 133), 
                       country = c('England','Scotland', 'Wales', 'London', 'Northern Ireland'))
print(pop_dens)

#This makes 4 countries into seperate vectors and the coordinates which will later be used 
#to make polygons that can then be concatenated together 
scotland <- rbind(c(-5, 58.6), c(-3, 58.6), c(-4, 57.6), 
                  c(-1.5, 57.6), c(-2, 55.8), c(-3, 55), 
                  c(-5, 55), c(-6, 56), c(-5, 58.6))
england <- rbind(c(-2,55.8),c(0.5, 52.8), c(1.6, 52.8), 
                 c(0.7, 50.7), c(-5.7,50), c(-2.7, 51.5), 
                 c(-3, 53.4),c(-3, 55), c(-2,55.8))
wales <- rbind(c(-2.5, 51.3), c(-5.3,51.8), c(-4.5, 53.4),
               c(-2.8, 53.4),  c(-2.5, 51.3))
ireland <- rbind(c(-10,51.5), c(-10, 54.2), c(-7.5, 55.3),
                 c(-5.9, 55.3), c(-5.9, 52.2), c(-10,51.5))

#This makes the variable into a polygon 
scotland <- st_polygon(list(scotland))
england <- st_polygon(list(england))
wales <- st_polygon(list(wales))
ireland <- st_polygon(list(ireland))

#Combine geometries into a simple feature column 
uk_eire <- st_sfc(wales, england, scotland, ireland, crs=4326)
#This is what then plots the polygons and makes it into a figure of polygons 
plot(uk_eire, asp=1)

#This creates point locations for capitals 
#Creates a dataframe with the longitudes and latitudes of capital cities with their names 
uk_eire_capitals <- data.frame(long= c(-0.1, -3.2, -3.2, -6.0, -6.25),
                               lat=c(51.5, 51.5, 55.8, 54.6, 53.30),
                               name=c('London', 'Cardiff', 'Edinburgh', 'Belfast', 'Dublin'))
#This plots the coordinates of the capitals into a dataframe 
uk_eire_capitals <- st_as_sf(uk_eire_capitals, coords=c('long','lat'), crs=4326)

#Plotting key locations 
#Uses buffer to create a polygon for london 
st_pauls <- st_point(x=c(-0.098056, 51.513611))
london <- st_buffer(st_pauls, 0.25)

#Remove london from the englad polygon so that we can set different population
#densities fo rthe regions using the DIFFERENCE operation 
england_no_london <- st_difference(england, london)

# Count the points and show the number of rings within the polygon features
lengths
#Feature has a different structure and consists of two rings
#One ring for the 18 points along the xternal border and a second ring of 242 points 
#for the internal hole 
#the other un-holet polygons oly contain a single ring for  their external border
lengths(england_no_london)

#Uses same operation to tidy up wales- want bits of wales that are different from england 
wales <- st_difference(wales, england)

#Use intersection operation to seperate Northern Ireland from the island of Ireland 
#Create a rough polygon that includes Northern Ireland and sticks out into the sea 
#We then find the intersection and difference of that with the ireland polyhon to get Northern Ireland and Eire 

#Start by aking a rough polygon that includes Northenr Ireland and surrounding sea 
#Note that this is the alternative way of providing the coordinates 

ni_area <- st_polygon(list(cbind(x=c(-8.1, -6, -5, -6, -8.1), y=c(54.4, 56, 55, 54, 54.4))))
northern_ireland <- st_intersection(ireland, ni_area)
eire <- st_difference(ireland, ni_area)

#Combine the final geometries 
uk_eire <- st_sfc(wales, england_no_london, scotland, london, northern_ireland, eire, crs=4326)
plot(uk_eire, asp =1)

#3.3 Features and geometries 
#The uk_entire object now contains 6 features: note that a feature is a set of vector GIS 
#Geomtries that represent a spatial unit we are interested in 
#Each of the 6 features is a polygon, england has a hole in it

#Use the union operation to create a single feature that contains all of those 
#geomteitries in one multipolygon geometry 

#Make the UK ito a single feature 
uk_country <- st_union(uk_eire[-6])
#compare six polygon features with one mltipolygon feature 
print(uk_eire)
print(uk_country)

#Plot them 
par(mfrow=c(1, 2), mar=c(3,3,1,1))
plot(uk_eire, asp=1, col=rainbow(6))
plot(st_geometry(uk_eire_capitals), add=TRUE)
plot(uk_country, asp=1, col='lightblue')

#3.4 Vecotr data and attributes 

#So far we just have vector geometries but GIS data is about pairing sptial features 
#with data about those features often called attributes or properties 
#The sf package introducte the sf object type: 
#Normal data frame with an additional simple feature column 
uk_eire <- st_sf(name=c('Wales', 'England','Scotland', 'London', 
                        'Northern Ireland', 'Eire'),
                 geometry=uk_eire)

plot(uk_eire, asp=1)

#As sf is a dataframe we can add attributes by adding fields directly 
uk_eire$capital <- c('London', 'Edinburgh','Cardiff', NA, 'Belfast','Dublin')

#Can also try the merge command to merge data in- need to use by.x and by.y 
#to say which columns to expect to match 
#We can also use all.x=TRUE otherwise Eire will be dropped from the spatial data 
#because it has no population density estimate in the data frame
#If we look at the result, we get some header information about the spatial data and then soemthing 
#that looks very like a datafeame printout with the extra geometry column 

uk_eire <- merge(uk_eire, pop_dens, by.x='name', by.y='country', all.x=TRUE)
print(uk_eire)

#3.5 Spatial attributes 

uk_eire_centroids <- st_centroid(uk_eire)
## Warning in st_centroid.sf(uk_eire): st_centroid assumes attributes are
## constant over geometries of x
## Warning in st_centroid.sfc(st_geometry(x), of_largest_polygon =
## of_largest_polygon): st_centroid does not give correct centroids for
## longitude/latitude data

st_coordinates(uk_eire_centroids)

uk_eire$area <- st_area(uk_eire)
# The length of a polygon is the perimeter length 
#- note that this includes the length of internal holes.
uk_eire$length <- st_length(uk_eire)
#Look at the result 
print(uk_eire)

#You can change units in a neat way 
uk_eire$area <- set_units(uk_eire$area, 'km^2')
uk_eire$length <- set_units(uk_eire$length, 'km')
# And which won't let you make silly error like turning a length into weight
uk_eire$area <- set_units(uk_eire$area, 'kg')

# Or you can simply convert the `units` version to simple numbers

uk_eire$length <- as.numeric(uk_eire$length)
print(uk_eire)

#Another useful function is the distance between objects: sf gives us the closest 
#distance between geometries, which might be zero if two features are touching 

st_distance(uk_eire)
st_distance(uk_eire_centroids)

#3.6 plotting sf objects
plot(uk_eire['n_km2'], asp=1)

#3.7 Reprojecting vector data 

#Reproject UK and Eire map onto a good choice of local projected coordinate system
#The British National Grid
#Also use a bad choice : the UTM 50N projection, which is apprirate for Borneo 

# British National Grid (EPSG:27700)
uk_eire_BNG <- st_transform(uk_eire, 27700)
# The bounding box of the data shows the change in units
st_bbox(uk_eire)

st_bbox(uk_eire_BNG)
# UTM50N (EPSG:32650)
uk_eire_UTM50N <- st_transform(uk_eire, 32650)
# Plot the results
par(mfrow=c(1, 3), mar=c(3,3,1,1))
plot(st_geometry(uk_eire), asp=1, axes=TRUE, main='WGS 84')
plot(st_geometry(uk_eire_BNG), axes=TRUE, main='OSGB 1936 / BNG')
plot(st_geometry(uk_eire_UTM50N), axes=TRUE, main='UTM 50N')


#3.7.1 Proj4 strings 
# The EPSG ID codes saves us from proj4 strings; these texxt strings
#contain often long and cnfusing sets of options and parameters 
#that actually define a particularl projection 

#3.7.2 Degrees are not constant 
# Units of geographic coordinate systems are anles of lat and longitude 
# These are not constant units of distance and as lnes od longitude converfe towards to pole, the physical length ofa degree decreases. this is why our 0.25 buffered 
#point for london is stupid and why it is distorted in the projected data 

# Set up some points separated by 1 degree latitude and longitude from St. Pauls
st_pauls <- st_sfc(st_pauls, crs=4326)
one_deg_west_pt <- st_sfc(st_pauls - c(1, 0), crs=4326) # near Goring
one_deg_north_pt <-  st_sfc(st_pauls + c(0, 1), crs=4326) # near Peterborough
# Calculate the distance between St Pauls and each point
st_distance(st_pauls, one_deg_west_pt)

st_distance(st_pauls, one_deg_north_pt)

st_distance(st_transform(st_pauls, 27700), st_transform(one_deg_west_pt, 27700))

#4 Rasters 
#Rasters are the other major type of spatial data. 
#Consist of a regular grid in space defined by a coordinate system
#, an origin point, a resolution and a number of rows and columns. 
# Effectively hold a matrix of data- raster package 

# Create an empty raster object covering UK and Eire
uk_raster_WGS84 <- raster(xmn=-11,  xmx=2,  ymn=49.5, ymx=59, 
                          res=0.5, crs="+init=EPSG:4326")
hasValues(uk_raster_WGS84)

# Add data to the raster: just the number 1 to number of cells
values(uk_raster_WGS84) <- seq(length(uk_raster_WGS84))

plot(uk_raster_WGS84)
plot(st_geometry(uk_eire), add=TRUE, border='black', lwd=2, col='#FFFFFF44')

#4.2 changing raster resolution- more resolute or not- involves aggregating or dispersing data 
# Define a simple 4 x 4 square raster
m <- matrix(c(1, 1, 3, 3,
              1, 2, 4, 3,
              5, 5, 7, 8,
              6, 6, 7, 7), ncol=4, byrow=TRUE)
square <- raster(m)

#4.2.1 Aggregating rasters
# Average values
square_agg_mean <- aggregate(square, fact=2, fun=mean)
values(square_agg_mean)

# Maximum values
square_agg_max <- aggregate(square, fact=2, fun=max)
values(square_agg_max)

# Modal values for categories
square_agg_modal <- aggregate(square, fact=2, fun=modal)
values(square_agg_modal)

#4.2.2. Disaggregating rasters 
# Copy parents
square_disagg <- disaggregate(square, fact=2)
# Interpolate
square_disagg_interp <- disaggregate(square, fact=2, method='bilinear')

#4.3 Reprojecting a raster 
# make two simple `sfc` objects containing points in  the
# lower left and top right of the two grids
uk_pts_WGS84 <- st_sfc(st_point(c(-11, 49.5)), st_point(c(2, 59)), crs=4326)
uk_pts_BNG <- st_sfc(st_point(c(-2e5, 0)), st_point(c(7e5, 1e6)), crs=27700)

#  Use st_make_grid to quickly create a polygon grid with the right cellsize
uk_grid_WGS84 <- st_make_grid(uk_pts_WGS84, cellsize=0.5)
uk_grid_BNG <- st_make_grid(uk_pts_BNG, cellsize=1e5)

# Reproject BNG grid into WGS84
uk_grid_BNG_as_WGS84 <- st_transform(uk_grid_BNG, 4326)

# Plot the features
plot(uk_grid_WGS84, asp=1, border='grey', xlim=c(-13,4))
plot(st_geometry(uk_eire), add=TRUE, border='darkgreen', lwd=2)
plot(uk_grid_BNG_as_WGS84, border='red', add=TRUE)

# Create the target raster
uk_raster_BNG <- raster(xmn=-200000, xmx=700000, ymn=0, ymx=1000000,
                        res=100000, crs='+init=EPSG:27700')
uk_raster_BNG_interp <- projectRaster(uk_raster_WGS84, uk_raster_BNG, method='bilinear')
uk_raster_BNG_ngb <- projectRaster(uk_raster_WGS84, uk_raster_BNG, method='ngb')
# compare the values in the top row
round(values(uk_raster_BNG_interp)[1:9], 2)

values(uk_raster_BNG_ngb)[1:9]

par(mfrow=c(1,3), mar=c(1,1,2,1))
plot(uk_raster_BNG_interp, main='Interpolated', axes=FALSE, legend=FALSE)
plot(uk_raster_BNG_ngb, main='Nearest Neighbour',axes=FALSE, legend=FALSE)


#5.1 vector to raster 
# Create the target raster 
uk_20km <- raster(xmn=-200000, xmx=650000, ymn=0, ymx=1000000, 
                  res=20000, crs='+init=EPSG:27700')

# Rasterizing polygons
uk_eire_poly_20km  <- rasterize(as(uk_eire_BNG, 'Spatial'), uk_20km, field='name')

# Rasterizing lines
uk_eire_BNG_line <- st_cast(uk_eire_BNG, 'LINESTRING')

st_agr(uk_eire_BNG) <- 'constant'

# Rasterizing lines
uk_eire_BNG_line <- st_cast(uk_eire_BNG, 'LINESTRING')
uk_eire_line_20km <- rasterize(as(uk_eire_BNG_line, 'Spatial'), uk_20km, field='name')

# Rasterizing points 
# - This isn't quite as neat. You need to take two steps in the cast and need to convert 
#   the name factor to numeric.
uk_eire_BNG_point <- st_cast(st_cast(uk_eire_BNG, 'MULTIPOINT'), 'POINT')
uk_eire_BNG_point$name <- as.numeric(uk_eire_BNG_point$name)
uk_eire_point_20km <- rasterize(as(uk_eire_BNG_point, 'Spatial'), uk_20km, field='name')

# Plotting those different outcomes
par(mfrow=c(1,3), mar=c(1,1,1,1))
plot(uk_eire_poly_20km, col=viridis(6, alpha=0.5), legend=FALSE, axes=FALSE)
plot(st_geometry(uk_eire_BNG), add=TRUE, border='grey')

plot(uk_eire_line_20km, col=viridis(6, alpha=0.5), legend=FALSE, axes=FALSE)
plot(st_geometry(uk_eire_BNG), add=TRUE, border='grey')

plot(uk_eire_point_20km, col=viridis(6, alpha=0.5), legend=FALSE, axes=FALSE)
plot(st_geometry(uk_eire_BNG), add=TRUE, border='grey')

#Raster to vector 
# rasterToPolygons returns a polygon for each cell and returns a Spatial object
poly_from_rast <- rasterToPolygons(uk_eire_poly_20km)
poly_from_rast <- as(poly_from_rast, 'sf')

# but can be set to dissolve the boundaries between cells with identical values
poly_from_rast_dissolve <- rasterToPolygons(uk_eire_poly_20km, dissolve=TRUE)
poly_from_rast_dissolve <- as(poly_from_rast_dissolve, 'sf')

# rasterToPoints returns a matrix of coordinates and values.
points_from_rast <- rasterToPoints(uk_eire_poly_20km)
points_from_rast <- st_as_sf(data.frame(points_from_rast), coords=c('x','y'))

# Plot the outputs - using key.pos=NULL to suppress the key and
# reset=FALSE to avoid plot.sf altering the par() options
par(mfrow=c(1,3), mar=c(1,1,1,1))
plot(poly_from_rast['layer'], key.pos = NULL, reset = FALSE)
plot(poly_from_rast_dissolve, key.pos = NULL, reset = FALSE)
plot(points_from_rast, key.pos = NULL, reset = FALSE)

#6 Using data in files 
#6.1 Saving vector data 

st_write(uk_eire, 'data-5/uk_eire_WGS84.shp')
## Writing layer `uk_eire_WGS84' to data source `data/uk_eire_WGS84.shp' using driver `ESRI Shapefile'
## Writing 6 features with 5 fields and geometry type Polygon.

st_write(uk_eire_BNG, 'data-5/uk_eire_BNG.shp')
## Writing layer `uk_eire_BNG' to data source `data/uk_eire_BNG.shp' using driver `ESRI Shapefile'
## Writing 6 features with 5 fields and geometry type Polygon.

st_write(uk_eire, 'data-5/uk_eire_WGS84.geojson')
## Writing layer `uk_eire_WGS84' to data source `data/uk_eire_WGS84.geojson' using driver `GeoJSON'
## Writing 6 features with 5 fields and geometry type Polygon.

st_write(uk_eire, 'data-5/uk_eire_WGS84.gpkg')
## Updating layer `uk_eire_WGS84' to data source `data/uk_eire_WGS84.gpkg' using driver `GPKG'
## Writing 6 features with 5 fields and geometry type Polygon.
st_write(uk_eire, 'data-5/uk_eire_WGS84.json', driver='GeoJSON')
## Writing layer `uk_eire_WGS84' to data source `data/uk_eire_WGS84.json' using driver `GeoJSON'
## Writing 6 features with 5 fields and geometry type Polygon.

# Save a GeoTiff
writeRaster(uk_raster_BNG_interp, 'data-5/uk_raster_BNG_interp.tif')
# Save an ASCII format file: human readable text. 
# Note that this format does not contain the projection details!
writeRaster(uk_raster_BNG_ngb, 'data-5/uk_raster_BNG_ngb.asc', format='ascii')

#6.3 Loading vector data 
# Load a vector shapefile
ne_110 <- st_read('data-5/ne_110m_admin_0_countries/ne_110m_admin_0_countries.shp')

life_exp <- read.csv(file = "data-5/WHOSIS_000001.csv")

# Read in Southern Ocean example data
so_data <- read.csv('data-5/Southern_Ocean.csv', header=TRUE)
head(so_data)

so_data <- st_as_sf(so_data, coords=c('long', 'lat'), crs=4326)
head(so_data)

etopo_25 <- raster('data-5/etopo_25.tif')
print(etopo_25)

# Download bioclim data: global maximum temperature at 10 arc minute resolution
tmax <- getData('worldclim', download=TRUE, path='data-5', var='tmax', res=10)
# The data has 12 layers ...
print(tmax)

dir('data-5/wc10')

# scale the data
tmax <- tmax / 10
# Extract  January and July data and the annual maximum by location.
tmax_jan <- tmax[[1]]
tmax_jul <- tmax[[7]]
tmax_max <- max(tmax)
# Plot those maps
par(mfrow=c(3,1), mar=c(2,2,1,1))
bks <- seq(-500, 500, length=101)
pal <- colorRampPalette(c('lightblue','grey', 'firebrick'))
cols <- pal(100)
ax.args <- list(at= seq(-500, 500, by=100))
plot(tmax_jan, col=cols, breaks=bks, axis.args=ax.args, main='January maximum temperature')
plot(tmax_jul, col=cols, breaks=bks, axis.args=ax.args, main='July maximum temperature')
plot(tmax_max, col=cols, breaks=bks, , axis.args=ax.args, main='Annual maximum temperature')


#Overlying raster and vector data 
#Cropping data 

so_extent <- extent(-60, -20, -65, -45)
# The crop function for raster data...
so_topo <- crop(etopo_25, so_extent)
# ... and the st_crop function to reduce some higher resolution coastline data
ne_10 <- st_read('data-5/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp')
st_agr(ne_10) <- 'constant'
so_ne_10 <- st_crop(ne_10, so_extent)

#Spatial joins and raster data extraction 
#Spatial joining 

# extract Africa from the ne_110 data and keep the columns we want to use
africa <- subset(ne_110, CONTINENT=='Africa', select=c('ADMIN', 'POP_EST'))

# transform to the Robinson projection
africa <- st_transform(africa, crs=54030)
# create a random sample of points
mosquito_points <- st_sample(africa, 1000)

# Create the plot
plot(st_geometry(africa), col='khaki')
plot(mosquito_points, col='firebrick', add=TRUE)
mosquito_points <- st_sf(mosquito_points)
mosquito_points <- st_join(mosquito_points, africa['ADMIN'])

plot(st_geometry(africa), col='khaki')
plot(mosquito_points['ADMIN'], add=TRUE)

mosquito_points_agg <- aggregate(mosquito_points, by=list(country=mosquito_points$ADMIN), FUN=length)
names(mosquito_points_agg)[2] <-'n_outbreaks'
head(mosquito_points_agg)

africa <- st_join(africa, mosquito_points_agg)
africa$area <- as.numeric(st_area(africa))
head(africa)

par(mfrow=c(1,2), mar=c(3,3,1,1), mgp=c(2,1, 0))
plot(n_outbreaks ~ POP_EST, data=africa, log='xy', 
     ylab='Number of outbreaks', xlab='Population size')
plot(n_outbreaks ~ area, data=africa, log='xy',
     ylab='Number of outbreaks', xlab='Area (m2)')

uk_eire_etopo <- raster('data-5/etopo_uk.tif')


st_layers('data-5/National_Trails_Pennine_Way.gpx')
pennine_way <- st_read('data-5/National_Trails_Pennine_Way.gpx', layer='routes', 
                       query="select * from routes where name='Pennine Way'")

# Simplify the data
pennine_way_BNG_simple <- st_simplify(pennine_way_BNG,  dTolerance=100)

# Zoom in to the whole route and plot the data
par(mfrow=c(1,2), mar=c(1,1,1,1))

plot(uk_eire_elev_BNG, xlim=c(3e5, 5e5), ylim=c(3.8e5, 6.3e5),
     axes=FALSE, legend=FALSE)
plot(st_geometry(pennine_way_BNG), add=TRUE, col='black')
plot(pennine_way_BNG_simple, add=TRUE, col='darkred')
## Warning in plot.sf(pennine_way_BNG_simple, add = TRUE, col = "darkred"):
## ignoring all but the first attribute

# Add a zoom box and use that to create a new plot
zoom <- extent(3.77e5, 3.89e5, 4.7e5, 4.85e5)
plot(zoom, add=TRUE)

# Zoomed in plot
plot(uk_eire_elev_BNG, ext=zoom, axes=FALSE, legend=FALSE)
plot(st_geometry(pennine_way_BNG), add=TRUE, col='black')
plot(pennine_way_BNG_simple, add=TRUE, col='darkred')
## Warning in plot.sf(pennine_way_BNG_simple, add = TRUE, col = "darkred"):
## ignoring all but the first attribute






