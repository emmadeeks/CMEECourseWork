#Map of the world American
load("../data/GPDDFiltered.RData")
library("maps")


map(database = "world", regions = ".")
points(gpdd$long, gpdd$lat, pch = 16, col = "green", cex = 1)

#
