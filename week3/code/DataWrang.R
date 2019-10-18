

# header = false because the raw data dont have real headers
MyData <- as.matrix(read.csv("../data/PoundHillData.csv",header = F))

# header = true because we do have metadata headers
MyMetaData <- read.csv("../data/PoundHillMetaData.csv",header = T, sep=";", stringsAsFactors = F)
### Inspecting the dataset

head(MyData)
dim(MyData)
str(MyData)
fix(MyData)
fix(MyMetaData)

### Transpose
#To get those species into columns and treatments into rows
MyData <- t(MyData)
head(MyData)
dim(MyData)

# Replace species abundances with zeros
MyData[MyData == ""] = 0

# Convert raw matrix to data frame
TempData <- as.data.frame(MyData[-1,],stringsAsFactors = F)
#stringsAsFactors = F is important!
colnames(TempData) <- MyData[1,] #assign column names from original data

## Convert from wide to long format
require(reshape2) #Load the reshape2 package

?melt #checkout the melt function
MyWrangledData <- melt(TempData, id=c("Cultivation", "Block", "Plot", "Quadrat"), variable.name = "species", value.name = "Count")

MyWrangledData[, "Cultivation"] <- as.factor(MyWrangledData[, "Cultivation"])
MyWrangledData[, "Block"] <- as.factor(MyWrangledData[, "Block"])
MyWrangledData[, "Plot"] <- as.factor(MyWrangledData[, "Plot"])
MyWrangledData[, "Quadrat"] <- as.factor(MyWrangledData[, "Quadrat"])
MyWrangledData[, "Count"] <- as.integer(MyWrangledData[, "Count"])

str(MyWrangledData)
head(MyWrangledData)
dim(MyWrangledData)


MyData[MyData == ""] = 0

