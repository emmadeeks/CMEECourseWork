# A simple script to illustrate R input-output.
# Run line by line and check inputs putput to
# understand what is happening

# import with headers
MyData <- read.csv("../data/trees.csv", header = TRUE)

# Write it out as a new file
write.csv(MyData, "../results/MyData.csv")

# Append to it
write.table(MyData[1,], file = "../results/MyData.csv", append=TRUE)

# Write row names
write.csv(MyData, "../results/MyData.csv", row.names=TRUE)

# Ignore column names
write.table(MyData, "../results/MyData.csv", col.names=FALSE)

