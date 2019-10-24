
#Script that makes three pdf lattices of prey mass, predator mass and the size ratio of prey mass over predator mass.
#Script also calculates the mean, median and log of these for prey mass, predator mass and size ratio into a csv file.
#Script calculates this for the data subsetted by feeding type.

#Reads the data in and assigns the table to a variable
MyDF <- read.csv("../data/EcolArchives-E089-51-D1.csv")

library(lattice)

pdf("../results/Pred_Lattice.pdf", # Open blank pdf page using a relative path
    11.7, 8.3)
densityplot(~log(Predator.mass) | Type.of.feeding.interaction, data=MyDF) #Plots the data
dev.off()


pdf("../results/Prey_Lattice.pdf", # Open blank pdf page using a relative path
    11.7, 8.3)
densityplot(~log(Prey.mass) | Type.of.feeding.interaction, data=MyDF)
dev.off()

pdf("../results/SizeRatio_Lattice.pdf", # Open blank pdf page using a relative path
    11.7, 8.3)
densityplot(~log(Prey.mass/Predator.mass) | Type.of.feeding.interaction, data=MyDF)
dev.off()

#Uses diplyr to calculate mean and medians of all the categories; prey mass, predator mass and rato

dat2 <- ddply(MyDF, "Type.of.feeding.interaction", summarise, mean=mean(Predator.mass), median=median(Predator.mass), log_mean= mean(log(Predator.mass)), log_median= median(log(Predator.mass)))
dat3 <- ddply(MyDF, "Type.of.feeding.interaction", summarise, mean=mean(Prey.mass), median=median(Prey.mass), log_mean= mean(log(Prey.mass)), log_median= median(log(Prey.mass)))
dat4 <- ddply(MyDF, "Type.of.feeding.interaction", summarise, mean=mean(Prey.mass/Predator.mass), median=median(Prey.mass/Predator.mass), log_mean= mean(log(Prey.mass/Predator.mass)), log_median= median(log(Prey.mass/Predator.mass)))

#Binds the dataframes with means together
my_frame <- rbind(dat2,dat3,dat4)
#Adds rows titles to the dataframe
a <- c('Predator_mass', 'Prey_mass', 'Prey_predator_size_ratio')
b <- rep(a, each= 5)

#Adds row
my_frame$Category <- b
#Gives column names
my_frame <- my_frame[ ,c("Category", "Type.of.feeding.interaction", "mean", "median", "log_mean", "log_median")]
#Writes csv
write.csv(my_frame, "../results/PP_Results.csv", row.names = FALSE)

