

setwd("/Users/emmadeeks/Desktop/CMEECourseWork/project/data")
acoustic <- read.csv("acoustic_no_DG.csv")

acoustic$Date <- substr(acoustic$Date, 0, 10)
#acoustic$Date <- as.POSIXct(acoustic$Date, format="%Y-%m-%d")

code_nest <- acoustic %>%
  nest(data= -Code)





adding = as.data.frame(matrix(nrow = 1, ncol = 4))
for (i in 1:length(code_nest$Code)){
  one <- code_nest$data[[i]]
  ID <- code_nest$Code[[i]]
  min <- min(one$Date, na.rm=T)
  max <- max(one$Date, na.rm=T)
  diff <- difftime(max, min)
  diff <- as.numeric(diff)
  toadd <- c(ID, min, max, diff)
  adding <- rbind(adding, toadd)
}


adding <- adding[-1,]
cols <- c("Code", "min", "max", "days")
colnames(adding) <- cols  
write.csv(adding, "acoustic/standardising_tags.csv")  

summary_tags <- read.csv('summary_tags.csv', header = T)
summary_tags$month <- paste0(summary_tags$month, "-01")


summary_tags$count_tag <- 
  sapply(summary_tags$month, function(x)
    sum(as.Date(adding$min, "%Y-%m-%d") <= as.Date(x, "%Y-%m-%d") &
          as.Date(adding$max, "%Y-%m-%d") >= as.Date(x, "%Y-%m-%d")))


sum(as.Date(adding$min, "%Y-%m-%d") <= as.Date(summary_tags$month[1], "%Y-%m-%d") &
      as.Date(adding$max, "%Y-%m-%d") >= as.Date(summary_tags$month[1], "%Y-%m-%d"))



write.csv(summary_tags, "acoustic/standardised_tags_summary.csv")  

acoustic$NewDate <- substr(acoustic$Date, 0, 7)
cross_tab = xtabs(~ Code + NewDate, acoustic)
write.csv(cross_tab, "acoustic/contingency_tags_all.csv")  

  