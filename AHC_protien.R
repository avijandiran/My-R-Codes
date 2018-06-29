library(cluster)
food <- read.csv("protein.csv")
foodagg=agnes(food,diss=FALSE,metric="euclidian")
plot(foodagg)

