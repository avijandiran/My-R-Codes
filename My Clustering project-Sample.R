#loading my dataset
mydataset <- read.csv("Wholesale customers data.csv")

mydataset <- mydataset[1:50,1:8]

mydataset1<- mydataset[1:50,1:8]

View(mydataset1)
mydataset1$Channel <- NULL
mydataset1$Region <- NULL

mydataset1

library(stats)
library(cluster)
distance <- dist(mydataset1)
print(distance,digitis=3)
hc <- hclust(distance)
plot(hc,hang=-1,main="Hclust Complete linkage")
plot(hc,labels = mydataset$Channel)


#kmeans clustring

kc3 <- kmeans(mydataset1,3)
kc3
plot(Milk~Grocery,mydataset,col=kc3$cluster,main="k=3")


kc4 <- kmeans(mydataset1,4)
kc4
plot(Milk~Grocery,mydataset,col=kc4$cluster,main="k=4")

kc5 <- kmeans(mydataset1,5)
kc5
plot(Milk~Grocery,mydataset,col=kc5$cluster,main="k=5")

#View plotting k=3,4,5
par(mfrow=c(3,1))
plot(Milk~Grocery,mydataset,col=kc3$cluster,main="k=3")
plot(Milk~Grocery,mydataset,col=kc4$cluster,main="k=4")
plot(Milk~Grocery,mydataset,col=kc5$cluster,main="k=5")


#visualising the clustring in clustplot and fpc plot

library(cluster)
clusplot(mydataset1, kc5$cluster, main = 'Cusplot',color=TRUE, shade=TRUE, labels=2, lines=0)


library(fpc)
plotcluster(mydataset1, kc5$cluster)


