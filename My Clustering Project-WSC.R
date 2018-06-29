#loading my dataset
mydataset <- read.csv("Wholesale customers data.csv")

#Pre Processig my dataset
View(mydataset)
head(mydataset)
str(mydataset)
dim(mydataset)
summary(mydataset)

#removing the Channel and Region Columns 

mydataset.c <- mydataset
mydataset.c$Channel <- NULL
mydataset.c$Region <- NULL

names(mydataset.c)


#hieraechial clustring  with compelete linkage(hc.c)

distance <- dist(mydataset.c)
print(distance,digitis=3)
hc.c <- hclust(distance)
plot(hc.c,hang=-1,main="Hclust Complete Linkage")
plot(hc.c,labels = mydataset$Channel)



#hieraechial clustring with average linkage(hc.a)

hc.a <- hclust(distance,method="average")
plot(hc.a,hang=-1,main="Hclust Average Linkage")


#Compare the hclust model using complete link Vs Average link
par(mfrow=c(2,1))
plot(hc.c,hang=-1,main="Hclust Complete Linkage")
plot(hc.a,hang=-1,main="Hclust Average Linkage")


# clustering membersip

mem.c <- cutree(hc.c,3)
mem.a <- cutree(hc.a,3)
table(mem.c,mem.a)

# cluster Means 
aggregate(mydataset.c,list(mem.c),mean)


#silhouette plot

library(cluster)
plot(silhouette(cutree(hc.c,3),distance))


#scree plot

wss <- (nrow(mydataset.c)-1)*sum(apply(mydataset.c,2,var))
for(i in 2:20) wss[i] <- sum(kmeans(mydataset.c,centers = i)$withinss)
plot(1:20,wss,type="b",xlab = "number of clusters",ylab = "within group ss")

#kmeans clustring for k=3,4,5,6

kc3 <- kmeans(mydataset.c,3)
kc3
plot(Milk~Grocery,mydataset,col=kc3$cluster)


kc4 <- kmeans(mydataset.c,4)
kc4
plot(Milk~Grocery,mydataset,col=kc4$cluster)

kc5 <- kmeans(mydataset.c,5)
kc5
plot(Milk~Grocery,mydataset,col=kc5$cluster)

kc6 <- kmeans(mydataset.c,6)
kc6
plot(Milk~Grocery,mydataset,col=kc6$cluster)


#View plotting k=3,4,5,6
par(mfrow=c(2,2))
plot(Milk~Grocery,mydataset,col=kc3$cluster,main="k=3")
plot(Milk~Grocery,mydataset,col=kc4$cluster,main="k=4")
plot(Milk~Grocery,mydataset,col=kc5$cluster,main="k=5")
plot(Milk~Grocery,mydataset,col=kc6$cluster,main="k=6")

#visualising the clustring in clustplot and fpc plot

library(cluster)
clusplot(mydataset.c, kc5$cluster, main = 'Cusplot',color=TRUE, shade=TRUE, labels=2, lines=0)


library(fpc)
plotcluster(mydataset.c, kc5$cluster)



