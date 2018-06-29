#loading my dataset
mydataset <- read.csv("Wholesale customers data.csv")

#Viewing my dataset
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


#normilaisation 
mean <- apply(mydataset.c,2,mean)
sd <- apply(mydataset.c,2,sd)
mydataset.z <- scale(mydataset.c,mean,sd)
distance <- dist(mydataset.z)
print(distance,digitis=3)

# heriarchial clustring with normailation 
library(stats)
library(cluster)
hc. <- hclust(distance)
plot(hc.c)

#hieraechial clustring without normilaisation and compelete linkage

distance <- dist(mydataset.c)
print(distance,digitis=3)
hc.d <- hclust(distance)
plot(hc.d,hang=-1)

#agglomerative clustering 

library(cluster)
mydataset.agg=agnes(mydataset.c,diss=FALSE,metric="euclidian")
plot(mydataset.agg)

#hieraechial clustring without normilaisation and average linkage

hc.a <- hclust(distance,method="average")
plot(hc.a,hang=-1)

# clustring membersip

mem.c <- cutree(hc.d,3)
mem.a <- cutree(hc.a,3)
table(mem.c,mem.a)

# cluster Means 
aggregate(mydataset.c,list(mem.c),mean)


#silhouette plot

library(cluster)
plot(silhouette(cutree(hc.d,3),distance))


#scree plot

wss <- (nrow(mydataset.c)-1)*sum(apply(mydataset.c,2,var))
for(i in 2:20) wss[i] <- sum(kmeans(mydataset.c,centers = i)$withinss)
plot(1:20,wss,type="b",xlab = "number of clusters",ylab = "within group ss")

#kmeans clustring

kc <- kmeans(mydataset.c,3)
kc
plot(Milk~Grocery,mydataset,col=kc$cluster)


kc2 <- kmeans(mydataset.c,4)
kc2
plot(Milk~Grocery,mydataset,col=kc2$cluster)

kc3 <- kmeans(mydataset.c,5)
kc3
plot(Milk~Grocery,mydataset,col=kc3$cluster)

kc4 <- kmeans(mydataset.c,6)
kc4
plot(Milk~Grocery,mydataset,col=kc4$cluster)

library(cluster)
clusplot(mydataset.c, kc3$cluster, main = 'Cusplot',color=TRUE, shade=TRUE, labels=2, lines=0)

clusplot(mydataset.c, kc3$cluster, main = 'Cusplot',color=TRUE, shade=TRUE, labels=0, lines=0)

library(fpc)
plotcluster(mydataset.c, kc3$cluster)

