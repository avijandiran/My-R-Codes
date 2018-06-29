moviedata<- read.csv("movie_metadata.csv")

attach(moviedata)

View(moviedata)

movie<- data.matrix(moviedata)

movie <- na.omit(movie)

smp <- movie[sample(nrow(movie),500),]

smpshort <- smp[,c(9,23)]
smpmatrix <- data.matrix(smpshort)
View(smpmatrix)

wss <- (nrow(smpmatrix)-1)*sum(apply(smpmatrix,2,var))
for (i in 2:15) wss[i]<-sum(kmeans(smpmatrix,centers=i)$withinss)

plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within Sum of Squares")

cl <- kmeans(smpmatrix,3,nstart=25)

plot(smpmatrix, col =(cl$cluster +1) , main="k-means result with 2 clusters", pch=1, cex=1, las=1)

points(cl$centers, col = "black", pch = 17, cex = 2)

# https://www.edureka.co/blog/k-means-clustering-algorithm/



