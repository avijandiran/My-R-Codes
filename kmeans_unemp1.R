unemp <- read.csv("unemp.csv")

View(unemp)

#unemp1 <- unemp
#View(unemp1)
#unemp1<- unemp1$state<- NULL
#View(unemp1)

results <- kmeans(unemp[,c("mean","stddev")],centers=5)

results

plot(unemp$mean,unemp$stddev,type="n",xlab="mean",ylab="standar deviation")

text(x=unemp$mean,y=unemp$stddev,lables=unemp$state,col=results$cluster)

