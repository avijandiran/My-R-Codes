library(e1071)
plot(iris)

plot(iris$Sepal.Length,iris$Sepal.Width,col=iris$Species)
plot(iris$Petal.Length,iris$Petal.Width,col=iris$Species)

s <- sample(150,100)
col <- c("Petal.Length","Petal.Width","Species")
iris_train <-iris[s,col]
iris_test <- iris[-s,col]

#kernal=linear
 
svmfit <- svm(Species~.,data=iris_train,kernel="linear",cost=.1,scale=FALSE)

print(svmfit)
plot(svmfit,iris_train[,col])

#kernal=radical
svmfit <- svm(Species~.,data=iris_train,cost=.1,scale=FALSE)

print(svmfit)
plot(svmfit,iris_train[,col])

#kernal=sigmoid

svmfit <- svm(Species~.,data=iris_train,cost=.1,scale=FALSE)

print(svmfit)
plot(svmfit,iris_train[,col])
tunned <- tune(svm ,Species~., data=iris_train, kernel="linear", range=list(cost=c(0.001,0.01,.1,1,10,100)))

summary(tunned)

#tunning function

tunned <- tune(svm ,Species~., data=iris_train, kernel="Sigmoid", range=list(cost=c(0.001,0.01,.1,1,10,100)))

summary(tunned)

# prediction
p <- predict(svmfit,iris_test[,col],type="class")
plot(p)
table(p,iris_test[,3])
mean(p==iris_test[,3])

#confusion matrix and miscallisfication error

pred <- predict(svmfit,iris)
tab <- table(predicted=pred,Actual=iris$Species)
tab

1-sum(diag(tab))/sum(tab)

#best model is the #kernal=radical has less support vectors and less misclassification error .
svmfit <- svm(Species~.,data=iris_train,cost=.1,scale=FALSE)

print(svmfit)
plot(svmfit,iris_train[,col])


