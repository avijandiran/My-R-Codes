
#load the data into R
libray(MASS)
data("Boston")
View(Boston)

#for data description
?Boston

#we will split the data into tranining and testing sets
set.seed(2)
library(caTools)

#spliting the data into two in the ratio 70% and 30%

split <- sample.split(Boston$medv,SplitRatio = 0.7)

split

traningData <- subset(Boston,split=="TRUE")
testingData <- subset(Boston,split=="FALSE")

View(traningData) 
View(testingData)

# to view the correlation of variables
plot(Boston$crim,Boston$medv,cex=0.5,xlab="Crime Rate",ylab="Price")
COR <- cor(Boston)

# creating the scatter plot matrix

attach(Boston)
library(lattice)
splom(~Boston[c(1:6,14)],groups=NULL,data=Boston,axis.line.tck=0,axis.text.alpha=0)
splom(~Boston[c(7:14)],groups=NULL,data=Boston,axis.line.tck=0,axis.text.alpha=0)

#studying rm and medv
plot(rm,medv) 
abline(lm(medv~rm),col="red")  #regression fit line


#we can use corplot to visualize 
library(corrplot)
corrplot(COR,type="lower")
corrplot(COR,method="number")


# finding multicollinearity
library(caret)

#to exclude the medv(output) data

Boston_a <- subset(Boston,select=-c(medv))
numericData <- Boston_a[sapply(Boston_a,is.numeric)]
descrCOR <- cor(numericData)

#VIF

library(car)
model <- lm(medv~.,data=traningData)
vif(model)    

#now we create the model , we will use all columns

model <- lm(medv~.,data=traningData)

#or 

model <- lm(medv~crim+zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+black+lstat,data=traningData)

#for summary
summary(model)

#model creation after removing tax

model <- lm(medv~crim+zn+indus+chas+nox+rm+age+dis+rad
            +ptratio+black+lstat,data=traningData)
summary(model)

#model removing indus and age
model <- lm(medv~crim+zn+chas+nox+rm+dis+rad
            +ptratio+black+lstat,data=traningData)
summary(model)

#now we can use this model to predict the output of testing data'
prediction <- predict(model,testingData)
prediction

#to compare predicted values and actual values ,we can use plots
plot(testingData$medv,type="l",lty=1.8,col="green")
lines(prediction,type="l",col="blue")

#now we can use this model to predict the output of sample data set
prediction <- predict(model,sample_data)
prediction





