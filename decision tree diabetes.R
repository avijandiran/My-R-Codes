wdiabetes <- read.csv("Diabetes.csv")
View(wdiabetes)
View(DiabetesPedigreeFunction)

#divide the data into training and testing data set
# insted of using libray caTools and splitting the data ,we can split data by 

set.seed(3)
id <- sample(2,nrow(wdiabetes),prob = c(0.7,0.3),replace=TRUE)
diatrain <- wdiabetes[id==1,]
diatest <- wdiabetes[id==2,]

nrow(wdiabetes)
nrow(diatrain)
nrow(diatest)

View(diatrain)
str(diatrain)

View(diatest)
str(diatest)

#building a Decision Tree 
#for rpart() we need to load rpart library

library(rpart)
library(rpart.plot)
attach(wdiabetes)
colnames(wdiabetes)


#https://stackoverflow.com/questions/33767804/invalid-prediction-for-rpart-object-error (Error in predict.rpart(diamodel, data = diatest, type = "class") : Invalid prediction for "rpart" object)
diatrain$Outcome <- factor(diatrain$Outcome)

diatest$Outcome <- factor(diatest$Outcome)

#here we are using all columns for this model
diamodel <- rpart(Outcome~.,data=diatrain) 

diamodel

#we can plo it as
plot(diamodel,margin = 0.1)

#margin is used to adjust the size of the plot ,for viewing labels
text(diamodel,use.n=TRUE,pretty=TRUE,cex=0.8)

#prediction of the test dataset

Predictdia <- predict(diamodel,data =diatest,type = "class")

#predicted Values

Predictdia 

#actual Values

diatest$Outcome

tab <- table(Predictdia,diatrain$Outcome)

tab

colnames(Predictdia)

#Error in table(diatest$Outcome, Predictdia) :all arguments must have the same length



library(caret)

#Error in table(Predictdia, diatest$Outcome) : all arguments must have the same length

confusionMatrix(table(Predictdia,diatest$Outcome))




