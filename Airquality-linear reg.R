data("airquality")

names(airquality)

plot(Ozone~Solar.R,data=airquality)

mean.ozone <- mean(airquality$Ozone,na.rm = T)
abline(h=mean.ozone)

model1 <- lm(Ozone~Solar.R,data=airquality)
model1 

abline(model1,col="red")
plot(model) # shows the residulas and the distribustion(normal or not )

termplot(model1)
summary(model1)
