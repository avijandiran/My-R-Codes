mtcars
data(mtcars)

COR <- cor(mtcars)

COR

class(mtcars)

library(corrplot)
corrplot(COR,type="lower")
corrplot(COR,method = "number")

?mtcars





mul.reg <- lm(disp~wt,data=mtcars)

mul.reg

add1(mul.reg,scope = mtcars)

mul.reg <- lm(disp~wt+disp,data=mtcars)
