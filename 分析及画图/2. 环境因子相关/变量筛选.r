##变量筛选
#2019.4.12

library(leaps)
?leaps
x<-matrix(rnorm(100),ncol=4)
y<-rnorm(25)
leaps(x,y)

#regsubsets函数也可以

#https://cran.r-project.org/web/packages/olsrr/vignettes/variable_selection.html
library(olsrr)
?olsrr
model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
k <- ols_step_all_possible(model);k
plot(k)
?ols_step_all_possible

#最好的子集
p <- ols_step_best_subset(model);p
plot(p)

#Stepwise Forward Regression
model <- lm(y ~ ., data = surgical)
k<- ols_step_forward_p(model)
plot(k)

#Stepwise Backward Regression
model <- lm(y ~ ., data = surgical)
k <- ols_step_backward_p(model)
plot(k)

#Stepwise Regression
model <- lm(y ~ ., data = surgical)
k <- ols_step_both_p(model)
plot(k)

#Stepwise AIC Forward Regression
model <- lm(y ~ ., data = surgical)
k <- ols_step_forward_aic(model)
plot(k)

#Stepwise AIC Backward Regression
model <- lm(y ~ ., data = surgical)
k <- ols_step_backward_aic(model)

#Stepwise AIC Regression
model <- lm(y ~ ., data = surgical)
ols_step_both_aic(model)


##MASS
library(MASS)
data(swiss)
str(swiss)

lm <- lm(Fertility ~ ., data = swiss)
lm$coefficients
## (Intercept)      Agriculture      Examination        Education         Catholic 
## 66.9151817       -0.1721140       -0.2580082       -0.8709401        0.1041153 
## Infant.Mortality 
## 1.0770481

st1 <- stepAIC(lm, direction = "both")
st2 <- stepAIC(lm, direction = "forward")
st3 <- stepAIC(lm, direction = "backward")

summary(st1)
summary(st2)
summary(st3)

?stepAIC
