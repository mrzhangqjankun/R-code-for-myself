##2018.5.26

##R做线性回归 

##https://mp.weixin.qq.com/s?__biz=MzUzMjA4Njc1MA==&mid=2247485622&idx=1&sn=66db644bef7e3d1d3353692ba00dce82&chksm=fab9e207cdce6b11ecc4f339297498938f0ee36d46be9630aac4196cca0be61a0f26f8dce04c&scene=0#rd

library(ggplot2)
library(car)
library(caret)
library(corrplot)

data(mtcars)
head(mtcars)
#确保分类变量存储为因子。 
mtcars$am <- as.factor(mtcars$am)
mtcars$cyl <- as.factor(mtcars$cyl)
mtcars$vs <- as.factor(mtcars$vs)
mtcars$gear <-  as.factor(mtcars$gear)

##识别和修正共线性
mtcars_a <- subset(mtcars, select = -c(mpg))
numericData <- mtcars_a[sapply(mtcars_a, is.numeric)]
descrCor <- cor(numericData)
print(descrCor)

corrplot(
  descrCor,
  order = "FPC",
  method = "color",
  type = "lower",
  tl.cex = 0.7,
  tl.col = rgb(0,0,0)
)

##
?findCorrelation  ##成对样本之间的相关性

highlyCorrelated <- findCorrelation(descrCor, cutoff=0.7);highlyCorrelated 
highlyCorCol <- colnames(numericData)[highlyCorrelated];highlyCorCol
dat3 <- mtcars[, -which(colnames(mtcars) %in% highlyCorCol)]
dim(dat3)
#有三个变量“hp”“disp”“wt”被发现是高度相关的。 我们已经删除它们以避免共线。 现在，我们有7个独立变量和1个因变量。

##开发回归模型
#在这一步，我们正在建立多元线性回归模型。
fit <- lm(mpg ~ ., data=dat3)
summary(fit)
summary(fit)$coeff
anova(fit)
par(mfrow=c(2,2))
plot(fit)
# 残留物与拟合值
# 正常Q-Q
# 缩放位置
# 残差与杠杆

summary(fit)$r.squared
summary(fit)$adj.r.squared
AIC(fit)
BIC(fit)
#更高的R平方和调整的R平方值，更好的模型。 然而，更低的AIC和BIC得分，更好的模型。

#AIC和BIC是拟合度的衡量标准。 他们惩罚复杂的模型。 
#换句话说，它会惩罚更多的估计参数。 
#它相信一个概念，即一个具有较少参数的模型将被一个具有更多参数的模型要好。 一般来说，BIC比AIC更为免费参数惩罚模型。 两个标准都取决于估计模型的似然函数L的最大值。
#AIC值大致等于参数的数量减去整个模型的似然性。 假设你有两个模型，AIC和BIC分数较低的模型更好。

# 变量选择方法
# 有三种变量选择方法 - 向前，向后，逐步。
# 1.以单个变量开始，然后基于AIC（“前进”）一次添加一个变量
# 2.从所有变量开始，基于AIC（’后退’）迭代地去除那些重要性低的变量
# 3.双向运行（’逐步’）
library(MASS)
step <- stepAIC(fit, direction="both")
summary(step)
step <- stepAIC(fit, direction="forward")
summary(step)
n <- dim(dat3)[1]
stepBIC <- stepAIC(fit,k=log(n))
summary(stepBIC)

AIC(stepBIC)
BIC(stepBIC)

# 计算标准化系数
# 标准化系数有助于根据标准化估计值的绝对值排列预测值。 值越高，变量越重要。
#使用QuantPsyc包的lm.beta函数计算
library(QuantPsyc)
lm.beta(stepBIC)

#自定义函数计算
stdz.coff <- function (regmodel)
{ b <- summary(regmodel)$coef[-1,1]
sx <- sapply(regmodel$model[-1], sd)
sy <- sapply(regmodel$model[1], sd)
beta <- b * sx / sy
return(beta)
}

std.Coeff <- data.frame(Standardized.Coeff = stdz.coff(stepBIC))
std.Coeff <- cbind(Variable = row.names(std.Coeff), std.Coeff)
row.names(std.Coeff) <- NULL

# 计算方差膨胀因子（VIF）
# 与独立变量高度相关的情况相比，差异膨胀因子衡量的是系数的变化幅度。 它应该小于5。
vif(stepBIC)
#测试其它假设
#Autocorrelation Test
durbinWatsonTest(stepBIC)

#Normality Of Residuals (Should be > 0.05)
res=residuals(stepBIC,type="pearson")
shapiro.test(res)

#Testing for heteroscedasticity (Should be > 0.05)
ncvTest(stepBIC)

#Outliers – Bonferonni test
outlierTest(stepBIC)

#See Residuals
resid = residuals(stepBIC)

#Relative Importance
#install.packages("relaimpo")
library(relaimpo)
calc.relimp(stepBIC)
#查看实际值和预测值
#See Predicted Value
pred <- predict(stepBIC,dat3)
#See Actual vs. Predicted Value
finaldata <-  cbind(mtcars,pred)
print(head(subset(finaldata, select = c(mpg,pred))))
#其它有用的函数
#Calculating RMSE
rmse = sqrt(mean((dat3$mpg - pred)^2))
print(rmse)

#Calculating Rsquared manually
y = dat3[,c("mpg")]
R.squared = 1 - sum((y-pred)^2)/sum((y-mean(y))^2)
print(R.squared)

#Calculating Adj. Rsquared manually
n = dim(dat3)[1]
p = dim(summary(stepBIC)$coeff)[1] - 1
adj.r.squared = 1 - (1 - R.squared) * ((n - 1)/(n-p-1))
print(adj.r.squared)

#Box Cox Transformation
library(lmSupport)
modelBoxCox(stepBIC)
# K-fold交叉验证
# 在下面的程序中，我们正在进行5倍交叉验证。 在5倍交叉验证中，数据被随机分成5个相同大小的样本。 在5个样本中，随机20％数据的单个样本保留为验证数据，其余80％用作训练数据。 然后这个过程重复5次，5个样本中的每一个都只用作验证数据一次。 稍后我们将结果平均。
library(DAAG)
kfold = cv.lm(data=dat3, stepBIC, m=5)