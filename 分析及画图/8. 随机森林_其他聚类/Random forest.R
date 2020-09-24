##2018.4.27
##
##RandomForest：随机森林预测生物标记biomarker——分类
##https://mp.weixin.qq.com/s?__biz=MzUzMjA4Njc1MA==&mid=2247485517&idx=1&sn=56a28fd87abd35fe9c931f91ff84f28d&chksm=fab9e2fccdce6bea35b9f694c834f2323d0e7c2083a9e57a2ef669816cf40d6ac876c2302c10&mpshare=1&scene=1&srcid=04277r2XvFG8Le60PIz90A7Z&pass_ticket=9R9zyVuk89osfxfQ5ijyKOD8OJIczOCYPml8JMjhjcAJVW8G9TJGn8S1tImplEzX#rd

rm(list=ls(all=TRUE))

install.packages("randomForest")

library(randomForest)

data(iris);head(iris)

set.seed(315)

iris.rf = randomForest(Species~.,data = iris,importance= TRUE,proximity = TRUE)
iris.rf

#结果如下：包括分析的命令，优化选择的分类变量个数2，和数据再分类和错误率统计结果。
#此例 中使用2个变量分类，三种花的分类错误率为4%，每组中分类结果和错误率详见表格。

#查看每个变量的分类贡献度，显示仅保留两位小数可读性更好
round(importance(iris.rf),2)
?importance

#可视化MeanDecreaseAccuracy，直译为平均减少准确度，即没有这个Feature时，分类准确度下降的程度，相当于我们常用的分类贡献度的概念
varImpPlot(iris.rf)


## 分类结果主坐轴分析和可视化 Do PCoA/MDS on 1 - proximity:    ##看不懂了
iris.mds= cmdscale(1 - iris.rf$proximity, eig=TRUE)

# 设置显示样品点，而不是变量点
op= par(pty="s")

# 散点图展示每个Feature与PCoA1/2轴间的相关分布
pairs(cbind(iris[,1:4], iris.mds$points), cex=0.6, gap=0,
      col=c("red", "green", "blue")[as.numeric(iris$Species)],
      main="Iris Data: Predictors and MDS of Proximity Based on RandomForest")
par(op)
print(iris.mds$GOF)
#可以看到各Feature间的相关性，如Petal.Width与Petal.Length正相关非常好，而且大小也可以很好分开不同组；
#更可以看到PCoA轴1/2与每个Features分布样式，是如何区分组的。


#随机选取2/3预测，1/3验证

# 随机1-2有放回抽样150次，概率分别为0.67和0.33，用于获取训练集
ind=sample(2,nrow(iris),replace=TRUE, prob=c(0.67,0.33))  
# 2/3作预测建模
iris.rf = randomForest(Species ~ ., iris[ind==1,], ntree=1000, 
                       nPerm=10, mtry=3, proximity=TRUE, importance=TRUE)  
print(iris.rf)  
# 1/3验证预测模型
iris.pred = predict(iris.rf, iris[ind==2,] )  
# 输出预测与观测对应表
table(observed=iris[ind==2,"Species"], predicted=iris.pred) 

#无监督分类

## 无监督分类 The `unsupervised' case:
set.seed(315)
iris.urf= randomForest(iris[, -5])
# 主坐标轴分析并展示
MDSplot(iris.urf, iris$Species)

#分层抽样

#stratified sampling: draw 20, 30, and 20 of the species to grow each tree.
(iris.rf2= randomForest(iris[1:4], iris$Species, sampsize=c(20, 30, 20)))


####5.2
##https://mp.weixin.qq.com/s?__biz=MzUzMjA4Njc1MA==&mid=2247485545&idx=1&sn=19f43d7478e909c65ea2d7e74b4d46c7&chksm=fab9e2d8cdce6bceaa91f36d939b6fb611e94e21b0758054ddfbfca054c8a1ee11c16a23f6c9&mpshare=1&scene=1&srcid=0502m0zYJH6Mxr7ejlKIf5Ht&pass_ticket=NTy3UgQet3gm6bzhGXV4wN%2BVjO2RdXvzXKcceeq2faFe31YhGcJFnjYjmyEw2Uey#rd

##randomForest实现回归分析的实战代码。回归的应用主要包括时间序列预测模式，如预测股票、尸体死亡时间等。
library(randomForest)

data(airquality)
head(airquality)

set.seed(315)

ozone.rf= randomForest(Ozone ~ ., data=airquality, mtry=3,
                       importance=TRUE, na.action=na.omit)
print(ozone.rf)

#查看每个变量的分类贡献度，显示仅保留两位小数可读性更好
round(importance(ozone.rf), 2)

#%IncMSE是Increased in mean squared error (%)，直译为增长的错误率平方均值，即去除该变量后，对目标预测的准确度下降的低，可理解为对目标变量预测准确的贡献度。
#IncNodePurity是Increased node purity，是另一种评估的方法。

varImpPlot(ozone.rf)  

####交叉验证cross-validation

# 先清空NA的样本，验证不允许有NA
airquality = na.omit(airquality)
myairquality= cbind(airquality[1:6], matrix(runif(96 * nrow(airquality)), nrow(airquality), 96))
# 交驻验证添加了随机数的训练集，分组，交叉验证的次数
result= rfcv(myairquality, airquality$Ozone, cv.fold=3)

# 绘制错误率曲线，观查错误率与使用Markers数量的变化
with(result, plot(n.var, error.cv, log="x", type="o", lwd=2))

# 使用replicate进行多次交叉验证，可选
result= replicate(5, rfcv(myairquality, airquality$Ozone), simplify=FALSE)
error.cv= sapply(result, "[[", "error.cv")
matplot(result[[1]]$n.var, cbind(rowMeans(error.cv), error.cv), type="l",
        lwd=c(2, rep(1, ncol(error.cv))), col=1, lty=1, log="x",
        xlab="Number of variables", ylab="CV Error")

##2018.5.22
##https://mp.weixin.qq.com/s?__biz=MzA3MTM3NTA5Ng==&mid=2651058052&idx=1&sn=d419ad2e772c7721640aa36e3c29ac2d&chksm=84d9ce13b3ae47053ed3d1183c3a3d11c8f819240fc6f1e0c62323956fa7ea96166a6bf1dafd&scene=0#rd
##基于随机森林的分类与回归    R语言中文社区
#####内容没有看