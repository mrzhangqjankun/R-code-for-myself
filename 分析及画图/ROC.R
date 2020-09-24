##2018.5.20

####1.
##https://mp.weixin.qq.com/s?__biz=MzAwMDY0MzQ0Ng==&mid=2247483791&idx=2&sn=6f5f8889bc50f9109cbba70c19e28c55&chksm=9ae49996ad931080e3416c89dd278763343ebd6a1991d347057108645509b294a3530bb169df&mpshare=1&scene=1&srcid=0520Du103leGXaitwyrZXKiM&pass_ticket=a%2FDZKnr7xDnM7txpzaFEK%2BPJtwU6W9%2BkB4QMG1yU6Lht0mbLenrBKIaxi9BbMLim#rd
##生存曲线分析及绘制(一) 
# 乘积极限法（Kaplan -Meier），另一种基于寿命表法（Life table），
# 生存曲线的比较则主要通过Log-Rank检验、Breslow法以及似然比法检验。
rm(list=ls())
library(survival)
## 列出包中的所有数据
data(package = "survival")
## 载入肺lung的数据：data(lung)
#查看每一列数据的解释：
?lung

# Kaplan-Meier方法：
# 构建一个survival对象，Surv是构建函数，time是天数统计的存活时间，status==2表示选择dead样本，
?with
lung$SurvObj <- with(lung, Surv(time, status == 2))
# 查看数据，发现最后多了一列SurvObj，可以看出status为1的样本所对应的SurvObj都多了个+，表示还存活。
head(lung)

## Kaplan-Meier方法构建， 优先选择"log-log"置信区间，
# survfit第一个参数是个公式，如果右边是1，则生成单一生存曲线，conf.type的参数也可以是"none"，不显示置信区间，默认是"log"#单一生存曲线构建对象：
?survfit
km.as.one <- survfit(SurvObj ~ 1, data = lung, conf.type = "log-log")
# sex为因素构建对象：
km.by.sex <- survfit(SurvObj ~ sex, data = lung, conf.type = "log-log")

# 单一生存曲线图，有置信区间：
plot(km.as.one)
# sex为因素作生存曲线：
plot(km.by.sex)

#设置在哪个时间显示置信区间，不想显示可以设的大一点，另外还有conf.cap设置置信区间的宽，1最大。
# 还有conf.offset,当图上有多条曲线时，设置置信度的偏移量。值为1的是绘图区域的宽度。
#如果这是一个单一数字，那么每个曲线的条都会偏离现有曲线条形的这个数值，当然也可以使用向量。
# mark.time是用来标注是否显示censor检验点。
plot(km.as.one, conf.time=100, conf.cap=0.01,conf.offset = 0.1,mark.time = T)


####2.
##https://mp.weixin.qq.com/s?__biz=MzAwMDY0MzQ0Ng==&mid=2247483793&idx=1&sn=06112670780cbf0bc1562b150f10da66&chksm=9ae49988ad93109ec97426d265f3160b3197451c8b0d355e1384c57b76f435ddd8f92281e5de&mpshare=1&scene=1&srcid=0520jpDumh3DnFc8xZqkAG4Y&pass_ticket=a%2FDZKnr7xDnM7txpzaFEK%2BPJtwU6W9%2BkB4QMG1yU6Lht0mbLenrBKIaxi9BbMLim#rd
##生存曲线分析及绘制(二) 
install.packages("ggfortify")
library(ggfortify)
library(survival)
#ggplot2::autoplot可以做生存彩图：
# 构建对象并拟合：
fit <- survfit(Surv(time, status) ~ sex, data = lung)
autoplot(fit)
## 设置虚线，不显示置信区间，censored点显示为*以及大小，设置分面2
autoplot(fit, surv.linetype = 'dashed', conf.int = FALSE,
         censor.shape = '*', censor.size = 5, facets = TRUE, ncol = 2)
## 单一生存曲线：设置线的颜色为橘黄色，censored点为红色。
autoplot(survfit(Surv(time, status) ~ 1, data = lung), surv.colour = 'orange', censor.colour = 'red')
# sex为因子：
autoplot(fit, fun = 'event')

## 用aareg做多个Surv实例：
autoplot(aareg(Surv(time, status) ~ age + sex + ph.ecog, data = lung))


####3.
##https://mp.weixin.qq.com/s?__biz=MzAwMDY0MzQ0Ng==&mid=2247483788&idx=1&sn=43845d3dd86c410f201c90f7372fe579&chksm=9ae49995ad9310831eacd790f970f9e09761ef450871d91d5a8caf29da88e7e3e127987a0c7d&mpshare=1&scene=1&srcid=0520lDxaaZkILEtb7DEpR99r&pass_ticket=a%2FDZKnr7xDnM7txpzaFEK%2BPJtwU6W9%2BkB4QMG1yU6Lht0mbLenrBKIaxi9BbMLim#rd
##ROC曲线含义及绘制。 
# ROC曲线：
rm(list=ls());library(MASS)
data(cats)
head(cats) ##Sex（性别）、Bwt（体重）、Hwt（心脏重量）
# 使用caTools绘制ROC曲线并计算ROC曲线面积AUC：
library(caTools)
?caTools
?colAUC  ##默认按照列计算
colAUC(cats[,2:3],cats[,1],plotROC = T) ##值为面积

##当然也可以使用ROCR包绘制并计算ＡＵＣ的面积：
# 使用ROCR绘制ROC曲线：library(ROCR)
pred <- prediction(cats[,2:3], cbind(cats[,1],cats[,1]));
perf <- performance(pred,"tpr","fpr");
plot(perf,colorize=TRUE);

###计算曲线下的AUC即面积
auc<-  performance(pred,"auc"); 
# Bwt的面积：slot(auc,"y.values")[[1]] # 结果为0.83
# Hwt的面积：slot(auc,"y.values")[[2]] #结果为0.76

##我们以FPR为横轴,TPR为纵轴,得到ROC空间。AUC值为ROC曲线所覆盖的区域面积,显然,AUC越大,分类器分类效果越好。
# AUC = 1，是完美分类器，采用这个预测模型时，不管设定什么阈值都能得出完美预测。绝大多数预测的场合，不存在完美分类器。
# 0.5 < AUC < 1，优于随机猜测。这个分类器（模型）妥善设定阈值的话，能有预测价值。
# AUC = 0.5，跟随机猜测一样（例：丢铜板），模型没有预测价值。
# AUC < 0.5，比随机猜测还差；但只要总是反预测而行，就优于随机猜测。


###################################
#2020.4.17
#https://mp.weixin.qq.com/s/xT1-4O7vYjbvZqUraMO57g
#受试者工作特征（ROC）曲线及其在R中实现
##7个画ROC的包

