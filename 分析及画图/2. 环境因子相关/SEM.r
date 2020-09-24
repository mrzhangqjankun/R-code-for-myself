##2019.5.5
#SEM

#https://zhuanlan.zhihu.com/p/22811566
# install.packages("lavaan", dependencies=TRUE)
# install.packages("stringr")
# install.packages("DiagrammeR")
# install.packages("dplyr")
# install.packages("semPlot")
# install.packages("nloptr")

library("stringr")
library('lavaan')
library("DiagrammeR")
library("dplyr")
library('nloptr')
library("semPlot")

?lavaan
?model.syntax

##语法见知乎网站
#方法一：最简化描述

#只需指定最基本的要素即可，其他的由函数自动实现，对模型的控制力度最弱。只使用于函数cfa()和sem()
model<-'visual=~x1+x2+x3
textual=~x4+x5+x6
speed=~x7+x8+x9'
fit <- cfa(model, data = HolzingerSwineford1939) #输入的数据就是环境因子形式的
fit

#需要注意的是，这种指定模型的方式在进行拟合时，会默认指定潜变量的第一个测量变量的因子载荷为1，如果要指定潜变量的方差为1，可以：
model.bis <- 'visual =~ NA*x1 + x2 + x3 
               textual =~ NA*x4 + x5 + x6 
               speed =~ NA*x7 + x8 + x9 
               visual ~~ 1*visual 
               textual ~~ 1*textual 
               speed ~~ 1*speed'

#方法二：完全描述
#需要指定所有的要素，对模型控制力最强，适用于lavaan()函数，适合高阶使用者
model.full<- '  visual =~ 1*x1 + x2 +x3
textual =~ 1*x4 + x5 + x6
speed =~ 1*x7 + x8 +x9
x1 ~~ x1
x2 ~~ x2
x3 ~~ x3
x4 ~~ x4
x5 ~~ x5
x6 ~~ x6
x7 ~~ x7
x8 ~~ x8
x9 ~~ x9
visual ~~ visual
textual ~~ textual
speed ~~ speed
visual ~~ textual +speed
textual ~~ speed'
fit <- lavaan(model.full, data = HolzingerSwineford1939)
fit

#方法三：不完全描述
#最简化和完全描述的混合版，在拟合时增加 auto.* 参数，适用于lavaan()函数
model.mixed<- '# latent variables
visual =~ 1*x1 + x2 +x3
textual =~ 1*x4 + x5 + x6
speed =~ 1*x7 + x8 +x9
# factor covariances
visual ~~ textual + speed
textual ~~ speed'
 fit <- lavaan(model.mixed, data = HolzingerSwineford1939, auto.var = TRUE)


#查看拟合结果的最简单方法是用summary()函数，例如
summary(fit, fit.measures=TRUE)
#但summary()只适合展示结果，parameterEstimates()会返回一个数据框，方便进一步的处理
parameterEstimates(fit,ci=FALSE,standardized = TRUE)
#获得大于10的修正指数
MI<- modificationindices(fit)
subset(MI,mi>10)


##结构方程模型
#1）设定模型
model<- '
# measurement model
ind60 =~ x1 + x2 +x3
dem60 =~ y1 + y2 + y3 + y4
dem65 =~ y5 + y6 + y7 + y8
# regressions
dem60 ~ ind60
dem65 ~ ind60 + dem60
# redisual covariances
y1 ~~ y5
y2 ~~ y4 +y6
y3 ~~ y7
y4 ~~ y8
y6 ~~ y8'
#（2）模型拟合
fit <- sem(model, data = PoliticalDemocracy) 
summary(fit, standardized = TRUE)
#（3）给回归系数设置标签
#给回归系数设定标签在做有约束条件的结构方程模型时会很有用。当两个参数具有相同的标签时，会被视为同一个，只计算一次。
model.equal <- '# measurement model   
ind60 =~ x1 + x2 + x3 + 
dem60 =~ y1 + d1*y2 + d2*y3 + d3*y4 
dem65 =~ y5 + d1*y6 + d2*y7 + d3*y8 
# regressions 
dem60 ~ ind60 
dem65 ~ ind60 + dem60 
# residual covariances 
y1 ~~ y5 
y2 ~~ y4 + y6 
y3 ~~ y7 
y4 ~~ y8 
y6 ~~ y8'
#（4）多组比较
anova(fit, fit.equal)
#anova()会计算出卡方差异检验
#（5）拟合系数
#lavaan包可以高度定制化的计算出你想要的拟合指标值，例如，我想计算出卡方、自由度、p值、CFI、NFI、IFI、RMSEA、EVCI的值
fitMeasures(fit,c("chisq","df","pvalue","cfi","nfi","ifi","rmsea","EVCI"))


##作图
# semPaths(object, what = "paths", whatLabels, layout = "tree", ……）
#          （1）object：是拟合的对象，就是上文中的“fit”
#          （2）what：设定图中线的属性， 默认为paths,图中所有的线都为灰色，不显示参数估计值；
semPaths(fit)

#若what设定为est、par，则展示估计值，并将线的颜色、粗细、透明度根据参数估计值的大小和显著性做出改变
semPaths(fit,what = "est")

#若设置为stand、std，则展示标准参数估计
semPaths(fit,what = "stand")

# 若设置为eq、cons，则与默认path相同，如果有限制等式，被限制的相同参数会打上相同的颜色；
# 
# （3）whatLabels：设定图中线的标签
# name、label、path、diagram:将边名作为展示的标签
# est、par:参数估计值作为边的标签
# stand、std:标准参数估计值作为边的标签
# eq、cons：参数号作为标签，0表示固定参数，被限制相同的参数编号相同
# no、omit、hide、invisible：隐藏标签
# 
# （4）layout:布局
# 主要有树状和环状两种布局，每种布局又分别有两种风格。
# 默认为“tree”,树状的第二种风格如下图，比第一种看起来舒服都了
semPaths(fit,layout = "tree2")
semPaths(fit,layout = "circle")#环状
semPaths(fit,layout = "spring")

semPaths(fit, intercept = FALSE, whatLabel = "est",
         residuals = FALSE, exoCov = FALSE)


#########2019.5.31何晴数据
data1 = read.table(file="E:/桌面/R script 2017/SEM_filter_imput.txt",header=T,row.names=1,sep='\t')
data1
library(vegan);?decostand() #数据应该不能有负的，会算不出标准偏差和P。
data2 = decostand(data1,margin=2,method="standardize")
data2
data = decostand(data2,margin=2,method="range")
#data = data2 +3
model<-'diversity_PC1 =~ PH+Temperature+Redoxpotential+SO4+N
        function_PC1 =~ PH+Temperature+Redoxpotential+SO4+N
        diversity_PC1 ~~ function_PC1
        diversity_PC1 ~~ diversity_PC1
        function_PC1 ~~ function_PC1
        PH ~~ PH
        Temperature ~~ Temperature
        Redoxpotential ~~ Redoxpotential
        SO4 ~~ SO4
        N ~~ N'

fit <- lavaan(model, data = data, auto.var = TRUE)
summary(fit,fit.measure=TRUE)
parameterEstimates(fit,ci=FALSE,standardized = TRUE)
semPaths(fit,what = "est")
semPaths(fit,what = "stand")
semPaths(fit,layout = "tree2")

##它的样本太少了，一直会报错。

##2019.9.6
###https://mp.weixin.qq.com/s/Pr-bbjC3uOEy0seS0Yzl-A