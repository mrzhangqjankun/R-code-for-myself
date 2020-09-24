##2019.1.28
#https://mp.weixin.qq.com/s/uDkAZ5GfiFp0UwnlJJDOmA
#R语言实现46种距离算法 

rm(list=ls())

#install.packages("philentropy")
library(philentropy)

#查看所有距离
getDistMethods()
?distance()

# 1 distance(): 计算距离
# 2 getDistMethods()，获得距离算法列表
# 3 dist.diversity()，概率密度函数之间的距离差异
# 4 estimate.probability()，从计数向量估计概率向量
# 5 lin.cor()，线性相关性判断
# 6 H(): 香农熵, Shannon’s Entropy H(X)
# 7 JE() : 联合熵, Joint-Entropy H(X,Y)
# 8 CE() : 条件熵, Conditional-Entropy H(X|Y)
# 9 MI() : 互信息, Shannon’s Mutual Information I(X,Y)
# 10 KL() : KL散度, Kullback–Leibler Divergence
# 11 JSD() : JS散度，Jensen-Shannon Divergence
# 12 gJSD() : 通用JS散度，Generalized Jensen-Shannon Divergence
# 13 binned.kernel.est()，实现了KernSmooth包提供的核密度估计函数的接口

library(magrittr)
head(iris)
iris
# distance(x, method = "euclidean", p = NULL, test.na = TRUE, unit = "log", est.prob = NULL)
# 
# 参数列表：
# x, 数值类型的向量或数据集
# method, 算法的名称
# p, minkowski闵可夫斯基距离的p值，p=1为曼哈顿距离，p=2为欧氏距离，p取极限时是切比雪夫距离
# test.na, 检测数据集是否有NA值，不检测为FALSE，计算会快。
# unit，对数化的单位，依赖于日志计算的距离
# est.prob 从计数估计概率，默认值为NULL

####distance()
dat1 = iris[1:2,-5];dat1
distance(dat1,method="euclidean")
#dist()
dist(dat1)
#公式计算
(dat1[1,]-dat1[2,])^2 %>% sum %>% sqrt
##三个结果完全一样

####dist.diversity() 计算所有距离的值。由于有一些距离有对于数据集本身的要求，所以我们需要构建一个能适应所有距离算法的数据集。
# 生成数据集，2个点，10个维度
 P <- 1:10/sum(1:10)
 Q <- 20:29/sum(20:29)
 x <- rbind(P,Q)
 head(x)

dist.diversity(x)

####estimate.probability()，采用数字计数来计算向量的估计概率。estimate.probability()函数方法实现，目前只有一个方法实现，计算每个值占合计的比率，其实就是一种数据标准归的计算方法

#我们新建一个向量，用estimate.probability()函数，来计算向量的估计概率。
# 新建x1向量
 x1<-runif(100);head(x1)

 # 计算估计概率
  x2<-estimate.probability(x1);head(x2)
  summary(x2)
  plot(x1,x2)

  # lin.cor()函数，用来计算两个向量之间的线性相关，或计算矩阵的相关矩阵。
  # 函数定义:
  #   1lin.cor(x, y = NULL, method = "pearson", test.na = FALSE)
  # 
  # 参数列表：
  # x,变量1
  # y,变量2，与x进行比较
  # method,相关性算法的名称，默认为pearson距离算法，支持5种算法分为是pearson，pearson2，sq_pearson，kendall，spearman
  # test.na, 检测数据集是否有NA值，不检测为FALSE，计算会快。
  # 相关性计算，最大值1为完全正相关，最小值-1为完全负相关，0为不相关。我们来创建数据集，进行相关性的测试。 
   x1<-runif(100)
   x2<-estimate.probability(x1)
   x3<-rnorm(100)
  
  # 判断x1,x2的相关性，pearson 皮尔森相关系数
   lin.cor(x1,x2) #1
   lin.cor(x1,x3)
   lin.cor(x1,x3,method = "pearson2")
   lin.cor(x1,x3,method = "sq_pearson")
   lin.cor(x1,x3,method = "kendall")
   lin.cor(x1,x3,method = "spearman")
   