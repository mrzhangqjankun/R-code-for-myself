##2019.12.2

##K-PCA，ICA

##https://mp.weixin.qq.com/s/5q00nF6U8vfqZD3xSjr8hA

# 主成分分析（PCA），是一种常用的数据分析方法。PCA通过线性变换将原始数据变换为一组各维度线性无关的表示，可用于取主成分（主要信息），摒弃冗余信息（次要信息），常用于高维数据的降维。本质是将方差最大的方向作为主要特征，并且在各个正交方向上将数据“离相关”，也就是让它们在不同正交方向上没有相关性。主要应用于高斯分布的线性数据的降维。
# 核主成分分析（K-PCA），是PCA的升级版主要是解决线性数据的限制，它可以将非线性可分的数据转换到一个适合对齐进行线性分类的新的低维子空间上。其本质同PCA。
# 独立成分分析（ICA），指在只知道混合信号，而不知道源信号、噪声以及混合机制的情况下，分离或近似地分离出源信号的一种分析过程；是盲信号分析领域的一个强有力方法，也是求非高斯分布数据隐含因子的方法
# ICA与PCA区别：
# 1） PCA是将原始数据降维并提取出不相关的属性，而ICA是将原始数据降维并提取出相互独立的属性。
# 
# 2） PCA目的是找到这样一组分量表示，使得重构误差最小，即最能代表原事物的特征。ICA的目的是找到这样一组分量表示，使得每个分量最大化独立，能够发现一些隐藏因素。由此可见，ICA的条件比PCA更强些。
# 
# 3） ICA要求找到最大独立的方向，各个成分是独立的；PCA要求找到最大方差的方向，各个成分是正交的。
# 
# 4） ICA认为观测信号是若干个统计独立的分量的线性组合，ICA要做的是一个解混过程。而PCA是一个信息提取的过程，将原始数据降维，现已成为ICA将数据标准化的预处理步骤。

#KPCA的实现需要安装包BKPC,其中的kPCA函数可以实现核主成分分析。
library(BKPC);?kPCA

data(iris)
testset <- sample(1:150,20)
train <- as.matrix(iris[-testset,-5])# 训练数据的生成
test <- as.matrix(iris[testset,-5]) # 测试数据的生成
kpcData <- kPCA(train) #模型的构建
pairs(kpcData$KPCs[ , 1 : 3], col =iris[-testset, 5])# 前三个主成分的可视化结果

KPCpred1 <- predict(kpcData, test)#模型预测
pairs(KPCpred1[ , 1 : 3], col =iris[testset, 5])#预测结果的展示

#当然还有一个更加完善的R包kernlab，它可以进行各种PCA模型的构建。其核心函数是kpca。
library(kernlab);?kpca
data(iris)
test <- sample(1:150,20)
kpc <-kpca(~.,data=iris[-test,-5],kernel="rbfdot",
           kpar=list(sigma=0.2),features=2)

pcv(kpc)#获取模型结果

plot(rotated(kpc),col=as.integer(iris[-test,5]),
     xlab="1st Principal Component",ylab="2nd PrincipalComponent")#可视化结果

emb <- predict(kpc,iris[test,-5])#模型的预测
points(emb,col=as.integer(iris[test,5]))#结果展示



#ICA的实现可以借助R包fastICA。其主要的函数是fastICA
library(fastICA);?fastICA

# 其中主要的是alg.typ,其分为两类，一个是将所有元素一起提取独立因素（parallel）；另一种是分别提取独立因素（deflation）。
# 下面我们看下两种情况的实例：
# 
# Parallel：
S <- matrix(runif(10000), 5000, 2)
A <- matrix(c(1, 1, -1, 3), 2, 2, byrow= TRUE)
X <- S %*% A

a <- fastICA(X, 2, alg.typ ="parallel", fun = "logcosh", alpha = 1,
             method = "C", row.norm = FALSE, maxit = 200,
             tol = 0.0001, verbose = TRUE)

par(mfrow = c(1, 3))
plot(a$X, main = "Pre-processeddata")
plot(a$X %*% a$K, main = "PCAcomponents")
plot(a$S, main = "ICAcomponents")

#Deflation：
if(require(MASS)){
  x <- mvrnorm(n = 1000, mu = c(0, 0),Sigma = matrix(c(10, 3, 3, 1), 2, 2))
  x1 <- mvrnorm(n = 1000, mu = c(-1, 2),Sigma = matrix(c(10, 3, 3, 1), 2, 2))
  X <- rbind(x, x1)
  
  a <- fastICA(X, 2, alg.typ ="deflation", fun = "logcosh", alpha = 1,
               method = "R", row.norm = FALSE, maxit = 200,
               tol = 0.0001, verbose = TRUE)
  
  par(mfrow = c(1, 3))
  plot(a$X, main = "Pre-processeddata")
  plot(a$X %*% a$K, main = "PCAcomponents")
  plot(a$S, main = "ICAcomponents")
}
