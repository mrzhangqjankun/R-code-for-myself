##li 2017.7.5 
##http://blog.sciencenet.cn/blog-255662-523462.html
##http://blog.csdn.net/yujunbeta/article/details/24142545

rm(list=ls(all=TRUE))

dat <- c(119,120,131,209,210,337,332,287,146,129,232,169,208,253,142,105,419,179,
         324,287,115,132,308,356,286,221,204,
         105,45,245)   ## 30个数据

###########利用循环
hist(dat,col="green")
boot.sample <- list()  #生成一个存储器
#循环1000次，有放回的抽样，每次生成的新样本存储在boot.sample中。
for(i in 1:1000){
  boot.sample[[i]] <- sample(dat,size = 30, replace = TRUE)
}
boot.sample

## 求每个样本的mean,结果为1000个bootstrap样本的mean
boot.mean <- unlist(lapply(boot.sample, mean))  #lapply,对样本boot.sample做求平均值mean的操作。
#unlist()函数的作用，就是将list结构的数据，变成非list的数据，即将list数据变成字符串向量或者数字向量的形式。
## 频数直方图
hist(boot.mean, col = "green",ylim=c(0,250))

## 求95%的置信区间
CI95 <- quantile(boot.mean, probs = c(0.025, 0.975))
CI95
## 在频数直方图上加置信区间
abline(v = CI95, col = "red")  ##v是竖线，h是水平线。

###########利用bootstrap函数
install.packages("bootstrap")
library(bootstrap)
##bootstrap(x,nboot,theta,…, func=NULL)　x：原始抽样数据　　theta：统计量T(如平均值，方差等)　　nboot：构造Bootstrap数据集个数
x=dat
theta <- function(x) {  
  median(x)  
}  
results <- bootstrap(x, 1000, theta)    ##等于13-21行
results$thetastar

boot.mean <- unlist(results$thetastar)  
#unlist()函数的作用，就是将list结构的数据，变成非list的数据，即将list数据变成字符串向量或者数字向量的形式。
## 频数直方图
hist(boot.mean, col = "green",ylim=c(0,250))

## 求95%的置信区间
CI95 <- quantile(boot.mean, probs = c(0.025, 0.975))
CI95
## 在频数直方图上加置信区间
abline(v = CI95, col = "red")  ##v是竖线，h是水


##2019.6.25
#https://mp.weixin.qq.com/s/w7OsbFIssQref7hmoCGkcA
#Bootstrap Method

#用于层次聚类分析的一个例子，使用Pvclust这个包，用于层次聚类，
#并通过multiscale bootstrap resampling给出相应的p-value用于评估聚类结果的不确定性。
#提供了两种p-values，AU（approximately unbiased p-value, 
#通过multiscale bootstrap resampling计算）和
#BP（bootstrap probability, 通过normal bootstrap resampling计算），
#AU比BP较为unbiased。

#Pvclust使用hclust（来自于stats包）进行层次聚类，并自动计算所有的子类的p-value。
library(pvclust)  # loads package
a <- matrix(rnorm(1000), 100, 10, dimnames=list(paste("g", 1:100, sep=""), paste("t",1:10, sep="")))  # creats sample data
at <- t(a);at  # transposes of a
?pvclust  #r=seq(.5,1.4,by=.1)
cl <- pvclust(at, nboot=1000)  #performs hierarchical cluster analysis with multiscale bootstrap with 1000 repetitions.
pvrect(cl, alpha=0.95)  #highlights with red rectangles all clusters in the dendrogram which have an AU value above 95%, AU p-value > 0.95, the hypothesis that “the cluster does not exist” is rejected with significance level 0.05;

#pick significant clusters.
clsig <- pvpick(cl, alpha=0.95, pv="au", type="geq", max.only=TRUE)  

