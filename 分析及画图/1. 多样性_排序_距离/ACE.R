###lii2017.8.31
### fossil packages
setwd('E:/桌面/Btrim loose/resample-24000')
rm(list=ls(all=TRUE))

f_dat = read.table("Galaxy62-resample_UPARSE_otu.txt",sep="\t",head=TRUE,row.names=1)
library("fossil")
f=t(f_dat)
ACE(f_dat,taxa.row = TRUE)
ICE(f_dat)




##########################################fossil
?fossil #古生态与古地理分析工具

###几种外推物种数量的方法
#chao1基于abundance;chao2基于incidence，会自动先转化为0-1数据。chao2可计算理论物种数量。
#ACE,ICE默认行为OTU，列为样本。ACE只用于abundance数据；ICE基于incidence。会自动先转化为0-1数据。
#jack1和jack2计算1阶和2阶的jacknife数。也是外推的物种数。默认abund = TRUE计算abundance数据。abund = FALSE 计算0-1数据。
#bootstrap。默认abund = TRUE计算abundance数据。abund = FALSE 计算0-1数据。
a<-c(0,5,1,1,2,0,0,1,0,0,8,45)
ACE(a)
ICE(a)
chao1(a)

a<-matrix(c(0,5,1,1,2,0,0,1,0,0,8,45),4,3)
ACE(a)
ICE(a)
chao1(a)
chao2(a)
bootstrap(a)
bootstrap(a,,FALSE)  ##注意两个逗号才是abund。bootstrap(x, taxa.row = TRUE, abund = TRUE, samples = NA)

## presence absence matrix
a<-matrix(c(0,1,1,1,1,0,0,1,0,0,1,1),4,3)
chao1(a)
chao2(a)
chao.sd(a) ##计算sd
ACE(a)
ICE(a)
jack1(a)
jack1(a,abund = FALSE)
jack2(a)
jack2(a,abund = FALSE)
bootstrap(a,,FALSE)
bootstrap(a,,TRUE)

#int.chao 计算chao1或chao2.有时候经典算法会出现NaN.这个函数可以避免
a<-c(4,5,1,1,2,0,0,1,3,0,8,45,23)
int.chao(a)



##aic.nest(comm1, comm2, base=exp(1)) 基于AIC，计算两个样本是否来源于一个群落。计算nestedness
#base,base of the log used in the calculation of Shannon's diversity
#for example, two different communities
a<-c(12,4,12,1,4,0,6,5,0,0,0)
b<-c(0,11,4,3,6,7,7,2,23,5,8)
#if the aic score is lower, it is the better model
aic.nest(a,b)   ##AIC越低越好
#from the same community
a<-c(5,6,5,6,5,6,5,6,5,2,1,1)
b<-c(2,3,2,3,2,3,2,3,2,1,0,0)
aic.nest(a,b)

##chao.sorenson(x, y);chao.jaccard(x, y).x和y的OTU顺序必须完全一样。
##Species counts from two different locations
a <- c(1,0,4,3,5,0,0,7)
b <- c(2,1,3,0,0,1,0,6)
chao.sorenson(a,b)  ##返回结果为相似性指数。
chao.jaccard(a,b)

##rclust样本聚类。
#rclust(dist, clusters = 2, runs = 1000, counter = FALSE)
#dist,成对距离矩阵。clusters聚成几类。runs运行次数。counter是否显示运行到哪了。
data(fdata.mat)
fd.dist <- dino.dist(fdata.mat)  #计算距离矩阵。默认sorenson
rclust(fd.dist, clusters = 2, runs = 10, counter = TRUE)

##经纬度的TAR。lats是经纬度文件。spp是OTU
##sac(lats, spp)
#fdata species/area relationship
data(fdata.lats)
data(fdata.mat)
a<-sac(fdata.lats, fdata.mat)
plot(log(a$areavsspp))

##模拟TAR。
#sim.occ(total.species = 100, endemics = 0.1, regions = 3, locs = 30, avg.abund = 1)
#total.species总物种数。endemics整个地区特有物种比例.avg.abund样本的平均丰度
sim.occ(regions=2, locs=5)


##spp.test 估计物种多样性
#spp.est(x, rand = 10, abund = TRUE, counter = FALSE, max.est = 'all')
#rand计算次数。counter计算过程是否显示。max.est参与计算的样本数
#abundance数据：计算richness,chao1,ACE,jack1及95%上下区间
#incidince数据：计算richness,chao2,ICE,jack1及95%上下区间
#abundance example with sample data set
data(fdata.mat)
spp.est(fdata.mat, abund = TRUE, counter = TRUE)

#occurrence example with sample data set
data(fdata.mat)
spp.est(fdata.mat, abund = FALSE, counter = FALSE)


#######计算置信区间
http://blog.sina.com.cn/s/blog_633bf5c10100z73d.html
t.test(fdata.mat)

# 如果要求任意置信度下的置信区间的话，就需要自己编一个函数了。
# 当然，有两点要记住的，置信区间的计算在知道方差和不知道方差的情况下，计算公式是不一样的。
# 下面做一个两种情况下都可以用的函数。
confint<-function(x,sigma=-1,alpha=0.05)
{
  n<-length(x)
  xb<-mean(x)
  if(sigma>=0)
  {
    tmp<-sigma/sqrt(n)*qnorm(1-alpha/2);df<-n
  }
  else{
    tmp<-sd(x)/sqrt(n)*qt(1-alpha/2,n-1);df<- n-1
  }
  data.frame(mean=xb,df=df,a=xb-tmp,b=xb+tmp)
}
# 这个函数的使用：
# 如果不知道方差，则confint（x,alpha）  知道方差，则confint（x,sigma,alpha)
# 这样就能计算出结果了。
confint(fdata.mat)

https://www.jianshu.com/p/843109d0ad5a

