##2018.5.25
##基于 Vegan 软件包的生态学数据排序分析

rm(list=ls())
setwd("E:/桌面/R script 2017/Vegan")
library(vegan)
gtsdata=read.table("gtsdata.txt", header=T) ;head(gtsdata)# t(OTU)。行是样本，列是OTU
gtsenv= read.table("gtsenv.txt", header=T) ;head(gtsenv)  # env

#进行排序分析之前，首先要判断是选择线性模型（PCA 和 RDA） 还是单峰模型(CA和 CCA)的排序方法。
#一般来说，如果物种分布变化大，选择单峰模型效果比较好，反之，线性模型也是不错。
#可以先通过 DCA 分析里面“Lengths of gradient”来判别式选择线性排序还是单峰排序

##################################################################DCA
decorana(gtsdata);?decorana #查看Axi lengths
#如果 DCA 排序前 4 个轴中最大值超过 4，选择单峰模型排序更合适。如果是小于 3， 则选择线性模型更好
#本例最大3.2595，CCA和RDA都可以。

################################################################PCA
gts.pca = rda(gtsdata);gts.pca
#总的特征根（Inertia）为 352.1，这个值也是物种矩阵中各个物种的方
#差和 ，可以理解为物种分布的总变化量， 可以用 apply 函数检验
sum(apply(gtsdata,2,var)) #352.1 。对列（OTU）求方差再求和

#PCA 排序结果中 Eigenvalues for unconstrained axes 表示每个非约束排序轴所负荷的特征根
#的量，也可以表示每个轴所能解释方差变化的量。 例如，对于第一轴而言，
#111.779/352.1=31.7% 便是第一轴对物种分布的解释量，

summary(gts.pca)

plot(gts.pca)
biplot(gts.pca);?biplot ##OTU会有一个箭头

# 此时可以对物种数据进行单位方差标准化（先中心化再标准化后均值为 0，方差为 1）。单位方差化可以通过参数
# scale 来实现。 重新做图，新的排序图比没有标准化之前效果好。
gts.pca = rda(gtsdata,scale = T)
plot(gts.pca,scaling  = 3)
#如果排序图内关注物种之间的关系，可以选择 scaling=1； 如果是关注样方之间
#的关系， scaling=2； 如果是关注样方与物种之间的关系，可以选择 scaling=3
biplot(gts.pca,scaling = 3)

#通过 display 参数分别生成物种和样方排序图，可以用 choices 参数选择所要展示的轴，轴不一定是连续的
?plot.cca
plot(gts.pca,display = "sp",choices = c(1,3)) #sp代表物种。choices=c(1,3)表示第一和第三轴
plot(gts.pca,display = "si")  ##si代表样方,即样本。sites
plot(gts.pca,display = "sp",choices = c(1,3))
plot(gts.pca,display = "sp",choices = c(1,3))

############################################################CA
#CA 和 CCA 在 R 软件里也是同一个函数 cca()来实现，如果括号内只用物种矩阵，就
#表示 CA 分析；如果同时有物种矩阵和环境矩阵，就表示是 CCA 分析：
gts.ca = cca(gtsdata);gts.ca
# PCA以欧氏距离(Euclidern distance)作为排序标准，而 CA 是卡方距离(chi-square distance)进行
#排序。 Total intertia 值可以通过卡方检验验证：
?sum
chisq.test(gtsdata/sum(gtsdata))
plot(gts.ca,scaling = 3)
#对于 CA 的排序图，可以这样解读：如果一个物种(OTU)靠近某个样方(sample)，
#表明该物种可能对该样方的位置起很大的作用
#只在少数样方出现的物种通常在排序空间的边缘，表明它们只是偶然发生，或它们只存在于稀有生境，如米槠（CASCAR）。
#在排序空间中心的物种，可能在取样区域是该物种最优分布区。

############################################################RDA
# PCA 和 CA 都属于间接分析，只能分析一个矩阵的数据。 在分析物种分布与环境因
# 子关系的时需要用直接排序，也就是约束排序（Constrained ordination）。约束排序的主要
# 类型有 RDA 和 CCA，分别是 PCA 和 CA 在排序过程加入环境因子进行线性回归最终得
# 到的排序结果。所以，约束排序与非约束排序的区别不仅在于约束排序图加入环境因子，
# 也在于非约束排序轴展示的总特征根是所有物种分布的变化量（方差）。 而在约束排序里，
# 只展示能被环境因子所解释的物种分布变化量。 从约束排序结果里可以看出，约束排序轴
# 明显比非约束排序轴的解释量小。

gts.rda=rda(gtsdata,gtsenv)
gts.rda=rda(gtsdata~elev+slope+N+K+P,data = gtsenv)#这种优点是可以选择所需要的环境因子进入分析。
gts.rda
# RDA 的 Total Inertia 与 PCA 相同， 但 Constrained Inertia 为 137.4，表明 8 个环境因子对
# 物种分布的解释量为 137.4/352.1=39.02%。 不能解释的物种分布变量为 60.98%。 rank 表示
# 排序轴的数据，本例中有 8 个环境因子，就会有 8 个约束排序轴。

plot(gts.rda,display = c("sp","bp"),scaling=3) #物种与环境因子
#显示样方与环境因子，可以表示为 display=c("si","bp")，如果物种、样方和环境因子同时显示，可以设定
#display=c("sp","si","bp")

#约束排序有偏分析法（Partial methods）， 可以分别分析主环境变量与协变量对物种分布的影响。
#partial RDA
gts.prda = rda(gtsdata,gtsenv[,1:4],gtsenv[,5:8])

#############################################################CCA
gts.cca = cca(gtsdata,gtsenv)
gts.cca

#一般来说单峰模型拟合效果比线性模型好，也反映了物种与环境因子之间的
#复杂关系。 同样， CCA 也有偏分析算法，算法和分析与 pRDA 类似。

###############################检验环境因子与物种分布相关的显著性，可用蒙特卡罗置换检验实现
#在 R 软件中，可用函数 permutest()进行实现
?permutest #The function performs an ANOVA like permutation test
permutest(gts.cca,permu = 999)
anova(gts.cca)  #这套数据两种方法结果相同

##检验每个环境因子的显著性，可以用函数 envfit()
###注意,我们用的是anova。这两种方法差异可能会很大。
?envfit
ef = envfit(gts.cca,gtsenv,permu=999)
ef
#anova
ind.p=array(0,dim=c(1,ncol(gtsenv)))
ind.F=array(0,dim=c(1,ncol(gtsenv)))
for(j in 1:ncol(gtsenv)){
  ind.cca = cca(gtsdata, gtsenv[,j]) 
  ind.sig = anova(ind.cca,step=1000)
  ind.p[1,j] = ind.sig$Pr[1]
  ind.F[1,j] = ind.sig$F[1]
}
ind.p
ind.F

colnames(ind.p) = colnames(gtsenv)

Fp=rbind(ind.F,ind.p)
row.names(Fp)=c("F","p")
Fp
###anova结果和env.fit结果确实不一样
