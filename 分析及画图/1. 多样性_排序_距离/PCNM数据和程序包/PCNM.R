##2018.5.24

##PCNM
##http://blog.sciencenet.cn/blog-267448-1022075.html
##PCNM（邻体矩阵主坐标分析） 

##我们的代码用的是Imap包。直接pcnm这个函数就出结果

rm(list = ls())
setwd("E:/桌面/R script 2017/PCNM数据和程序包/")
# 载入本章所用的程序包
library(ape)
library(spdep) 
library(vegan)
library(ade4)
# 以下几个程序包可以从https://r-forge.r-project.org/R/?group_id=195下载本地安装
library(packfor)  
library(spacemakeR) 
library(AEM)     ##这个包需要spdep
library(PCNM)    ##这个包需要AEM，packfor
# 导入数据
mite <-read.csv("mite.csv",header=T, row.names=1)  ##t(otu) 转置了的OTU
mite.env <- read.csv("mite_env.csv",header=T, row.names=1)  ##环境因子
mite.xy <- read.csv("mite_xy.csv",header=T, row.names=1)  ##每个样本的坐标（平面直角坐标）
?decostand #hellinger: square root of method = "total" 
#total: divide by margin total (default MARGIN = 1).每个值除以行的和,再求平方根。（每一行是一个样本）即sample/rowSums(sample)

mite.h <- decostand(mite, "hellinger");head(mite.h) #OTU 标准化
mite.xy.c <-scale(mite.xy, center=TRUE, scale=FALSE) #不对坐标进行标准化,只做中心化。即（每个值-平均值）


# 1b. ...或自动构建PCNM变量
# library(PCNM) # 如果还未加载PCNM程序包

xy.d1 <- dist(mite.xy.c) ##转换为矩阵
mite.PCNM.auto <-PCNM(xy.d1)
str(mite.PCNM.auto)
?PCNM  ##pcnm在vegan包
summary(mite.PCNM.auto)
# PCNM变量Moran指数（由第一距离等级0到削减阈值）；也见PCNM（）函数
# 产生的图（此处无显示图）
# Moran指数的期望值（代表无空间相关）
mite.PCNM.auto$expected_Moran
mite.PCNM.auto$Moran_I
# 正空间相关的特征函数
(select <- which(mite.PCNM.auto$Moran_I$Positive == TRUE))
  length(select)  # I > E(I)条件下PCNM变量的数量
mite.PCNM.pos <-as.data.frame(mite.PCNM.auto$vectors)[,select]

# 2.运行基于去趋势甲螨数据的全模型PCNM分析
#------------------------------------------------------------
mite.PCNM.rda <-rda(mite.h, mite.PCNM.pos)
anova.cca(mite.PCNM.rda)
# 3.如果分析为显著，计算校正R2
(mite.R2a <-RsquareAdj(mite.PCNM.rda)$adj.r.squared) #The functions finds the adjusted R-square.
?RsquareAdj

# 甲螨-环境-PCNM变差分解
# *****************************

# 1. 环境变量检验和前向选择
# 将环境变量3-5重新编码成二元变量
substrate <-model.matrix(~mite.env[,3])[,-1]
shrubs <-model.matrix(~mite.env[,4])[,-1]
topo <-model.matrix(~mite.env[,5])[,-1]
mite.env2 <-cbind(mite.env[,1:2], substrate, shrubs, topo)
head(mite.env2)
# 环境变量的前向选择
mite.env.rda <-rda(mite.h, mite.env2)
mite.env.R2a <-RsquareAdj(mite.env.rda)$adj.r.squared;mite.env.R2a
?forward.sel
mite.env.fwd <-forward.sel(mite.h, mite.env2, adjR2thresh=mite.env.R2a,nperm=9999)
mite.env.fwd
env.sign <-sort(mite.env.fwd$order)
env.red <-mite.env2[,c(env.sign)]
colnames(env.red)
env.red
# 3. PCNM变量的前向选择
# 运行未去趋势甲螨数据的全模型PCNM分析
mite.undet.PCNM.rda <-rda(mite.h, mite.PCNM.pos)
anova.cca(mite.undet.PCNM.rda)
# 如果分析表明显著，计算校正R2和运行PCNM变量前向选择
(mite.undet.PCNM.R2a <-RsquareAdj(mite.undet.PCNM.rda)$adj.r.squared)
(mite.undet.PCNM.fwd <-forward.sel(mite.h, as.matrix(mite.PCNM.pos), 
                                   adjR2thresh=mite.undet.PCNM.R2a))
# 根据R2a准则，如果保留12个PCNM变量，获得的校正R2已经稍大于全模
# 型的校正R2。但这个“稍微超过”也是可行，并不一定很严格。
(nb.sig.PCNM <-nrow(mite.undet.PCNM.fwd)) # 显著的PCNM变量的数量
# 按顺序排列显著的PCNM变量
(PCNM.sign <-sort(mite.undet.PCNM.fwd$order))
# 赋予所有显著PCNM变量一个新的对象
PCNM.red <-mite.PCNM.pos[,c(PCNM.sign)]
# 5.甲螨-环境-趋势-PCNM变差分解
(mite.varpart <-varpart(mite.h, env.red, PCNM.red ))
par(mfrow=c(1,2))
showvarparts(2)
plot(mite.varpart,digits=2)
# 检验单独解释部分[a],  [c] 和 [d] 
#********************************
# [a]部分，环境变量单独解释部分
anova.cca(rda(mite.h,env.red, PCNM.red ))
# [b]部分，趋势单独解释部分
anova.cca(rda(mite.h,PCNM.red , env.red))
#仅有环境变量和宽尺度空间变量单独解释部分显著。
