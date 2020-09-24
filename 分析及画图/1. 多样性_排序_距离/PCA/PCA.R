##li 2017.7.5 
##http://www.omicshare.com/forum/thread-147-1-1.html
##[R语言] PCA分析代码分享

rm(list=ls(all=TRUE))


#-------------------------------------
# name: pca.r
# copyright: genedenovo
# ‘#’号后面为注释信息
#------------------------------------
#install.packages("gmodels")
library(gmodels)
library(ggplot2)

# 设置好工作路径
setwd('D:/文件存放/galaxy pipeline/galaxy/自己整理代码/PCA')
## 输入
# input file name **
inname = "all.rpkm.tsv"

# out PCA figure name ** 
outname = "lychee.PCA.png"

# define the color for points  **
group <- c("0h","0h","5h","5h","10h","10h")

## step 1: 数据的读取和处理
# read the expr data
expr <- read.table(inname, header=T, row.names=1)

# transpose the data
data <- t(as.matrix(expr))  ##转置后时间为行，基因名为列。为什么需要转置？
data
## step2：PCA分析
# do PCA 
data.pca <- fast.prcomp(data,scale=T)  ##对给定的矩阵做PCA。gmodels包中的方法。
data.pca
#data.pca <- fast.prcomp(data,retx=T,scale=T,center=T)    # 启用这句话，则将对数据进行归一化

## step3： PCA结果解析
# fetch the proportion of PC1 and PC2
a <- summary(data.pca)
tmp <- a[4]$importance
pro1 <- as.numeric(sprintf("%.3f",tmp[2,1]))*100
pro2 <- as.numeric(sprintf("%.3f",tmp[2,2]))*100

# 将成分矩阵转换为数据框
pc = as.data.frame(a$x) 
##pc = as.data.frame(a)   #没有$x会报错不能把""summary.prcomp""类别强迫变成数据框。

# 给pc的数据框添加名称列和分组列（用来画图）
pc$group = group
pc$names = rownames(pc)

## step 4: 绘图
# draw PCA plot figure
xlab=paste("PC1(",pro1,"%)",sep="") 
ylab=paste("PC2(",pro2,"%)",sep="")
pca=ggplot(pc,aes(PC1,PC2)) + 
  geom_point(size=3,aes(shape=group,color=group)) + 
  geom_text(aes(label=names),size=4)+labs(x=xlab,y=ylab,title="PCA") + 
  geom_hline(yintercept=0,linetype=4,color="grey") + 
  geom_vline(xintercept=0,linetype=4,color="grey") + 
  theme_bw()

# 保存结果
ggsave(outname,pca,width=10,height=8)
