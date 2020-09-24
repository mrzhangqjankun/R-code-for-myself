##li 2017.7.5 
##http://blog.sciencenet.cn/home.php?mod=space&uid=651374&do=blog&quickforward=1&id=1007083
##在R中正确运行ANOSIM——样品组间差异显著性检测及注意事项


##Analysis of similarities (ANOSIM) is a non-parametric statistical test widely used 
##in the field of ecology.  The test was first suggested by K. R. Clarke as an ANOVA-like test,
##where instead of operating on raw data, operates on a ranked dissimilarity matrix.

##ANOSIM（Analysis ofsimilarities）等可用于检验样品组间（不是种间）的差异显著性。
##比如对多组数据进行聚类分析后得到3个大类，但是想知道这3个大类直接的差异是否显著，
##可用ANOSIM方法（但一般情况更推荐用PREMANOVA方法）。

rm(list=ls(all=TRUE))
setwd('D:/文件存放/galaxy pipeline/galaxy/自己整理代码/anosim')

otu <- read.table("resample_OTU.txt",header=T,row.names= 1) 
#otu
group<-read.table("group file.txt",header=T,row.names= 1) ##必须有表头，且名字和anosim中的一致。（group）
#group

library("vegan")
distance.bray<-vegdist(t(otu),method = 'bray')  ##注意要转置
distance.bray
#is.na(distance.bray)
#distance.bray=as.data.frame(distance.bray)  ##报错 不能把""dist""类别强迫变成数据框
hclust.fish<-hclust(distance.bray,method = "average")
plot(hclust.fish)

##anosim
#1
#anosim.result<-anosim(distance.bray,group$group,permutations = 999) ##直接跑报错NAs are not allowed in subscripted assignments

#2
#x <- data.matrix(distance.bray)    
#x
#which(is.na(x)==T)
#anosim.result<-anosim(x,group$group)  ##报错Error in matrix(nrow = N, ncol = N) : non-numeric matrix extent

#3
#y=unlist(x)
#anosim.result<-anosim(y,group$group)  ##报错Error in matrix(nrow = N, ncol = N) : non-numeric matrix extent
  
#4
#z=as.numeric(x)
#anosim.result<-anosim(z,group$group)  报错'x' must be an array of at least two dimensions。做anosim必须是数组。

##上面一直出错是因为group文件没有表头，所以group$group找不到这个变量。加上就可以了。

anosim.result<-anosim(distance.bray,group$group,permutations = 99)
summary(anosim.result)


##traceback() 


