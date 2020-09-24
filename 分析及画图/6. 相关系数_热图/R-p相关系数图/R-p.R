##li 2017.7.5 
##http://www.omicshare.com/forum/thread-2515-1-1.html
##[R语言] R语言--如何画相关系数图？

rm(list=ls(all=TRUE))
setwd('D:/文件存放/galaxy pipeline/galaxy/自己整理代码/R-p相关系数图')
#install.packages("corrplot")
a <- read.table("R.txt",header=T,row.names= 1) ##相关系数R值，有标题行和列

b<-read.table("p.txt",header=T,row.names= 1)   ##相关系数P值，有标题行和列

pmat=apply(b,2,as.numeric) #apply函数。1为行，2为列。对矩阵b的列进行as.numeric操作。字符转换为数字
pmat
d=apply(a,2,as.numeric)
d
rownames(d)<-rownames(a)

library(corrplot)

corrplot(d,method="ellipse",mar=c(0,0,0,0),tl.col="black",addCoef.col="black",
         tl.srt=0,tl.cex=1,cl.pos="n", p.mat=pmat,sig.level=0.05,insig=c("blank"),
         number.cex=1,number.font=4)
## method 包括“circle”, “square”, “ellipse”, “number”, “shade”, “color”, “pie”


