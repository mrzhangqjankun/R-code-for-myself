#setwd('C:/Users/dell/Desktop/英文文章/数据分析/230-270')
setwd('D:/文件存放/galaxy pipeline/galaxy/自己整理代码/种面积关系')
tar <- read.table("TAR.txt",sep="\t",head=TRUE)
library(ggplot2)
ggplot(tar,aes(Area,Taxa))+
  geom_point(aes(color=factor(Sample)),alpha=0.4,size=5)

ggplot(tar,aes(Area,Taxa))+
  stat_smooth(method="lm",se=FALSE,aes(col=factor(Sample)))+
  theme_classic()
  
