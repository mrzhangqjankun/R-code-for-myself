##li 2017.7.12 learn
#setwd('D:/文件存放/galaxy pipeline/galaxy/自己整理代码/cca/')

library(vegan)
library(Imap)

#setwd('F:/')
rm(list=ls(all=TRUE))


dist.matrix = read.csv("WS36_Uclust_Bray-Curtis.csv", header=T,row.names=1)#file is downloaded from galaxy


mnds = metaMDS(as.dist(dist.matrix))
mnds$points
plot(mnds, type="t")




