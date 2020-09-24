##li 2017.7.12 learn
setwd('D:/文件存放/galaxy pipeline/galaxy/自己整理代码/cca/')
rm(list=ls(all=TRUE))

library(vegan)
##setwd("D:/Kai/Desktop/RDA")
##rm(list=ls(all=TRUE))

otu=read.table("OTU.txt", header=T, row.names = 1)
out=t(otu)

env=read.table("Env factor2017.6.28.txt", header=T, row.names = 1)

otu.rda=rda(out,env)

otu.rda
summary (otu.rda)
