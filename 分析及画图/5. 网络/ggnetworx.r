##2018.9.7
#ggnetworx：让ggtree支持phylogenetic networks 
#https://mp.weixin.qq.com/s?__biz=MzI5NjUyNzkxMg==&mid=2247486331&idx=1&sn=9c65655bfef45e6577bb99b38c21b365&chksm=ec43ba3cdb34332a7c6f81f5a6a3e587e23336afeccea15dd1e3eabca03ad51bf32bd31ca777&scene=0#rd


rm(list=ls())

## https://github.com/KlausVigo/phangorn
#install.packages("phangorn")

library(phangorn)

?phangorn
library(help = phangorn)
browseVignettes("phangorn")

## https://github.com/KlausVigo/ggnetworx
#devtools::install_github("KlausVigo/ggnetworx")

library(ggplot2)
library(ggnetworx)
?ggnetworx

##
setwd("E:/桌面/test_data/")
phytree = read.tree("FastTree.nwk");phytree  ##FastTree结果文件

summary(phytree)

ggnetworx(phytree) + geom_tiplab2()  ##报错aes不对。
##

data(yeast, package="phangorn")
dm <- phangorn::dist.ml(yeast)    ##转化为矩阵
nnet <- phangorn::neighborNet(dm)  ##Computes a neighborNet from a distance matrix
nnet
ggnetworx(nnet) + geom_tiplab2()
