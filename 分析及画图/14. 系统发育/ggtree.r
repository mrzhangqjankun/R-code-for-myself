##2018.8.16

##五彩进化树与热图更配-ggtree美颜进化树(宏基因组扩增子) 

##https://mp.weixin.qq.com/s?__biz=MzUzMjA4Njc1MA%3D%3D&mid=2247484203&idx=1&sn=a8fda0759648607a24af8b63c400dc2e&scene=45#wechat_redirect


# Install related packages

# Work well in R3.3.3

if (FALSE){
  
  source("https://bioconductor.org/biocLite.R")
  
  biocLite(c("ggtree","colorspace"))
  
}



## Basic plotting stuff

# Set working enviroment in Rstudio, select Session - Set working directory - To source file location, default is runing directory

rm(list=ls()) # clean enviroment object

setwd(system("pwd", intern = T))

setwd("result")

library("ggtree")

library("colorspace")



tree <- read.tree("tax_rep_seqs.tree")

tax <- read.table("tax_rep_seqs.tax",row.names=1)

colnames(tax) = c("kingdom","phylum","class","order")





groupInfo <- split(row.names(tax), tax$phylum) # OTU and phylum for group

tree <- groupOTU(tree, groupInfo)

pdf(file="ggtree_phylum.pdf", width=8, height=8)

ggtree(tree, layout="fan", ladderize = FALSE, branch.length = "none",aes(color=group))+
  
  scale_color_manual(values=c(rainbow_hcl(length(unique(tax$phylum))+1)), breaks=1:length(unique(tax$phylum)), labels=levels(tax$phylum))+
  
  theme(legend.position = "right") +geom_tiplab2(size=3)

dev.off()

png(file="ggtree_phylum.png", width=8, height=8, units = "in", res = 300)

ggtree(tree, layout="fan", ladderize = FALSE, branch.length = "none",aes(color=group))+
  
  scale_color_manual(values=c(rainbow_hcl(length(unique(tax$phylum))+1)), breaks=1:length(unique(tax$phylum)), labels=levels(tax$phylum))+
  
  theme(legend.position = "right") +geom_tiplab2(size=3)

dev.off()







groupInfo <- split(row.names(tax), tax$class) # OTU and class for group

tree <- groupOTU(tree, groupInfo)

pdf(file="ggtree_class.pdf", width=8, height=8)

ggtree(tree, layout="fan", ladderize = FALSE, branch.length = "none",aes(color=group))+
  
  scale_color_manual(values=c(rainbow_hcl(length(unique(tax$class))+1)), breaks=1:length(unique(tax$class)), labels=levels(tax$class))+
  
  theme(legend.position = "right") +geom_tiplab2(size=3)

dev.off()

png(file="ggtree_class.png", width=8, height=8, units = "in", res = 300)

ggtree(tree, layout="fan", ladderize = FALSE, branch.length = "none",aes(color=group))+
  
  scale_color_manual(values=c(rainbow_hcl(length(unique(tax$class))+1)), breaks=1:length(unique(tax$class)), labels=levels(tax$class))+
  
  theme(legend.position = "right") +geom_tiplab2(size=3)

dev.off()







groupInfo <- split(row.names(tax), tax$order) # OTU and order for group

tree <- groupOTU(tree, groupInfo)

pdf(file="ggtree_order.pdf", width=8, height=8)

ggtree(tree, layout="fan", ladderize = FALSE, branch.length = "none",aes(color=group))+
  
  scale_color_manual(values=c(rainbow_hcl(length(unique(tax$order))+1)), breaks=1:length(unique(tax$order)), labels=levels(tax$order))+
  
  theme(legend.position = "right") +geom_tiplab2(size=3)

dev.off()

png(file="ggtree_order.png", width=8, height=8, units = "in", res = 300)

ggtree(tree, layout="fan", ladderize = FALSE, branch.length = "none",aes(color=group))+
  
  scale_color_manual(values=c(rainbow_hcl(length(unique(tax$order))+1)), breaks=1:length(unique(tax$order)), labels=levels(tax$order))+
  
  theme(legend.position = "right") +geom_tiplab2(size=3)

dev.off()

########################
#2019.6.28
#https://mp.weixin.qq.com/s/0WGS6b11F1Ul0ZkceKYOvQ
#倒不如画一些渐变色的线条吧！

#我们一般给树上颜色，画所谓的热树（heat-tree）用的是子节点的状态来给线条上色，
#这个需求是线条的颜色从父节点给子节点是渐变的，体现出从祖先节点到当前物种的一个进化转变过程。
library(ggtree)
anole.tree<-read.tree("http://www.phytools.org/eqg2015/data/anole.tre")

svl <- read.csv("http://www.phytools.org/eqg2015/data/svl.csv",row.names=1)
svl <- as.matrix(svl)[,1]
fit <- phytools::fastAnc(anole.tree,svl,vars=TRUE,CI=TRUE)

#这里使用phytools的fastAnc进行祖先节点的状态估计，我们设vars = TRUE和CI = TRUE，这样它会把方差和置信区间也估算出来。
td <- data.frame(node = nodeid(anole.tree, names(svl)),
                 trait = svl)
nd <- data.frame(node = names(fit$ace), trait = fit$ace)

d <- rbind(td, nd)
d$node <- as.numeric(d$node)

#那么我们把这个叶子节点的测量数据和祖先节点的估算数据整合在一个数据框里。然后使用一个full_join方法，就可以把数据框整合到树对象中：
library(tidyverse)
#update.packages()
tree <- full_join(anole.tree, d, by = 'node')

#这会报错：Error in UseMethod("full_join") : 
#no applicable method for 'full_join' applied to an object of class "phylo"
#没有解决

#传入新的参数continuous = TRUE，ggtree就知道你画的热树原来是要渐变的，然后它就会干所有该干的事情。
ggtree(tree, aes(color=trait), layout = 'circular', 
       ladderize = FALSE, continuous = TRUE, size=2) +
  scale_color_gradientn(colours=c("red", 'orange', 'green', 'cyan', 'blue')) +
  geom_tiplab2(hjust = -.1) + xlim(0, 1.2) + theme(legend.position = c(.05, .85))

#有祖先状态的树，用来画2维树就正好，这种也有个名字叫phenogram，把表型数据投射到y轴上，
#ggtree画起来毫无压力。显然对于体重的进化，从上图是比较难以看出什么门道来的，但是下图，却异常地清晰了。
#没有什么图是完美的，但有些图能够帮助我们解释某些问题，而ggtree提供了画各种图的可能性。
ggtree(tree, aes(color=trait), continuous = TRUE, yscale = "trait") + 
  scale_color_viridis_c() + theme_minimal()