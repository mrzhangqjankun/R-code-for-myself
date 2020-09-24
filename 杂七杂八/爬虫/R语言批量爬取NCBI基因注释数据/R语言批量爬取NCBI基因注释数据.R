##2018.5.26
##R语言批量爬取NCBI基因注释数据 
##https://mp.weixin.qq.com/s?__biz=MzAwMDY0MzQ0Ng==&mid=2247484004&idx=1&sn=43ee22a2f2c75ba98d05f441de5a0868&chksm=9ae49a7dad93136b8a0a80503cd3afa579054824099c6268761d92397844abd9c03621b1ed37&mpshare=1&scene=1&srcid=0520B1yQmzBwle87lbdp6iGZ&pass_ticket=a%2FDZKnr7xDnM7txpzaFEK%2BPJtwU6W9%2BkB4QMG1yU6Lht0mbLenrBKIaxi9BbMLim#rd

#使用R爬取NCBI人类基因信息流程如下：
rm(list = ls())
setwd("E:/桌面/R script 2017/R语言批量爬取NCBI基因注释数据/")
library(RCurl)
library(stringr)
library(XML)

library(clusterProfiler)

genes <- read.table("test_genes.txt",header = T,stringsAsFactors = F)
