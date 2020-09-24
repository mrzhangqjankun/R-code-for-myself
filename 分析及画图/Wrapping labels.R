##2018.9.2
###https://mp.weixin.qq.com/s?__biz=MzI5NjUyNzkxMg==&mid=2247483972&idx=1&sn=fa1f4b6299f94eeb52baf5bda6a5ff6b&chksm=ec43b303db343a150806b3b1ab22a1df1c90405235bb399b155f7c50de2a0a23faf2f4ad0beb&mpshare=1&scene=1&srcid=081548jodM08vTUFQtq4mzHy&pass_ticket=GoYZjtRA8BqQpFWA3ZOIh%2Br6yHBRTWjpCZeYRoLsB9fO2qg%2F4%2FywcHGanke2JFjJ#rd
##wrapping labels in ggplot2 


##用stringr包的str_wrap来完成文本自动换行,对图上很长的样本名换行。str_wrap(x, width=10)

rm(list=ls())

# source("https://bioconductor.org/biocLite.R")
# biocLite("clusterProfiler")

library(stringr)
library(ggplot2)
library(clusterProfiler)
data(geneList)
de <- names(geneList)[1:100];de
x <- enrichKEGG(de);x
p <- barplot(x) ;p
p + scale_x_discrete(labels=function(x) str_wrap(x, width=10))

