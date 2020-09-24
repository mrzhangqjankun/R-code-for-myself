##2018.4.5
##https://mp.weixin.qq.com/s?__biz=MzIyNzgyNDAxMg==&mid=2247485276&idx=1&sn=ecbb6d4f10a6d98dc0c740756abf5571&chksm=e85a1fbfdf2d96a90c2d8781ad19029806e50176de45e135116c70274b420046f777877b0ca5&mpshare=1&scene=1&srcid=0404KVU5NkavVDNR0lt5QMhM&pass_ticket=8j2pu3Xu8tOeCA0lOUJcl33DPLov1JIkEwh5hdSeTNzTwIxnM5W3DMSSbQMfcAx%2B#rd
##超过六个元素的韦恩图你就玩不转了？ 

##https://www.jianshu.com/p/324aae3d5ea4
##UpSetR：集合可视化神包

install.packages("UpSetR")
rm(list=ls(all=TRUE))

library(UpSetR)
??UpSetR
#“visualizes set intersections in a matrix layout and introduces aggregates based on groupings and queries”。
#简单来讲就是矩阵数据中每个维度数据之间交集的可视化表达工具。
#Depending on how the featuers are selected, UpSet can display between 25-65 sets and between 40-100 intersections.

# example of list input (list of named vectors)

listInput <- list(one = c(1, 2, 3, 5, 7, 8, 11, 12, 13), two = c(1, 2, 4, 5, 10), three = c(1, 5, 6, 7, 8, 9, 10, 12, 13))
listInput

upset(fromList(listInput))


#UpSetR自带的示例文件（mutations.csv）能够很方便地帮助我们学习其支持的文件输入格式。
#该文件存储以逗号分隔的矩阵数据，包含100个gene的283个特性，其中“1”表示该基因有此特性，
#“0”表示该基因无此特性。UpSetR会计算全部基因中特性为1的交集个数。示例代码如下：

mutations <- read.csv(system.file("extdata", "mutations.csv", package = "UpSetR"), header = T, sep = ",")
head(mutations)
#write.csv(mutations,"mutation.csv", sep = ",")
upset(mutations)

#我们指定了6个待交互的元素，并设置颜色为红色，同时令交互个数为0的组合也显示出来，并按照交集个数由大到小排列。
upset(mutations, sets = c("PTEN", "TP53", "EGFR", "PIK3R1", "RB1", "SPTA1"), sets.bar.color = "red",order.by = "freq", empty.intersections = "on")


##官网
#http://caleydo.org/tools/upset/

##github
#https://github.com/hms-dbmi/UpSetR
#https://github.com/VCG/upset/wiki

##注意，输入文件格式，逗号分隔的csv
Row;A;B;C
R1;1;0;0
R2;0;1;0
R3;0;0;1

setwd("E:/桌面")
mutations <- read.csv("111.csv" , header = T, sep = ",")
head(mutations)

upset(mutations)

##2018.6.20
##CS6: ChIPseeker的可视化方法（中秋节的视觉饕餮）
##https://mp.weixin.qq.com/s?__biz=MzI5NjUyNzkxMg==&mid=2247484887&idx=1&sn=899efd5476443f9c358b0b81214588c5&scene=21#wechat_redirect
source("https://bioconductor.org/biocLite.R")
biocLite("ChIPseeker")

library(ChIPseeker)  
require(TxDb.Hsapiens.UCSC.hg19.knownGene) 
txdb <- TxDb.Hsapiens.UCSC.hg19.knownGene 
peakfile <- system.file("extdata", "sample_peaks.txt", package="ChIPseeker") 
peakAnno <- annotatePeak(peakfile, tssRegion=c(-3000, 3000), TxDb=txdb) 
peakAnno 


plotAnnoPie(peakAnno)
vennpie(peakAnno)
upsetplot(peakAnno)
upsetplot(peakAnno, vennpie=TRUE)


##物种venn图
setwd("E:/桌面/test.data")
mut <- read.csv("genus.csv" , header = T, sep = ",") ##注意，列是样本，行是物种。一定要先转化为0和1再做。
##注意不要数字开头，不能有-和_，_会显示为点
head(mut)
upset(mut,order.by = "freq", empty.intersections = "on")

#upset(mu, order.by = c("freq", "degree"), decreasing = c(TRUE,FALSE))

upset(mut, sets = c("A_e", "A_c", "B_e", "B_c", "C_e", "C_c","BC_e","BC_c"), sets.bar.color = "red",order.by = "freq", empty.intersections = "on")

###2019.1.8
#一些样本中共有的线变成红色
##mb.ratio：控制上方条形图以及下方点图的比例。#order.by：如何排序，这里 freq 表示从大到小排序展示，其他选项有 degree 以及先按 freq 再按 degree 排序。
p1 = upset(mut, sets = c("A_e", "A_c", "B_e", "B_c", "C_e", "C_c","BC_e","BC_c"),
           # number.angles = 30, 
           point.size = 2, line.size = 1,
           mainbar.y.label = "OTU", sets.x.label = "OTU Per Treatment",
           text.scale = c(2, 2, 1.5,1.5, 1.5, 1.5),mb.ratio = c(0.7, 0.3),
           order.by = "freq",keep.order = TRUE,
           queries = list(list(query = intersects, params = 
                                 list("A_e", "A_c", "B_e", "B_c", "C_e", "C_c","BC_e","BC_c"), color = "red", active = T)))

p1


###最多画到7元的venn
##https://mp.weixin.qq.com/s?__biz=MzI4OTc0OTI2NA==&mid=2247485379&idx=2&sn=dc0e77d58e2d8c3f3a813598a5b04997&chksm=ec2b27bcdb5caeaa05a1a356693a0f6d6c8d6854607f6f706ccd56a9acf43e31ce5ffd78b84f&mpshare=1&scene=1&srcid=0601cHamG11G4I5iJ3FuGpK1&pass_ticket=kMch7gBgGPki75NP%2FtSwMEnqtGbtXcK57CtBKlnyuiGs2XI5Nxalc9ga6qh%2BSua4#rd

install.packages('venn')
library(venn)
?venn
mut2 <- read.csv("genus.csv" , header = T, sep = ",",row.names=1)
head(mut2)
mut2=mut2[,1:7]
venn(mut2,zcolor='style')


##几种维恩图的绘制方法 
##https://mp.weixin.qq.com/s?__biz=MjM5OTcyNjU2OQ==&mid=2651432101&idx=1&sn=cb36e3ca839cf56e2ef0cdac109c99bf&chksm=bcca18238bbd913571bf3776b09792cb1db0db1f80e8f9aeac370044af53a26fd97675707b81&scene=0#rd

##2019.1.8
#扩增子16s核心OTU挑选-基于otu_table的UpSet和韦恩图 
https://mp.weixin.qq.com/s/d89-m1EphUcFK4WIPHwc1A