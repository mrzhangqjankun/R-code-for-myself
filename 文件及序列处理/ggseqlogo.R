##2018.3.14

##sequence logo图用来可视化一段序列某个位点的保守性，据根提供的序列组展示位点信息。
##这方面有很多在线小工具可以完成，这里使用R包ggseqlogo进行可视化。

##https://mp.weixin.qq.com/s?__biz=MzA3MTM3NTA5Ng==&mid=2651057758&idx=1&sn=a0791c47b6b88faa8d1d6cd6dc71831b&chksm=84d9cfc9b3ae46dfc19ed78c8e6590f5216ca6d8f30bb2b0a83123f3c97507967f7a22862c3e&scene=0#rd

rm(list=ls(all=TRUE))

install.packages("ggseqlogo")

library(ggplot2)
library(ggseqlogo)
?ggseqlogo
#加载数据
data(ggseqlogo_sample)

#ggseqlogo_sample数据集是一个列表，里面包含了三个数据集：

#•seqs_dna:12种转录因子的结合位点序列
#•pfms_dna:四种转录因子的位置频率矩阵
#•seqs_aa:一组激动酶底物磷酸化位点序列
head(seqs_dna)[1]
head(pfms_dna)[1]
head(seqs_aa)[1]

##可视化
ggplot()+geom_logo(seqs_dna$MA0001.1)+theme_logo()
ggseqlogo(seqs_dna$MA0001.1)

p1 <- ggseqlogo(seqs_dna$MA0001.1, method="bits")

p2 <- ggseqlogo(seqs_dna$MA0001.1, method="prob")

#install.packages("gridExtra")
gridExtra::grid.arrange(p1,p2)


https://mp.weixin.qq.com/s?__biz=MzI5MTcwNjA4NQ%3D%3D&mid=2247485823&idx=1&sn=becc9729ed8daa76f50cf4274c79a1fb&scene=45#wechat_redirect
