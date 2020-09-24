## 2017.7.6 li
##http://blog.sciencenet.cn/blog-651374-988817.html
##用R作“系统（层次）聚类分析”


rm(list=ls(all=TRUE))

#------------第一步：导入数据 ex.-------------------------------------------------
data(dune)

#-----------第二步：将原始转换成“距离”矩阵  #假设数据不需要进行标准化--------
library(vegan)
distance.ex<-vegdist(dune,method="euc",na.rm=TRUE)

#计算距离的method （Dissimilarity index）包括：

#"manhattan", "euclidean", "canberra","bray", "kulczynski", "jaccard", "gower", 
##"altGower", "morisita", "horn","mountford", "raup" , "binomial", "chao", "cao" or "mahalanobis".

#其中"bray"是指 "Bray–Curtis Dissimilarity index"



#-----------第三步：聚类分析--------------------------------------------------------

hclust.ex <- hclust(distance.ex,method="ward.D2")
hclust.ex
#聚类的方法包括：

#"ward.D", "ward.D2", "single", "complete", "average" (= UPGMA), "mcquitty"(= WPGMA), "median" (= WPGMC) or "centroid" (= UPGMC).

# 注意一般软件ward算法相对应的hclust中为ward.D2，小心用错

# agnes(*, method="ward") corresponds to hclust(*, "ward.D2").

#-----------第四步：作树状图--------------------------------------------------------

plot(hclust.ex,hang=-1)    # hang取负数时，树状图y轴 从0 开始。

#---------------注意点：------------------------

#（1）聚类方法"centroid" 相对应使用的距离为平方欧式距离 squared Euclidean distances.   如：hc1ust.centroid <- hclust(dist(cent)^2, method = "cen")

#（2）聚类方法"ward.D2" 相对应使用的距离为欧式距离 "Euclidean" distances.

#（3）聚类方法"average"(=UPGMA) 相对应使用的距离为 "bray"(=Bray-Curtis) distances.


###2018.12.4
#何晴数据

setwd("E:/桌面")
a = read.table(file="otu-table-revise.txt",header =T,row.names=1,sep="\t")

library(vegan)
distance.ex<-vegdist(t(a),method="bray",na.rm=TRUE)
#"manhattan", "euclidean", "canberra","bray", "kulczynski", "jaccard", "gower", 
##"altGower", "morisita", "horn","mountford", "raup" , "binomial", "chao", "cao" or "mahalanobis".

hclust.ex <- hclust(distance.ex,method="average")
hclust.ex
#"ward.D", "ward.D2", "single", "complete", "average" (= UPGMA), 
#"mcquitty"(= WPGMA), "median" (= WPGMC) or "centroid" (= UPGMC).

plot(hclust.ex,hang=-1)    # hang取负数时，树状图y轴 从0 开始。

?hclust


########2019.8.4
#ggtree做hclust  ggtree画层次聚类 
#https://mp.weixin.qq.com/s/6LYi6pfLX_-ybbOY94i3hA

hc <- hclust(dist(mtcars))
hc
plot(hc,hang=-1) 

#生成的hclust对象描述了聚类的过程，它可以转化为dendrogram的对象，这一对象使用的是嵌套的list。
den <- as.dendrogram(hc)
den

#不管是hclust还是dendrogram都可以转化为phylo对象，我们可以通过ape::as.phylo来转hclust，
#也可以通过phylogram::as.phylo来转dendrogram。
#转化为phylo之后，我们知道ggtree最擅长画phylo，也就是说有了这个转换之后，
#我们就可以直接上ggtree来画层次聚类的结果了。而且有ggtree的加持，要定制一下图，
#要在树上加点什么，那就太方便了，而这通常是没办法搞的。


#layout_dendrogram直接就给你画从上到下的布局，然后再有一个theme_dendrogram，
#这个和theme_tree相似，但显示的是树高，而且以最远的叶子为0。
?cutree
clus <- cutree(hc, 4)  #Cuts a tree, e.g., as resulting from hclust, into several groups either by specifying the desired number(s) of groups or the cut height(s).
d = data.frame(label=names(clus), member=factor(clus))

library(dplyr)
remotes::install_github("guangchuangyu/ggtree")
devtools::install_github("guangchuangyu/ggtree")
library(ggtree)
ggtree(as.phylo(hc), linetype='dashed', color = "#487AA1") %<+% d + 
  layout_dendrogram() + 
  geom_tiplab(aes(color = member), angle=90, hjust=1) + 
  theme_dendrogram(plot.margin=margin(6,6,80,6))


###2019.12.20
#
##取子集cutree
?cutree  #{stats}
hc=hclust(dist(mtcars));plot(hc)
g=cutree(hc,3) #组数
table(g)
