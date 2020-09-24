##li 2017.7.5 
##http://meiweiping.cn/在R中正确运行PERMANOVA-and-pairwise-comparison及注意事项/
##PERMANOVA and pairwise comparison——样品组间差异显著性分析及事后两两比较。

##PERMANOVA与ANOSIM（Analysis of similarities）等方法目的类似，即比较样品组间的差异显著性。
##例：对多组数据进行聚类分析后得到3个大类，但是想知道这3个大类之间的差异是否显著，即可用上述方法。

##ANOSIM比较的是组内或组间距离的平均值；对于样本量大小变化很敏感，
##适用于样本在欧式平面（Euclidean space）中单个数据变化有重要意义的情况；
##另，ANOSIM对异质性数据（方差不齐）很敏感，方差不等的情况不宜使用。

##PERMANOVA比ANOSIM更强大，比较的是各组重心之间的差异；对样本数N以及方差的齐次性要求不高，推荐使用。


rm(list=ls(all=TRUE))
setwd('D:/文件存放/galaxy pipeline/galaxy/自己整理代码/permanova and pairwise comparison')

otu <- read.table("resample_OTU.txt",header=T,row.names= 1) 
#otu
group<-read.table("group file.txt",header=T,row.names= 1)  ##必须有表头，且名字和adonis中的一致。（group）
#group
##group建议使用字母排序，如A，A，B，C，C，etc；不能使用纯数字，如1，1，2，2，3，etc，
##因为纯数字在R软件中会被认为是只有1个变量，即使增加类别，df不会改变，为1。


##PERMANOVA
library(vegan)

a=adonis(otu ~ group,data =group, permutations = 99,method="bray")
##报错如下。此问题没有解决。算距离的时候出现了空行，没有意义。
##Error in G * t(hat) : non-conformable arrays
##In addition: Warning messages:
##  1: In vegdist(lhs, method = method, ...) :
##  you have empty rows: their dissimilarities may be meaningless in method “bray”
##2: In vegdist(lhs, method = method, ...) : missing values in results

#查看数据类型  mode(x)  class(x)  str(x)  typeof(x)
#mode(otu)
#class(group)
#str(otu)
#typeof(group)
?adonis

##sample
data(dune)
data(dune.env)
adonis(dune ~ Management*A1, data=dune.env, permutations=99)

##pairwise comparison for PERMANOVA in R software
#-------copy pairwise.adonis function code in R -----------
pairwise.adonis <-function(x,factors, sim.method, p.adjust.m)
{
  library(vegan)
  co = as.matrix(combn(unique(factors),2))
  pairs = c()
  F.Model =c()
  R2 = c()
  p.value = c()
  for(elem in 1:ncol(co)){
    ad = adonis(x[factors %in%c(as.character(co[1,elem]),as.character(co[2,elem])),] ~
                  factors[factors %in%c(as.character(co[1,elem]),as.character(co[2,elem]))] , method =sim.method);
    pairs =c(pairs,paste(co[1,elem],'vs',co[2,elem]));
    F.Model =c(F.Model,ad$aov.tab[1,4]);
    R2 = c(R2,ad$aov.tab[1,5]);
    p.value = c(p.value,ad$aov.tab[1,6])
  }
  p.adjusted =p.adjust(p.value,method=p.adjust.m)
  pairw.res = data.frame(pairs,F.Model,R2,p.value,p.adjusted)
  return(pairw.res)
}
#-----------end copy-------------
pairwise.adonis(otu, group$group, sim.method="bray", p.adjust.m= "bonferroni")
##报的错和做adonis一样。



