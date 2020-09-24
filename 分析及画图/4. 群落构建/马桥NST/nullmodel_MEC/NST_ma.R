

library(NST)

setwd("E:/桌面/马桥NST/")
comm= read.table("Galaxy45-[resample_UPARSE_otu_table.txt].tabular",sep="\t",header=T,row.names=1)
group=read.table("null group.txt",sep="\t",header=T,row.names=1)
comm = t(comm)
?tNST
tnst=tNST(comm=comm, group=group, dist.method="jaccard",
          abundance.weighted=TRUE, rand=20,
          nworker=1, null.model="PF", between.group=TRUE,output.rand=TRUE,
          SES=TRUE, RC=TRUE)

#dist.method = "manhattan","mManhattan", "euclidean","mEuclidean", "canberra", "bray", "kulczynski", "jaccard", "gower", "altGower", "mGower", "morisita", "horn", "binomial", "chao", "cao". default is "jaccard"
#rand = default is 1000
#nworker = default is 4, means 4 threads will be run.
#null.model = "EE", "EP", "EF", "PE", "PP", "PF", "FE", "FP", "FF", etc. The first letter indicate how to constraint species occurrence frequency, the second letter indicate how to constraint richness in each sample. see null.models for details. default is "PF".
#SES = whether to calculate standardized effect size
#RC = whether to calculate modified Raup-Crick metric

tnst

#结果输出
options(max.print = 999999)
sink("tnst.res.txt")
tnst
sink()
# index.pair
# D.ij, 观测到的不相似度，未标准化; 
# G.ij, 零模型平均期望的不相似度, 未标准化; 
# Ds.ij, 观测到的不相似度，标准化到0-1;
# Gs.ij, 零模型平均期望的不相似度, 标准化; 
# C.ij and E.ij 相似度和零模型平均期望的相似度,不相似度如果没有设置上限则标准化; 
# ST.ij, stochasticity ratio calculated by previous method (Zhou et al 2014); 
# MST.ij, modified stochasticity ratio calculated by a modified method (Liang et al 2019; Guo et al 2018); 
# SES.ij, standard effect size of difference between observed and null dissimilarity (Kraft et al 2011); 
# RC.ij, modified Roup-Crick metrics (Chase et al 2011, Stegen et al 2013).

#index.grp
# 组内平均值。包括ST, NST和MST

# index.pair.grp
# index.pair结果中同一组内的挑出来

# index.between
# 类似于index.grp，只是两个组之间的平均值

# index.pair.between
# index.pair结果中不是同一组内的挑出来
# index.pair.between

#检验各组NST的分布情况及各组NST之间差异的显著性。
nst.bt=nst.boot(nst.result=tnst, group=NULL, rand=99,
                trace=TRUE, two.tail=FALSE, out.detail=FALSE,
                between.group=TRUE, nworker=1)
# group=NULL, 默认使用nst的分组
nst.bt
#结果输出
sink("nst.bt.txt")
nst.bt
sink()


#NST组间进行Permutational multivariate ANOVA
nst.pova=nst.panova(nst.result=tnst, rand=20)

#结果输出
nst.pova
sink("nst.pova.txt")
nst.pova
sink()


