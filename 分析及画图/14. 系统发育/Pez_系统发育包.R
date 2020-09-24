##2020.4.16

##https://academic.oup.com/bioinformatics/article/31/17/2888/183404

##pez: phylogenetics for the environmental sciences 

#install.packages("pez")

library(pez);?pez

#comparative.comm创建一个群落生态对象。这是pez所有功能的基础。
#comparative.comm(phy, comm, traits = NULL, env = NULL, warn = TRUE,force.root = -1)
#phy 系统发育树，phylo格式
#comm 物种信息。列为物种行为样本
#traits 物种特征
#env 环境因子数据
data(laja)
data <- comparative.comm(invert.tree, river.sites, invert.traits, river.env)


#pez.shape计算shape
#pez.evenness计算evenness
#pez.dispersion计算dispersion
#pez.dissimilarity计算dissimilarity
(output<-pez.shape(data))
(output<-pez.evenness(data))

#pze还有其他很多强大的功能,如：
#scape和sim函数可以模拟系统发育树；以及给定发育树的基础上模拟群落结构
#phylo和pianka函数可以基于系统发育特征，以及群落与环境因子构建共存矩阵
#communityPGLMM构建群落数据的系统发育广义线性混合模型
#ConDivSim构建Null models
