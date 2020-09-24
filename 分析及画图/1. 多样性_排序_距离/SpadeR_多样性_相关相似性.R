##2020.2.17

##SpadeR

##https://cran.r-project.org/web/packages/SpadeR/index.html

install.packages("SpadeR")

library(SpadeR)
?SpadeR
#计算基于个体(丰度)数据或基于采样单元(发生率)数据的各种生物多样性指数和相关相似性指标

#包含6个主要函数
1.ChaoSpecies，估计群落物种多样性
data(ChaoSpeciesData)
ChaoSpecies(ChaoSpeciesData$Abu,"abundance",k=10,conf=0.95)

2.Diversity，计算richness, Shannon diversity and Simpson diversity
data(DiversityData)
Diversity(DiversityData$Abu,"abundance",q=c(0,0.5,1,1.5,2))
#q为多样性阶数

3.ChaoShared，计算两群落共有的物种
data(ChaoSharedData)
ChaoShared(ChaoSharedData$Abu,"abundance",se=TRUE,nboot=200,conf=0.95)


4.SimilartyPair，计算两群落的相似性指数
data(SimilarityPairData)
SimilarityPair(SimilarityPairData$Abu,"abundance",nboot=200)


5.SimilarityMult，计算多个群落的相似性指数
6.Genetics，计算基因数据的等位基因不相似性

