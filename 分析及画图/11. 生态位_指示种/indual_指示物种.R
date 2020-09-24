##2019.4.25

#install.packages("labdsv")
library(labdsv);??labdsv
#https://cran.r-project.org/web/packages/labdsv/index.html
#生态学的包。常见的NMDS，PCA等都有



#indual  指示物种，同时考虑发生率与丰度
data(bryceveg) # 导入数据，列是物种，行为OTU
#dis.bc <- dsvdis(bryceveg,index='bray/curtis') # 计算距离矩阵
clust <- sample(1:5,nrow(bryceveg),replace=TRUE) #构造一个分组
indval(bryceveg,clust)

#isamic 只考虑发生率
data(bryceveg)
dis.bc <- dsvdis(bryceveg,'bray/curtis')
clust <- sample(1:5,nrow(bryceveg),replace=TRUE)
isamic(bryceveg,clust)

#2012 Methods in ecology and evolution
#https://besjournals.onlinelibrary.wiley.com/doi/pdf/10.1111/j.2041-210X.2012.00246.x