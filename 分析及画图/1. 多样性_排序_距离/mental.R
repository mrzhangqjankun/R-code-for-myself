##li 2017.7.11 
##http://blog.sina.com.cn/s/blog_4b678be40100o464.html
##http://blog.sina.com.cn/s/blog_b5c8908c0101e8of.html
##Mantel test，顾名思义，是一种检验。既然是检验就得有原假设，它的原假设是两个矩阵见没有相关关系。
##检验过程如下：两个矩阵都对应展开，变量两列，
##计算相关系数（理论上什么相关系数都可以计算，但常用pearson相关系数），
##然后其中一列或两列同时置换，再计算一个值，permutation 成千上万次，
##看实际的r值在所得r值分布中的位置，如果跟随机置换得到的结果站队较近，则不大相关，
##如果远远比随机由此得到显著性。


rm(list=ls(all=TRUE))
data(varespec) 
pc = prcomp(varespec,scale = TRUE) ;pc      ##PCA
pc = scores(pc,display = "sites",choice = 1:4); pc  ##取出pc1 到 pc4 的值
edis = vegdist(pc,method = "euclid")  ; edis       ## pc1 到 pc4 的值,做距离矩阵。前两步就是算这个距离。其他没啥用。
vare.dis = vegdist(wisconsin(sqrt(varespec))) ;vare.dis ## 平方根，wisconsin，再算距离
mantel(vare.dis,edis)     ##两距离矩阵做mantel test.

plot(vare.dis,edis)
##如果呈或多或少的单调相互关系，或者，甚至是正的线性关系，那就搞定了。
##在空间模型里，可能会观察到隆起一个“包”，这表明空间上的聚集。