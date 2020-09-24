##2020.5.31

##adespatial：分解beta多样性的另一种方法
#之前介绍过beta多样性的分解，详见：
R——分解beta多样性betapart包简介
再论betapart
#上文：EM：不同海拔细菌和真菌多样性及驱动因素
#利用adespatial对beta多样性进行了分解。本文简单介绍。

library(adespatial)
#adespatial：多变量多尺度空间分析。

beta.div.comp(mat, coef = "J", quant = FALSE, save.abc = FALSE)
#mat:OTU
#coef:beta多样性指数。可根据丰度或发生率数据进行选择。

#例子
  data(doubs)
  fish.sp = doubs$fish[-8,]  
  out = beta.div.comp(fish.sp, coef="J", quant=FALSE)
  out$part
#BDtotal：总beta多样性
#Repl: 物种替代的多样性
#RichDif: 丰度变化的多样性

adespatial这个包功能十分强大，如还可以进行向前筛选（forward.sel）。但是注意forward.sel只能用于RDA，而vegan中的ordistep可用于RDA和CCA。
变量筛选之前也写过：
MRM中进行变量筛选

此外还有很多基于MEM的方法，可以对空间结构进行建模。
MEM即Moran特征根图（Moran's eigenvector map）。 
这个如果不熟悉，那么PCNM（principal coordinate of neighbour matrice）是属于MEM中的一个方法，应该较多的人知道。
PCNM简单地说，即把样本之间的距离也当作一个环境因子。先设定一个欧氏距离阈值，小于此阈值的距离都保留，而大于此阈值的距离全部设定为4倍阈值。
在此基础上对简化后的距离矩阵进行PCoA分析。保留具有正空间相关的特征向量。
接着就可以利用这些保留的特征向量作为空间解释变量，与OTU进行相关性分析。
而MEM是PCNM的一般化，可将任何的相似矩阵代替距离矩阵，对空间结构进行解析。
adespatial中针对MEM的具体函数不再赘述。