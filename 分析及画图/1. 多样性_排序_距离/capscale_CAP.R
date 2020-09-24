##2019.3.6
#capscale {vegan}
#[Partial] Constrained Analysis of Principal Coordinates or distance-based RDA

#下面是1.16-32版本的vegan，用的是上面的名字。
#http://cc.oulu.fi/~jarioksa/softhelp/vegan/html/capscale.html
#目前vegan已经更新到了2.5-1，改名为[Partial] Distance-based Redundancy Analysis
#但是功能还是一样。和rda的区别在于rda只能用欧氏距离做不相似性矩阵，而这个可用其他的距离矩阵。
#capscale和dbrda可做限制性的PcoA分析，及非限制性的PcoA。
?capscale
#Usage
capscale(formula, data, distance = "euclidean", sqrt.dist = FALSE,
         comm = NULL, add = FALSE,  dfun = vegdist, metaMDSdist = FALSE,
         na.action = na.fail, subset = NULL, ...)
dbrda(formula, data, distance = "euclidean", sqrt.dist = FALSE,
      add = FALSE, dfun = vegdist, metaMDSdist = FALSE,
      na.action = na.fail, subset = NULL, ...)

#sqrt.dist不相似性矩阵求平方根
#add加一个常数，使得特征值为非负
#metaMDSdist 采用NMDS的方法算相似性

#capscale 先对不相似性数据进行排序，再做rda
#dbrda直接对不相似性进行分解（decomposes）,不使用rda
#使用欧氏距离时，两个函数等同于rda。其他矩阵可能会得到负的特征值。capscale会忽略产生负值的轴，只用产生正值的轴；dbrda都会用。
#这两个函数都可用于partial分析
data(varespec)
data(varechem)
## Basic Analysis
vare.cap <- capscale(varespec ~ N + P + K + Condition(Al), varechem,
                     dist="bray")
vare.cap
plot(vare.cap)
anova(vare.cap)
## Avoid negative eigenvalues with additive constant
capscale(varespec ~ N + P + K + Condition(Al), varechem,
         dist="bray", add =TRUE)
## Avoid negative eigenvalues by taking square roots of dissimilarities
capscale(varespec ~ N + P + K + Condition(Al), varechem,
         dist = "bray", sqrt.dist= TRUE)
## Principal coordinates analysis with extended dissimilarities
capscale(varespec ~ 1, dist="bray", metaMDS = TRUE)
## dbrda
dbrda(varespec ~ N + P + K + Condition(Al), varechem,
      dist="bray")
## avoid negative eigenvalues also with Jaccard distances
dbrda(varespec ~ N + P + K + Condition(Al), varechem,
      dist="jaccard")
