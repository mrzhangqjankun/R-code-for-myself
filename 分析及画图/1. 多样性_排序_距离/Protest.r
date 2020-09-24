##2020.2.19

##Protest: Procrustes test
#Procrustes通过将一个矩阵旋转到与目标矩阵的最大相似度，从而最小化差异的平方和。

library(vegan)

procrustes(X, Y, scale = TRUE, symmetric = FALSE, scores = "sites", ...)
X:目标矩阵
Y:要变换的矩阵
scale:允许缩放Y轴


data(varespec)
vare.dist <- vegdist(wisconsin(varespec))
mds.null <- monoMDS(vare.dist, y = cmdscale(vare.dist))
mds.alt <- monoMDS(vare.dist)
vare.proc <- protest(mds.alt, mds.null)
vare.proc
summary(vare.proc)
plot(vare.proc)
plot(vare.proc, kind=2)
residuals(vare.proc)
