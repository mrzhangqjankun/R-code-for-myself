##2020.6.10

前文：Procrustes test
介绍了其基本用法。但是存在一个局限性，只能输入两个矩阵且没办法分组。
正好昨天有读者问我这个，我顺手查了一下发现还有其他一些包可以做，并可以实现更一般化的分析Generalised Procrustes Analysis


#更新R
library(installr)
updateR()

#library(SensoMineR)
#library(multiway)

library(FactoMineR)
?GPA
##GPA,Generalised Procrustes Analysis数据中可包含缺失值，并可以添加多个分组
data(wine)
res.gpa <- GPA(wine[,-(1:2)], group=c(5,3,10,9,2),
               name.group=c("olf","vis","olfag","gust","ens"))
#res.gpa$correlations
### If you want to construct the partial points for some individuals only
plotGPApartial (res.gpa)
X11()
plot (res.gpa)
dev.off()
##另外还发现三个包shapes，Evomorph和smacof也可以做GPA，但是不能分组

library(shapes)
#可以做Generalised Procrustes analysis，还能做Ordinary和Weighted Procrustes analysis

library(Evomorph)

library(smacof)




