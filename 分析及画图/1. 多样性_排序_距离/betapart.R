##betapart
##https://cran.r-project.org/web/packages/betapart/betapart.pdf
##西北典型旱地生态系统细菌和真菌Beta多样性的模式和驱动因素
##把beta多样性分割为物种的替换和嵌套
rm(list=ls(all=TRUE))


library(betapart)
?betapart
data(ceram.s)  #行是样本，列是OTU
ceram.core.s<-betapart.core(ceram.s) 

#得到三个矩阵，两两成对比较。分别为总beta多样性jac or sor,物种转换jtu or sim,物种增减jne or sne.
ceram.dist.jac<-beta.pair(ceram.core.s, index.family="jac")   
ceram.dist.sor<-beta.pair(ceram.core.s, index.family="sor") 

#得到三个值。所有点总的beta及其分解。
ceram.multi.jac<-beta.multi(ceram.core.s, index.family="jac") 
ceram.multi.sor<-beta.multi(ceram.core.s, index.family="sor")

# sampling across equal sites  重抽一些样本再计算.sites为样本数。samples为计算次数。
ceram.s.samp <- beta.sample(ceram.core.s, sites=10, samples=100)

# plotting the distributions of components
dist.s <- ceram.s.samp$sampled.values
str(dist.s)
plot(density(dist.s$beta.SOR),
     xlim=c(0,8), ylim=c(0, 19), xlab='Beta diversity', main='', lwd=3)
lines(density(dist.s$beta.SNE), lty=1, lwd=2)
lines(density(dist.s$beta.SIM), lty=2, lwd=2)
lines(density(dist.s$beta.SOR), col='grey60',lwd=3)
lines(density(dist.s$beta.SNE), col='grey60',lty=1, lwd=2)
lines(density(dist.s$beta.SIM), col='grey60',lty=2, lwd=2)

# plotting clusters  聚类
dist.s <- ceram.s.samp$sampled.values
plot(hclust(ceram.dist.sor$beta.sim, method="average"), hang=-1, main='', sub='', xlab='')
title(xlab=expression(beta[sim]), line=3)
plot(hclust(ceram.dist.sor$beta.sne, method="average"), hang=-1, main='', sub='', xlab='')
title(xlab=expression(beta[sne]), line=3)

##时间序列 beta.temp
data(bbsData)
bbs.t <- beta.temp(bbs1980, bbs2000, index.family="sor")
# plotting root transformed components
with(bbs.t, plot(sqrt(beta.sim) ~ 
                   sqrt(beta.sne), type='n', ylab=expression(sqrt(beta[sim])), 
                 xlab=expression(sqrt(beta[sne]))))
with(bbs.t, text(y= sqrt(beta.sim), x=sqrt
                 (beta.sne), labels=rownames(bbs1980)))



require(vegan)
data(BCI) ## UTM Coordinates (in metres) 
UTM.EW <- rep(seq(625754, 626654, by=100), each=5) 
UTM.NS <- rep(seq(1011569, 1011969, by=100), len=50)
spat.dist<-dist(data.frame(UTM.EW, UTM.NS))
dissim.BCI<-beta.pair.abund(BCI)$beta.bray.bal
plot(spat.dist, dissim.BCI, ylim=c(0,1), xlim=c(0, max(spat.dist)))
BCI.decay.exp<-decay.model(dissim.BCI, spat.dist, model.type="exp", perm=100)
BCI.decay.pow<-decay.model(dissim.BCI, spat.dist, model.type="pow", perm=100)
plot.decay(BCI.decay.exp, col=rgb(0,0,0,0.5)) 
plot.decay(BCI.decay.exp, col="red", remove.dots=TRUE, add=TRUE) 
plot.decay(BCI.decay.pow, col="blue", remove.dots=TRUE, add=TRUE)


##protest测试非随机性的显著性  #群落结构一致性的随机化测试（PROTEST）
?protest()
data(varespec)
vare.dist <- vegdist(wisconsin(varespec))
mds.null <- monoMDS(vare.dist, y = cmdscale(vare.dist))
mds.alt <- monoMDS(vare.dist)
vare.proc <- procrustes(mds.alt, mds.null)
vare.proc
summary(vare.proc)
plot(vare.proc)
plot(vare.proc, kind=2)
residuals(vare.proc)
# https://sci-hub.tw/https://www.tandfonline.com/doi/abs/10.1080/11956860.1995.11682297
# https://mp.weixin.qq.com/s?__biz=MzIzODU4Njc4MQ==&mid=2247485341&idx=1&sn=290e4f650054f9c7fa1a5cd03ca85658&chksm=e936596fde41d079cbbe8eaa3a13a69969a8b8294dd621260fca9193e876ae107da2028396d9&mpshare=1&scene=1&srcid=0619rhW5NMdtt0HhhG6QqvGK&pass_ticket=7Wr8HH817FPWW%2F%2BpeXEXkT%2BkfHyuaSvGUXXfUqj3yToWgE4iq3%2FbboLmjZJ2njGO#rd
# PICRUSt功能预测和GeoChip检测结果的比较 


citation("betapart")
