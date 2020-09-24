#install.packages(c("picante"))
setwd("E:/桌面/代码及数据/")
library(ape)
library(picante)
phylo<-read.tree("phylo.txt")
phylo
samp<-read.table("sample.txt")
mat<- sample2matrix(samp);?sample2matrix
pd<- ses.pd(mat, phylo, null.model = "taxa.labels",run = 999,iterations = 1000);?ses.pd
write.csv(pd, file="SES.PD.csv")

comdist.is<-function(x){
  as.matrix(comdist(randomizeMatrix(x, null.model = "independentswap"), cophenetic(phylo), abundance.weighted = T))
}
?randomizeMatrix ##随机化群落
?comdist #计算群落间平均两两距离
?replicate

#a = randomizeMatrix(mat, null.model = "independentswap")

# ma = matrix(c(1:6),2,3)  #得和tree匹配才能出结果。否则无结果
# comdist.is(ma)
# comdistnt.is(ma)

nulls <- replicate(20,comdist.is(mat))
nulls.means<-apply(nulls, c(1:2), mean, na.rm = T)
nulls.sds<-apply(nulls, c(1:2), sd, na.rm = T)
obs<-as.matrix(comdist(mat, cophenetic(phylo), abundance.weighted = T))
ses.dpw<-(obs - nulls.means)/nulls.sds

?comdistnt #计算群落间最接近分类单元的平均距离,即MNTD
comdistnt.is<-function(x){
  as.matrix(comdistnt(randomizeMatrix(x, null.model = "independentswap"), cophenetic(phylo), abundance.weighted = T, exclude.conspecifics = F))
}
nulls<-replicate(20,comdistnt.is(mat))
nulls.means<-apply(nulls, c(1:2), mean, na.rm = T)
nulls.sds<-apply(nulls, c(1:2), sd, na.rm = T)
obs<-as.matrix(comdistnt(mat, cophenetic(phylo), abundance.weighted = T))
ses<-(obs - nulls.means)/nulls.sds

install.packages(c("SoDA"))
library(SoDA)
spatial<-read.csv("xys.csv",header=T,row.names=1)
names(spatial)
beta.xys<-geoXY(spatial$latitude, spatial$longitude, unit = 1)
write.csv(as.matrix(beta.xys),file="beta.xys.csv")
write.csv(as.matrix(ses),file="ses.csv")
row.names(beta.xys)<-row.names(spatial)
env.dist<-dist(beta.xys)
geo.dist<-as.matrix(env.dist)
ses[is.na(ses)] <- 0
mantel(ses,geo.dist)

spatial<-read.csv("xys.csv",header=T)
names(spatial)
beta.xys<-geoXY(spatial$latitude, spatial$longitude, unit = 1)
row.names(beta.xys)<-row.names(spatial)
write.csv(as.matrix(beta.xys),file="beta.xys.csv")
write.csv(as.matrix(ses.dpw),file="ses.dpw.csv")
env.dist<-dist(beta.xys)
geo.dist<-as.matrix(env.dist)
mantel(ses.dpw,geo.dist)

install.packages(c("ecodist"))
library(ecodist)
ses[is.na(ses)] <- 0
beta.xys<-beta.xys[row.names(mat), ]
summary(lm(ses~env.dist))
MRM(ses~env.dist)


