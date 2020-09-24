#2019.3.6
#from our Galaxy
# BioEnv analysis

suppressMessages(library(vegan))

x<-read.table(file=inputFile,sep="\t",header=T,row.names=1)  ##OTU
y<-read.table(file=en_file,sep="\t",header=T,row.names=1) # 环境因子。 by default, y is scaled automatically
x[is.na(x)] <- 0

samp.x <- colnames(x)
samp.y <- rownames(y)
matched <- match(samp.x,samp.y)
x <- x[,matched]

x.bioenv<-bioenv(t(x),y,index=as.character(dis_method),metric=as.character(met))
sink(result)
summary(x.bioenv)
sink()
#dis_method：euclidean or bray, for OTU dissimilarity index
#metric:euclidean or mahalanobis or manhattan or gower, for environmental distances
?bioenv
#寻找环境因子的一个子集，使得标准化后的环境因子距离矩阵和OTU不相似性矩阵之间的相关系数最高。

# method = spearman  检验相关性的方法
#parallel  并行运算
data(varespec)
data(varechem)
sol <- bioenv(wisconsin(varespec) ~ log(N) + P + K + Ca + pH + Al, varechem)
sol$
summary(sol)

