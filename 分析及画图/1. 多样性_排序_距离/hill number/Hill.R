##2018.6.27
##hill.number.pl


#Hill numbers include the three widely used species diversity measures as special cases: 
#Species richness(q=0), Shannon diversity (q=1), and Simpson diversity (q=2).


source("file://E:/桌面/R script 2017/hill number/chao.hill.2016.r")
##resample OTU table
otu.table=read.table("otu_table", row.names = 1, header = T, sep="\t")
otu.table2 <- t(otu.table)
otu.table2=otu.table2[,colSums(otu.table2)>0] #取出不为空的列（OTU）
boot.time=200
chill=t(sapply(1:nrow(otu.table2),  ##对于每个样本，运行chao.hill.2016.r
               function(i)
               {                 
                 dati=as.vector(as.matrix(otu.table2[i,]))
                 out=singleton.ChaoHill(dat=dati,datatype="abundance",from=0,to=2,interval=1,B=boot.time,conf = 0.95)
                 #singleton.ChaoHill  最后一个函数。interval q增加的步长。B 自举数，200节约时间
                 c(out$Singleton.est,as.vector(as.matrix(out$EST)),as.vector(as.matrix(out$SD)),
                   as.vector(as.matrix(out$LCI)),as.vector(as.matrix(out$UCI)))
               }))
n1=paste0("Hill.",c("obs","Chao"),".q")
n2=paste0(rep(n1,3),c(0,0,1,1,2,2))
colnames(chill)=c("Singleton.estim",n2,paste(n2,"SD",sep="."),paste(n2,"lowCI",sep="."),paste(n2,"upCI",sep="."))
rownames(chill)=rownames(otu.table2)
write.table(chill,file="out_result",sep = "\t",row.names = T)


##Hill diversity|不一样的群落多样性分析 
https://mp.weixin.qq.com/s?__biz=MzIzODU4Njc4MQ==&mid=2247485400&idx=1&sn=b479916aa4eb8924d28c7013cc3dbd40&chksm=e936592ade41d03c29e207168d97633322adf7ee29000be41869edf120924e354cb94153e4b4&scene=0#rd
##Hill多样性指数，也称为物种的有效数 (effective number of species)，通常用来衡量和比较群落的物种多样性。
##Hill多样性指数的计算依赖于q，q 为Hill多样性指数的阶。

# 当q=0时，Hill多样性指数指的是物种数量的多少，即物种丰富度，不考虑物种的相对丰度，优势种和稀有种同等对待。
# 当q=1时，在计算Hill多样性指数时，优势物种的权重较小，稀有物种的权重较大。
# 当q=2时，在计算Hill多样性指数时，优势物种的权重较大，稀有物种的权重较小。



