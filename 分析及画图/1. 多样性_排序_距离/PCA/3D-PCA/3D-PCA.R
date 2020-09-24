##li 2017.7.5 
##http://www.omicshare.com/forum/thread-1870-1-1.html
##[R语言] 三维PCA图R代码 

rm(list=ls(all=TRUE))
setwd('D:/文件存放/galaxy pipeline/galaxy/自己整理代码/PCA/3D-PCA')
##install.packages("plot3D")
##install.packages("scatterplot3d")
##install.packages("rgl")
library(plot3D)
library(scatterplot3d)
a=read.table("pca123.test.txt",header=TRUE)
x=a$PC1
y=a$PC2
z=a$PC3
color=a$color
dat <- data.frame(x,y,z)
plot3d <- scatterplot3d(x,y,z,color=color,pch=1,xlab="PC1",ylab="PC2",zlab="PC3")
legend(plot3d$xyz.convert(0.17,0.05,0.05),pch=1,legend=c("Wild 1","Wild 2","Wild 3","Wild 4"),col=c("red","blue","orange","green"))


##或者下面的也可以
pca=read.table("pca123.test.txt",header=TRUE)
plot3d(pca$PC1,pca$PC2,pca$PC3,size=5,pch=c(rep(1,10),rep(1,12),rep(1,13),rep(1,14),
        rep(1,15),rep(1,16)),col=c(rep("orange",10),rep("red2",12),rep("purple",13),
        rep("darkgreen",14),rep("green",15),rep("blue",16)),lwd=1.5,cex=0.7, xlab="PC1",ylab="PC2",zlab="PC3")
##画图，其中pch=c(rep(1,10)，……)这个格式中代表你各个分组的信息，1代表画图的图形（圆圈），在R下0-25绘图各个代表不同的形状，具体请百度，
##10代表你该分组中样品的数量，就是去掉第一行header后的前十行，该示例中，有六个组，第一组前十行，第二组是接下来的12行，第三组时接下来的13行……以此类推。
##col=c()表示每个分组的颜色，10、12、13、14……相应的表示样品数。
legend("topright",c("Wild 1","Wild 2","Wild 3","Cultivated 1","Cultivated 2","Cultivated 3"),pch=1,col=c("orange","red2","purple","darkgreen","green","blue")) 
##添加图例
rgl.postscript("pca123.pdf", "pdf", drawText = TRUE)  ###保存为pdf文件


