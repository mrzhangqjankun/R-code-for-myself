##2018.11.21
#还在用PCA降维？快学学大牛最爱的t-SNE算法吧（附Python/R代码）
https://mp.weixin.qq.com/s?__biz=MzI5MTcwNjA4NQ==&mid=2247484978&idx=1&sn=07b7f734ad0ad44562186c1ef3663057&scene=21#wechat_redirect

##（t-SNE）t-分布式随机邻域嵌入是一种用于挖掘高维数据的非线性降维算法。 
##它将多维数据映射到适合于人类观察的两个或多个维度。

##PCA的局限性
#PCA是一种线性算法。 它不能解释特征之间的复杂多项式关系。 
#另一方面，t-SNE是基于在邻域图上随机游走的概率分布，可以在数据中找到其结构关系。

#线性降维算法的一个主要问题是它们集中将不相似的数据点放置在较低维度区域时，数据点相距甚远。##马鞍形问题

##t-SNE在数据点的数量上具有二次时间和空间复杂性。 
##这使得它应用于超过10,000个观察对象组成的数据集的时候特别慢和特别消耗资源。

## 非线性降维算法t-SNE通过基于具有多个特征的数据点的相似性识别观察到的模式来找到数据中的规律。
##它不是一个聚类算法，而是一个降维算法。这是因为当它把高维数据映射到低维空间时，
##原数据中的特征值不复存在。所以不能仅基于t-SNE的输出进行任何推断。
##因此，本质上它主要是一种数据探索和可视化技术。

#但是t-SNE可以用于分类器和聚类中，用它来生成其他分类算法的输入特征值。

##t-SNE产生的结果优于PCA和其它线性降维模型。
##这是因为诸如经典缩放的线性方法不利于建模曲面的流型。 
##它专注于保持远离的数据点之间的距离，而不是保留临近数据点之间的距离。

#install.packages("Rtsne")
?Rtsne
## calling the installed package
train<- read.csv(file.choose()) ## Choose the train.csv file downloaded from the link above  
library(Rtsne)
## Curating the database for analysis with both t-SNE and PCA
Labels<-train$label
train$label<-as.factor(train$label)
## for plotting
colors = rainbow(length(unique(train$label)))
?rainbow
names(colors) = unique(train$label)

## Executing the algorithm on curated data
tsne<- Rtsne(train[,-1], dims = 2, perplexity=30, verbose=TRUE, max_iter = 500)
exeTimeTsne<- system.time(Rtsne(train[,-1], dims = 2, perplexity=30, verbose=TRUE, max_iter = 500))

## Plotting
plot(tsne$Y, t='n', main="tsne")
text(tsne$Y, labels=train$label, col=colors[train$label])

##Examples

iris_unique <- unique(iris) # Remove duplicates
iris_matrix <- as.matrix(iris_unique[,1:4])

# Set a seed if you want reproducible results
set.seed(42)
tsne_out <- Rtsne(iris_matrix,pca=FALSE,perplexity=30,theta=0.0) # Run TSNE

# Show the objects in the 2D tsne representation
plot(tsne_out$Y,col=iris_unique$Species, asp=1)

# data.frame as input
tsne_out <- Rtsne(iris_unique,pca=FALSE, theta=0.0)

# Using a dist object
set.seed(42)
tsne_out <- Rtsne(dist(normalize_input(iris_matrix)), theta=0.0)
plot(tsne_out$Y,col=iris_unique$Species, asp=1)

set.seed(42)
tsne_out <- Rtsne(as.matrix(dist(normalize_input(iris_matrix))),theta=0.0)
plot(tsne_out$Y,col=iris_unique$Species, asp=1)

# Supplying starting positions (example: continue from earlier embedding)
set.seed(42)
tsne_part1 <- Rtsne(iris_unique[,1:4], theta=0.0, pca=FALSE, max_iter=350)
tsne_part2 <- Rtsne(iris_unique[,1:4], theta=0.0, pca=FALSE, max_iter=650, Y_init=tsne_part1$Y)
plot(tsne_part2$Y,col=iris_unique$Species, asp=1)
## Not run: 
# Fast PCA and multicore

tsne_out <- Rtsne(iris_matrix, theta=0.1, partial_pca = TRUE, initial_dims=3)
tsne_out <- Rtsne(iris_matrix, theta=0.1, num_threads = 2)

## End(Not run)

##my own data
setwd("E:/桌面/长兴岛水样及底泥2018.6/数据分析/2018.8.4-16S")
x<-read.table(file="Galaxy72-[resample_UPARSE_otu_table.txt].txt",sep="\t",header=T,row.names=1)
group0 = read.table("group0.txt", sep="\t", row.names=1 )
####PCA
library(vegan)
x.pca = rda(x);x.pca
outputpca = summary(x.pca)
str(outputpca)

a = outputpca$species ;a
b = outputpca$cont$importance ;b

pc1 = round(b[2,1],2);pc1
pc2 = round(b[2,2],2);pc2

####ggplot2
sam.a=as.data.frame(a)
sam.a = sam.a[,1:2]

##match group and analysis results
rowgroup = rownames(group0)   ##
rowsam = rownames(sam.a)
mat = match(rowgroup,rowsam)
sam.a = sam.a[mat,]

colnames(group0) = "group"    ##
data.plot = cbind(sam.a,group0)  ##
data.plot

rowname = row.names(sam.a)
library(ggplot2)
p = ggplot(data.plot,aes(PC1,PC2))
p = p + geom_point(aes(colour = factor(group)),alpha=0.5,size = 3);p
p = p + xlab(paste("PC1=",pc1*100,"%",sep=""))+ylab(paste("PC2=",pc2*100,"%",sep=""))+ labs(title = "PCA analysis") ;p
p = p + geom_text(aes(label =rowname,colour = factor(group)),position = position_dodge(0.5),vjust=-1) ;p
p = p +  plot_theme + theme(plot.title=element_text(hjust=0.5));p
p = p + theme_bw();p
#p = p +  stat_ellipse(aes(PC1,PC2,fill=group),geom="polygon", level=0.95, alpha=0.2,linetype = 2);p  ##椭圆
#p = p +  guides(color=guide_legend("Group"),fill=guide_legend("Group"));p
p=p+theme(axis.text= element_text(size=16, color="black", family  = "serif", face= "bold", vjust=0.5, hjust=0.5))+
  theme(axis.title = element_text(size=16, color="black", family  = "serif",face= "bold", vjust=0.5, hjust=0.5))+
  theme(legend.text = element_text(colour = 'black', size = 16,  family  = "serif",face = 'bold'))+
  theme(legend.title=element_blank());p
p=p+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())
p
p = p + stat_ellipse(aes(colour = factor(group)),level=0.95,linetype = 2);p  
# row = rownames(data.plot)
# p=p+geom_text(label=paste(row),aes(colour=factor(group)),size=4,vjust=-1.5)
# p

####t-SNE
library(Rtsne)
colors_x = rainbow(length(colnames(x)))
#x_unique <- unique(x)   ##针对向量，默认去掉重复的行。
#在OTU中应该是去掉数字完全相同的行。但是这是不应该去掉的。所以不做这一步。
#注意转置。默认按照行计算。perplexity需要调小一些。
#tsne_out <- Rtsne(t(x_unique),pca=FALSE,perplexity=10,theta=0.0,check_duplicates = TRUE) # Run TSNE

tsne_out <- Rtsne(t(x),pca=FALSE,perplexity=10,theta=0.0,check_duplicates = F) # Run TSNE
tsne_out
str(tsne_out)
# Show the objects in the 2D tsne representation
plot(tsne_out$Y,col=colors_x, asp=1)


##https://mp.weixin.qq.com/s/59sWLbeJ-TTttSlgPgr1lg
##https://mp.weixin.qq.com/s/59sWLbeJ-TTttSlgPgr1lg

#2019.11.30
#https://mp.weixin.qq.com/s/wdWad94x4N56-uht7A5vUQ