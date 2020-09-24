#R code
#Written and modified by Kai Feng
#2017.7.12 li look

setwd('D:/文件存放/galaxy pipeline/galaxy/自己整理代码/CCA/')
#setwd('D:/Kai/Desktop/16S测序分析-20160426/Uparse/heatmap')
rm(list=ls(all=TRUE))


library(vegan)

#otu.table= read.table('heatmap_Geobacter.txt',sep = "\t", row.names=1, header = T)
otu.table= read.table('OTU.txt',sep = "\t", row.names=1, header = T)

x<- otu.table[,2:ncol(otu.table)]

tiff(file="Geobacter_yellow-red.tif", res=300, units="in", width=8, height=10)
x_heatmap <- pheatmap::pheatmap(x,
                                color= colorRampPalette(c("darkgreen","yellow","red"))(50),
                                #color= cm.colors(50),
                                cluster_rows = TRUE, cluster_cols =F,
                                clustering_distance_rows = "euclidean",
                                scale = "row",   # 标准化
                                fontsize_row=12,  # 行字体大小 
                                fontsize_col=12, # 列字体大小
                                #cellwidth = 40,
                                #cellheight = 15,
                                main = "heatmap",
                                #border_color = "grey70",
                                treeheight_row = 50,
                                treeheight_col = 50,
                                #treeheight_row = ifelse((class(cluster_rows) == "hclust") || cluster_rows,50, 0), 
                                #treeheight_col = ifelse((class(cluster_cols) == "hclust") || cluster_cols, 50, 0),
                                legend = T,      # 图例
                                height = NA,
                                width = NA 
                                )
dev.off()
# tiff(file="genus_100.tif", res=300, units="in", width=10, height=10)默认的单位是px，也可以选in(inches),cm,mm
# grid.draw(x_heatmap)
# dev.off()
#pheatmap注释
#color 表示颜色，复制渐变颜色调色板属性，colorRampPalette, 选择“绿，黑，红”渐变，分为100个等级,也有rainbow(),heat.colors(),
#terrain.colors(),topo.colors(),cm.colors()等
#kmeans_k 表示kmeans聚类的个数
#cellheight, cellwidth 表示每个单元格的高度和宽度
#scale 表示值集中化的方向，可选为row, column, 或者none
#cluster_cols, cluster_rows， 表示是否双向聚类，值可以是T或者F
#cluster_distance_row 表示距离矩阵的方法, 可以是correlation，或者是dist中的距离，如euclidean等correlation,euclidean,maximum,manhattan,canberra,minkowski
#cluster_method 表示聚类方法，值可以是hclust的任何一种，如ward.D, single, complete, average, mcquitty, centroid, ward.D2等
#treeheight_row, treeheight_col, 表示用于构建heatmap图的树的行和列的高度，默认为50
#show_rownames, show_colnames 表示显示行名和列名
#main 表示heatmap的title
#fontsize_row, fontsize_col 表示行和列的字体大小
#width, height 表示图的宽度和高度
#margins 表示页边空白的大小 如margins=c(5,10)
library(heatmap.plus)
library(heatmap3)
library(gplots)

y <- data.matrix(x)
z<- data.matrix(x)
y = decostand(z, method = "standardize", MARGIN=1)   ##标准化
y[is.na[y]]=0
x_heatmap <- heatmap.2(y, Rowv = T, Colv = T,   ##报错 Error in hclustfun(distr) : NA/NaN/Inf in foreign function call (arg 11)
                     col= topo.colors(16),
                     margins=c(5,10),
                     labRow=row.names(x),
                     cexRow=1,
                     cexCol=0.5,
                     trace="none")

x_heatmap <-heatmap.2(y, Rowv = T, Colv = T, 
                      col= heat.colors(16),
                      scale="row",
                      key.title=NA, # no title
                      key.xlab=NA, # no xlab
                      key.par=list(mgp=c(1.5, 0.5, 0),
                                   mar=c(2.5, 2.5, 1, 0)),
                      key.ytickfun=function(y)
                        margins=c(5,10),
                      labRow=row.names(x),
                      cexRow=1,
                      cexCol=1,
                      trace="none")

x_heatmap <- heatmap.plus(y, Rowv = T, Colv = T, 
                          col=cm.colors(256),
                          margins=c(5,10),
                          labRow=row.names(x),
                          cexRow=1,
                          cexCol=0.5
                     )
