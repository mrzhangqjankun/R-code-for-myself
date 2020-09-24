#### 2017.7.6 li learn
##http://www.omicshare.com/forum/forum.php?mod=viewthread&tid=741&extra=page%3D1%26filter%3Dtypeid%26typeid%3D18
##如何生成相关系数矩阵，并且绘制相关性热图

##（1）计算相关系数；
##在R语言里面，相关系数的命令是 cor。这个命令是可以计算两个向量的相关系数。
##但你如果输入数据是数据框的，而且cor命令自动计算所有列（向量）的两两相关系数（范例文件列方向正好是样本）。
##（2）绘制热图
##绘制热图可以使用pheatmap这个命令。

##cor.test 两个样本之间相关系数。见种面积关系.R

rm(list=ls(all=TRUE))
setwd('D:/文件存放/galaxy pipeline/galaxy/自己整理代码/相关系数及热图')
#范例文件（txt）是一个20个样本，30个基因的表达量表格矩阵。每一行是1个基因，每一列对应1个样本。

library(pheatmap)   #加载pheatmap 包；
data=read.table("exp_top30.original.txt",header=T,row.names=1,sep="\t")

matrix=cor(data)   #计算相关系数；
matrix
#write.table(matrix,"coefficient_matrix.txt",sep="\t")             #将相关系数计算结果输出存储到你的电脑里，存储为1个txt文件；
#pheatmap(matrix,cluster_rows=T,cluster_cols=T,display_numbers=T)
pheatmap(matrix,cluster_rows=F,cluster_cols=F,display_numbers=T) # 行和列都不聚类，并且在热图中显示数值；

##如果要计算每一行的相关系数（这里行方向是基因，即行间相关系数就是基因间的相关系数），
##则需要对矩阵做个转置，即行列对调，使用t()命令即可。
data=t(data)  # 对数据做转置；
matrix=cor(data)   #计算相关系数；
##write.table(matrix,"coefficient_matrix.txt",sep="\t")            #将相关系数计算结果输出存储到你的电脑里，存储为1个txt文件；
pheatmap(matrix,cluster_rows=F,cluster_cols=F,display_numbers=T,fontsize_number=4,number_format = "%.2f") 
# 注意，由于格子比较多，所以用fontsize_number定义了格子中数字的字体大小, number_format 可以控制有效小数的位数，这里是保留两位小数；

setwd('E:/桌面')
data=read.table("lii.txt",header=T,row.names=1,sep="\t")
library(pheatmap)   #加载pheatmap 包；
pheatmap(data,cluster_rows=T,cluster_cols=F) 

?pheatmap


setwd("E:\\桌面\\陈鹏数据\\")
data = read.table(file="16S结果/resample28741_UPARSE_otu_table.txt",sep="\t",header=T,row.names= 1)
data = read.table(file="ITS结果/resample28147_UPARSE_otu_table.txt",sep="\t",header=T,row.names= 1)


matrix=cor(data)   #计算相关系数；
matrix
options(max.print=9999999)
write.table(matrix,file="16S结果/样本相关性热图数据.txt",sep="\t",col.names=NA)
write.table(matrix,file="ITS结果/样本相关性热图数据.txt",sep="\t",col.names=NA)
pheatmap(matrix,cluster_rows=F,cluster_cols=F,display_numbers=T) # 行和列都不聚类，并且在热图中显示数值；
