##2020.1.2

##https://mp.weixin.qq.com/s/VPMEgjZoNH6rNYwYUT7nhQ

##手把手重现Science的主图Maptree 

##最大的圈代表门水平，逐渐缩小的圈按照梯度分别代表纲，科，属。不同颜色标记差异的微生物。


rm(list=ls())
getwd()

if (!requireNamespace("BiocManager", quietly = TRUE)) 
install.packages("BiocManager") 
BiocManager::install("ggtree") 
library(ggplot2) 
library(ggtree) 


#导入功能函数
source("maptree//MaptreeForMicrobiome.R")

#导入otu表格
otu = read.delim("maptree/otutab.txt",row.names = 1)
head(otu)
otu = as.matrix(otu)
# str(otu)
#导入注释文件
tax = read.delim("maptree/taxonomy.txt",row.names = 1)
head(tax)
#将物种注释文件转化为矩阵，方便使用phyloseq封装
tax = as.matrix(tax) 


#基本函数，画出maptree
#data_to_maptree 函数功能为将提供的文件转换为maptree需要的格式，并提供简单出图，参数N代表选取OTU的数量（按照丰度排序，选取丰度最高的前N个OTU做展示），这里我设置N = 200。
mapdata = data_to_maptree (otu,tax,200) #丰度排序，用前200个OTU画
#提取图形部分并保存
p1= mapdata[[1]]
p1
ggsave("./maptree1.pdf", p1, width = 12, height =10 )

#maptree_add1_plot函数承接data_to_maptree 函数，并使用data_to_maptree 函数的输出作为输入文件，并对data_to_maptree的结果进行按照门水平上色。
#按照平均丰度修改大小和按照门水平上色颜色
mapadd = maptree_add1_plot(mapdata)
p2 = mapadd[[1]]
p2
#d导出ggplot对象的优点就是可以随心所欲的修改图形上的内容
p2 +scale_fill_brewer()
# p2 + scale_color_gradient2()

ggsave("./maptree.pdf", p2, width = 12, height =10)



#补充一 ：ggraph版本的物种分类树
#我们造的轮子在ggraph包中还可以做一些有意思的图形，这里我就本次数据结合ggraph的layout做一个简单的优化展示，为大家带来ggraph版本的物种分类树
graph = mapadd[[2]]

#展示物种分类关系
ggraph(graph, 'dendrogram', circular = TRUE) +
  geom_edge_elbow() +
  scale_edge_colour_distiller(palette = "RdPu") +
  geom_node_point(aes( x = x, y=y, filter = leaf,size=mean,colour=Phylum, alpha=0.2)) +
  geom_node_text(aes(x = x*1.05, y=y*1.05, filter = leaf, label=name,
                     hjust='outward', angle = -((-node_angle(x, y)+90)%%180)+90, size=3,
                     colour=Phylum), size=0.8, alpha=1)  + theme_void()

graph = mapadd[[2]]

# data = create_layout(graph, layout = 'dendrogram', circular = TRUE)
# head(data)

#展示为无数种分类树，这种方式也是我知道
ggraph(graph, layout = 'dendrogram', circular = TRUE) +
  geom_edge_diagonal(colour="blue") +
  scale_edge_colour_distiller(palette = "RdPu") +
  geom_node_point(aes( x = x, y=y, filter = leaf,size=mean,colour=Phylum, alpha=0.2)) +
  geom_node_text(aes(x = x*1.05, y=y*1.05, filter = leaf, label=name,
                     hjust='outward', angle = -((-node_angle(x, y)+90)%%180)+90, size=3,
                     colour=Phylum), size=0.5, alpha=1)  + theme_void()
