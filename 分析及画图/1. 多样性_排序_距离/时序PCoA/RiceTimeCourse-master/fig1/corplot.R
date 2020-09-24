##2018.6.14
##https://mp.weixin.qq.com/s?__biz=MzUzMjA4Njc1MA==&mid=2247485813&idx=1&sn=931dce150468a6f22cfed76d8bd91244&chksm=fab9e3c4cdce6ad298c3c0bc6bb8e007b4c3080cf564e018787423cb24b91753399a7484f878&scene=0#rd
##水稻微生物组时间序列分析2a-相关分析 

##图2，corplot

rm(list = ls())
library("corrplot")
library("pheatmap")
library(ggcorrplot)

design = read.table("../data/design.txt", header=T, row.names= 1, sep="\t") 
otu_table = read.delim("../data/bray_curtis_otu_table_css.txt", row.names= 1,  header=T, sep="\t")
?read.delim ##是read.table的具体化，规定了一些参数

#实验设计筛选：这里只筛选昌平的日本晴相关组(A50)
# setting subset design
if (TRUE){
  sub_design = subset(design,groupID %in% c("A50Cp0","A50Cp1","A50Cp2","A50Cp3","A50Cp7","A50Cp10","A50Cp14","A50Cp21","A50Cp28","A50Cp35","A50Cp42","A50Cp49","A50Cp63","A50Cp70","A50Cp77","A50Cp84","A50Cp91","A50Cp98","A50Cp112","A50Cp119") ) # select group1
}else{
  sub_design = design
}
sub_design$group=sub_design$groupID

#设置组顺序，这是必须的，否则时间10，100天会位于1的后面。
# Set group order
if ("TRUE" == "TRUE") {
  sub_design$group  = factor(sub_design$group, levels=c("A50Cp0","A50Cp1","A50Cp2","A50Cp3","A50Cp7","A50Cp10","A50Cp14","A50Cp21","A50Cp28","A50Cp35","A50Cp42","A50Cp49","A50Cp63","A50Cp70","A50Cp77","A50Cp84","A50Cp91","A50Cp98","A50Cp112","A50Cp119"))
}else{sub_design$group  = as.factor(sub_design$group)}

print(paste("Number of group: ",length(unique(sub_design$group)),sep="")) # show group numbers

#筛选后的实验设计样本与OTU表交叉筛选
# sub and reorder subdesign and otu_table
idx = rownames(sub_design) %in% colnames(otu_table)
sub_design = sub_design[idx,]
count = otu_table[, rownames(sub_design)]

#OTU表标准化为百分比，在R中只需一小行代码
norm = t(t(count)/colSums(count,na=T)) * 100 # normalization to total 100

#按组合并：因为样本太多，一小部分过百个样本，展示太乱，按组求均值，组间比较更容易发现规律
# Pearson correlation among groups
sampFile = as.data.frame(sub_design$group,row.names = row.names(sub_design))
colnames(sampFile)[1] = "group"
mat = norm
mat_t = t(mat)

mat_t2 = merge(sampFile, mat_t, by="row.names")
mat_t2 = mat_t2[,-1]

mat_mean = aggregate(mat_t2[,-1], by=mat_t2[1], FUN=mean) # mean
mat_mean_final = do.call(rbind, mat_mean)[-1,]
geno = mat_mean$group
colnames(mat_mean_final) = geno

#计算相关系数，并保留3位小数
sim=cor(mat_mean_final,method="pearson")
sim=round(sim,3)
sim

#ggcorrplot绘制相关矩阵
pdf(file="ggcorrplot_pearson_A50Cp.pdf", height = 2.5, width = 2.5)
ggcorrplot(sim, type = "upper", colors = c("green", "yellow", "red")) # , method = "circle"
dev.off()

###corrplot
# 人为设置颜色度
col1 <- colorRampPalette(c("green", "green", "red"))

pdf(file="corplot_pie_pearson_A50Cp.pdf", height = 2.5, width = 2.5)
corrplot(sim, method="pie", type="lower", col=col1(100)) # , diag=F , na.label = "1"
dev.off()

#生成图例
col1 <- colorRampPalette(c("green", "red"))
corrplot(sim, method="pie", type="lower", col=col1(100)) # , diag=F , na.label = "1"

# 生成时间热图，分别为土和植物的
time1 = c(0,1,2,3,7,10,14,21,28,35,42,49,63,70,77,84,91,98,112,119)
time2 = c(0,41,48,54,62,77,84,90,97,119,0,0,0,0,0,0,0,0,0,0)
time=data.frame(time1,time2);time
pheatmap(time, cluster_rows = F,  cluster_cols = F)
pheatmap(time, cluster_rows = F,  cluster_cols = F, filename = "corplot_pie_legend_time.pdf" ,width=2, height=4)
