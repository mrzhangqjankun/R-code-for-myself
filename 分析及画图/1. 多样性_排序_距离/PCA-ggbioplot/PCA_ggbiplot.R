##2017.12.25
##https://mp.weixin.qq.com/s?__biz=MzUzMjA4Njc1MA==&mid=2247484710&idx=1&sn=340739fd1727defa6c67668359f104db&chksm=fab9ef97cdce6681d9b3666b82b38ef5bf393ca3735f0bb8e0521a7aca88621062375839a842&mpshare=1&scene=1&srcid=1222kWUgkzzB5qsA0xBdPUY6&pass_ticket=wUxYy0hIGC9InyCHvGkV0xdIy3CMZEetAKzxSv9176ddoFR4rh8HiF9xy2eCuiYs#rd
##ggbiplot-最好看的PCA作图：样品PCA散点+分组椭圆+变量贡献与相关 
##https://github.com/vqv/ggbiplot/blob/master/README.md

rm(list=ls(all=TRUE))

library(devtools)
install_github("vqv/ggbiplot")

library(ggbiplot)
data(wine)
wine.pca <- prcomp(wine, scale. = TRUE)
?prcomp
plot(wine.pca$x)

ggbiplot(wine.pca, obs.scale = 1, var.scale = 1,
         groups = wine.class, ellipse = TRUE, circle = TRUE) +
  scale_color_discrete(name = '') +
  theme(legend.direction = 'horizontal', legend.position = 'top')



##宏基因组公众号的数据PE-250
setwd('E:/桌面/R script 2017/PCA-ggbioplot/')

# 读入实验设计
design = read.table("design.txt", header=T, row.names= 1, sep="\t") 

# 读取OTU表
otu_table = read.delim("otu_table.txt", row.names= 1,  header=T, sep="\t")

# 过滤数据并排序
idx = rownames(design) %in% colnames(otu_table) 
sub_design = design[idx,]
count = otu_table[, rownames(sub_design)]

# 基于OTU表PCA分析
otu.pca <- prcomp(t(count), scale. = TRUE)

plot(otu.pca$x)

# 绘制PCA图，并按组添加椭圆
ggbiplot(otu.pca, obs.scale = 1, var.scale = 1,
         groups = sub_design$genotype, ellipse = T,var.axes = F)

######展示主要差异菌与主成分的关系
# 显著高丰度菌的影响

# 转换原始数据为百分比
norm = t(t(count)/colSums(count,na=T)) * 100 # normalization to total 100

# 筛选mad值大于0.5的OTU
mad.5 = norm[apply(norm,1,mad)>0.5,]
# 另一种方法：按mad值排序取前6波动最大的OTUs
mad.5 = head(norm[order(apply(norm,1,mad), decreasing=T),],n=6)
# 计算PCA和菌与菌轴的相关性
otu.pca <- prcomp(t(mad.5))
ggbiplot(otu.pca, obs.scale = 1, var.scale = 1,
         groups = sub_design$genotype, ellipse = TRUE,var.axes = T)


###ggbiplot
ggbiplot(pcobj, choices = 1:2, scale = 1, pc.biplot =
           TRUE, obs.scale = 1 - scale, var.scale = scale, groups =
           NULL, ellipse = FALSE, ellipse.prob = 0.68, labels =
           NULL, labels.size = 3, alpha = 1, var.axes = TRUE, circle
         = FALSE, circle.prob = 0.69, varname.size = 3,
         varname.adjust = 1.5, varname.abbrev = FALSE, ...)

pcobj # prcomp()或princomp()返回结果
choices    # 选择轴，默认1：2
scale    # covariance biplot (scale = 1), form biplot (scale = 0). When scale = 1, the inner product between the variables approximates the covariance and the distance between the points approximates the Mahalanobis distance.
obs.scale # 标准化观测值
var.scale # 标准化变异
pc.biplot # 兼容 biplot.princomp()
groups # 组信息，并按组上色
ellipse    # 添加组椭圆
ellipse.prob # 置信区间
labels    # 向量名称
labels.size    # 名称大小
alpha # 点透明度 (0 = TRUEransparent, 1 = opaque)
circle # 绘制相关环(only applies when prcomp was called with scale = TRUE and when var.scale = 1)
var.axes # 绘制变量线-菌相关
varname.size # 变量名大小
varname.adjust # 标签与箭头距离 >= 1 means farther from the arrow
varname.abbrev # 标签是否缩写



###2018.7.17
##一文看懂主成分分析
https://mp.weixin.qq.com/s?__biz=MzAxMDkxODM1Ng==&mid=2247487020&idx=1&sn=e28ed93276a971c193d843307ab92274&chksm=9b484e97ac3fc781e10502d9c90f8c9fbb1032a1ff7cf96611647e1afc30086c5ed80a72b116&scene=0#rd

# R语言有非常多的途径做主成分分析，比如自带的princomp()和psych包的principal()函数，
# 还有gmodels包的fast.prcomp函数。
# R包factoextra
# 包ropls 
#而我们直接用rda()