##2019.12.16

##R语言绘制双标图GGEBiplotGUI 和 ggbiplot

##https://mp.weixin.qq.com/s/sguv3ASBjEw_A9DOByXqoQ

# install.packages("GGEBiplotGUI")
# library(devtools)
# install_github("vqv/ggbiplot")

library(GGEBiplotGUI); ?GGEBiplotGUI
Ontario
data(Ontario)
GGEBiplot(Data = Ontario)

#上图主要说明：两环境线段之间的夹角的余弦值是它们的相关系数，
#夹角小于90度表示正相关，说明两环境对品种排序相似，大于90度表示负相关，
#表示两环境对品种排序相反，等于90度说明两环境不相关。同时，线段越长，区分能力越强。

library(ggbiplot)
data(wine)
wine.pca <- prcomp(wine, scale. = TRUE)
print(ggbiplot(wine.pca, obs.scale = 1,var.scale = 1, groups = wine.class, ellipse = TRUE, circle = TRUE))
