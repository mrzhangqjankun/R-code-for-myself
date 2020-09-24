##2018.11.16

#ggplot2的3维画图时代 
https://mp.weixin.qq.com/s?__biz=MzI5NjUyNzkxMg==&mid=2247486559&idx=1&sn=9838eb97cc675d26cb63abffd19fc36b&chksm=ec43bd18db34340ec33bb763578a6970507ff1946301c7710aa707b7c8fdbdbf2a83702ed2f2&scene=0#rd

rm(list=ls())
getwd()
#两个包threed和ggthreed，第一个包负责做坐标轴的转换，
#第二个包提供一些图层，方便我们直接ggplot出3维图，
#目前这个包只提供了geom_threedpie一个图层，可以期待以后出更多的图层

#install.packages("devtools")
#remove.packages("devtools")
#packageVersion("devtools") 
devtools::install_github("coolbutuseless/threed")  ##一直安装不上，会报错Error in read.dcf(path) : Found continuation line starting '    representation. ...' at begin of record.
devtools::install_github("coolbutuseless/ggthreed")

p_load(githubinstall)
githubinstall("threed")
githubinstall("ggthreed")

library(ggplot2)
library(ggthreed)
ggplot(mtcars) + 
  geom_threedpie(aes(x = as.factor(cyl))) + 
  theme_void() + 
  theme(legend.position = 'bottom')

library(threed)
library(dplyr)

camera_to_world <- look_at_matrix(eye = c(1.5, 1.75, 4), at = c(0, 0, 0))

obj <- threed::mesh3dobj$teapot %>%
  transform_by(invert_matrix(camera_to_world)) %>%
  perspective_projection() 
#
ggplot(obj, aes(x, y, group = zorder)) +
  geom_polygon(aes(fill = zorder, colour = zorder)) +
  theme_minimal() +
  theme(
    legend.position = 'none',
    axis.text       = element_blank()
  ) +
  coord_equal() +
  scale_fill_viridis_d (option = 'A') +
  scale_color_viridis_d(option = 'A')

##2018.11.26
#3D-PCA
http://www.vccoo.com/v/b92ab1?source=rss&tdsourcetag=s_pcqq_aiomsg

setwd("E:/桌面/长兴岛水样及底泥2018.6/数据分析/2018.8.4-16S")
x<-read.table(file="Galaxy72-[resample_UPARSE_otu_table.txt].txt",sep="\t",header=T,row.names=1)
group0 = read.table("group0.txt", sep="\t", row.names=1 )
library(vegan)
x.pca = rda(x);x.pca
outputpca = summary(x.pca)
str(outputpca)

a = outputpca$species ;a
b = outputpca$cont$importance ;b

pc1 = round(b[2,1],2);pc1
pc2 = round(b[2,2],2);pc2

library(plot3D) 
library(scatterplot3d)

##PC1,PC2,PC3
x=a[,1]
y=a[,2]
z=a[,3]
color= rainbow(length(unique(group0[,1])))
dat <- data.frame(x,y,z)
plot3d <- scatterplot3d(x,y,z,color=color,pch=1,xlab="PC1",ylab="PC2",zlab="PC3")
legend(plot3d$xyz.convert(0.17,0.05,10),pch=1,legend=row.names(group0),col=color)
str(plot3d)
#xyz.convert  规定legend位置
?scatterplot3d

##2019.5.28
#https://mp.weixin.qq.com/s/ubzefd4VUw_4ITGW67QCLQ
#如何绘制绚丽2D/3D的PCA图？

#############加载多个包
need.packages=c("rgl","pca3d")
lapply(need.packages,library,character.only=TRUE)

#### 安装未安装的包 
need.packs = c("rgl", "pca3d")  # 想要安装的R包
has = need.packs %in% rownames(installed.packages())  
has

##P_load
library(pacman) 
options(warn = -1)    # 这里
p_load(ggplot2)  ##p_load = install.packages + library()

p_load(rgl,pca3d)

#载入自带的测试数据
data(metabo)  #数据是三组个体血清代谢产物的相对丰度
dim(metabo)   #数据集包含136行和424列
table(metabo[,1])  #展示分组信息
#PCA
pca <- prcomp( metabo[,-1], scale.=TRUE)  #数据选用所有的行及除第一列的数据（第一列为分组信息）

#制作2D的PCA图
pca2d( pca, group= metabo[,1] );?pca2d

#制作3D的PCA图
pca3d( pca, group= metabo[,1] );?pca3d
##同时3D的PCA图通过点击鼠标可以进行翻转等不同平面的操作
##3D的PCA图增加椭圆
pca3d(pca, group=gr, show.ellipses=TRUE,
      ellipse.ci=0.75, show.plane=FALSE)

##3D的PCA可以通过改变背景，线图及点的方式呈现更加绚丽的图片
pca3d( pca, group= metabo[,1],
       fancy= TRUE, bg= "black",
       show.group.labels=TRUE,
       axes.color= "white", new= TRUE )

##2019.7.20
#一个震撼的交互型3D可视化R包 - 可直接转ggplot2图为3D 
##https://mp.weixin.qq.com/s/seAdPe92KXLAz1J8_yqn0Q

# 安装rayshader包
#install.packages("remote")
#remotes::install_github("tylermorganwall/rayshader")
devtools::install_github("tylermorganwall/rayshader")  ##报错装不上Error: Failed to install 'rayshader' from GitHub: Could not find tools necessary to compile a package
library(rayshader)
library(ggplot2)

# 查看数据格式
head(diamonds)

# 绘制二维密度图
gg = ggplot(diamonds, aes(x, depth)) +
  stat_density_2d(aes(fill = stat(nlevel)), #绘制密度等高线图
                  geom = "polygon",
                  n = 100,bins = 10, contour = TRUE) +
  facet_wrap(clarity~.) +    # 按clarity分类
  scale_fill_viridis_c(option = "A") # 将map颜色设置为“岩浆”色，简称为“A”，共有“A”，“B”，“C”，“D”和“E”五种；gg # 绘制2D图

# 转成3D图，只需要plot_gg函数即可
plot_gg(gg,multicore=TRUE,width=5,height=5,scale=250)