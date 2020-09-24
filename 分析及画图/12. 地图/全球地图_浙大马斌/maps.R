##2019.5.24
##浙大马斌

# Beeswarm ----------------------------------------------------------------

#https://microbma.github.io/2019/05/13/bee.html
#Beeswarm plot with ggplot2  #ggbeeswarm package
#Beeswarm绘图是一种绘制点的方法，这些点通常会重叠，因此它们会彼此相邻。
#除了减少覆盖之外，它还帮助可视化数据在每个点上的密度(类似于小提琴图)，同时仍然单独显示每个数据点。

install.packages('ggbeeswarm')
set.seed(12345)
library(ggplot2)
library(ggbeeswarm);?ggbeeswarm
ggplot(iris, aes(Species, Sepal.Length)) + 
  geom_quasirandom()

#Plotting with various width:
ggplot(iris, aes(Species, Sepal.Length)) + 
  geom_quasirandom(varwidth = TRUE)

#Plotting with fixed width:
ggplot(iris, aes(Species, Sepal.Length)) + 
  geom_quasirandom(dodge.width=1)

##点还能改为其他分布形式
ggplot(iris, aes(Species, Sepal.Length)) + geom_quasirandom(method = "tukeyDense") + 
  ggtitle("Tukey + density")

ggplot(iris, aes(Species, Sepal.Length)) + geom_quasirandom(method = "tukey")+
  ggtitle("Tukey texture")

ggplot(iris, aes(Species, Sepal.Length)) + geom_quasirandom(method = "frowney") + 
  ggtitle("Banded frowns")

ggplot(iris, aes(Species, Sepal.Length)) + geom_quasirandom(method = "smiley")         + ggtitle("Banded smiles")


#geom_beeswarm
ggplot(iris,aes(Species, Sepal.Length)) + 
  geom_beeswarm()

ggplot(iris,aes(Species, Sepal.Length)) +       
  geom_beeswarm(cex=4,priority='density') #cex点之间缩放距离

# global map with points --------------------------------------------------
library(rgdal)
library(ggplot2)

# read shapefile
?readOGR
wmap <- readOGR(dsn="E:/桌面/R script 2017/全球地图_浙大马斌", layer="ne_110m_land")

# convert to dataframe
wmap_df <- fortify(wmap);?fortify()

# create a blank ggplot theme
theme_opts <-list(theme(panel.grid.minor = element_blank(),
                        panel.grid.major = element_blank(),
                        panel.background = element_blank(),
                        plot.background = element_rect(fill="#97CBFF"),
                        panel.border = element_blank(),
                        axis.line = element_blank(),
                        axis.text.x = element_blank(),
                        axis.text.y = element_blank(),
                        axis.ticks = element_blank(),
                        axis.title.x = element_blank(),
                        axis.title.y = element_blank(),
                        plot.title = element_text(size=22,hjust = .5)))

# plot map
ggplot(wmap_df, aes(long,lat, group=group)) + 
  geom_polygon() + 
  labs(title="World map (longlat)") + 
  coord_equal() + 
  theme_opts
#reproject from longlat to robinson
wmap_robin <- spTransform(wmap, CRS("+proj=robin"))
wmap_df_robin <- fortify(wmap_robin)
ggplot(wmap_df_robin, aes(long,lat, group=group)) + 
  geom_polygon() + 
  labs(title="World map (robinson)") + 
  coord_equal() +
  theme_opts

#show the hole
ggplot(wmap_df_robin, aes(long,lat, group=group, fill=hole)) + 
  geom_polygon() + 
  labs(title="World map (Robinson)") + 
  coord_equal() + 
  theme_opts +
  scale_fill_manual(values=c("#262626", "#97CBFF"), guide="none") 

# add graticule and bounding box (longlat)
grat <- readOGR("E:/桌面/R script 2017/全球地图_浙大马斌", layer="ne_110m_graticules_15") 
grat_df <- fortify(grat)

bbox <- readOGR("E:/桌面/R script 2017/全球地图_浙大马斌", layer="ne_110m_wgs84_bounding_box") 
bbox_df<- fortify(bbox)

ggplot(bbox_df, aes(long,lat, group=group)) + 
  geom_polygon(fill="white") +
  geom_polygon(data=wmap_df, aes(long,lat, group=group, fill=hole)) + 
  geom_path(data=grat_df, aes(long, lat, group=group, fill=NULL), linetype="dashed", color="grey50") +
  labs(title="World map + graticule (longlat)") + 
  coord_equal() + 
  theme_opts +
  scale_fill_manual(values=c("black", "white"), guide="none")

#robinson projection with graticule
grat_robin <- spTransform(grat, CRS("+proj=robin"))  # reproject graticule
grat_df_robin <- fortify(grat_robin)
bbox_robin <- spTransform(bbox, CRS("+proj=robin"))  # reproject bounding box
bbox_robin_df <- fortify(bbox_robin)

ggplot(bbox_robin_df, aes(long,lat, group=group)) + 
  geom_polygon(fill="#97CBFF") +
  geom_polygon(data=wmap_df_robin, aes(long,lat, group=group, fill=hole)) + 
  geom_path(data=grat_df_robin, aes(long, lat, group=group, fill=NULL),
            linetype="dashed", color="grey60",size =.1) +
  labs(title="World map (Robinson)") + 
  coord_equal() + 
  theme_opts + theme(plot.background = element_blank())+
  scale_fill_manual(values=c("grey30", "#97CBFF"), guide="none") 
# change colors & remove legend

#Add points on maps
#cord.info.df这个是点的文件，包含经纬度
places_robin_df <- project(cbind(cord.info.df$lon, cord.info.df$lat),
                           proj="+init=ESRI:54030") 
places_robin_df <- as.data.frame(places_robin_df)
names(places_robin_df) <- c("LONGITUDE", "LATITUDE")


ggplot(bbox_robin_df, aes(long,lat, group=group)) + 
  geom_polygon(fill="#97CBFF") +
  geom_polygon(data=wmap_df_robin, aes(long,lat, group=group, fill=hole)) + 
  geom_path(data=grat_df_robin, aes(long, lat, group=group, fill=NULL),
            linetype="dashed", color="grey60",size =.1) +
  labs(title="World map (Robinson)") + 
  coord_equal() + 
  theme_opts + 
  theme(plot.background = element_blank())+
  scale_fill_manual(values=c("grey30", "#97CBFF"), guide="none")+ 
  geom_point(data=places_robin_df,
             aes(LONGITUDE, LATITUDE),
             color=qualitative_hcl(5,alpha = .4,"Dark3")[2],
             size=1,
             inherit.aes = FALSE)

# neten package for network enhancement -----------------------------------
#Network Enhancement (NE)提高对称网络信噪比的新方法，从而便于下游网络分析。
#NE利用网络的传递边，利用局部结构增强簇内信号，减弱簇间信号。
devtools::install_github("microbma/neten")

library(neten)
#输入是一个相关系数矩阵
mat1 <- cor(matrix(rnorm(400),nrow = 20))
mat2 <- Network_Enhancement(mat1);?Network_Enhancement()
