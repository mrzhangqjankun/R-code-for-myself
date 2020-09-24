##3.16 中国地图加地点
##https://yihui.name/cn/2008/10/china-map-and-city-locations-with-r/

rm (list = ls (all=TRUE))

setwd("E:/桌面/R script 2017/Chinese map_2018.3.16")

#install.packages("mapdata")
#install.packages("maptools")


#library(maps)
library(mapdata)
library(maptools)  ### A package for building maps  地图导入
library(ggplot2)
library(plyr)       # To manipulate data 合并数据

x=readShapePoly("bou2_4p.shp")  #1,2,3,4 国，省，市，县级地图。4p-4百万分之一
x1 = x@data;head(x1)
x2 = data.frame(id = rownames(x1),x1);head(x2)
x_map = fortify(x);?fortify    ## #转化为数据框
x_map_data = join(x_map,x2,type = "full")
  
  
plot(x)
map(x, col = "darkgray", ylim = c(18, 54), panel.first = grid())


# #dat = read.csv(text = "城市,jd,wd  ##注意这个数据，城市名字中间有空格，必须去掉空格才能正常显示
#                北 京,116.4666667,39.9
#                上 海,121.4833333,31.23333333
#                天 津,117.1833333,39.15
#                重 庆,106.5333333,29.53333333
#                哈尔滨,126.6833333,45.75
#                长 春,125.3166667,43.86666667
#                沈 阳,123.4,41.83333333
#                呼和浩特,111.8,40.81666667
#                石家庄,114.4666667,38.03333333
#                太 原,112.5666667,37.86666667
#                济 南,117,36.63333333
#                郑 州,113.7,34.8
#                西 安,108.9,34.26666667
#                兰 州,103.8166667,36.05
#                银 川,106.2666667,38.33333333
#                西 宁,101.75,36.63333333
#                乌鲁木齐,87.6,43.8
#                合 肥,117.3,31.85
#                南 京,118.8333333,32.03333333
#                杭 州,120.15,30.23333333
#                长 沙,113,28.18333333
#                南 昌,115.8666667,28.68333333
#                武 汉,114.35,30.61666667
#                成 都,104.0833333,30.65
#                贵 阳,106.7,26.58333333
#                福 州,119.3,26.08333333
#                台 北,121.5166667,25.05
#                广 州,113.25,23.13333333
#                海 口,110.3333333,20.03333333
#                南 宁,108.3333333,22.8
#                昆 明,102.6833333,25
#                拉 萨,91.16666667,29.66666667
#                香 港,114.1666667,22.3
# #               澳门,113.5,22.2")

par(mar=rep(0,4))
dat = read.csv(text = "采样,jd,wd
               北京,116.4666667,39.9
               长沙,113,28.18333333
               西藏,85.7525,29.6058");dat


#map("china", col = "darkgray", ylim = c(18, 54), panel.first = grid())
#points(dat$jd, dat$wd, pch = 19, col = rgb(0, 0, 0, 0.5))
#text(dat$jd, dat$wd, dat[, 1], cex = 0.9, col = rgb(0,0, 0, 0.7), pos = c(2, 4, 4, 4, 3, 4, 2, 3, 4, 2, 4, 2, 2,
 #                                                                       4, 3, 2, 1, 3, 1, 1, 2, 3, 2, 2, 1, 2, 4, 3, 1, 2, 2, 4, 4, 2))
#text(dat$jd, dat$wd, dat[, 1], cex = 0.9, col = rgb(0,0, 0, 0.7))
#axis(1, lwd = 0); axis(2, lwd = 0); axis(3, lwd = 0); axis(4, lwd = 0)


points(dat$jd, dat$wd, pch = 19, col = "red")
text(dat$jd, dat$wd, dat[, 1], cex = 0.9, col = "red")

#####################################################

##6.26
#R语言ggplot2地理信息可视化上+下
https://mp.weixin.qq.com/s?__biz=MzA3MTM3NTA5Ng==&mid=2651058091&idx=2&sn=235f6612f4bca215f42a5334c857177a&chksm=84d9ce3cb3ae472a2e3b97a394b30b5a4a1536e9ded0e61157fd3a4b3dc10dff267331f151f7&scene=21#wechat_redirect

https://mp.weixin.qq.com/s?__biz=MzA3MTM3NTA5Ng==&mid=2651058111&idx=2&sn=7b60bc6d10d87f652b9bac63e538fd89&chksm=84d9ce28b3ae473e3613ebab077410e4d5b6f530c11f4f642f41354354e30fa1fdee98f98360&scene=21#wechat_redirect



##带柱形的地图（BarMap）是柱形图和地图的组合，可以用柱形系列表示地理位置的一系列数据指标，
#柱形的高度对应指标的数据，不同的指标使用不同的颜色区分。
##在R语言中，可以使用geom_polygon()函数绘制地图，geom_rect()函数绘制柱形数据，
#geom_text()函数添加数据标签。

##带饼图的地图（PieMap）是饼图和地图的组合，
#可以用饼图系列表示地理位置的一系列类别的数值占比情况，
#饼图的占比对应类别的数据，不同的类别也可以使用不同的颜色区分。
#R语言scatterpie包的geom_scatterpie()函数可以绘制散点复合饼图和气泡复合饼图。
#先使用geom_polygon()函数绘制地图，
#再使用geom_scatterpie()函数在地图图层上就可以绘制散点复合饼图或者气泡复合饼图。



#http://blog.sina.com.cn/s/blog_6bc5205e0102vr65.html
#R笔记7：ggplot绘制商务图表--地图上的迷你柱形图 
ggplot()+
  geom_polygon(aes(x=long, y=lat, group=group), data=x, fill="beige", colour="grey60") +     #画地图底图
  geom_errorbar(aes(x=jd-0.5, ymin=wd, ymax=wd+zhibiao2014/max(zhibiao2014, zhibiao2015)*5), data=province_dat, size=5, color="steelblue", width=0, alpha=0.8)+     #误差线画柱形图，x左偏0.5
  geom_errorbar(aes(x=jd+0.5, ymin=wd, ymax=wd+zhibiao2015/max(zhibiao2014, zhibiao2015)*5), data=province_dat, size=5,color="orange", width=0, alpha=0.8)+     #误差线画柱形图，x右偏0.5
  geom_text(aes(x=jd, y=wd-0.5, label=paste(province, ifelse(changepct >0 ,"▲","▼"), round(changepct,3)*100, "%", sep='')), data=province_dat)+     #标注省名
  theme(               #清除不需要的元素
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  ) +
  annotate("text", x=80, y=25, label="● 2014", color= "steelblue", size=8) +     #画图例
  annotate("text", x=80, y=23, label="● 2015", color= "orange", size=8)

##http://bbs.pinggu.org/forum.php?mod=viewthread&tid=4182165&page=1#pid33890512
##[程序分享] R 绘制中国地图不同方法的比较

