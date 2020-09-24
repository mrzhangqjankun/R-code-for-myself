##2020.4.29

##好的beta多样性排序图可不是加一个置信椭圆就好了

##https://mp.weixin.qq.com/s/ilmFfu8tg1LJ5rS-OwmX2w

library('vegan')
library('ggplot2')
data(dune, dune.env)
#--------nmds排序函数#---
ord <- metaMDS(dune)
scrs <- scores(ord, display = 'sites')
# 组合排序坐标和分组信息
scrs <- cbind(as.data.frame(scrs), Management = dune.env$Management)
# 求均值
cent <- aggregate(cbind(NMDS1, NMDS2) ~ Management, data = scrs, FUN = mean)
# 合并到样本坐标数据中
segs <- merge(scrs, setNames(cent, c('Management','oNMDS1','oNMDS2')),
              by = 'Management', sort = FALSE)

ggplot(scrs, aes(x = NMDS1, y = NMDS2, colour = Management)) +
  geom_point(data = cent, size = 3) +                         # centroids
  geom_point() +                                              # sample scores
  coord_fixed()  +                                             # same axis scaling
  theme_void()

#置信椭圆
ggplot(scrs, aes(x = NMDS1, y = NMDS2, colour = Management)) +
  geom_point(data = cent, size = 3) +
  geom_point() +
  coord_fixed()  +
  stat_ellipse( linetype = 2,level = 0.65,aes(group  = Management, colour =  Management)) +
  theme_void()

#连线放射
ggplot(scrs, aes(x = NMDS1, y = NMDS2, colour = Management)) +
  geom_segment(data = segs,
               mapping = aes(xend = oNMDS1, yend = oNMDS2)) + # spiders
  geom_point(data = cent, size = 3) +                         # centroids
  geom_point() +                                              # sample scores
  coord_fixed()  +                                             # same axis scaling
  theme_void()

#波普样式
library(ggalt)
ggplot(scrs, aes(x = NMDS1, y = NMDS2, colour = Management)) +
  geom_point(data = cent, size = 3) +
  geom_point() +
  coord_fixed()  +
  geom_bkde2d() +
  theme_void()

#外点连线
ggplot(scrs, aes(x = NMDS1, y = NMDS2, colour = Management)) +
  geom_point(data = cent, size = 3) +
  geom_point() +
  coord_fixed()  +
  geom_encircle(s_shape=1, expand=0) +
  theme_void()
