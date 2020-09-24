##2020.4.15

##ggforce 放大ggplot

##https://mp.weixin.qq.com/s/BxPDHi57EUWmYZ6_5UZ4NA

#install.packages("ggforce")
library(ggforce);?ggforce

#先看一个正常的图：
ggplot(iris, aes(Petal.Length, Petal.Width, colour = Species)) +
  geom_point()
#
#我们取一个分类的数据进行放大：
ggplot(iris, aes(Petal.Length, Petal.Width, colour = Species)) +
  geom_point() +
  facet_zoom(x = Species == 'versicolor')

#学术版本：
ggplot(iris, aes(Petal.Length, Petal.Width, colour = Species)) +
  geom_point() +
  facet_zoom(x = Species == 'versicolor') + cowplot::theme_cowplot()

#娱乐版本：
ggplot(iris, aes(Petal.Length, Petal.Width, colour = Species)) +
  geom_point() +
  facet_zoom(x = Species == 'versicolor') + theme_dark()

#还可以选择性展示数据：
ggplot(iris, aes(Petal.Length, Petal.Width, colour = Species)) +
  geom_point() +
  facet_zoom(x = Species == 'versicolor', zoom.data = Species == 'versicolor') +cowplot::theme_cowplot()

#按坐标范围放大：
ggplot(iris, aes(Petal.Length, Petal.Width, colour = Species)) +
  geom_point() +
  facet_zoom(xlim = c(2, 4)) + cowplot::theme_cowplot()

#更多学习见包文档：https://ggforce.data-imaginist.com/