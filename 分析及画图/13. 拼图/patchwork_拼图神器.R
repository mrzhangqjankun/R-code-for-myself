##2019.1.30
#https://mp.weixin.qq.com/s/xw4SVhaxAg920hOH6xKRHQ
#https://github.com/thomasp85/patchwork
#绘图专题 | ggplot2拼图神器patchwork 

#devtools::install_github("thomasp85/patchwork")

library(ggplot2)
library(patchwork)

# + 左右组合
p1 <- ggplot(mtcars) + geom_point(aes(mpg, disp))
p2 <- ggplot(mtcars) + geom_boxplot(aes(gear, disp, group = gear))

p1 + p2
# or
ggplot(mtcars) +
  geom_point(aes(mpg, disp)) +
ggplot(mtcars) + 
  geom_boxplot(aes(gear, disp, group = gear))


# plot_layout()自定义排布
p1 + p2 + plot_layout(ncol = 1, heights = c(3, 1)) #一列，上下高度比例

p1 + plot_spacer() + p2 #中间加空白
p1 + plot_spacer() + p2 + plot_layout(nrow = 1, width=c(5,1,5)) #一行，左右宽度比

# 高级的组合
p3 <- ggplot(mtcars) + geom_smooth(aes(disp, qsec))
p4 <- ggplot(mtcars) + geom_bar(aes(carb))

p1 + {
  p2 + {
    p3 +
      p4 +
      plot_layout(ncol = 1)
  }
} +
  plot_layout(ncol = 1)

p1 + p2 + p3 + plot_layout(ncol = 1)

#两种效果
p1 + p2 + p3 + plot_layout(nrow = 2,height=c(2,1)) #p3在左边
p1 + p2 + plot_spacer() + p3 + plot_layout(nrow = 2)

# -表明前后两者处于同一等级。默认按照行排列。
p1 + p2 - p3 + plot_layout(ncol = 1)

# | 操作符用于设置水平排布， / 操作符垂直排布
(p1 | p2 | p3) / p4

#下面三种等同
(p1 + p2)/p3 + plot_layout(height = c(2,1)) 
(p1 | p2)/p3 + plot_layout(height = c(2,1)) 
(p1 + p2)-p3 + plot_layout(ncol=1, height = c(2,1)) 

#更复杂一点
((p1 + p2/p3) - p4) + plot_layout(width = c(2,1)) # -换成+效果相同

#所有图统一调整格式  &
(p1 + (p2 + p3) + p4 + plot_layout(ncol = 1)) & 
  (
    theme_bw() + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
   )

# *只能将格式应用在当前嵌套水平上


##2019.11.23

##cowplot乃旧爱，patchwork是新欢 

##https://mp.weixin.qq.com/s/Y_uK2H7ketmFpJMvkNUNtg
library(patchwork)
library(ggplot2)

p1 <- ggplot(mtcars) +
  geom_point(aes(mpg, disp)) +
  ggtitle('图一')

p2 <- ggplot(mtcars) +
  geom_boxplot(aes(gear, disp, group = gear)) +
  ggtitle('图二')

p3 <- ggplot(mtcars) +
  geom_point(aes(hp, wt, colour = mpg)) +
  ggtitle('图三')

p4 <- ggplot(mtcars) +
  geom_bar(aes(gear)) +
  facet_wrap(~cyl) +
  ggtitle('图四')

#用plot_spacer()来填空白，一如既往地用+号来拼：
p1 + plot_spacer() +
  p2 + plot_spacer() +
  p3 + plot_spacer()

#+号大家都知道，按照row来拼，用/，按照column来拼，用|，这个语法绝了！
#而且一堆+号，在拼图多的时候也很乱，用/和|配以()和换行，可读性太强了。
(p2 / p3 ) | p1

#+，/和|毕竟是语法糖，要控制细节，全在plot_layout里。
p1 + p2 + p3 + p4 +
  plot_layout(widths = c(3, 1))

#拼图都是把整张图分为网格，往里面填，当你要拼复杂点的时候，那就是嵌套去拼，
#比如在cowplot里，plot_grid套plot_grid，这样你才能够使一张图在网格里占用不止一个格子。但嵌套有时候很难保证对齐。
#这里#代表空白，ABCD分别对应这四张图，这个layout中ABCD的位置，就对应于图在网络中的位置。

layout <- "
##BBBB
AACCDD
##CCDD
"
p1 + p2 + p3 + p4 +
  plot_layout(design = layout)

#你还可以用area来指定在网络中的位置，这样更加容易编程控制细节：
layout <- c(
  area(t = 2, l = 1, b = 5, r = 4),
  area(t = 1, l = 3, b = 3, r = 5)
)
p1 + p2 +
  plot_layout(design = layout)
#当然这些格子的相对高度和宽度，也是可以通过playout_layout中的widths和heights参数控制的。

#指定guides='collect'可以帮助我们把legend都给集中在一起，放到图的一侧：
g <- ggplot(mtcars) +
  geom_point(aes(mpg, disp, colour = mpg, size = wt))
g1 <- g + ggtitle("图一")
g2 <- g + scale_colour_viridis_c() + ggtitle("图二")

(p3 | (g1 / g2)) +
  plot_layout(guides = 'collect')

#它厉害就厉害在会把所有图的图例都收集，并且会比较，不同的，比如说g1和g2的颜色映射不一样，图例都有，而点大小的图例是一样的，只出来一个。比较图例这个工作，必须服。
#如果想要图例单独放在一个格子，用guide_area()：
g1 + p2 + p3 +
  guide_area() +
  plot_layout(guides = 'collect')
