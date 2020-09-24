##2020.3.10

##https://mp.weixin.qq.com/s/Jqdqvs_vWj7zuDdEFzjOFA

##自动出来的图例不好调？自己徒手画一个！

#用ggplot2来生成个legend，放在base plot上去。
#这里用base plot画了一个图，又用ggplot2画了一个，用cowplot把legend抽出来，然后再用我的另一个包ggimage进行图上嵌图
col = colorspace::rainbow_hcl(3)
names(col) = unique(iris$Species)

library(ggplotify)
color = col[iris$Species]

p = as.ggplot(~plot(iris$Sepal.Length, iris$Sepal.Width, col=color, pch=15))

library(ggplot2)
p2 = ggplot(iris, aes(Sepal.Length, Sepal.Width, color=Species)) +
  geom_point() + scale_color_manual(values=col, name="")

legend = cowplot::get_legend(p2)

p + ggimage::geom_subview(x=.7, y=.6, subview=legend)


######
#用ggplot2来来徒手一下。画出下面这个图例：

p2 <- ggplot() +
  annotate("point", x=1,y=1:3,shape=15, color=col) +
  annotate("text", x=1.01, y=1:3, label=names(col), hjust=0) +
  xlim(0.99, 1.2) + ylim(0, 4) + theme_void()
p2

p + ggimage::geom_subview(x=.25, y=.8, subview=p2)


###########3
#接画出legend
y = c(.77,.8,.83)
p + annotate("point", x=.2,y=y,shape=15, color=col, size=3) +
  annotate("text", x=.21, y=y, label=names(col), hjust=0)
