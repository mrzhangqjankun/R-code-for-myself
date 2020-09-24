##2018.11.13
#转UpSet图为ggplot? 
https://mp.weixin.qq.com/s?__biz=MzI5NjUyNzkxMg==&mid=2247486543&idx=1&sn=db5c7ee75a3165e3a2c63fa0d4adeb3d&chksm=ec43bd08db34341e4e53be3c858592093a4383bb360f545e2e7682e7c3c56cad29ac15fee551&mpshare=1&scene=1&srcid=11138DPNV3YSgr0qDxec7eYL&pass_ticket=d1Pi8pPOrgdYesDy8Ybu9QvmkFE%2Fo90Unz0jEGptuPmQY6XegSYGPWSJ4dnxEX9E#rd


rm(list=ls())

# install.packages("devtools")
# devtools::install_github("GuangchuangYu/UpSetR")
# install.packages("ggplotify")
require(UpSetR)
movies <- read.csv( system.file("extdata", "movies.csv", package = "UpSetR"), header=T, sep=";" )
p1 <- upset(movies)
p1
class(p1)
##转成ggplot对象
require(ggplotify)
g1 <- as.ggplot(p1)
g1
g1 = g1 + plot_theme

# 转成ggplot好处很多，比如你可以随意在上面加图层，画个图，做文字标记等自然不在话下。
# 就是嵌个小图也是随手拈来，因为我们用ggimage包。
# 这里我使用yyplot来画venn图，《ggplot2版本的维恩图》，然后使用ggimage包来嵌图，
# 这样就可以结合venn和upset两种图的优势，并且可以让upset图更加紧凑些。

# devtools::install_github("GuangchuangYu/yyplot")

require(yyplot)
g2 <- ggvenn(movies[, c(3,6,9,15,17)])
g2
require(ggimage)
g3 <- g1 + geom_subview(subview = g2 + theme_void(), x=.7, y=.7, w=.6, h=.6)
g3

##拼图
p4 <- upset(movies,attribute.plots=list(gridrows=60,
                                        plots=list(list(plot=scatter_plot, x="ReleaseDate", y="AvgRating"),
                                                   list(plot=scatter_plot, x="ReleaseDate", y="Watches"),
                                                   list(plot=scatter_plot, x="Watches", y="AvgRating"),
                                                   list(plot=histogram, x="ReleaseDate")), ncols = 2)
)

g4 <- as.ggplot(p4)
g4

require(cowplot)

plot_grid(
  plot_grid(
    plot_grid(g1, g2, ncol=2, labels=c("A", "B")),
    g3, ncol=1, labels=c("", "C")
  ),
  g4, ncol=2, labels=c("", "D")
)


?as.ggplot  #convert plot to ggplot object # as.ggplot(plot, scale = 1)

?ggvenn  #venn plot using ggplot2  # ggvenn(x, alpha = 0.5),alpha	transparency of color
set.seed(2017-11-08)
x <- matrix(sample(0:4, 40, TRUE, c(.5, .1, .1, .1, .1)), ncol=10)
colnames(x) <- LETTERS[1:10];x
ggvenn(x)
## ggeven 理论上可以做无数个样本?  10个没有问题

?geom_subview   #subview geom
?theme_void     #A completely empty theme.

