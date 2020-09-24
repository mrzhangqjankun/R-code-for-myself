##2018.11.4
##一个敲有趣的R语言拼图工具 customLayout
#https://mp.weixin.qq.com/s?__biz=MzA3Njc0NzA0MA==&mid=2653192634&idx=1&sn=4a07a00023e23ac6bf38fa9307c350c5&chksm=848cb9f5b3fb30e3eefe629e2d0de9a1861bff91f1ec46a7bdd2ea045aa5e7a0021fd302fb72&scene=0#rd

rm(list=ls())

# install.packages("customLayout")
# devtools::install_github("zzawadz/customLayout")

library(customLayout)
library(magrittr)

lay = lay_new(
  mat = matrix(1:4,ncol = 2),
  widths = c(3,2),
  heights = c(2,1)
)
lay_show(lay)

lay2 <- lay_new(
  matrix(1:9, nc = 3),
  widths = c(3, 5,4),
  heights = c(2, 4,6))
lay_show(lay2)

cl = lay_bind_col(lay, lay2, widths = c(3, 1))
lay_show(cl)

lay3 <- lay_new(matrix(1:2))
lay4 <- lay_bind_row(cl, lay3, heights = c(5, 2))
lay_show(lay4)

#也可以将一个模块嵌入到两一个模块特定位置。
#这里就将lay2嵌入到lay模块的第四个区域，但may2内部的布局结构任然不变。
lay <- lay_new(
  matrix(1:4, nc = 2),
  widths = c(3, 2),
  heights = c(2, 1))
lay_show(lay)

lay2 <- lay_new(
  matrix(1:4, nc = 2),
  widths = c(3, 5),
  heights = c(2, 4))
lay_show(lay2)

slay <- lay_split_field(lay, lay2, field = 4)
lay_show(slay)


#基础绘图对象的拼接：
par(mar = c(3, 2, 2, 1))
lay  <- lay_new(
  matrix(1:4, nc = 2),
  widths = c(3, 2),
  heights = c(2, 1))
lay2 <- lay_new(matrix(1:3))
cl   <- lay_bind_col(lay, lay2, widths = c(3, 1))
lay_show(cl)

lay_set(cl) # initialize drawing areaset.seed(123)
plot(1:100 + rnorm(100))
plot(rnorm(100), type = "l")
hist(rnorm(500))
acf(rnorm(100))
pie(c(3, 4, 6), col = 2:4)
pie(c(3, 2, 7), col = 2:4 + 3)
pie(c(5, 4, 2), col = 2:4 + 6)

##grid(ggplot2)图形对象的拼接:
library(ggplot2)
library(gridExtra)

lay  <- lay_new( matrix(1:2, ncol = 1))
lay2 <- lay_new(matrix(1:3))
cl   <- lay_bind_col(lay, lay2, widths = c(3, 1))
lay_show(cl)
cuts <- sort(unique(diamonds[["cut"]]),decreasing = TRUE)

make_cut_plot <- function(cut) {
  dd <- diamonds[diamonds[["cut"]] == cut, ]
  ggplot(dd) +
    geom_point(aes(carat, price)) +
    facet_wrap("cut")
}

plots <- lapply(cuts, make_cut_plot)
lay_grid(plots, cl)

