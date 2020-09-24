##2019.11.4

##ggcor |相关系数矩阵可视化 

##https://mp.weixin.qq.com/s/mLImkisEMUZhPLOoPFlacA

if(!require(devtools))
  install.packages("devtools")
if(!require(ggcor))
  devtools::install_github("houyunhuang/ggcor",force=TRUE)

library(ggcor)
#as_cor_tbl
## function(x,
##          type = c("full", "upper", "lower"),
##          show.diag = TRUE,
##          p = NULL,
##          low = NULL,
##          upp = NULL,
##          cluster.type = c("none", "all", "row", "col"),
##          ...)

?as_cor_tbl
corr <- cor(mtcars)
df <- as_cor_tbl(corr)
df ## return a tibble

#fortify_cor()即调用cor()求相关系数
## function(
##          x,
##          y = NULL,
##          type = c("full", "upper", "lower"),
##          show.diag = FALSE,
##          cor.test = FALSE,
##          cor.test.alt = "two.sided",
##          cor.test.method = "pearson",
##          cluster.type = c("none", "all", "row", "col"),
##          cluster.method = "HC",
##          ... )

df01 <- fortify_cor(mtcars, cor.test = TRUE, cluster.type = "all")
df01

#as_cor_tbl()和fortify_cor()返回值均是cor_tbl类的数据框（准确的说是tibble），并包含其它额外特殊属性，要充分利用ggcor包中一系列工具函数带来的便捷性，必须调用（手动比较麻烦）这两个函数预处理数据。


#####ggcor()
ggcor(mtcars)
ggcor(mtcars, type = "lower")

df02 <- fortify_cor(mtcars, type = "upper")
ggcor(df02, panel.backgroud = "#66C2A5")

ggcor(mtcars) + geom_square()
ggcor(mtcars, type = "upper") + geom_circle2()
ggcor(mtcars, type = "lower", show.diag = TRUE) + geom_ellipse2()
ggcor(mtcars, type = "full", cluster.type = "all") + geom_pie2()

ggcor(mtcars, cluster.type = "all") +
  geom_colour() +
  geom_num(aes(num = r), colour = "grey90", size = 3.5)

ggcor(mtcars, type = "full", cor.test = TRUE) + geom_confbox()

ggcor(mtcars, type = "full", cor.test = TRUE, cluster.type = "all") +
  geom_colour() + geom_cross()

ggcor(mtcars, type = "full", cor.test = TRUE) +
  geom_square() + geom_cross()

##设置sig.thres，即要过滤的显著性临界值。
ggcor(mtcars, type = "full", cor.test = TRUE, cluster.type = "all") +
  geom_raster() +
  geom_mark(sig.thres = 0.05, size = 3, colour = "grey90")

##若是只要统计显著性标记的"*"号，不要系数
ggcor(mtcars, type = "full", cor.test = TRUE, cluster.type = "all") +
  geom_raster() +
  geom_mark(r = NA, sig.thres = 0.05, size = 5, colour = "grey90")


#那要改变星号标记规则，只要小于0.05的标记一颗星，其它什么都不标记呢？注意：因为星号在文本中显示在偏上的位置，若不设置vjust参数，看上去纵向会不居中。
ggcor(mtcars, type = "full", cor.test = TRUE, cluster.type = "all") +
  geom_raster() +
  geom_mark(r = NA, sig.level = 0.05, mark = "*", vjust = 0.65, size = 6, colour = "grey90")


##########################非对称相关系数矩阵
#非对称相关系数矩阵和非对称矩阵是有细微的区别的，
#前者表示行列代表不同的变量集合，相互之间的顺序可以打乱。
#所以，有时候要分析两个表中每个变量之间的相关性，
#此时得到的结果就是非对称的相关系数矩阵。
library(vegan) # 使用vegan包所带的数据集
data(varechem)
data(varespec)
df03 <- fortify_cor(x = varechem, y = varespec[ , 1:30], cluster.type = "col")
ggcor(df03) + geom_colour()

df04 <- fortify_cor(x = varespec[ , 1:30], y = varechem, cor.test = TRUE)
ggcor(df04) + geom_square() + geom_cross(size = 0.2)# 数据集不好，没几个显著的

# 上下三角不一样怎么画？
# ggcor提供了很多辅助函数来对cor_tbl数据进行过滤，函数命名规则上都以get_*开头。
# get_lower_data() —— 获取相关系数矩阵下三角所在行，仅支持对称的相关系数矩阵。
# get_upper_data() —— 获取相关系数矩阵上三角所在行，仅支持对称的相关系数矩阵。
# get_diag_data() —— 获取相关系数矩阵对角线所在行，仅支持对称的相关系数矩阵。
# get_diag_tri() —— 删除相关系数矩阵对角线所在行，仅支持对称的相关系数矩阵。
# get_data() —— 是以上四个函数的重新包装，主要在画图时使用，稍后通过例子详细说明。

df05 <- fortify_cor(x = varechem, cor.test = TRUE, cluster.type = "all")
ggcor(df05) + geom_circle2()

df05_lower <- get_lower_data(df05, show.diag = FALSE)
ggcor(df05_lower) + geom_circle2()

ggcor(df05) +
  geom_pie2(data = get_data(type = "upper", show.diag = FALSE)) +
  geom_ellipse2(data = get_data(type = "lower", show.diag = TRUE))

ggcor(df05) +
  geom_segment(aes(x = x - 0.5, y = y + 0.5, xend = x + 0.5, yend = y - 0.5),
               data = get_data(type = "diag"), size = 0.5, colour = "grey60") +
  geom_colour(data = get_data(type = "upper", show.diag = FALSE)) +
  geom_mark(data = get_data(type = "upper", show.diag = FALSE), size = 3) +
  geom_circle2(data = get_data(r >= 0.5, type = "lower", show.diag = FALSE),
               r = 0.8, fill = "#66C2A5") +
  geom_num(aes(num = r), data = get_data(type = "lower",
                                         show.diag = FALSE), size = 3)


#列名放在对角线？
ggcor(mtcars, cor.test = TRUE, cluster.type = "all") +
  geom_confbox(data = get_data(type = "upper", show.diag = FALSE)) +
  geom_num(aes(num = r), data = get_data(type = "lower", show.diag = FALSE), size = 3.5) +
  add_diaglab(size = 4.56) + remove_axis()


#想对颜色分组？
ggcor(mtcars, fill.bin = TRUE) + geom_square() # 默认分组

ggcor(mtcars, fill.bin = TRUE, legend.breaks = seq(-1, 1, length.out = 11)) +
  geom_square() #指定分组，0.2为一个区间

col <- col <- ggcor:::.default_colors
col[6] <- "#F2F2F2"
ggcor(mtcars, cluster.type = "all", fill.colours = col, fill.bin = T,
      legend.breaks = c(-1, -0.8, -0.5, 0.5, 0.8, 1)) +
  geom_colour()


#玩点花活
#这部分内容要在线下载表情，很多时候会因为网络问题下载失败。不给图了。
library(ggimage)
emoji <- c("1f004", "1f0cf", "1f170", "1f171", "1f17e",
           "1f17f", "1f18e", "1f191", "1f192", "1f193",
           "1f194", "1f195", "1f196", "1f197")
ggcor(df05) +
  geom_pokemon(aes(image=ifelse(r > 0.5, 'pikachu', 'tauros')),
               data = get_data(type = "lower", show.diag = FALSE)) +
  geom_emoji(aes(image = ifelse(p <= 0.05, '1f600', '1f622')),
             data = get_data(type = "upper", show.diag = FALSE)) +
  geom_emoji(aes(image = emoji), data = get_data(type = "diag"))
ggcor(df05) +
  geom_pokemon(aes(image=ifelse(r > 0.5, 'pikachu', 'tauros')),
               data = get_data(type = "lower", show.diag = FALSE)) +
  geom_colour(data = get_data(type = "upper", show.diag = FALSE)) +
  geom_shade(data = get_data(type = "upper", show.diag = FALSE),
             sign = -1, size = 0.1) +
  geom_emoji(aes(image = emoji), data = get_data(type = "diag"))



###################mantel 检验组合图
#ggcor提供了mantel检验的封装函数fortify_mantel()，
#支持vegan包中的mantel()、mantel.partial()和ade4包中的mantel.randtest()、mantel.rtest()函数，
#差别上说mantel.partial()是偏mantel检验（有控制变量），其它三个是mantel检验，
#当不使用并行计算时，mantel.randtest()速度最快（底层是C语言），mantel.rtest()最慢，
#纯粹R代码实现。

library(vegan) # 使用vegan包所带的数据集
data(varechem)
data(varespec)

mantel <- fortify_mantel(varespec, varechem, spec.select = list(spec01 = 22:25,
                                                                spec02 = 1:4,
                                                                spec03 = 38:43,
                                                                spec04 = 15:20))
df06 <- as_cor_tbl(mantel)
ggcor(df06) + geom_pie2() + geom_cross()

#mantel检验组合图
#mantel组合图是与相关性分析高度整合的，依赖于相关性分析函数，
#换句话说mantel组合图只是在相关性分析图的基础上额外叠加了一个图层。核心函数是add_link()

corr <- fortify_cor(varechem, type = "upper", show.diag = TRUE,
                    cor.test = TRUE, cluster.type = "all")
mantel <- fortify_mantel(varespec, varechem,
                         spec.select = list(spec01 = 22:25,
                                            spec02 = 1:4,
                                            spec03 = 38:43,
                                            spec04 = 15:20),
                         mantel.fun = "mantel.randtest")
ggcor(corr, xlim = c(-5, 14.5)) +
  add_link(mantel, diag.label = TRUE) +
  add_diaglab(angle = 45) +
  geom_square() + remove_axis("y")



corr <- fortify_cor(varechem, type = "upper", show.diag = FALSE,
                    cor.test = TRUE, cluster.type = "all")
ggcor(corr, xlim = c(-5, 14.5)) +
  add_link(mantel, diag.label = TRUE) +
  add_diaglab(angle = 45) +
  geom_pie2() + remove_axis("y")



corr <- fortify_cor(varechem, type = "lower", show.diag = FALSE,
                    cor.test = TRUE, cluster.type = "all")
ggcor(corr, xlim = c(0.5, 20)) +
  add_link(mantel, diag.label = TRUE) +
  add_diaglab(angle = 45) +
  geom_ellipse2() + remove_axis("y")



corr <- fortify_cor(varechem, varechem[ , 1:7], type = "full", show.diag = TRUE,
                    cor.test = TRUE, cluster.type = "all")
mantel <- fortify_mantel(varespec, varechem,
                         spec.select = list(spec01 = 22:25,
                                            spec02 = 1:4,
                                            spec03 = 38:43,
                                            spec04 = 15:20),
                         mantel.fun = "mantel.randtest", nrepet = 2000)
extra.params <- extra_params(group.label = text_params(size = 6),
                             link.params = link_params(group.point.hjust = 2))
ggcor(corr, axis.y.position = "left", legend.position = "left", xlim = c(0.5, 14.5)) +
  add_link(mantel, extra.params = extra.params) +
  geom_circle2()



corr <- fortify_cor(varechem, type = "upper", show.diag = TRUE,
                    cor.test = TRUE, cluster.type = "all")
mantel <- fortify_mantel(varespec, varechem,
                         spec.select = list(spec01 = 22:25,
                                            spec02 = 1:4,
                                            spec03 = 38:43,
                                            spec04 = 15:20),
                         mantel.fun = "mantel.randtest")
mantel <- dplyr::filter(mantel, p <= 0.05)

ggcor(corr, xlim = c(-5, 14.5)) +
  add_link(mantel, diag.label = TRUE, legend.drop = TRUE) +
  add_diaglab(angle = 45) +
  geom_square() + remove_axis("y")


#只要简单的相关性？
corr <- fortify_cor(varechem, type = "upper", show.diag = TRUE,
                    cor.test = TRUE, cluster.type = "all")
corr01 <- fortify_cor(varechem, varespec[ , 38:39], type = "upper", show.diag = TRUE,
                      cor.test = TRUE, cluster.type = "all")
mantel <- fortify_mantel(varespec, varechem,
                         spec.select = list(spec01 = 22:25,
                                            spec02 = 1:4,
                                            spec03 = 38:43,
                                            spec04 = 15:20),
                         mantel.fun = "mantel.randtest")

ggcor(corr, xlim = c(-5, 14.5)) +
  add_link(x = corr01, diag.label = TRUE,
           link.line.colours = c("#E31A1C", "#33A02C")) +
  add_diaglab(angle = 45) +
  geom_square() + remove_axis("y")


#相关性网络图
#这块内容不会整合在ggcor包里面，但是利用ggcor里面的函数很容易导出相关性分析数据供其它函数使用。

df07 <- fortify_cor(varespec, type = "upper", show.diag = FALSE,
                    cor.test = TRUE, keep.name = TRUE)
df07


## make graph
library(ggraph)
library(tidygraph)
graph <- as_tbl_graph(df07)
ggraph(graph, layout = 'linear', circular = TRUE) +
  geom_edge_arc(aes(colour = r, alpha = p)) +
  scale_edge_alpha_continuous(range = c(1, 0.1)) +
  coord_fixed() #颜色是相关性，线条浓淡是统计检验P值