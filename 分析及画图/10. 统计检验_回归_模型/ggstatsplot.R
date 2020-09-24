##2019.5.14
#https://mp.weixin.qq.com/s/84b5I0Qeo0hqkhogvtKhiw
#一条指令把统计+画图都给做了？似乎太美好！
#ggstatsplot: 对ggplot2进行扩展，生成带有统计试验结果的可视化图形.R (≥ 3.5.0)
#https://github.com/IndrajeetPatil/ggstatsplot

#更新R
#https://www.jianshu.com/p/2483070db99b
library(installr)
updateR()
#更新所有包
rvcheck::update_all()


#devtools::install_github("IndrajeetPatil/ggstatsplot")
#utils::install.packages(pkgs = "ggstatsplot")
library("ggstatsplot")
library(ggplot2)
head(mtcars)
#轮子数目和自动波/手波是否有关联
ggpiestats(data = mtcars,
           main = am,                
           condition = cyl) +
  scale_fill_brewer(palette = "Dark2");?ggpiestats #pie chart with p

#组间比较
ggbetweenstats(data = iris, 
               x = Species, 
               y = Sepal.Length);?ggbetweenstats #A combination of box and violin plots along with jittered data points with p

#相关性
ggscatterstats(data = iris, 
               x = Sepal.Length, 
               y = Petal.Length,
               title = "Dataset: Iris flower data set");?ggscatterstats # x,y柱形图及相关性散点及线性拟合

ggscatterstats(data = iris, 
               x = Sepal.Length, 
               y = Petal.Length,
               title = "Dataset: Iris flower data set",marginal.type = "density")

ggscatterstats(data = iris, 
               x = Sepal.Length, 
               y = Petal.Length,
               title = "Dataset: Iris flower data set",marginal.type = "boxplot")

#相关性矩阵
ggcorrmat(
  data = subset(iris, Species == "versicolor"),
  cor.vars = c(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width));?ggcorrmat #Visualization of a correlalogram

#分组做图并拼图

#像上面的相关性矩阵，我只是切了versicolor一个物种来画，iris数据我们知道有三个物种，我们可以一个一个做，然后用cowplot来拼图。
#下面我先定义一个函数来画图：

plot_fun = function(data) {
  ggscatterstats(
    data = data,
    x = Sepal.Length,
    y = Sepal.Width,
    marginal.type = "boxplot",
    title =
      glue::glue("Species: {(data$Species)} (n = {length(data$Sepal.Length)})")
  )
}
#然后分组做图：

library(dplyr)
library(tidyr)
library(purrr)
nested_df <- iris %>% 
  group_by(Species) %>% 
  nest() %>% 
  mutate(p = map(data, plot_fun)) ##plot_fun调用
#这样就有三张图，用cowplot当然很容易拼，但我们经常需要拼完再加个title, 再底部加个注释什么的。你当然可以自己再画个tible什么的，然后再去拼。而ggstatsplot直接封装了cowplot，并提供了一些参数，让你加标题和注释什么的。比如上面画的三张图，我用combine_plots拼出来：

combine_plots(
  plotlist = nested_df$p,
  labels = c("(a)", "(b)", "(c)"),
  nrow = 3,
  ncol = 1,
  title.text = "Relationship between sepal length and width for all Iris species",
  title.size = 14,
  title.colour = "blue",
  caption.text = expression(
    paste(
      italic("Note"),
      ": Iris flower dataset was collected by Edgar Anderson."
    ),
    caption.size = 10
  )
)
