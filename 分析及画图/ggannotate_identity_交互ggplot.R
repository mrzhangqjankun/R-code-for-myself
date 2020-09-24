##2020.4.17

##https://mp.weixin.qq.com/s/lVTvj1MFsOlVdFOYUZYdSw

#《identify: 交互式操作进化树》

#可交互注释你的ggplot图ggannotate

remotes::install_github("mattcowgill/ggannotate")

#一种方法是选中你的代码，然后去点Rstudio插件。

##另一种办法是直接调用包，比如：

library(ggplot2)
p <- ggplot(mtcars,
            aes(x = wt, y = mpg)) +
  geom_point()

ggannotate::ggannotate(p)