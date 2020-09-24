##2019.8.12

##https://mp.weixin.qq.com/s/EmGOfytnc4051UgeVgi-pQ

##cowplot作者的新包，ggtext，让你的ggplot2支持用markdown/html语法去画文本，附送图片支持。

##https://github.com/clauswilke/ggtext

##github安装总报错，有人建议加上这一句，但是还是不行 #Could not find tools necessary to compile a package
options(buildtools.check = function(action) TRUE )

devtools::install_github("clauswilke/gridtext")
devtools::install_github("clauswilke/ggtext")


library(ggplot2)
library(ggtext)

ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
  geom_point(size = 3) +
  scale_color_manual(
    name = NULL,
    values = c(setosa = "#0072B2", virginica = "#009E73", versicolor = "#D55E00"),
    labels = c(
      setosa = "<i style='color:#0072B2'>I. setosa</i>",
      virginica = "<i style='color:#009E73'>I. virginica</i>",
      versicolor = "<i style='color:#D55E00'>I. versicolor</i>")
  ) +
  labs(
    title = "**Fisher's *Iris* dataset**  
    <span style='font-size:11'>Sepal width vs. sepal length for three *Iris*
    species</span>",
    x = "Sepal length (cm)", y = "Sepal width (cm)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_markdown(lineheight = 1.1),
    legend.text = element_markdown(size = 11)
  )