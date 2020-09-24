###2018.9.22
##创建属于自己的调色板 

##https://mp.weixin.qq.com/s?__biz=MzUzMjA4Njc1MA==&mid=2247486641&idx=2&sn=d8b2e30bacbb6445312091afd15ab379&chksm=fab9e600cdce6f16794e75c6aaa6bb14526bb67a95da38f41f287ab5c5bca232f90fba375f99&scene=0#rd

# 将颜色与其对应的十六进制矢量联系起来
# 创建访问十六进制矢量的函数
# 调色板命名
# 访问调用调色板
# 创建兼容ggplot2的尺度函数

rm(list=ls())

#首先我们创建一个颜色变量
library(ggplot2)
theme_set(theme_minimal())
my_colors <- c(
  `purple` = "#7b0099",
  `yellow` = "#ff9900",
  `black`  = "#221f1f",
  `white`  = "#f5f5f1",
  `red`    = "#e50914"
)
# 后续如果需要对颜色进行修改或添加都可以在这个颜色变量中进行。
# 下面创建一个可以从该变量中提取颜色对应的十六进制的函数
#' Function to extract my_colors as hex codes
#'
#' @param ... Character names of my_colors
#'
my_cols <- function(...){
  cols <- c(...)
  if (is.null(cols))
    return(my_colors)
  my_colors[cols]
}    

#此时我们可以十分方便获取颜色的十六进制，下面演示一下：  
my_cols()

##    purple    yellow     black     white       red
## "#7b0099" "#ff9900" "#221f1f" "#f5f5f1" "#e50914"

my_cols("purple")

##    purple
## "#7b0099"

my_cols("purple","yellow")

##    purple    yellow
## "#7b0099" "#ff9900"

my_cols("yellow","purple")

##    yellow    purple
## "#ff9900" "#7b0099" 

#此时我们可以在ggplot2中调用这些颜色
#使用gapminder包中的数据集gapminder来演示
library(gapminder)
head(gapminder)
ggplot(gapminder, aes(gdpPercap, lifeExp))+
  geom_point(color=my_cols("purple"), size=2, alpha=0.8)+
  scale_x_log10()

########创建调色板
#调色板就是一个颜色列表方便我们后面使用，将各种颜色组合搭配
my_palettes <- list(
  `main`  = my_cols("purple","yellow","red"),
  `cool`  = my_cols("purple","yellow"),
  `hot`   = my_cols("yellow","black","red"),
  `mixed` = my_cols("purple","yellow","white","red"),
  `bw`    = my_cols("black","white")
)
#在这个颜色列表中实际上我们创建了好几种颜色组合，下面创建一个函数来访问并调用它们
#' Return function to interpolate a my_palettes
#'
#' @param palette Character name of palette in my_palettes
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments to pass to colorRampPalette()
#'
my_pal <- function(palette="main", reverse=FALSE, ...){
  pal <- my_palettes[[palette]]
  if (reverse) pal <- rev(pal)
  colorRampPalette(pal, ...)
}
#该函数通过不同颜色组合名称从列表中获取一个颜色组合（默认条件下是main），同时可以设置是否颠倒颜色顺序，之后传递给函数colorRampPaette()创建调色板。因此其返回的是一个函数。   
my_pal("cool")


#通过这个函数可以基于调色板返回不同数目的颜色，进而可以在原始颜色之间创建多级颜色梯度   
my_pal("cool")(10)


pie(rep(1,10), col = my_pal("cool")(10))

#################Scales for ggplot2
#ggplot2中颜色映射有color和fill即颜色和填充。

#' Color scale constructor for my_colors
#'
#' @param palette Character name of palette in my_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or scale_color_gradientn(), used #' #' respectively when discrete is TRUE or FALSE
#'
scale_color_my <- function(palette="main", discrete=TRUE, reverse=FALSE, ...){
  pal <- my_pal(palette = palette, reverse = reverse)
  if (discrete){
    discrete_scale("colour", paste0("my_", palette), palette = pal, ...)
  }else{
    scale_color_gradientn(colours = pal(256), ...)
  }
}
#' Fill scale constructor for my_colors
#'
#' @param palette Character name of palette in my_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or scale_color_gradientn(), used #' #' respectively when discrete is TRUE or FALSE
#'
scale_fill_my <- function(palette="main", discrete=TRUE, reverse=FALSE, ...){
  pal <- my_pal(palette = palette, reverse = reverse)
  if (discrete){
    discrete_scale("fill",paste0("my_", palette), palette = pal, ...)
  }else{
    scale_fill_gradientn(colours = pal(256), ...)
  }
}
#上面每个函数都制定一个调色板（默认是main），调色板是基于离散变量或连续变量，以及是否颠倒颜色顺序，同时还传递给了相关的ggplot2函数。下面可以试试它们在绘图过程中的表现
ggplot(gapminder,aes(gdpPercap, lifeExp, color=continent))+
  geom_point(size=2, alpha=0.8)+
  scale_x_log10()+
  scale_color_my()

#我们可以更换调色板试试   
ggplot(gapminder,aes(gdpPercap, lifeExp, color=continent))+
  geom_point(size=2, alpha=0.8)+
  scale_x_log10()+
  scale_color_my(palette = "hot")

#我们可以更换调色板试试   
ggplot(gapminder,aes(gdpPercap, lifeExp, color=continent))+
  geom_point(size=2, alpha=0.8)+
  scale_x_log10()+
  scale_color_my(palette = "hot")

#上面是颜色，下面试试填充
ggplot(mpg, aes(manufacturer, fill = manufacturer)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_my(palette = "mixed", guide = "none")