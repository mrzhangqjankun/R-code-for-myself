##2019.1.3

##看Y叔的R包，写自己的图层。 对角线热图

##https://mp.weixin.qq.com/s/oayD6rlM8ISaocIogSvjKA


library(grid)
triangleGrob <- function(fill="red",col=NULL,vp=NULL, name=NULL,...) {
  x = c(0,0,1)
  y = c(0,1,1)
  polygonGrob(x,y, name=name, vp=vp,
              gp =gpar(fill=fill,col=col))
}

grid.newpage()
grid.draw(triangleGrob())

#因为grid中的视图窗口viewport可以旋转，所以把这个三角形旋转180度得到下三角形,(绿色)
vp1 <- viewport(angle = 180)
grid.draw(triangleGrob(fill = 'green',vp=vp1))

#而Y叔说过，图上无论是三角形，四边形，圆形，五角星，蛋糕，他们本质上都是一个个中空的点。
#写图层的过程就是，在对应的点的位置画上一个个图形，点的位置的改变就靠的是每次建立不同的viewport
#接下来新建一个viewport并展示
grid.newpage()
vp2 <- viewport(0.7,0.8,0.2,0.2)
grid.show.viewport(vp2,vp.ex = 1)

#此时如果我来画图就会被画到蓝色小框的区域
grid.draw(triangleGrob(fill = 'red',vp=vp2))



#好了准备工作做完了，就仿造geom_cake写了一个图层 geom_triangle
#看geom_cake的源代码仿写新的图层
library(ggplot2)
geom_triangle <- function(mapping = NULL, data = NULL,
                          stat = "identity", position = "identity",
                          ...,
                          #linejoin = "mitre",
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomTriangle,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      #linejoin = linejoin,
      na.rm = na.rm,
      ...
    )
  )
}

GeomTriangle <- ggproto("GeomHeart", Geom,
                        draw_panel = function(data, panel_params, coord) {
                          data <- coord$transform(data, panel_params)
                          grobs <- lapply(1:nrow(data), function(i) {
                            vp <- viewport(x=data$x[i], y=data$y[i],
                                           width=data$size[i], height=data$size[i],
                                           angle = data$angle[i],
                                           just = c("center", "center"),
                                           default.units = "native")
                            triangleGrob(vp=vp, name=i,fill = data$fill[i])
                          })
                          class(grobs) <- "gList"
                          ggplot2:::ggname("geom_triangle",gTree(children = grobs))
                        },
                        default_aes = aes(colour = NA,fill="red", size = .09, linetype = 1,
                                          alpha = NA,angle=0),
                        required_aes = c("x", "y"),
                        draw_key = draw_key_polygon
)


#这个图层可以控制颜色，控制三角形的方向
library(ggplot2)
p <- ggplot(mtcars, aes(wt, mpg))
p1 <- p + geom_triangle()
p2 <- p + geom_triangle(aes(fill=gear),angle=45)
p3 <- p + geom_triangle(aes(fill=gear),angle=45)+
  scale_fill_viridis_c()
library(patchwork)
p1 + p2 + p3


#现在用geom_triangle来画热图
#准备一个测试数据
library(dplyr)
data <- cor(mtcars) %>%
  as.data.frame() %>% 
  tibble::rownames_to_column("sample1") %>% 
  tidyr::pivot_longer(cols = -1,names_to = "sample2",values_to = "expre1") %>% 
  mutate(expre2 = expre1+sample(seq(1000,2000)/2000,nrow(.))) 
data


library(ggplot2)
ggplot(data, aes(sample1, sample2))+
  scale_fill_viridis_c()+
  geom_triangle(aes(fill = expre1))+
  ggnewscale::new_scale_fill()+
  geom_triangle(aes(fill = expre2),angle=180)
#
ggplot(data, aes(sample1, sample2))+
  scale_fill_viridis_c()+
  geom_triangle(aes(fill = expre1,angle=sample(seq(0,360),121)))

#如果一不小心把三角形设置大了一点，还要意外效果
ggplot(data, aes(sample1, sample2))+
  scale_fill_viridis_c()+
  geom_triangle(aes(fill = expre1),size=0.5)
