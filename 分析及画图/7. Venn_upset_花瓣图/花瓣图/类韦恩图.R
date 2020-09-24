##2019.11.22

##https://mp.weixin.qq.com/s/ukixIvnJzAqsMAtfBMC-Gw

##类韦恩图(花型)

# flower_plot函数中几个有意义的参数设置:
# sample：设置特有环的标签；
# value：设置特有环的数量信息；
# start：设置旋转角度，90即可；
# a：设置椭圆宽度；
# b：设置标签外围大小；
# cer_label：设置中心圆标签；
# col_c：设置中心园标签颜色；
# ellipse_col：设置椭圆颜色；
# circle_col：设置中心圆颜色；
# circle_text_cex：设置中心圆标签字体大小。

library("plotrix");?plotrix #专业的绘图和绘图配件

flower_plot <- function(sample, value, start, a, b, cer_label,col_c,
                        ellipse_col = rgb(135, 206, 235, 150, max = 255), 
                        circle_col = rgb(0, 162, 214, max = 255),
                        circle_text_cex = 1.5
) {
  par( bty = "n", ann = F, xaxt = "n", yaxt = "n", mar = c(1,1,1,1))
  plot(c(0,10),c(0,10),type="n")
  n   <- length(sample)
  deg <- 360 / n
  res <- lapply(1:n, function(t){
    draw.ellipse(x = 5 + cos((start + deg * (t - 1)) * pi / 180), 
                 y = 5 + sin((start + deg * (t - 1)) * pi / 180), 
                 col = ellipse_col,
                 border = ellipse_col,
                 a = a, b = b, angle = deg * (t - 1))
    text(x = 5 + 2.5 * cos((start + deg * (t - 1)) * pi / 180),
         y = 5 + 2.5 * sin((start + deg * (t - 1)) * pi / 180),
         value[t]
    )
    
    if (deg * (t - 1) < 180 && deg * (t - 1) > 0 ) {
      text(x = 5 + 3.3 * cos((start + deg * (t - 1)) * pi / 180),
           y = 5 + 3.3 * sin((start + deg * (t - 1)) * pi / 180),
           sample[t],
           srt = deg * (t - 1) - start,
           adj = 1,
           cex = circle_text_cex
      )
      
    } else {
      text(x = 5 + 3.3 * cos((start + deg * (t - 1)) * pi / 180),
           y = 5 + 3.3 * sin((start + deg * (t - 1)) * pi / 180),
           sample[t],
           srt = deg * (t - 1) + start,
           adj = 0,
           cex = circle_text_cex
      )
    }			
  })
  draw.circle(x = 5, y = 5, r = 1.3, col = circle_col, border = circle_col)
  
  text(x = 5, y = 5,cer_label,cex = 2,col = col_c)
}


library(RColorBrewer)#调色板调用包

# #调用所有这个包中的调色板
# display.brewer.all()
# #提取特定个数的调色板颜色，会出图显示
# display.brewer.pal(9,"Oranges")
# display.brewer.pal(8,"Set1")
# ##仅仅只显示色号,我们要在颜色上并显示色号才好
mi = brewer.pal(9,"Blues")
mi = brewer.pal(12,"Set1")

library("scales")
show_col(mi)




flower_plot(sinID,
            sin_num,
            ellipse_col =  mi[4],
            circle_col = mi[8],
            90, 0.4, 2,
            cer_label = paste("all",":",all_num,sep = ""), col_c = mi[1]
            )





# sample = c("WSM419", "A321", "M1", "M2", "M22", "M58", "M102", "M161", "KH36b", "KH36c", "KH36d", "KH53a", "KH53b")
# value = c(519, 556, 83, 62, 415, 425, 357, 441, 22, 41, 33, 44, 43)
# 
# start = 90
# a = 0.5
# b =  2
# ellipse_col = rgb(135, 206, 235, 150, max = 255)
# circle_col = rgb(0, 162, 214, max = 255)
# circle_text_cex = 1.5
# 
# par( bty = "n", ann = F, xaxt = "n", yaxt = "n", mar = c(1,1,1,1))
# plot(c(0,10),c(0,10),type="n")
# n   <- length(sample)
# deg <- 360 / n
# 
# t =1
# res <- lapply(1:n, function(t){
#   draw.ellipse(x = 5 + cos((start + deg * (t - 1)) * pi / 180), 
#                y = 5 + sin((start + deg * (t - 1)) * pi / 180), 
#                col = ellipse_col,
#                border = ellipse_col,
#                a = a, b = b, angle = deg * (t - 1))
#   text(x = 5 + 2.5 * cos((start + deg * (t - 1)) * pi / 180),
#        y = 5 + 2.5 * sin((start + deg * (t - 1)) * pi / 180),
#        value[t]
#   )
#   
#   if (deg * (t - 1) < 180 && deg * (t - 1) > 0 ) {
#     text(x = 5 + 3.3 * cos((start + deg * (t - 1)) * pi / 180),
#          y = 5 + 3.3 * sin((start + deg * (t - 1)) * pi / 180),
#          sample[t],
#          srt = deg * (t - 1) - start,
#          adj = 1,
#          cex = circle_text_cex
#     )
#     
#   } else {
#     text(x = 5 + 3.3 * cos((start + deg * (t - 1)) * pi / 180),
#          y = 5 + 3.3 * sin((start + deg * (t - 1)) * pi / 180),
#          sample[t],
#          srt = deg * (t - 1) + start,
#          adj = 0,
#          cex = circle_text_cex
#     )
#   }			
# }
# 
# 
# )
# 
# 
# 
# draw.circle(x = 5, y = 5, r = 1.3, col = circle_col, border = circle_col)





library(ggplot2)
library(ggforce)


ggplot() +
  geom_ellipse(aes(x0 = 0, y0 = 0, a = 10, b = 3, angle = 0)) +
  coord_fixed()

# Rotation
# Note that it expects radians and rotates the ellipse counter-clockwise
ggplot() +
  geom_ellipse(aes(x0 = 0, y0 = 0, a = 10, b = 3, angle = pi / 4)) +
  coord_fixed()

# Draw a super ellipse
ggplot() +
  geom_ellipse(aes(x0 = 0, y0 = 0, a = 6, b = 3, angle = -pi / 3, m1 = 3)) +
  coord_fixed()














