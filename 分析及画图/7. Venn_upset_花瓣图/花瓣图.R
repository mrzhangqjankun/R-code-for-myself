##2020.9.23

##花瓣图

##1. https://www.cnblogs.com/xudongliang/p/7884667.html
##  http://www.maimengkong.com/kyjc/585.html

setwd("E:/桌面/陈鹏数据")
##首先读入花瓣图作图数据，并计算得到各样本所包含OTU种类总数以及所有样本共有OTU的数量等。
#读入做图文件，预处理。自己造轮子。第二个链接算错了，他算的是每个样本的OTU，而不是每个样本独有的OTU。
flower_dat <- read.delim('16S结果/UPARSE_otu_table.txt', header = T, sep = '\t',row.names = 1, stringsAsFactors = F, check.names = F)
flower_dat <- read.delim('ITS结果/UPARSE_otu_table.txt', header = T, sep = '\t',row.names = 1, stringsAsFactors = F, check.names = F)
sample_id <- colnames(flower_dat)

dat = flower_dat
#core_num为所有样本的共有OTU种类数。
library(vegan);?decostand
dat2 = decostand(dat,"pa")
z = apply(dat2,1,sum)==ncol(dat)
core_num = length(z[z==TRUE])    #sum(z, na.rm = T)

#otu_num为各样本中所含特有的OTU种类总数
otu_num = c()
for (j in 1:ncol(dat2)){  #  j=5
  count = 0
  for (i in 1:nrow(dat2)){  #  i=100
    if ( (dat2[i,j]==1 & (apply(dat2[i,-j],1,sum)==0)) == TRUE ){
           count = count + 1}
  }
    otu_num = c(otu_num,count)
    print (j)
}

otu_num
sum(otu_num)
#sum(apply(dat2,1,sum)==1)  #一共18354个独特的OTU   5892


#draw.ellipse
library(plotrix)
##第一个参数为样本名字构成的向量，
##第二个参数为每个样本独有的数目，
##第三个参数为起始椭圆的角度，
##第四个参数为椭圆的短轴的长度，
##第五个参数为椭圆的长轴的长度
# flower_plot <- function(sample, value, start, a, b, 
#                         ellipse_col = rgb(135, 206, 235, 150, max = 255), 
#                         circle_col = rgb(0, 162, 214, max = 255),
#                         circle_text_cex = 1.5
# ) {
#   par( bty = "n", ann = F, xaxt = "n", yaxt = "n", mar = c(1,1,1,1))
#   plot(c(0,10),c(0,10),type="n")
#   n   <- length(sample)
#   deg <- 360 / n
#   res <- lapply(1:n, function(t){
#     draw.ellipse(x = 5 + cos((start + deg * (t - 1)) * pi / 180), 
#                  y = 5 + sin((start + deg * (t - 1)) * pi / 180), 
#                  col = ellipse_col,
#                  border = ellipse_col,
#                  a = a, b = b, angle = deg * (t - 1))
#     text(x = 5 + 2.5 * cos((start + deg * (t - 1)) * pi / 180),
#          y = 5 + 2.5 * sin((start + deg * (t - 1)) * pi / 180),
#          value[t]
#     )
#     
#     if (deg * (t - 1) < 180 && deg * (t - 1) > 0 ) {
#       text(x = 5 + 3.3 * cos((start + deg * (t - 1)) * pi / 180),
#            y = 5 + 3.3 * sin((start + deg * (t - 1)) * pi / 180),
#            sample[t],
#            srt = deg * (t - 1) - start,
#            adj = 1,
#            cex = circle_text_cex
#       )
#       
#     } else {
#       text(x = 5 + 3.3 * cos((start + deg * (t - 1)) * pi / 180),
#            y = 5 + 3.3 * sin((start + deg * (t - 1)) * pi / 180),
#            sample[t],
#            srt = deg * (t - 1) + start,
#            adj = 0,
#            cex = circle_text_cex
#       )
#     }			
#   })
#   draw.circle(x = 5, y = 5, r = 1.3, col = circle_col, border = circle_col)
# }
# 
# #写成了1个函数，函数的调用方式如下;
# flower_plot(c("WSM419", "A321", "M1", "M2", "M22", "M58", 
#               "M102", "M161", "KH36b", "KH36c", "KH36d", "KH53a", "KH53b"),
#             c(519, 556, 83, 62, 415, 425, 357, 441, 22, 41, 33, 44, 43), 90, 0.5, 2)

#第二个网址改进版
#定义备选颜色.原来只有第一行20个，现在是40个样本，double

ellipse_col <- c('#6181BD4E','#F348004E','#64A10E4E','#9300264E','#464E044E','#049a0b4E','#4E0C664E','#D000004E','#FF6C004E','#FF00FF4E','#c7475b4E','#00F5FF4E','#BDA5004E','#A5CFED4E','#f0301c4E','#2B8BC34E','#FDA1004E','#54adf54E','#CDD7E24E','#9295C14E',
                 '#6181BD4E','#F348004E','#64A10E4E','#9300264E','#464E044E','#049a0b4E','#4E0C664E','#D000004E','#FF6C004E','#FF00FF4E','#c7475b4E','#00F5FF4E','#BDA5004E','#A5CFED4E','#f0301c4E','#2B8BC34E','#FDA1004E','#54adf54E','#CDD7E24E','#9295C14E')
#ellipse_col <- rep('#6181BD4E',40)
#构建作图函数（参考自 https://www.cnblogs.com/xudongliang/p/7884667.html）
##注：参数a和b用于设置花瓣椭圆的尺寸，ellipse_col用于设置花瓣椭圆的颜色；参数r用于设置中心圆圈尺寸，circle_col用于设置中心圆圈的颜色
flower_plot <- function(sample, otu_num, core_otu, start, a, b, r, ellipse_col, circle_col) {
  par( bty = 'n', ann = F, xaxt = 'n', yaxt = 'n', mar = c(1,1,1,1))
  plot(c(0,10),c(0,10),type='n')
  n   <- length(sample)
  deg <- 360 / n
  res <- lapply(1:n, function(t){
    draw.ellipse(x = 5 + cos((start + deg * (t - 1)) * pi / 180), 
                 y = 5 + sin((start + deg * (t - 1)) * pi / 180), 
                 col = ellipse_col[t],
                 border = ellipse_col[t],
                 a = a, b = b, angle = deg * (t - 1))
    text(x = 5 + 2.5 * cos((start + deg * (t - 1)) * pi / 180),
         y = 5 + 2.5 * sin((start + deg * (t - 1)) * pi / 180),
         otu_num[t])
    
    if (deg * (t - 1) < 180 && deg * (t - 1) > 0 ) {
      text(x = 5 + 3.3 * cos((start + deg * (t - 1)) * pi / 180),
           y = 5 + 3.3 * sin((start + deg * (t - 1)) * pi / 180),
           sample[t],
           srt = deg * (t - 1) - start,
           adj = 1,
           cex = 1
      )
    } else {
      text(x = 5 + 3.3 * cos((start + deg * (t - 1)) * pi / 180),
           y = 5 + 3.3 * sin((start + deg * (t - 1)) * pi / 180),
           sample[t],
           srt = deg * (t - 1) + start,
           adj = 0,
           cex = 1
      )
    }            
  })
  draw.circle(x = 5, y = 5, r = r, col = circle_col, border = NA)
  text(x = 5, y = 5, paste('Core:', core_otu))
}
png('ITS结果/共有及特有OTU.png', width = 1500, height = 1500, res = 200, units = 'px')
flower_plot(sample = sample_id, otu_num = otu_num, core_otu = core_num, 
            start = 90, a = 0.5, b = 2, r = 1, ellipse_col = ellipse_col, circle_col = 'white')
dev.off()




##2. flowers包
#install.packages("flowers")
#devtools::install_github("mbjones/flowers")
library(flowers)
data(ohi)
?plot_flower
plot_flower(ohi, "OHI Example Flower")

library(dplyr)
df <- data.frame(order = c(1, 4, 3, 2),
                 score = c(90, 80, 70, 60),
                 weight = c(1, 1, 1, 1),
                 goal = c("F", "A", "I", "R"),
                 label = c("Findable", "Accessible", "Interoperable", "Reusable"),
                 category = c(NA, NA, NA, NA),
                 stringsAsFactors = FALSE) %>% arrange(order)
d1_colors <- c( "#c70a61", "#ff582d", "#1a6379", "#60c5e4")
plot_flower(df, title = "FAIR Metrics", fixed_colors=TRUE, colors = d1_colors)


##3.在线，不好用