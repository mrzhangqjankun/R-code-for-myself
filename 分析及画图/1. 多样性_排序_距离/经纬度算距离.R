##2019.9.25

##已知经纬度直接算地理距离的函数 

##http://wap.sciencenet.cn/blog-267448-1048066.html

install.packages("geosphere")
library(geosphere)
xy <- rbind(c(0,0),c(90,90),c(10,10),c(-120,-45))
distm(xy)
xy2 <- rbind(c(0,0),c(10,-10))
distm(xy, xy2)