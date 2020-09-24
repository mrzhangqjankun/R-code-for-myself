##2019.6.26
##图片右边和上边的边框

##panel.background 这个参数控制
p+theme(panel.background=element_rect(fill="white",color="red"),
        #panel.grid=element_line(color="grey50",size=2),
        #panel.grid.major=element_line(size=1,linetype =3,color="grey70"),
        #panel.grid.minor=element_line(size=1,linetype =3,color="grey70")
)

#https://www.cnblogs.com/xudongliang/p/6927573.html
par(omi = c(1, 1, 1, 1), mfrow = c(1, 2))
plot(1:5)
box(which = "plot",   col = "red", lwd = 2)  ##这个也是控制坐标轴边框
box(which = "figure", col = "red", lwd = 2)
plot(1:5)
box(which = "plot",   col = "blue", lwd = 2)
box(which = "figure", col = "blue", lwd = 2)
