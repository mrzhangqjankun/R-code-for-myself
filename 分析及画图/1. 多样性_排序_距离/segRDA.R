##2019.9.21

##segRDA

install.packages("segRDA")
library(segRDA)
?segRDA

#1. 数据排序
#OrdData，Ordinates both community and explanatory matrices based on the first RDA score.
#OrdData(x, y, axis = 1, method = NA, ...)
#x:环境因子
#y:群落数据
#axis:RDA哪一轴用于排序。默认第一轴
#method：数据标准化方法

data(sim1)
#这种标准化可以减少双零效应
sim1.o<-OrdData(x=sim1$envi, y=sim1$comm, method="hellinger")

#2. SMW，Split moving window analysis
#SMW(yo, ws, dist = "bray", rand = c("shift", "plot"), n.rand = 99)
#yo:排序过的群落数据
#ws:窗口大小
#dist:不相似性矩阵的方法，默认bray
#rand：随机化类型。shift:限制性的随机化。同一物种的数据随机移动；plot:非限制性的随机化。每个样本随机定位
#n.rand：随机化次数

#单一窗口
ws20<-SMW(yo=sim1.o$yo,ws=20)
plot(ws20)
#多个窗口pool
pool<-SMW(yo=sim1.o$yo,ws=c(20,30,40))
plot(pool,w.effect=TRUE)


#3. Piecewise redundancy analysis (pwRDA)
#pwRDA(x.ord, y.ord, BPs, n.rand = 99)
#x.ord:排序过的环境因子
#y.ord:排序过的群落
#BPs:上步得到的断点
sim1.pw<-pwRDA(sim1.o$xo,sim1.o$yo, BPs=bp(extract(ws20)))
