##转自赖江山的博客

rm(list=ls(all=TRUE))

##1.将大样地划分为任意矩形的小样地的函数 
##http://blog.sciencenet.cn/home.php?mod=space&uid=267448&do=blog&id=1047660

##2.从大样地里面随机抽取若干个小样方的程序 
##http://blog.sciencenet.cn/blog-267448-1029248.html

##3.计算大样地数据内不同尺度下样方-物种多度矩阵 
##http://blog.sciencenet.cn/blog-267448-857959.html

##4.将样方多度数据转为“样方-物种”矩阵的函数 
##http://blog.sciencenet.cn/blog-267448-1027387.html

##5.约束排序（RDA，CCA）两种筛选环境变量的方法 
##http://blog.sciencenet.cn/blog-267448-1023388.html
library(vegan)
data(dune)
data(dune.env)
## step基于AIC值
step(cca(dune ~  1, dune.env), reformulate(names(dune.env)), test="perm")
##ordistep基于P值
ordistep(cca(dune ~  1, dune.env), reformulate(names(dune.env)), perm.max=200)

##6.在非约束排序图中被动加入环境因子 
##http://blog.sciencenet.cn/blog-267448-1025741.html
##当环境变量解释量非常小的时候，这个时候用这种模式是不错的选择。
library(vegan)
data(varechem)
data(varespec)
vare.mds<- metaMDS(varespec)
##envfit线性回归函数，就是将非约束排序后样方再跟前几轴的坐标跟环境因子做回归，
#这个时候环境因子是作为响应变量（y)，而样方坐标是作为解释变量进行线性回归。
ef <- envfit(vare.mds, varechem, permu = 999)
ef

plot(vare.mds, display = "sites")
plot(ef, p.max = 0.1)

ef2 <- envfit(vare.mds ~ Al + Ca, varechem)
plot(vare.mds, display = "sites")
plot(ef2)

#ordisurf函数进行环境因子与排序轴的非线性回归（实际是做趋势面）。
tmp <- with(varechem, ordisurf(vare.mds, Al, add = TRUE))
with(varechem, ordisurf(vare.mds, Ca, add = TRUE, col = "green4"))

##7.已知经纬度直接算地理距离的函数 
##http://blog.sciencenet.cn/blog-267448-1048066.html
install.packages("geosphere")
library(geosphere)
xy <- rbind(c(0,0),c(90,90),c(10,10),c(-120,-45))
distm(xy)
xy2 <- rbind(c(0,0),c(10,-10))
distm(xy, xy2)

##8.只知道相关系数，如何算显著性p值
##http://blog.sciencenet.cn/blog-267448-1044024.html
#从别的地方的相关系数，如果知道样本量，在R里面输入下面的代码可以获得显著性，r是相关系数，n是样本量
#如果 r是负值，
pt(r*sqrt((n-2)/(1-r^2)),n-2)*2
#如果 r是正值
(1-pt(r*sqrt((n-2)/(1-r^2)),n-2))*2

##9.如何选择好的距离指数？ 
##http://blog.sciencenet.cn/blog-267448-1025196.html
##vegan包里的rankindex函数，这个函数的计算原则，在不同的指数条件下，去计算通过群落数据算出来的距离矩阵，
#跟通过环境数据算出了的距离矩阵进行spearman秩相关分析，看那个相关系数高，就选哪个指数。
#也就是说，群落数据算出距离，如果与环境数据算出的距离相关性越高，说明该指数越好！
library(vegan)
data(varespec)
data(varechem)
## The variables are automatically scaled
rankindex(varechem, varespec) ##先环境因子，后OTU
#结果
#euc    man      gow     bra      kul 

#0.2396330 0.2735087 0.2288358 0.2837910 0.2839834 

#结果显示kul最优，但是其实bra指数也挺好。

?rankindex









