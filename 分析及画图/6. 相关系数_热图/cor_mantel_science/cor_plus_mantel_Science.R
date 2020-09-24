##2019.4.24
##https://mp.weixin.qq.com/s/yVKqirVD5sX8j5UzGpouow
#Science文章
#https://science.sciencemag.org/content/348/6237/1261359/tab-figures-data
#R语言之照猫画虎1
#Spearman’s correlation 和 Mantel tests的结果做了叠加

library(ggplot2)

#构建数据框1，以绘制曲线终点，z 和 group 方便展示，没有实际意义
df1 <- data.frame(
  x=8:(-1),y =10:1,z=c(0.01,0.05),group=c("p<0.0001","0.0001<p<0.001","0.001<p<0.01","0.01<p<0.05","p>0.05"))
df1

#构建数据框2，以绘制散点图，mantelR 和 group 方便展示，没有实际意义
a<- data.frame(x=9,y=(1:9))
b<- data.frame(x=8,y=(1:8))
c<- data.frame(x=7,y=(1:7))
d<- data.frame(x=6,y=(1:6))
e<- data.frame(x=5,y=(1:5))
f<- data.frame(x=4,y=(1:4))
g<- data.frame(x=3,y=(1:3))
h<- data.frame(x=2,y=(1:2))
i<- data.frame(x=1,y=1)
h<- rbind(a,b,c,d,e,f,g,h,i)
df2<-data.frame(h,mantelR=c(0.5,0.25,0.1),group=c("p<0.0001","0.0001<p<0.001","0.001<p<0.01","0.01<p<0.05","p>0.05"))
df2

#构建数据框3和4，以绘制变量标签，A-J没有实际意义
df3 <- data.frame(
  x = 10,
  y = (9:1),
  text = c("B", "C", "D", "E","F","G","H","I","J")
)
df3

df4 <- data.frame(
  x = (9:0),
  y = (10:1),
  text = c("A", "B", "C", "D", "E","F","G","H","I","J")
)
df4

?geom_tile #画矩形
#绘图，大家可以根据自己喜好随意操作，这里去除了背景、边框、坐标轴等
###
p = ggplot() + 
  geom_tile(aes(x = x, y = y),df2,fill=NA,color='gray',size=1) ;p
p = p + geom_point(aes(x = x, y = y,size=mantelR,fill=mantelR),df2,
             shape=22,color='white') + 
  scale_size(range = c(1, 8));p
p = p + scale_fill_distiller(palette="RdYlBu");p
###  到这都是画环境因子的相关系数heatmap。

#散点图表示correlation，再用曲线表示Mantel。
#几个geom_curve就是几个OTU表。前面的x和y要自己设，为起点。
#geom_text是在heatmap上加环境因子的标签，还是属于之前的部分。
p = p + geom_curve(aes(x = -3, y = 5, xend = x, yend = y,size=z,colour=group),curvature = 0.2,df1);p  
p = p + geom_curve(aes(x = 0, y = 9, xend = x, yend = y,size=z,colour=group),curvature = 0.2,df1);p 
p = p + geom_text(aes(y,0,label = text),df3,angle=90);p 
p = p + geom_text(aes(x,y,label = text),df4);p 
#起点处加小方块和名字
p = p + geom_point(aes(x=-3,y=5),size=5,fill="grey",shape=23);p 
p = p + geom_text(aes(x=-3,y=5.5),size=4,color="black",label="Method1") ;p 
p = p + geom_point(aes(x=0,y=9),size=5,fill="grey",shape=23);p 
p = p + geom_text(aes(x=1,y=9.5),size=4,color="black",label="Method2");p 
#曲线末尾加小点
p = p + geom_point(aes(x=x,y=y),size=3,color="grey50",shape=16,df1);p 
p = p + xlim(-5, 10) + guides(size=FALSE) + scale_y_reverse() + theme_void()  
p

X11()
#https://mp.weixin.qq.com/s/M4rDjisSr_7bzcz-otaJXw
#R语言之照猫画虎2

#上三角部分主要是调用corrplot包中的corrplot()
#method = 'square'表示使用正方形符号，type = 'upper'表示只画上三角区域。
library(corrplot)
par(omi = c(1, 3, 1, 1),
    cex = 1,
    family = 'Times New Roman') # windows系统可能需要安装其他字体包
M <- cor(mtcars) #计算相关系数矩阵
corrplot(M, method = "circle", type = 'upper')

#没有原始数据，我随便模拟生成了三组，分别是“Group01”、“Group02”和“Group03”，
#因为每组都要和每个变量连线，所以线条的数量是组数和相关系数矩阵行数的乘机（这里是3*11 = 33个）
library(dplyr)
library(corrplot)
# 准备数据
set.seed(20190420)
n <- ncol(mtcars) #11个因子。
grp <- c('Group01', 'Group02', 'Group03') # 分组名称
sp <- c(rep(0.0008, 6), rep(0.007, 2), rep(0.03, 3), rep(0.13, 22)) # P值，33个
gx <- c(-4.5, -2.5, 1) # 分组的X坐标
gy <- c(n-1, n-5, 2.5) # 分组的Y坐标
df <- data.frame(
  grp = rep(grp, each = n), # 分组名称，每个重复n次
  gx = rep(gx, each = n), # 组X坐标，每个重复n次
  gy = rep(gy, each = n), # 组Y坐标，每个重复n次
  x = rep(0:(n - 1) - 0.5, 3), # 变量连接点X坐标
  y = rep(n:1, 3), # 变量连接点Y坐标
  p = sample(sp), # 对人工生成p值进行随机抽样
  r = sample(c(rep(0.8, 4), rep(0.31, 7), rep(0.12, 22))) 
  # 对人工生成r值进行随机抽样
)
df
# 这一部分代码是按照原图图例说明处理线条宽度和颜色映射
df <- df %>% 
  mutate(
    lcol = ifelse(p <= 0.001, '#1B9E77', NA), 
    # p值小于0.001时，颜色为绿色，下面依次类推
    lcol = ifelse(p > 0.001 & p <= 0.01, '#88419D', lcol),
    lcol = ifelse(p > 0.01 & p <= 0.05, '#A6D854', lcol),
    lcol = ifelse(p > 0.05, '#B3B3B3', lcol),
    lwd = ifelse(r >= 0.5, 14, NA),
    # r >= 0.5 时，线性宽度为14，下面依次类推
    lwd = ifelse(r >= 0.25 & r < 0.5, 7, lwd),
    lwd = ifelse(r < 0.25, 1, lwd)
  )
df
#
# 绘制连接线
segments(df$gx, df$gy, df$x, df$y, lty = 'solid', lwd = df$lwd, 
         col = df$lcol, xpd = TRUE)
# 组标记点
points(gx, gy, pch = 24, col = 'blue', bg = 'blue', cex = 3, xpd = TRUE) 
# 组名称
text(gx - 0.5, gy, labels = grp, adj = c(1, 0.5), cex = 1.5, xpd = TRUE)

#这一部分主要在前面基础图的基础上确定每个元素标记位置，出图之后根据细节进行微调，没有太多复杂的内容。
labels01 <- c('<= 0.001','0.001 < x <= 0.01','0.01 < x <= 0.05','> 0.05')
labels02 <- c('>= 0.5', '0.25 - 0.5', '< 0.25')
labels_x <- rep(-6, 4)
labels_y <- seq(4.6, 2.6, length.out = 4)
text(-6.5, 5.2, 'P-value', adj = c(0, 0.5), cex = 1.2, font = 2, xpd = TRUE)
text(labels_x, labels_y, labels01, adj = c(0, 0.5), cex = 1.2, xpd = TRUE)
points(labels_x - 0.5, labels_y, pch = 20, col = c('#1B9E77', '#88419D','#A6D854', '#B3B3B3'),
       cex = 3, xpd = TRUE)
lines_x <- c(-6.5, -3, 0.5)
lines_y <- rep(1.2, 3)
text(-6.5, 1.9, "Mantel's r", adj = c(0, 0.5), cex = 1.2, font = 2, xpd = TRUE)
text(lines_x + 1.5, lines_y, labels02, adj = c(0, 0.5), cex = 1.2, xpd = TRUE)
segments(lines_x, lines_y, lines_x + 1, lines_y, lwd = c(14, 7, 2.5), lty = 'solid', 
         col = '#B3B3B3', xpd = TRUE)
## 图例框框
segments(-6.9, 5.6, -2.8, 5.6, lty = 'solid', lwd = 1.2, 
         col = 'grey50', xpd = TRUE)
segments(-2.8, 5.6, -2.8, 1.8, lty = 'solid', lwd = 1.2, 
         col = 'grey50', xpd = TRUE)
segments(-2.8, 1.8, 3.6, 1.8, lty = 'solid', lwd = 1.2, 
         col = 'grey50', xpd = TRUE)
segments(3.6, 1.8, 3.6, 0.7, lty = 'solid', lwd = 1.2, 
         col = 'grey50', xpd = TRUE)
segments(3.6, 0.7, -6.9, 0.7, lty = 'solid', lwd = 1.2, 
         col = 'grey50', xpd = TRUE)
segments(-6.9, 0.7, -6.9, 5.6, lty = 'solid', lwd = 1.2, 
         col = 'grey50', xpd = TRUE)

###2019.7.12
#Science组合图表解读 
#https://mp.weixin.qq.com/s/nfBAkh3byd9TW9xZ04lFLg

setwd("E:/桌面/R script 2017/一个群落beta多样性分析的完整R脚本/")
rm(list = ls())
library(vegan)
otu <- read.table("otu.txt",row.names = 1, header = TRUE,sep='\t')
env <- read.table("env.txt", header = TRUE,sep='\t',row.names = 1)
env
library(corrplot)
rdf<-cor(env)

#得到相关矩阵图
corrplot(rdf,method = "ellipse" ,type = "upper",addrect = 1,insig="blank",rect.col = "blue",rect.lwd = 2)
#以上因子的排序是按照env中的原始排序。考虑到后面的操作，我们更愿意将相关性高的一类因子放在一起，因此可以加入参数order="AOE"，另外"FPC"，"hclust"也有类似的效果。但是需要特别注意的是，后续mantel test 结果表中的因子顺序需按此重新排序，以免发生错配

corrplot(cor(env),method = "ellipse",type = "upper",order="AOE",addrect = 1,insig="blank",rect.col = "blue",rect.lwd = 2)

#计算总类群及分类群与各个环境因子的mantel相关系数及显著性。
#函数的功能大概就是将env矩阵中的每一个环境因子（已通过筛选）与otu进行mantel test，
#并从返回的model中将相关系数statistic和p值signif提取出来，并按顺序返回到一个新的dataframe中。
#此函数大大降低了工作量，只需作者整理好完整物种矩阵及各个subsample矩阵。
#当然也可以是多个独立的，但是都与同一环境因子矩阵相关联的物种矩阵，但是会损失一些可比信息。
#导入函数后，我们直接运行：
mt<-function(otu,env){
     library(vegan)
     library(dplyr)
     vars <- colnames(env)
     models<-list()
     for (i in seq_along(vars)){  #i=1
         otu_bray<-vegdist(t(otu),method = "bray") ##注意这里自己加了转置
         env_dis<-vegdist(env[vars[i]],method = "euclidean")
         model <- mantel(otu_bray,env_dis) #;?mantel
         name <- vars[i]
         statistic <- model$statistic
         signif <- model$signif
         models[[i]] <- data.frame(name = name, statistic = statistic, signif = signif, row.names = NULL)
       }
     models %>%  bind_rows()
}

mantRpTotal<-mt(otu,env)

#不同subsample最好不要重合
otu_sub1<-otu[,1:12]
otu_sub2<-otu[,-(1:12)]

#自己加，环境因子也得具有同样的分组。否则mantel做cor的时候会报错不兼容的量度
env1=env[1:12,]
env2=env[-(1:12),]

#同样必须保证otu_sub1和otu_sub2中每一行的和不为0
mantRpTotal<-mt(otu,env)
mantRpsub1<-mt(otu_sub1,env1)
mantRpsub2<-mt(otu_sub2,env2)
#得到三组相关矩阵

#利用此表做出文献图1中右上部分的线段图还不够，还需要另外输入坐标信息。
set.seed(123)
n <- ncol(env)
grp <- c('Total', 'Sub1', 'Sub2') # 分组名称
subx <- c(-2, -1, 0) # 分组的X坐标
suby <- c(4.5, 2.5, 1) # 分组的Y坐标

df <- data.frame(
  grp = rep(grp, each = n), # 分组名称，每个重复n次
  subx = rep(subx, each = n), # 组X坐标，每个重复n次
  suby = rep(suby, each = n), # 组Y坐标，每个重复n次
  x = rep(0:(n - 1) - 0.5, 3), # 变量连接点X坐标
  y = rep(n:1, 3) # 变量连接点Y坐标
)
df

df2 <-rbind(mantRpTotal, mantRpsub1, mantRpsub2)
df_segment<-cbind(df,df2)
df_segment

#此时需考虑线条美学的映射，按原文图的表示，并不是按数值大小完全映射，而是划分范围后映射

df_segment <- df_segment %>% 
  mutate(
    lcol = ifelse(signif <= 0.001, '#1B9E77', NA), 
    # p值小于0.001时，颜色为绿色，下面依次类推
    lcol = ifelse(signif > 0.001 & signif <= 0.01, '#88419D', lcol),
    lcol = ifelse(signif > 0.01 & signif <= 0.05, '#A6D854', lcol),
    lcol = ifelse(signif > 0.05, '#B3B3B3', lcol),
    lwd = ifelse(statistic >= 0.3,6, NA),
    # statistic >= 0.3 时，线性宽度为6，下面依次类推
    lwd = ifelse(statistic >= 0.15 & statistic < 0.3, 3, lwd),
    lwd = ifelse(statistic < 0.15, 1, lwd)
  )

#一切就绪，开始拼图。
corrplot(cor(env),method = "ellipse",type = "upper", addrect = 1,insig="blank",rect.col = "blue",rect.lwd = 2)

segments(df_segment$subx, df_segment$suby, df_segment$x, df_segment$y, lty = 'solid', lwd = df_segment$lwd, col = df_segment$lcol, xpd = TRUE)

points(subx, suby, pch = 24, col = 'blue', bg = 'blue', cex = 2, xpd = TRUE)

points(x,y,pch=21,col="red",bg="red",cex=1,xpd=TRUE)

text(subx - 0.5, suby, labels = grp, adj = c(0.8, 0.5), cex = 1.2, xpd = TRUE)

#下面继续挖掘并聚焦已经找到的目标，pH。

library(vegan)
otu_rda<- rda(t(otu),env)
summary(otu_rda)
plot(otu_rda)

#获取各样本pH在RDA第一轴的坐标,然后与env$pH做散点图
pH_sca<- otu_rda$CCA$QR$qr$pH
