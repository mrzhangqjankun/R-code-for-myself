##li 2017.7.5 
##http://www.omicshare.com/forum/thread-1742-1-1.html
##http://www.omicshare.com/forum/thread-1996-1-1.html
##[多样性测序] RDA分析之一小段“vegan”代码分享
##[多样性测序] vegan+ggplot2：让你的CCA or RDA图形美起来

rm(list=ls(all=TRUE))
#安装 ”vegan“包#
install.packages("vegan")
#载入”vegan“包#
library("vegan")
#读取数据文件，“包括样本--物种数据”(命名为sp)#1.1
#setwd('C:/Users/dell/Desktop/statistics/1.1.txt')
sp <- read.table(file=file.choose(),sep="\t",header=T,row.names=1)
sp
#读取”样本--环境数据“（命名为se）#1.2
se <- read.table(file=file.choose(),sep="\t",header=T,row.names=1)
se
#1：非限制性排序；非度量多维尺度分析(non-metric multi-dimensional scaling, NMDS)#
ord <- metaMDS(sp)
ord
plot(ord, type = "t")
#环境因子相关性分析#
ef  <- envfit(ord,se) 
ef
#画图，如果想限定p值小于0.05，可在plot()内添加p.max=0.05#
plot(ef,p.max=0.05)


#2：限制性排序(constrained ordination)；可以做冗余分析(redundancy analysis,RDA)#
#也可以做典范对应分析(canonical correspondence analysis, CCA)；#
#RDA或CCA选择原则：先用species-sample做DCA分析# 
decorana(sp)    ##DCA
#根据看分析结果中Lengths of gradient 的第一轴的大小#
#如果大于4.0,就应选CCA；如果在3.0-4.0之间，选RDA和CCA均可#
#如果小于3.0, RDA的结果要好于CCA#
#非限制性PCA排序#
sp0 <- rda(sp ~ 1, se)  
sp0
plot(sp0)
#加入所有环境变量排序，RDA分析#
sp1 <- rda(sp ~ ., se)  
sp1
plot(sp1)
#得出RDA图#
#最后保存图片，退出R#
##?rda
##美化
##准备作图数据：提取RDA分析结果的数据,作为新图形元素
new<-sp1$CCA
new
#提取并转换“样本”数据
samples<-data.frame(sample=row.names(new$u),RDA1=new$u[,1],RDA2=new$u[,2])
samples
#提取并转换“物种”数据
species<-data.frame(spece=row.names(new$v),RDA1=new$v[,1],RDA2=new$v[,2])
species
#提取并转换“环境因子”数据
envi<-data.frame(en=row.names(new$biplot),RDA1=new$biplot[,1],RDA2=new$biplot[,2])
envi
#构建环境因子直线坐标
line_x = c(0,envi[1,2],0,envi[2,2],0,envi[3,2],0,envi[4,2],0,envi[5,2],0,envi[6,2])
line_x
line_y = c(0,envi[1,3],0,envi[2,3],0,envi[3,3],0,envi[4,3],0,envi[5,3],0,envi[6,3])
line_y
line_g = c("pH","pH","T","T","S2","S2","NH4","NH4","NO2","NO2","Fe2","Fe2")
line_g
line_data = data.frame(x=line_x,y=line_y,group=line_g)
line_data
#载入ggplot2包
library(ggplot2)
#开始重绘RDA图
#填充样本数据，分别以RDA1,RDA2为X,Y轴，不同样本以颜色区分
ggplot(data=samples,aes(RDA1,RDA2)) + geom_point(aes(color=sample),size=2) +
  #填充微生物物种数据，不同物种以图形区分
  geom_point(data=species,aes(shape=spece),size=2) + 
  #填充环境因子数据，直接展示
  geom_text(data=envi,aes(label=en),color="blue") +
  #添加0刻度纵横线
  geom_hline(yintercept=0) + geom_vline(xintercept=0)+
  #添加原点指向环境因子的直线
  geom_line(data=line_data,aes(x=x,y=y,group=group),color="green") +
  #去除背景颜色及多余网格线
  theme_bw() + theme(panel.grid=element_blank())
#大功告成，保存为矢量图等等
ggsave("RDA2.PDF")




