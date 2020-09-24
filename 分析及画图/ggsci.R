##2019.5.27
#https://mp.weixin.qq.com/s/_1dMzYC2-VkK88vwEVQruQ
#ggsci-让你的论文配图一步到位的满足期刊要求

#install.packages("ggsci")
library(ggsci)
#ggsci中的调色盘可以通过scales包中show_col函数查看：

library(scales);?pal_npg() #nature系列的配色
pal <- pal_npg("nrc", alpha=0.7)(9) #nrc是Palette Types，alpha用于调节透明度
show_col(pal)

#针对离散数据，ggsci中有两个函数用来控制配色scale_color_palname()和scale_fill_palname()，palname用于指定不同的调色盘。
#使用ggsci前：
data("mtcars")
library(ggplot2)
p1<- ggplot(mtcars)+   
  geom_boxplot(aes(mtcars$mpg,mtcars$wt,group=mtcars$cyl))+
  labs(y="wt",x="mpg");p1
#使用ggsci之后：
p1+scale_fill_npg()+theme_bw()+   #选择Nature调色盘 
  #coord_cartesian(ylim=c(0,1))+ #设置y轴范围
  theme(axis.text.x = element_text(size=rel(1.0),angle = 45,hjust = 1,color ="black"),#字体倾斜        
        panel.grid =element_blank())

#在ggsci中不同主题拥有的颜色数量是固定的，所以如果变量个数太多，
#有一些主题可能并不适用（Nature主题只有9个颜色），需要选择其他主题。
#未使用ggsci：
library(ggplot2)
library(reshape2)
norm_data<-read.csv("normfinder/normfinder_ref barplot.csv",sep=",",header=T,stringsAsFactors = F) #输入数据
mydata<-melt(norm_data,id.vars="TranscriptID",variable.name="Group",value.name="Value")
p2<-ggplot(mydata,aes(TranscriptID,value,fill=group))+     
  geom_bar(stat="identity",position="dodge")+
  ggtitle("Stability and variation value")
#使用ggsci之后：
p_load(Cairo)
CairoPDF(file="normfinder/normfinder_ref barplot.pdf",width = 9.26, height = 4.13)#用cairo可以进行字体设置
p2+ scale_fill_d3(palette = c("category20c"))+theme_bw()+ #选择D3调色盘
  theme(axis.text.x = element_text(size=rel(1.0),angle = 45,hjust = 1,color ="black"),
        panel.grid =element_blank(),#删除网格线
        plot.title = element_text(hjust = 0.5))#使标题居中
dev.off()

#D3调色盘的Palette Types有4种，其中category10提供10种颜色，category20和category20b,category20c提供20种配色。

#此外ggsci还有针对连续型数据的调色盘，用函数scalefillgsea()和scalefillmaterial控制配色，应用于相关性分析或热图。
library("reshape2")
data("mtcars") #使用R自带的数据
cor <- cor(unname(cbind(mtcars, mtcars, mtcars, mtcars)))
cor_melt <- melt(cor)
p3 <- ggplot( cor_melt,aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(colour = "black", size = 0.3) +
  theme_bw() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank())

p3_gsea <- p3 + scale_fill_gsea()
p3_gsea
#使用scalefillmaterial()可以选择不同的颜色
p3 + scale_fill_material("red")

#7.31
#https://mp.weixin.qq.com/s/MwLpFdPdhvI-lX93hsGlCQ