library(venn);?venn
library(vegan)
library(ggplot2)
library(ggpolypath)

#ve = read.table(file = "C:/Users/19021/Desktop/李燕data/venn和相关性.txt",sep="\t",header = T,row.names = 1)
ve = read.table(file = "C:/Users/19021/Desktop/李燕data/新数据/venn.txt",sep="\t",header = T,row.names = 1)
ven = decostand(ve,method = "pa")

###################
#https://mp.weixin.qq.com/s?src=11&timestamp=1594038920&ver=2444&signature=qFLELf0deNWaOuYQqebgSi2s-wKCGjJhXws-8HK*1B3fw5xR6hHzrLzMl0gMdDZABpa29NNCcZnrMJKyvrcoduprL8ATdBWzJ*pYGyJf3LLLKIkvD1*7gXRtbmX4*mdH&new=1
#https://mp.weixin.qq.com/s?src=11&timestamp=1594038920&ver=2444&signature=M3N3QYtIUFwEycZciUsCZ9xb3XXQE7ISb6An4svFmsM999w7Ogn6BcOIvq1TYZ9naZaN7N-I7zQDNqPw1FlwuPPDjzx-HnXsUd7v66PBjNURM0rgISdg7roI0c1HiUKn&new=1

data <- ifelse(ve > 0, rownames(ve),NA)
data <- as.data.frame(data)
df <- list()
for(i in 1:length(data)){
  a <- na.omit(data[,i])
  df <- c(df,list(a))
}
names(df) <- colnames(data)

library(VennDiagram)
library(RColorBrewer)
venn.diagram(df,filename = "venn.png",height = 5400,width = 5400,
             resolution = 600,imagetype = "png",units = "px",
             lwd = 2,lty = 1,fill = brewer.pal(length(data),"Set2"),cex = 1.3,
             cat.cex = 2,alpha = 0.8,margin = 0.05,fontface = 2,
             cat.fontface = 2, print.mode = c("raw","percent"))
dev.off()
##
T<-venn.diagram(df,
                filename=NULL,  #venn.png
                #height = 5400,width = 5400,resolution = 600,imagetype = "png",units = "px",
                lwd=1,#圈线粗度
                lty=1, #圈线类型
                col=c('#0099CC','#FF6666','#FFCC99'), #圈线颜色
                fill=c('#0099CC','#FF6666','#FFCC99'), #填充颜色   #brewer.pal(length(data),"Set2")
                cat.col=c('#0099CC','#FF6666','#FFCC99'),#A和B的颜色
                cat.cex = 2.5,# A和B的大小
                rotation.degree = 0,#旋转角度
                main = "A&B",#主标题内容
                main.cex = 2,#主标题大小
                sub = "plot : example",#亚标题内容
                sub.cex = 1,#亚标题字大小
                cex=1.5,#里面交集字的大小
                alpha = 0.5,#透明度
                #margin = 0.05,fontface = 2,cat.fontface = 2,
                reverse=TRUE)
grid.draw(T)

##################venn
venn(ven,zcolor='style')

gg <- venn(ven,zcolor='style',cexil = 20, #圈内字大小
           cexsn = 20) #样本名大小

#椭圆形
ellipse = TRUE

#花瓣形
ilabels = TRUE

# increasing the border size
venn(x, ggplot = TRUE, size = 1.5)

# with dashed lines
venn(x, ggplot = TRUE, linetype = "dashed")

