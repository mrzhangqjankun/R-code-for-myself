##2018.5.20
##https://mp.weixin.qq.com/s?__biz=MzAwMDY0MzQ0Ng==&mid=2247484226&idx=1&sn=d56587520be2b5e7582a79b4e77ea32f&chksm=9ae49b5bad93124d316785c3ac1df0c3b60d76744b039929c59d5539b0f03e5eb62d3edbecf2&mpshare=1&scene=1&srcid=0520mH4K2hhPMIHZsnHEpLr4&pass_ticket=a%2FDZKnr7xDnM7txpzaFEK%2BPJtwU6W9%2BkB4QMG1yU6Lht0mbLenrBKIaxi9BbMLim#rd
##ggplot2作分组箱线图并添加均值点连线及显著性程度标注 

rm(list = ls())
library(data.table);?data.table
library(ggplot2)
library(ggsignif)

Group1 = data.frame(A=runif(100,15,90),B=rnorm(100,30,3),C=runif(100,30,60),D=rexp(100,0.1),E=rpois(100,10),group = 1)
Group2 = data.frame(A=runif(100,0,100),B=rnorm(100,50,2),C=runif(100,40,70),D=rexp(100,0.3),E=rpois(100,20),group = 2)

b= rbind(Group1,Group2)
head(b);dim(b)

#将数据框宽格式变长格式方便制图：
?melt
b= melt(b,id.vars = c("group"));head(b);tail(b)#id.vars	:vector of id variables. Can be integer (variable position) or string (variable name). If blank, will use all non-measured variables.
b$group = as.factor(b$group)

#由于要做每个箱线图的均值点及均值连线，需要获得每个组每个属性的均值，并且定义每个组每个属性的X坐标为固定值。
#group1的mean:
c = copy(b);?copy
#sum(!(c == b)) #0,c==b
#identical(c,b) #TRUE
?setDF
setDF(c) ##c强制变为数据框
class(c);class(b)
c1 = tapply(c[c$group==1,"value"],c[c$group==1,"variable"],mean)
c2 = tapply(c[c$group==2,"value"],c[c$group==2,"variable"],mean)
c3 = rbind(data.frame(variable = names(c1),value = c1,group=1),data.frame(variable = names(c2),value = c2,group=2))
c3
c3$group = as.factor(c3$group)

# 分别计算两组均值，用来画折线图：
c3$variable2 = NA

c3[c3$group==1&c3$variable=="A","variable2"] = 0.795
c3[c3$group==1&c3$variable=="B","variable2"] = 1.795
c3[c3$group==1&c3$variable=="C","variable2"] = 2.795
c3[c3$group==1&c3$variable=="D","variable2"] = 3.795
c3[c3$group==1&c3$variable=="E","variable2"] = 4.795

c3[c3$group==2&c3$variable=="A","variable2"] = 1.185
c3[c3$group==2&c3$variable=="B","variable2"] = 2.185
c3[c3$group==2&c3$variable=="C","variable2"] = 3.185
c3[c3$group==2&c3$variable=="D","variable2"] = 4.185
c3[c3$group==2&c3$variable=="E","variable2"] = 5.185

c3$variable2

#用来做均值点和连线的数据如下
c3

#开始做两组每个属性的箱线图：
p1<-ggplot(b)+
  geom_boxplot(aes(x=variable,y=value,fill=group),width=0.6,position = position_dodge(0.8),outlier.size = 0,outlier.color = "white")+
  scale_fill_manual(values = c("red", "blue"),breaks=c("1","2"),labels=c("Group 1","Group 2"))+
  geom_point(data=c3,aes(x=variable2,y=value,color=group),shape=15,size=1)+
  geom_line(data=c3,aes(x=variable2,y=value,color=group),size=1,linetype = "dotted")+
  # geom_smooth(data=c3,aes(x=variable2,y=value,color=group),size=1,linetype = "dashed")+
  # stat_summary(fun.y = mean, geom = "errorbar", aes(x=variable,y=value,ymax = ..y.., ymin = ..y..,color=group),width = .75, linetype = "dashed")
  xlab("")+
  ylab("")+
  scale_y_continuous(limits = c(0,110),breaks=seq(0,110,5)) +
  geom_signif(stat="identity",
              data=data.frame(x=c(0.795,1.795,2.795,3.795,4.795), xend=c(1.185, 2.185,3.185,4.185,5.185),
                              y=c(106,66,70,30,35), annotation=c("***", " *** ","  ***  ","    **    ","*")),
              aes(x=x,xend=xend, y=y, yend=y, annotation=annotation)) +
  theme_bw()+
  theme(
    legend.position = "top",
    legend.background=element_blank(),
    legend.key = element_blank(),
    legend.margin=margin(0,0,0,0,"mm"),
    axis.text.x=element_text(size=rel(1.1),face="bold"),
    axis.line.x = element_line(size = 0.5, colour = "black"),
    axis.line.y = element_line(size = 0.5, colour = "black"),
    legend.text=element_text(size=rel(1.1)),
    legend.title=element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank()
  )+
  guides(color=FALSE)
p1
              
##然后做两组整体水平的箱线图：
p2<-ggplot(b)+
  geom_boxplot(aes(x=group,y=value,fill=group),width=0.8,position=position_dodge(1))+
  stat_summary(fun.y = mean, geom = "point", aes(x=group,y=value,color=group),shape=15)+
  scale_fill_manual(values = c("red", "blue"),breaks=c("1","2"),labels=c("Group 1","Group 2"))+
  scale_x_discrete(breaks=c("1","2"),labels=c("Group 1","Group 2"))+
  scale_y_continuous(limits = c(0,110),breaks=seq(0,110,5)) +
  geom_signif(stat="identity",
              data=data.frame(x=c(1), xend=c(2),
                              y=c(106), annotation=c("**")),
              aes(x=x,xend=xend, y=y, yend=y, annotation=annotation))+
  theme_bw()+
  theme(
    legend.position = "top",
    legend.background=element_blank(),
    legend.key = element_blank(),
    legend.margin=margin(0,0,0,0,"mm"),
    axis.text.x=element_text(size=rel(1.1),face="bold"),
    axis.line.x = element_line(size = 0.5, colour = "black"),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title = element_blank(),
    legend.text=element_text(size=rel(1.1)),
    legend.title=element_blank(),
    plot.margin = margin(11.5,0,7,0,"mm"),
    panel.border = element_blank(),
    panel.grid = element_blank()
  )+
  guides(fill=F,color=F)
p2
  
#最后合并两个图像：
# 如果没有安装则install.packages("gridExtra")
library(gridExtra)
# 合并两个图：
grid.arrange(p1, p2, nrow=1, ncol=2,widths=c(3.5,1),heights=c(4))

