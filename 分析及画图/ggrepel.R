##2018.6.30

##ggplot处理标签遮盖问题工具ggrepel 
https://mp.weixin.qq.com/s?__biz=MzI3Mzc1MzczMA==&mid=2247488700&idx=2&sn=ebc69542c6a225545eebda8cb6eaf63c&chksm=eb1f2751dc68ae4784e29ff202a677573f02223e6c67558798dc4eb4c2126095218b36ba55fc&mpshare=1&scene=1&srcid=06292qWNRGvan4cXFRT80OpB&pass_ticket=WSAwceNDt2fyyiVtkgRk2EI%2BfmY9b2PRhcBpWbOFzvIShQ9is6SDOPXZTmVlDzck#rd

https://github.com/slowkow/ggrepel

#devtools::install_github("slowkow/ggrepel")

##usage
https://cran.r-project.org/web/packages/ggrepel/vignettes/ggrepel.html

rm(list=ls())

library(ggplot2);library(ggrepel)

# ggrepel就一个函数geom_text_repel，是对geom_text的封装，
# geom_text的参数geom_text都可以用（除了vjust，hjust，position，check_overlap），
# 常用参数如下：
# 
# segment.color   连接点与标签线段的颜色,默认black
# segment.size    线段的粗细,默认0.5mm
# segment.alpha   线段的透明度，默认1
# box.padding     文本框周边填充
# point.padding   点周围填充
# arrow           线段添加箭头
# force           强制性将重叠文本散开,默认1
# max.oter        最大迭代次数
# nudge_x,nudge_y 标签开始位置在坐标轴的移动距离.默认0
# direction        移动的方向。默认both，可设置x,y

head(mtcars)

ggplot(mtcars, aes(wt, mpg)) +
  geom_point() +
  geom_text(aes(label = rownames(mtcars)))

ggplot(mtcars, aes(wt, mpg)) +
  geom_point() +
  geom_text_repel(aes(label =rownames(mtcars)))

ggplot(mtcars, aes(wt, mpg, col = factor(cyl))) +
  geom_point() +
  geom_text_repel(aes(label =rownames(mtcars)),
                  box.padding = unit(0.35,"lines"),
                  point.padding = unit(0.5,"lines"),
                  show.legend = F,
                  size = 3)


# All labels should be to the right of 3.
x_limits <- c(3, NA)

ggplot(mtcars, aes(wt, mpg, label = rownames(mtcars), color = factor(cyl))) +
  geom_vline(xintercept = x_limits, linetype = 3) +
  geom_point() +
  geom_label_repel(
    arrow = arrow(length = unit(0.03, "npc"), type = "closed", ends = "first"),
    force = 10,
    xlim  = x_limits
  ) +
  scale_color_discrete(name = "cyl")

###往上下移动
# hjust = 0 for left-align
# hjust = 0.5 for center
# hjust = 1 for right-align
# Sometimes the labels do not align perfectly. 
# Try using direction = "x" to limit label movement to the x-axis (left and right) 
# or direction = "y" to limit movement to the y-axis (up and down). 
# The default is direction = "both".

set.seed(42)

ggplot(mtcars, aes(x = wt, y = 1, label = rownames(mtcars))) +
  geom_point(color = "red") +
  geom_text_repel(
    nudge_y      = 0.05,  ##label在y轴上移动的距离。值越大越往上。
    direction    = "x",  ##按照x轴方向整齐排列。若没有这个参数label排列不好看。
    angle        = 90,
    vjust        = 0,
    segment.size = 0.2
  ) +
  xlim(1, 6) +
  ylim(1, 0.8) +
  theme(
    axis.line.y  = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y  = element_blank(),
    axis.title.y = element_blank()
  )


###往左右移动
#Set direction to “y” and try hjust 0.5, 0, and 1:
set.seed(42)

p <- ggplot(mtcars, aes(y = wt, x = 1, label = rownames(mtcars))) +
  geom_point(color = "red") +
  ylim(1, 5.5) +
  theme(
    axis.line.x  = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x  = element_blank(),
    axis.title.x = element_blank(),
    plot.title   = element_text(hjust = 0.5)
  )
p
p1 <- p +
  xlim(1, 1.375) +
  geom_text_repel(
    nudge_x      = 0.15,
    direction    = "y",
    hjust        = 0,
    segment.size = 0.2
  ) +
  ggtitle("hjust = 0")
p1
p2 <- p + 
  xlim(1, 1.375) +
  geom_text_repel(
    nudge_x      = 0.2,
    direction    = "y",
    hjust        = 0.5,
    segment.size = 0.2
  ) +
  ggtitle("hjust = 0.5 (default)")
p2
p3 <- p +
  xlim(0.25, 1) +
  scale_y_continuous(position = "right") +
  geom_text_repel(
    nudge_x      = -0.35,
    direction    = "y",
    hjust        = 1,
    segment.size = 0.2
  ) +
  ggtitle("hjust = 1")
p3
gridExtra::grid.arrange(p1, p2, p3, ncol = 3)



####左右同时移动
###Align text horizontally with nudge_x and hjust, 
#and allow the labels to move vertically with direction = "y":
set.seed(42)

dat <- subset(mtcars, wt > 2.75 & wt < 3.45)
dat$car <- rownames(dat)

ggplot(dat, aes(wt, mpg, label = car)) +
  geom_text_repel(
    data          = subset(dat, wt > 3),
    nudge_x       = 3.5 - subset(dat, wt > 3)$wt,
    segment.size  = 0.2,
    segment.color = "grey50",
    direction     = "y",
    hjust         = 0
  ) +
  geom_text_repel(
    data          = subset(dat, wt < 3),
    nudge_x       = 2.7 - subset(dat, wt < 3)$wt,
    segment.size  = 0.2,
    segment.color = "grey50",
    direction     = "y",
    hjust         = 1
  ) +
  scale_x_continuous(
    breaks = c(2.5, 2.75, 3, 3.25, 3.5),
    limits = c(2.4, 3.8)
  ) +
  geom_point(color = "red")



###按照引物
getwd()
even = read.table("Galaxy108-UPARSE_even.txt",sep="\t",header = T)
even
##列名变为group
###reshape2:melt
even2<-melt(
  even,                       #待转换的数据集名称
  id.vars=c("species","x"),  #要保留的主字段
  variable.name="primer",         #转换后的分类字段名称（维度）
  value.name="abundance"             #转换后的度量值名称
)
even2

###tidyr:gather
even3<-gather(
  data=even,      #待转换的数据集名称
  key="primer",       #转换后的分类字段名称（维度）
  value="abundance" ,    #转换后的度量值名称
  3:8 #选择将要被拉长的字段组合
)               #（可以使用x:y的格式选择连续列，也可以以-z的格式排除主字段）
even3


ggplot(even2, aes(x=12.5, abundance, color = primer,label=primer))+
  geom_point()+
  geom_text_repel(
    data          = subset(even2, primer == "E.1F2R"),
    nudge_x       = 0.2,
    segment.size  = 0.2,
    segment.color = "grey50",
    direction     = "y",
    hjust         = 0
  ) +
  geom_text_repel(
    data          = subset(even2, primer == "E.7F4R"),
    nudge_x       = 0.4,
    segment.size  = 0.2,
    segment.color = "grey50",
    direction     = "y",
    hjust         = 0.5
  ) +
  geom_text_repel(
    data          = subset(even2, primer == "E.5.8S4Fun"),
    nudge_x       = 0.6,
    segment.size  = 0.2,
    segment.color = "grey50",
    direction     = "y",
    hjust         = 1
  ) +


geom_text_repel(
  data          = subset(even2, primer == "EK.1F2R"),
  nudge_x       = -0.2,
  segment.size  = 0.2,
  segment.color = "grey50",
  direction     = "y",
  hjust         = 0
) +
  geom_text_repel(
    data          = subset(even2, primer == "EK.7F4R"),
    nudge_x       = -0.4,
    segment.size  = 0.2,
    segment.color = "grey50",
    direction     = "y",
    hjust         = 0.5
  ) +
  geom_text_repel(
    data          = subset(even2, primer == "EK.5.8S4Fun"),
    nudge_x       = -0.6,
    segment.size  = 0.2,
    segment.color = "grey50",
    direction     = "y",
    hjust         = 1
  )+
  scale_x_continuous(
    breaks = c(12.5),
    limits = c(12, 13)
  ) +
  geom_hline(yintercept = 12.5,linetype = 3,color = "red", size =2)+
    theme(
    axis.line.x  = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x  = element_blank(),
#    axis.title.x = element_blank(),
    plot.title   = element_text(hjust = 0.5)
  )+
  ggtitle("Even community")+
  xlab("Relative abundance of template (%) ") +
  ylab("Relative abundance of amplication (%) ")

###geom_jitter()