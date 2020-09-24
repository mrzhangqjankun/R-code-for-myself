##2018.6.22-6.23

######################1.ggplot2学习笔记系列之利用ggplot2绘制误差棒及显著性标记
#https://blog.csdn.net/kMD8d5R/article/details/79319870

library(ggplot2)
#创建数据集
df <- data.frame(treatment = factor(c(1, 1, 1, 2, 2, 2, 3, 3, 3)),
                 response = c(2, 5, 4, 6, 9, 7, 3, 5, 8),
                 group = factor(c(1, 2, 3, 1, 2, 3, 1, 2, 3)),
                 se = c(0.4, 0.2, 0.4, 0.5, 0.3, 0.2, 0.4, 0.6, 0.7))
head(df) #查看数据集

# 使用geom_errorbar()绘制带有误差棒的条形图
# 这里一定要注意position要与`geom_bar()`保持一致，由于系统默认dodge是0.9，
# 因此geom_errorbar()里面position需要设置0.9，width设置误差棒的大小
ggplot(data = df, aes(x = treatment, y = response, fill = group)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymax = response + se, ymin = response -  se),
                position = position_dodge(0.9), width = 0.15) +
  scale_fill_brewer(palette = "Set1")

#绘制带有显著性标记的条形图
label <- c("", "*", "**", "", "**", "*", "", "", "*") #这里随便设置的显著性，还有abcdef等显著性标记符号，原理一样，这里不再重复。
# 添加显著性标记跟上次讲的添加数据标签是一样的，这里我们假设1是对照
ggplot(data = df, aes(x = treatment, y = response, fill = group)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymax = response + se, ymin = response -  se),
                position = position_dodge(0.9), width = 0.15) +
  geom_text(aes(y = response +  1.5 * se, label = label, group = group),
            position = position_dodge(0.9), size = 5, fontface = "bold") +
  scale_fill_brewer(palette = "Set1") #这里的label就是刚才设置的，group是数据集中的，fontface设置字体。

#绘制两条形图中间带有星号的统计图
#创建一个简单的数据集
Control <- c(2.0,2.5,2.2,2.4,2.1)
Treatment <- c(3.0,3.3,3.1,3.2,3.2)
mean <- c(mean(Control), mean(Treatment))
sd <- c(sd(Control), sd(Treatment))
df1 <- data.frame(V=c("Control", "Treatment"), mean=mean, sd=sd)
df1$V <- factor(df1$V, levels=c("Control", "Treatment"))
#利用geom_segment()绘制图形
ggplot(data=df1, aes(x=V, y=mean, fill=V))+
  geom_bar(stat = "identity",position = position_dodge(0.9),color="black")+
  geom_errorbar(aes(ymax=mean+sd, ymin=mean-sd), width=0.05)+
  geom_segment(aes(x=1, y=2.5, xend=1, yend=3.8))+#绘制control端的竖线
  geom_segment(aes(x=2, y=3.3, xend=2, yend=3.8))+#绘制treatment端竖线
  geom_segment(aes(x=1, y=3.8, xend=1.45, yend=3.8))+
  geom_segment(aes(x=1.55, y=3.8, xend=2, yend=3.8))+#绘制两段横线
  annotate("text", x=1.5, y=3.8, label="〇", size=5)#annotate函数也可以添加标签

#为图形添加标题
#图形标题有图标题、坐标轴标题、图例标题等
p <- ggplot(data = df, aes(x = treatment, y = response, fill = group)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymax = response + se, ymin = response -  se),
                position = position_dodge(0.9), width = 0.15) +
  scale_fill_brewer(palette = "Set1")# 利用ggtitle()添加图标题,还有labs（）也可以添加标题，最后会提一下。（有一个问题就是ggtitle()添加的标题总是左对齐）
p + ggtitle("利用ggtitle()添加图标题")

# 利用xlab()\ylab()添加/修改坐标轴标题
p + ggtitle("利用ggtitle()添加图标题") +
  xlab("不同处理") +
  ylab("response") #标题的参数修改在theme里，theme是一个很大的函数，几乎可以定义一切，下次有时间会讲解

##最后再讲解一下如何将多副图至于一个页面 利用包gridExtra中grid.arrange()函数实现
# 将四幅图放置于一个页面中
p <- ggplot(data = df, aes(x = treatment, y = response, fill = group)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymax = response + se, ymin = response -  se),
                position = position_dodge(0.9), width = 0.15) +
  scale_fill_brewer(palette = "Set1")
p1 <- p + ggtitle("利用ggtitle()添加图标题")
p2 <- p + ggtitle("利用ggtitle()添加图标题") + xlab("不同处理") + ylab("response")
p3 <- ggplot(data = df, aes(x = treatment, y = response, fill = group)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymax = response + se, ymin = response -  se),
                position = position_dodge(0.9), width = 0.15) +
  geom_text(aes(y = response +  1.5 * se, label = label, group = group),
            position = position_dodge(0.9), size = 5, fontface = "bold") +
  scale_fill_brewer(palette = "Set1")
library(gridExtra) #没有安装此包先用install.packages('gridExtra')安装
grid.arrange(p, p1, p2, p3)


######################2.R语言可视化学习笔记之添加p-value和显著性标记 
#https://mp.weixin.qq.com/s?__biz=MzA3MTM3NTA5Ng==&mid=2651057507&idx=1&sn=ae611bd67a11ed03089b4ce53eefa6a8&chksm=84d9ccf4b3ae45e29b89b5f347a5407aca0f9bd3ad59575a2bd84b891c086f509a21264839b1&scene=21#wechat_redirect
#先加载包
library(ggpubr)
#加载数据集ToothGrowth
data("ToothGrowth")
head(ToothGrowth)

# R中常用的比较方法主要有下面几种：
#   
# T-test         t.test()        比较两组(参数)
# Wilcoxon test  wilcox.test()   比较两组(非参数)
# ANOVA          aov()或anova()  比较多组(参数)
# Kruskal-Wallis kruskal.test()  比较多组(非参数)