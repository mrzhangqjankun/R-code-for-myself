##堆叠柱状图各成分连线
##2018-02-06 朱微金 李陈浩 宏基因组
##https://mp.weixin.qq.com/s?__biz=MzUzMjA4Njc1MA==&mid=2247484986&idx=1&sn=5885daab9fd0fd74871e9d22f9043e99&chksm=fab9ec8bcdce659d618179a28b2f4bb45488e327c0fe5cd10e80ed6155855bc0f09fd34f586a&scene=0#rd

# 安装和加载tidyverse包
#install.packages("tidyverse")
library(tidyverse)
?tidyverse
library(reshape2)
#devtools::install_github("hadley/tidyverse")
#library(tidyverse)

#df = read.table(file = "C:/Users/19021/Desktop/李燕data//丰度.txt",sep="\t",header = T)
df = read.table(file = "C:/Users/19021/Desktop/李燕data/新数据/species.txt",sep="\t",header = T)

df.long <- df %>% gather(group, abundance, -Species)
df.long

## 组间连线数据：
## 假设第一列是Phylum
link_dat <- df %>% 
  arrange(by=desc(Species)) %>% 
  mutate_if(is.numeric, cumsum) ;link_dat
bar.width <- 0.7
link_dat <- link_dat[, c(1,2,rep(3:(ncol(link_dat)-1),each=2), ncol(link_dat))];link_dat
link_dat <- data.frame(y=t(matrix(t(link_dat[,-1]), nrow=2)));link_dat
link_dat$x.1 <- 1:(ncol(df)-2)+bar.width/2;link_dat$x.1
link_dat$x.2 <- 1:(ncol(df)-2)+(1-bar.width/2);link_dat$x.2

df.long$group <- factor(df.long$group,levels=c("S9", "S23", "S32"), ordered=TRUE)

library("ggthemes")
library("RColorBrewer")
?scale_colour_brewer()
display.brewer.all() 
colourCount = length(unique(df.long$Species))
getPalette = colorRampPalette(brewer.pal(9, 'Set1'))
ggplot(df.long, aes(x=group, y=abundance, fill=Species)) + 
  geom_bar(stat = "identity", width=bar.width, col='black')  + 
  geom_segment(data=link_dat,
               aes(x=x.1, xend=x.2, y=y.1, yend=y.2), inherit.aes = F)+
  xlab("Samples")+ylab("Abundance")+scale_fill_manual(values = getPalette(colourCount))

dev.off()
##不要连线
ggplot(df.long, aes(x=group, y=abundance, fill=Species)) + 
  geom_bar(stat = "identity", width=bar.width, col='black')  + 
  xlab("Samples")+ylab("Abundance")+scale_fill_manual(values = getPalette(colourCount))
dev.off()


##图例变成斜体
p = ggplot(df.long, aes(x=group, y=abundance, fill=Species)) + 
  geom_bar(stat = "identity", width=bar.width, col='black')  + 
  xlab("Samples")+ylab("Abundance")+scale_fill_manual(values = getPalette(colourCount))
p
p2 <- p + guides(fill = guide_legend(
  label =T, ##刻度是否显示，逻辑值
  label.position = "right", ##可以使用的是left 和 right
  label.theme = element_text(size = 8, face = "italic", color = "black", angle = 0)
  
));p2
#?guide_legend  #离散型
#?guide_colorbar #连续性

#####################第二种方法
#这种没有左对齐，且顺序已经不对了
labs <- sapply(df.long$Species, 
               function(x) {
                 parse(text = paste0("italic('",x,"')"))
               })
ggplot(df.long, aes(x=group, y=abundance, fill=Species)) + 
  geom_bar(stat = "identity", width=bar.width, col='black')  + 
  xlab("Samples")+ylab("Abundance")+scale_fill_manual(values = getPalette(colourCount))+
  scale_fill_discrete(labels = labs)


###################################横轴标签变成斜体参考
library(ggplot2)
tmp.data <- data.frame(bactnames=c("Staphylococcaceae","Moraxella","Streptococcus","Acinetobacter"),OTUname=c("OTU_1","OTU_2","OTU_3","OTU_4"),value=c(-0.5,0.5,2,3))

tmp.data$bactnames2 <- paste0(tmp.data$bactnames," (",tmp.data$OTUname,")")
tmp.data$finalnames <- factor(tmp.data$bactnames2,levels=tmp.data$bactnames2[order(tmp.data$value)],ordered=TRUE)
ggplot(tmp.data, aes(finalnames,value)) + geom_bar(stat="identity") + coord_flip()

labs <- sapply(strsplit(as.character(tmp.data$finalnames), " "), 
               function(x) {
                 parse(text = paste0("italic('", x[1], "')~", x[2]))
               })

ggplot(tmp.data, aes(finalnames,value)) + geom_bar(stat="identity") + 
  coord_flip() +
  scale_x_discrete(labels = labs)











