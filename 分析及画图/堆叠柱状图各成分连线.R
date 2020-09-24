##堆叠柱状图各成分连线
##2018-02-06 朱微金 李陈浩 宏基因组
##https://mp.weixin.qq.com/s?__biz=MzUzMjA4Njc1MA==&mid=2247484986&idx=1&sn=5885daab9fd0fd74871e9d22f9043e99&chksm=fab9ec8bcdce659d618179a28b2f4bb45488e327c0fe5cd10e80ed6155855bc0f09fd34f586a&scene=0#rd

# 安装和加载tidyverse包
install.packages("tidyverse")
library(tidyverse)
?tidyverse

#devtools::install_github("hadley/tidyverse")
#library(tidyverse)

# 生成测试数据
df=data.frame(
  Phylum=c("Ruminococcaceae","Bacteroidaceae","Eubacteriaceae","Lachnospiraceae","Porphyromonadaceae"),
  GroupA=c(37.7397,31.34317,222.08827,5.08956,3.7393),
  GroupB=c(113.2191,94.02951,66.26481,15.26868,11.2179)
)

df

# 计算连线起始点Y轴坐标，即累计丰度的值

#http://blog.csdn.net/flyfrommath/article/details/79013582
#R中的管道操作。
#df %>% arrange(by=desc(Phylum)) 等价于arrange(df,by=desc(Phylum))。即%>%左边的值作为右边表达式函数的第一个参数。
#arrange是plyr包的一个函数。arrange(df,var1,var2,...)按照列给数据框排序。df为数据框，var是变量.desc(Phylum)降序排列。
?arrange
#mutate是dplyr包的一个函数，可以增加新的列。这里是更新原来的列。cumsum累计求和
?mutate
#把下面三行分解开即为：
# a = df %>%  arrange(by=desc(Phylum));a
# b = a %>% mutate(GroupA=cumsum(GroupA), GroupB=cumsum(GroupB)) ;b

link_dat <- df %>% 
  arrange(by=desc(Phylum)) %>% 
  mutate(GroupA=cumsum(GroupA), GroupB=cumsum(GroupB)) 

# 数据格式转换，宽表格转换为ggplot2使用的长表格
df.long <- df %>% gather(group, abundance, -Phylum)
df.long

?gather
## 或者使用reshape2的melt函数
## df.long <- reshape2::melt(df, value.name='abundance', variable.name='group')

# 绘图，堆叠柱状图+组间连线
#geom_segment添加直线
p <- ggplot(df.long, aes(x=group, y=abundance, fill=Phylum));p
p <-  p+ geom_bar(stat = "identity", width=0.5, col='black');p
p <- p+  geom_segment(data=link_dat, aes(x=1.25, xend=1.75, y=GroupA, yend=GroupB));p

###############################################三组
# 画三个组间比较
library(reshape2)

# 读生一个测试数据宽表格
df=data.frame(
  Phylum=c("Ruminococcaceae","Bacteroidaceae","Eubacteriaceae","Lachnospiraceae","Porphyromonadaceae"),
  GroupA=c(37.7397,31.34317,222.08827,5.08956,3.7393),
  GroupB=c(113.2191,94.02951,66.26481,15.26868,11.2179),
  GroupC=c(123.2191,94.02951,46.26481,35.26868,1.2179)
)

# melt转换为长表格为ggplot2绘图通用格式
# geom_segment添加直线和曲线，arrange按门水平名称字母降序排列；cumsum先将数值累计，再用mutate取代；现在己有两组间的高度位置，再设置X轴位置1.25, 1.75, 和Y位置
ggplot(melt(df), aes(x=variable, y=value, fill=Phylum)) + 
  geom_bar(stat = "identity", width=0.5, col='black')  + theme_classic()+
  geom_segment(data=df %>% arrange(by=desc(Phylum)) %>% mutate(GroupA=cumsum(GroupA)) %>% mutate(GroupB=cumsum(GroupB)), aes(x=1.25, xend=1.75, y=GroupA, yend=GroupB))+ 
  geom_segment(data=df %>% arrange(by=desc(Phylum)) %>% mutate(GroupB=cumsum(GroupB)) %>% mutate(GroupC=cumsum(GroupC)), aes(x=2.25, xend=2.75, y=GroupB, yend=GroupC))
# 添加theme_classic()修改主题样式，这个经典主题我更喜欢
# x和xend分别为起始和终止，1，2组间X值起始分别为1.25和1.75，2，3组间则为2.25和2.75

################################################三组及以上
# 三组或更多组的画法，只需添加数据即可
library(tidyverse)

df <- data.frame(
  Phylum=c("Ruminococcaceae","Bacteroidaceae","Eubacteriaceae","Lachnospiraceae","Porphyromonadaceae"),
  GroupA=c(37.7397,31.34317,222.08827,5.08956,3.7393),
  GroupB=c(113.2191,94.02951,66.26481,15.26868,11.2179),
  GroupC=c(123.2191,94.02951,46.26481,35.26868,1.2179),
  GroupD=c(37.7397,31.34317,222.08827,5.08956,3.7393)
)
df

df.long <- df %>% gather(group, abundance, -Phylum)
df.long

## 组间连线数据：
## 假设第一列是Phylum
link_dat <- df %>% 
  arrange(by=desc(Phylum)) %>% 
  mutate_if(is.numeric, cumsum) ;link_dat
bar.width <- 0.7
link_dat <- link_dat[, c(1,2,rep(3:(ncol(link_dat)-1),each=2), ncol(link_dat))];link_dat
link_dat <- data.frame(y=t(matrix(t(link_dat[,-1]), nrow=2)));link_dat
link_dat$x.1 <- 1:(ncol(df)-2)+bar.width/2;link_dat$x.1
link_dat$x.2 <- 1:(ncol(df)-2)+(1-bar.width/2);link_dat$x.2

ggplot(df.long, aes(x=group, y=abundance, fill=Phylum)) + 
  geom_bar(stat = "identity", width=bar.width, col='black')  + 
  geom_segment(data=link_dat,
               aes(x=x.1, xend=x.2, y=y.1, yend=y.2), inherit.aes = F)

####此图比较适合展示时间序列、梯度变化有规律的连续组。
####因为只能连接相临的组，需要大家想好谁与谁比较很重要。对于需要全部两两比较是无法实现的。

##2018.12.17
#分类堆叠柱状图顺序排列及其添加合适条块标签 
https://mp.weixin.qq.com/s?__biz=MzUzMjYyMDE2OQ==&mid=2247484273&idx=1&sn=be46f67bba36dbb18aed658866404445&chksm=fab13597cdc6bc81654843d6d6c0a2760f26b17fca5cbacb796ea2f9c4d3d8b63aeba2ed1d89&mpshare=1&scene=1&srcid=1217NljIB6S4iIvHptT1QuaK&pass_ticket=gm1wJVkQVFHm73S6JXr2zI0WqrIap0TM1KzKUvgEk0xrYUJ2jqyvev8kEieOtB%2Bh#rd






























