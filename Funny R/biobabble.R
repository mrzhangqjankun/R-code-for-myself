##2018.9.10
#https://mp.weixin.qq.com/s?__biz=MzI5NjUyNzkxMg==&mid=2247486332&idx=1&sn=455a6939dd80a9416d9e2cf779e9fe21&chksm=ec43ba3bdb34332db0da60a6f5881a2e70d8c45881ed85d673fa46f96bccbc44dddb5ddd8674&scene=0#rd

##biobabble


# library(devtools)
# devtools::install_github("GuangchuangYu/biobabble")

data(lecture, package="biobabble")
head(lecture)


??biobabble


library(biobabble)
data(lecture)
library(ggplot2)
ggplot(lecture,aes(x,y) ) + geom_point()+
  scale_y_reverse() + xlab(NULL) +ylab(NULL) +
  labs(title="aaa",
       subtitle="bbb") +
  theme(plot.caption = element_text(hjust=0))
