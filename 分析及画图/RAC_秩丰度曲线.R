#2020.9.19
#秩-丰度曲线
##

setwd("E:\\桌面\\陈鹏数据\\")

library(tidyverse)
library(ggthemes)
library(ggsci)
#加载数据
otutab <- read.table("ITS结果/UPARSE_otu_table.txt",sep="\t",header=T,row.names = 1)


######1.https://www.jianshu.com/p/e00808488363
# 定义rank函数（需要排除为0的OTU）
myrank <- function(x){
  res <- length(x)-rank(x, ties.method = "first")+1
  res[x==0] <- NA
  return(res)
}
# 生成rank矩阵
otutab_rank <- map_dfr(otutab, myrank)
rownames(otutab_rank) <- rownames(otutab)
# 重塑数据
otutab2 <- otutab %>% rownames_to_column("FeatureID") %>% gather("Group", "Count", -FeatureID)
otutab_rank2 <- otutab_rank %>% rownames_to_column("FeatureID") %>%
  gather("Group", "Rank", -FeatureID)
# 合并otutab数据和ranks数据
data <- left_join(otutab2, otutab_rank2) %>% group_by(Group) %>%
  mutate(Abundance = Count/sum(Count)) %>% na.omit()
# 绘制 Rank-Abundance，注意y轴数据进行对数变化
ggplot(data, aes(x = Rank, y = Abundance, color = Group)) + geom_line() +
  scale_y_log10() +
  scale_color_jco(name = NULL) +
  labs(title="Rank-Abundance Curves") + ylab("% Abundance")+
  theme_bw() +
  theme(panel.grid=element_blank(), plot.title = element_text(lineheight=2.5, face="bold",hjust=0.5))


################
##2. 小白鱼
#https://mp.weixin.qq.com/s?__biz=MzIxNzc1Mzk3NQ==&mid=2247484157&idx=1&sn=eb5240296e18871ef66367aad8ac2273&chksm=97f5b2e5a0823bf309dc68157a9db924b7fff27c3c0d88c55949ca35720e67fcdb5816b97f30&token=66128012&lang=zh_CN&scene=21#wechat_redirect
#统计，BiodiversityR 包 rankabundance() 实现 OTU 排序
#虽然自己排序也很简单，但还是导个包统计省事点……
library(BiodiversityR)
?rankabuncomp

otu = t(otutab)
otu_relative <- otu / rowSums(otu) #转化为相对丰度
rank_dat <- data.frame()
for (i in rownames(otu_relative)) {
  rank_dat_i <- data.frame(rankabundance(subset(otu_relative, rownames(otu_relative) == i), digits = 6))[1:2]
  rank_dat_i$sample <- i
  rank_dat <- rbind(rank_dat, rank_dat_i)
  
}
rank_dat <- subset(rank_dat, abundance != 0)

write.table(rank_dat,file = "ITS结果/Rank-abundance curve.txt",sep='\t',col.names = NA)



#ggplot2 作图，更好的可视化效果，还请自行修改 ggplot2 作图细节
library(ggplot2)

ggplot(rank_dat, aes(rank, log(abundance, 10), color = sample)) +
  geom_line() +
  #scale_colour_manual(limits = c('a1', 'a2', 'a3', 'a4', 'a5', 'a6'), values = c('orange', 'purple', 'green', 'blue', 'red', 'gray40')) +
  labs(x = 'OTUs rank', y = 'Relative adundance (%)', color = NULL) +
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = 'transparent', color = 'black'), legend.key = element_rect(fill = 'transparent')) +
  scale_y_continuous(breaks = 0:-5, labels = c('100', '10', '1', '0.1', '0.01', '0.001'), limits = c(-5, 0))


#事实上，BiodiversityR包也自带了一些函数，如rankabunplot()、rankabuncomp()等，可以自动绘制Rank-abundance曲线。不过貌似效果比较一般。
#BiodiversityR 包自带的作图函数
#例如
rankabuncomp(otu, y = data.frame(name = rownames(otu)), type = 'l', factor = 'name', scale = 'logabun', legend = FALSE)
