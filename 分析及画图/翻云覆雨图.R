##2018.11.13
##翻云覆雨图
https://mp.weixin.qq.com/s?__biz=MzI5NjUyNzkxMg==&mid=2247486539&idx=1&sn=97ff6a0d5fe2daa2151ebae218c92fe1&chksm=ec43bd0cdb34341ae5a58cc575a111084e3bcbb4c54379ce58bad69efb0e9fe688e9c7571ad1&mpshare=1&scene=1&srcid=1110DRtY9XwPv6AxRKC9pqI0&pass_ticket=d1Pi8pPOrgdYesDy8Ybu9QvmkFE%2Fo90Unz0jEGptuPmQY6XegSYGPWSJ4dnxEX9E#rd


rm(list=ls())

#violin + boxplot + raw data
require(ggplot2)
ggplot(iris, aes(Species, Petal.Length, fill=Species))  +
  geom_violin(alpha=.5) +
  geom_boxplot(width=.1) +
  geom_jitter()  ##散点图

#violin + mean+-sd + raw data
require(dplyr)
d <- group_by(iris, Species) %>%
  summarize(mean = mean(Petal.Length),
            sd = sd(Petal.Length))

ggplot(iris, aes(Species, Petal.Length, fill=Species))  +
  geom_violin(alpha=.5) +
  geom_jitter() +
  geom_pointrange(aes(y=mean, ymin=mean-sd, ymax=mean+sd, color=Species), data=d, size=2)


#云雨图1：加上均值和标准误差。
## devtools::install_github("GuangchuangYu/gglayer")
require(gglayer)

ggplot(iris, aes(Species, Petal.Length, fill=Species))  +
  geom_flat_violin(position=position_nudge(x=.2)) +
  geom_jitter(aes(color=Species), width=.15) +
  geom_pointrange(aes(y=mean, ymin=mean-sd, ymax=mean+sd),
                  data=d, size=1, position=position_nudge(x=.25)) +
  coord_flip() + theme_bw() +
  theme(legend.position="bottom")

?geom_flat_violin
?coord_flip  ##x,y轴互换

#云雨图2：加上boxplot
ggplot(iris, aes(Species, Petal.Length, fill=Species))  +
  geom_flat_violin(position=position_nudge(x=.3)) +
  geom_jitter(aes(color=Species), width=.15) +
  geom_boxplot(width=.1, position=position_nudge(x=.22)) +
  coord_flip() + theme_bw() +
  theme(legend.position="bottom")

# 云雨图3：用堆叠的点图当雨点
ggplot(iris, aes(Species, Petal.Length, fill=Species))  +
geom_flat_violin(position=position_nudge(x=.2)) +
  geom_dotplot(binaxis="y", stackdir="down", dotsize=.35) +
  geom_boxplot(width=.1, position=position_nudge(x=.1)) +
  coord_flip() + theme_bw() +
  theme(legend.position="bottom")

?geom_dotplot


##github
#https://github.com/RainCloudPlots/RainCloudPlots

##PeerJ paper
#https://peerj.com/preprints/27137v1.pdf

######################################

##2020.1.16 更新

##https://mp.weixin.qq.com/s/NGkVxHBM3DmFN17ckzdhZg

##翻）云（覆）雨图
#gghalves，这个包实现了一半的小提琴图、箱式图和点图：

# geom_half_boxplot
# geom_half_violin
# geom_half_point

#geom_flat_violin也将从gglayer包中删掉

#install.packages("gghalves")

ggplot(iris, aes(Species, Petal.Length, fill=Species)) +
  geom_half_boxplot(width=.3) +
  geom_half_point(aes(color=Species))


# 现在流行的不单单是画boxplot，有以下几种方式是显得比较专业的：
# 
# boxplot + raw data
# violin plot + boxplot
# violin plot + raw data
# violin plot + boxplot + raw data


### violin + boxplot + raw data
# 
# require(ggplot2)
# ggplot(iris, aes(Species, Petal.Length, fill=Species))  +
#   geom_violin(alpha=.5) +
#   geom_boxplot(width=.1) +
#   geom_jitter()

### violin + mean+-sd + raw data
#统计量不一定要用boxplot来展示四分位数，你也可以是均值+-标准误差的方式，这样相当于你把barplot的信息也放进去了
require(dplyr)
d <- group_by(iris, Species) %>%
  summarize(mean = mean(Petal.Length),
            sd = sd(Petal.Length))

ggplot(iris, aes(Species, Petal.Length, fill=Species))  +
  geom_violin(alpha=.5) +
  geom_jitter() +
  geom_pointrange(aes(y=mean, ymin=mean-sd, ymax=mean+sd, color=Species), data=d, size=2)



#云雨图1：加上均值和标准误差

library(gghalves)

ggplot(iris, aes(Species, Petal.Length, fill=Species))  +
  geom_half_violin(position=position_nudge(x=.2), side=2) +
  geom_jitter(aes(color=Species), width=.15) +
  geom_pointrange(aes(y=mean, ymin=mean-sd, ymax=mean+sd),
                  data=d, size=1, position=position_nudge(x=.25)) +
  coord_flip() + theme_bw() +
  theme(legend.position="bottom")

#云雨图2：加上boxplot

ggplot(iris, aes(Species, Petal.Length, fill=Species))  +
  geom_half_violin(position=position_nudge(x=.3), side=2) +
  geom_jitter(aes(color=Species), width=.15) +
  geom_boxplot(width=.1, position=position_nudge(x=.22)) +
  coord_flip() + theme_bw() +
  theme(legend.position="bottom")


#云雨图3：用堆叠的点图当雨点

ggplot(iris, aes(Species, Petal.Length, fill=Species))  +
  geom_half_violin(position=position_nudge(x=.2), side=2) +
  geom_dotplot(binaxis="y", stackdir="down", dotsize=.35) +
  geom_boxplot(width=.1, position=position_nudge(x=.1)) +
  coord_flip() + theme_bw() +
  theme(legend.position="bottom")
