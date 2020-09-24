##li 2017.7.24 learn
##https://mp.weixin.qq.com/s?__biz=MzI5MTcwNjA4NQ%3D%3D&mid=2247483964&idx=1&sn=ee52ac37fb9a919f5c75c0abe2a49ad4&scene=45&pass_ticket=U83SgT2FQElCc3sFtKaoMvJOlF5ylIAB4ZDNKyXRhq5pkqnxcj%2FDiyUofx8BFmSE
##https://mp.weixin.qq.com/s?__biz=MzI5MTcwNjA4NQ%3D%3D&mid=2247483971&idx=1&sn=1b40a1137ccb8b2fa1ab3eb1d0f05de9&scene=45#wechat_redirect


##假设有这么一个基因表达矩阵，第一列为基因名字，后面几列为样品名字，想绘制下样品中基因表达的整体分布。
profile="Name;2cell_1;2cell_2;2cell_3;4cell_1;4cell_2;4cell_3;zygote_1;zygote_2;zygote_3
A;4;6;7;3.2;5.2;5.6;2;4;3
B;6;8;9;5.2;7.2;7.6;4;6;5
C;8;10;11;7.2;9.2;9.6;6;8;7
D;10;12;13;9.2;11.2;11.6;8;10;9
E;12;14;15;11.2;13.2;13.6;10;12;11
F;14;16;17;13.2;15.2;15.6;12;14;13
G;15;17;18;14.2;16.2;16.6;13;15;14
H;16;18;19;15.2;17.2;17.6;14;16;15
I;17;19;20;16.2;18.2;18.6;15;17;16
J;18;20;21;17.2;19.2;19.6;16;18;17
L;19;21;22;18.2;20.2;20.6;17;19;18
M;20;22;23;19.2;21.2;21.6;18;20;19
N;21;23;24;20.2;22.2;22.6;19;21;20
O;22;24;25;21.2;23.2;23.6;20;22;21"

profile_text <- read.table(text=profile, header=T, row.names=1, quote="",sep=";", check.names=F)
# 在melt时保留位置信息
# melt格式是ggplot2画图最喜欢的格式
# 好好体会下这个格式，虽然多占用了不少空间，但是确实很方便

library(ggplot2)
library(reshape2)
data_m <- melt(profile_text)
head(data_m)

# variable和value为矩阵melt后的两列的名字，内部变量, variable代表了点线的属性，value代表对应的值。
p <- ggplot(data_m, aes(x=variable, y=value),color=variable) + 
  geom_boxplot() + 
  theme(axis.text.x=element_text(angle=50,hjust=0.5, vjust=0.5)) +
  theme(legend.position="none")
p
#加颜色
# variable和value为矩阵melt后的两列的名字，内部变量, variable代表了点线的属性，value代表对应的值。
p1 <- ggplot(data_m, aes(x=variable, y=value),color=variable) + 
  geom_boxplot(aes(fill=factor(variable))) + 
  theme(axis.text.x=element_text(angle=50,hjust=0.5, vjust=0.5)) +
  theme(legend.position="none")
p1


#Violin plot
p2 <- ggplot(data_m, aes(x=variable, y=value),color=variable) + 
  geom_violin(aes(fill=factor(variable))) +      ##geom_violin 这里和上面比变化了
  theme(axis.text.x=element_text(angle=50,hjust=0.5, vjust=0.5)) +
  theme(legend.position="none")
p2

#Jitter plot (这里使用的是ggbeeswarm包)  ##抖动图

#install.packages("ggbeeswarm")
library(ggbeeswarm)
# 为了更好的效果，只保留其中一个样品的数据
# grepl类似于Linux的grep命令，获取特定模式的字符串

data_m2 <- data_m[grepl("_3", data_m$variable),]

# variable和value为矩阵melt后的两列的名字，内部变量, variable代表了点线的属性，value代表对应的值。
p3 <- ggplot(data_m2, aes(x=variable, y=value),color=variable) + 
  geom_quasirandom(aes(colour=factor(variable))) + 
  theme_bw() + theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), legend.key=element_blank()) +
  theme(legend.position="none")
# 也可以用geom_jitter(aes(colour=factor(variable)))代替geom_quasirandom(aes(colour=factor(variable)))
# 但个人认为geom_quasirandom给出的结果更有特色
p3   ##报错了
##ggsave(p, filename="jitterplot.pdf", width=14, height=8, units=c("cm"))








