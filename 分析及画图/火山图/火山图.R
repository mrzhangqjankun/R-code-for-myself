##li 2017.7.5
##http://www.omicshare.com/forum/thread-145-1-1.html
##[R语言] R语言ggplot2画图教程之——火山图

rm(list=ls(all=TRUE))
setwd('D:/文件存放/galaxy pipeline/galaxy/自己整理代码/火山图')
library(ggplot2)

data =read.table("R0-vs-R3.isoforms.filter.tsv",header=T,row.names=1)
# 画图
r03 = ggplot(data,aes(log2FC,-1*log10(FDR)))
r03 + geom_point()

# 如何改变点的颜色
r03 + geom_point(color="red")
r03 +geom_point(aes(color="red"))
r03 + geom_point(aes(color=significant))

# 设置坐标轴范围和标题
# xlim()，ylim()函数，labs(title=“..”,x=“..”,y=“..”)函数
r03xy = r03 +geom_point(aes(color=significant)) + xlim(-4,4) + ylim(0,30)
r03xy + labs(title="Volcanoplot",x="log2(FC)")
r03xy + labs(title="Volcanoplot",x=expression(log[2](FC)), y=expression(-log[10](FDR)))

# 自定义颜色
r03xyp = r03xy + labs(title="Volcanoplot",x=expression(log[2](FC)), y=expression(-log[10](FDR)))
r03xyp + scale_color_manual(values =c("green","black", "red"))
volcano = r03xyp +scale_color_manual(values = c("#00ba38","#619cff","#f8766d"))

# 添加阈值线
volcano+geom_hline(yintercept=1.3)+geom_vline(xintercept=c(-1,1))
volcano+geom_hline(yintercept=1.3,linetype=4)+geom_vline(xintercept=c(-1,1),linetype=4)

# 保存图片
ggsave("volcano.png")
ggsave("volcano8.png",volcano,width=8,height=8)
