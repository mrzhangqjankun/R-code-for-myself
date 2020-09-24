##li 2017.7.5 
##http://www.omicshare.com/forum/thread-2457-1-1.html
##[R语言] 如何绘制精美的PCoA图形？

##选择特定的相似性距离并计算距离矩阵。距离的选择可以有Bray-curits、Unifrac等，
##不同的距离有不同的作用和意义。
##相似性距离可以利用R的GUniFrac和vegan等包计算，也可以利用QIIME计算。
##进行PCoA分析，也就是利用表征分析选择最能表示样本距离的坐标轴。
##这个可以利用R的ape包的pcoa（）命令完成。PCoA图形展示。
##图形可以用ordiplot（）命令展示，但如果需要比较美观的图形，建议用ggplot来画。

rm(list=ls(all=TRUE))
setwd('D:/文件存放/galaxy pipeline/galaxy/自己整理代码/pCoA')


library(GUniFrac) #用于计算Unifrac距离
library(ape) # 用于pcoa分析
library(ggplot2) #用于画图

##读文件
Otu_tab <- read.table("otu_table",row.names=1,header=T,sep="\t",check.names=FALSE)
Tree    <- read.tree("Muscle.align.tree.nhx") #输入OTU表格
Otu_tab <- as.data.frame(t(Otu_tab))
Otu_tab_rff <- Rarefy(Otu_tab)$otu.tab.rff
unifracs <- GUniFrac(Otu_tab_rff,Tree,alpha=c(0, 0.5, 1))
du <- unifracs$unifracs[, , "d_UW"] # 计算Unweighted UniFrac距离
Group <- c('A','B','C')#按照目的输入样本
shape <- c("A" =16,"B" =17,"C" =16)#定义点形状
color <- c("A" ='#CCFF33',"B" ='#CCFF33',"C" ='#CCFF33')#定义点颜色
PCOA <- pcoa(du, correction="none", rn=NULL)#利用PCOA()指令做pcoa分析
result <-PCOA$values[,"Relative_eig"]
pro1 = as.numeric(sprintf("%.3f",result[1]))*100
pro2 = as.numeric(sprintf("%.3f",result[2]))*100
x = PCOA$vectors
sample_names = rownames(x)
pc = as.data.frame(PCOA$vectors)
pc$names = sample_names
legend_title = ""
group = Group
pc$group = group
xlab=paste("PCOA1(",pro1,"%)",sep="") 
ylab=paste("PCOA2(",pro2,"%)",sep="")
pca=ggplot(pc,aes(Axis.1,Axis.2)) + #用ggplot作图
  geom_point(size=3,aes(color=group,shape=group)) + 
  #  geom_text(aes(label=names),size=4,vjust=-1) +
  labs(x=xlab,y=ylab,title="PCOA",color=legend_title,shape=legend_title) + 
  geom_hline(yintercept=0,linetype=4,color="grey") + 
  geom_vline(xintercept=0,linetype=4,color="grey") + 
  scale_shape_manual(values=shape) +
  scale_color_manual(values=color) +
  theme_bw()