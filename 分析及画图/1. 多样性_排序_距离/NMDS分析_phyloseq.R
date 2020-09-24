#2019.8.26

##NMDS非度量多维尺度分析—基于微生物群落 

##https://mp.weixin.qq.com/s/OdDHOnbHqz06f_9iVKCdnQ

#清空内存
rm(list=ls())

##设定出图模板


Mytheme <- theme_bw() +
  #scale_fill_brewer(palette = "YIOrRd", guide = guide_legend(title = NULL), limits = c("CK1","CK3","CK5","CK7","CK9","CK11","CK13","CK15","CK17","CK19"))+
  theme(
    
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    
    plot.title = element_text(vjust = -8.5,hjust = 0.1),
    axis.title.y =element_text(size = 24,face = "bold",colour = "black"),
    axis.title.x =element_text(size = 24,face = "bold",colour = "black"),
    axis.text = element_text(size = 20,face = "bold"),
    axis.text.x = element_text(colour = "black",size = 14),
    axis.text.y = element_text(colour = "black",size = 14),
    legend.text = element_text(size = 15,face = "bold")
    #legend.position = "none"#是否删除图例
    
  ) 

#设定路径
path = getwd()

# 导入包
library(phyloseq)  ##R 3.6 不支持这个包
library(ggplot2)
suppressMessages(library(vegan))

# 使用示例数据，注意是phyloseq封装好的
data("GlobalPatterns")
ps = GlobalPatterns


vegan_otu <-  function(physeq){
  OTU <-  otu_table(physeq)
  if(taxa_are_rows(OTU)){
    OTU <-  t(OTU)
  }
  return(as(OTU,"matrix"))
}
x = as.data.frame(t(vegan_otu(GlobalPatterns)))
head(x)
x = as.matrix(x)

x = t(t(x)/colSums(x,na=T))* 100 # normalization to total 100
head(x)

##bray
	bray.mds<-metaMDS(t(x), distance="bray", k=2, trymax=100) #maximum numbers of random starts in search of stable solution
	bray.mds

##jackard
	x = decostand(x,"pa")
	jaccard.mds<-metaMDS(t(x), distance="jaccard", k=2, trymax=100)
  jaccard.mds

str(bray.mds)  #structure
# ##  输出坐标
bray_axis = bray.mds$points 
jaccard_axis = jaccard.mds$point
# 
# write.table(outbray,file = "bray_NMDS.txt",sep="\t",col.names=NA)
# write.table(outjaccard,file = "jaccard_NMDS.txt",sep="\t",col.names=NA)

# 读入实验设计和Alpha多样性值
design = as.data.frame(sample_data(ps))
head(design)

########outbray出图坐标准备
outbray = as.data.frame(bray_axis)
index = merge(outbray,design, by="row.names",all=F)
head(index)
dim(index)

stress = paste("bray ","stress: ",round(bray.mds$stress,3), sep = "")
stress 
mi = c("#FFF5EB" ,"#FEE6CE" ,"#FDD0A2", "#FDAE6B", "#FD8D3C", "#F16913", "#D94801", "#A63603", "#7F2704","black")
# mi=c("#1B9E77" ,"#D95F02", "#7570B3","#E7298A")
p <-ggplot(index, aes(x=MDS1, y=MDS2, fill = SampleType)) +
  geom_point(alpha=.7, size=5, pch = 21) +
  labs(x=paste("NMDS1",sep=""),
       y=paste("NMDS2" ,sep=""),
       title=stress)+
  #stat_ellipse( linetype = 2,level = 0.65,aes(group  =group, colour =  group))+
  #stat_ellipse( linetype = 1,level = 0.8)+
  #geom_text_repel(aes(label=points$id),size=4)+
  scale_fill_manual(values = mi)+
  #labs(title = "toamto hea and dis")+
  guides(color=guide_legend(title = NULL),shape=guide_legend(title = NULL))+
  #scale_y_continuous(expand = c(0,0))+
  geom_hline(aes(yintercept=0), colour="black", linetype=2) +
  geom_vline(aes(xintercept=max(index$MDS2/2)), colour="black", linetype="dashed")
p
# points$id=row.names(points)
# p+geom_text(aes(label=points$id),size=4)#?stat_ellipse
p = p + Mytheme
p

plot_name  = paste(path,"/a3_NMDS.pdf",sep = "")
ggsave(plot_name, p, width = 8, height = 6)

