##ggplot-diversity
library(ggplot2)
plot_theme = theme(panel.background=element_blank(),
                   panel.grid=element_blank(),
                   axis.line.x=element_line(size=.5, colour="black"),
                   axis.line.y=element_line(size=.5, colour="black"),
                   axis.ticks=element_line(color="black"),
                   axis.text=element_text(color="black", size=7),
                   legend.position="right",
                   legend.background=element_blank(),
                   legend.key=element_blank(),
                   legend.text= element_text(size=7),
                   text=element_text(family="sans", size=7)
) + theme_bw()


############################################################################ PCA
rm(list=ls())
setwd("E:/桌面/test.data/")
suppressMessages(library(vegan))
library(ggplot2)
x = read.table(file="Comnine-Galaxy296-resample_uparse_otu_ALS.txt",sep="\t",header=T,row.names=1)
group = read.table("group2.txt", sep="\t", row.names=1 )
options(warn=-1)
# setwd("E:/桌面/test.data/")
# x = read.table(file="Galaxy295-[resample_normalized_unoise_otu_ALS.tabular].txt",sep="\t",header=T,row.names=1)
# group = read.table("group_1.txt", sep="\t", row.names=1 )

x.pca = rda(x);x.pca
outputpca = summary(x.pca)
str(outputpca)

a = outputpca$species ;a
b = outputpca$cont$importance ;b

write.table(a, file="PCA_site.txt", sep="\t", col.names = NA)
write.table(b,file="PCA_evale.txt",sep="\t ",col.names = NA)
##
sink("PCA.txt")
outputpca
sink()

##plot
pc1 = round(b[2,1],2);pc1
pc2 = round(b[2,2],2);pc2

?plot.cca
plot(x.pca,dis="sp",type="text",xlab=paste("pc1=",pc1),ylab=paste("pc2=",pc2))
points(x.pca, pch=21, col="red", bg="red", cex=0.5)
#规定图的宽和高
##win.graph(width=6, height=6, pointsize=8)  规定宽和高及点的大小
##par(mai=c(2,2,2,2))#以数值向量表示的图形边界的大小，顺序为“下，左，上，右”，单位为英寸
##par(mar=c(2,2,2,2))#以数值向量表示的图形边界的大小，顺序为“下，左，上，右”，单位为英分
##par(pin=c(3,3)) #图形的宽和高，以英寸表示

####ggplot2
sam.a=as.data.frame(a);head(sam.a);class(sam.a)
sam.a = sam.a[,1:2]

##match group and analysis results
rowgroup = rownames(group)
rowsam = rownames(sam.a)
mat = match(rowgroup,rowsam)
sam.a = sam.a[mat,];sam.a

colnames(group) = "group"
data.plot = cbind(sam.a,group)
data.plot

##Rstudio插件，图形界面画ggplot
esquisse::esquisser()
options("esquisse.display.mode" = "browser") ##打开新的浏览器. #pane : RStudio's Viewer
esquisse::esquisser()library(ggplot2)


#ggplot2
library(ggplot2)
rowname = row.names(sam.a)

p = ggplot(data.plot,aes(PC1,PC2))
p = p + geom_point(aes(colour = group,shape = group),alpha=0.5,size = 3);p
p = p + xlab(paste("PC1=",pc1*100,"%",sep=""))+ylab(paste("PC2=",pc2*100,"%",sep=""))+ labs(title = "PCA analysis") ;p
#p = p + geom_text(aes(label =rowname,colour = group),position = position_dodge(0.5),vjust=-1) ;p
p = p +  plot_theme + theme(plot.title=element_text(hjust=0.5));p
p = p + theme_bw();p
#p = p +  stat_ellipse(aes(PC1,PC2,fill=group),geom="polygon", level=0.95, alpha=0.2,linetype = 2);p  ##椭圆
#p = p +  guides(color=guide_legend("Group"),fill=guide_legend("Group"));p
p=p+theme(axis.text= element_text(size=16, color="black", family  = "serif", face= "bold", vjust=0.5, hjust=0.5))+
  theme(axis.title = element_text(size=16, color="black", family  = "serif",face= "bold", vjust=0.5, hjust=0.5))+
  theme(legend.text = element_text(colour = 'black', size = 16,  family  = "serif",face = 'bold'))+
  theme(legend.title=element_blank());p
p=p+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())
p
p = p +  stat_ellipse(aes(colour = group),level=0.95,linetype = 2);p  
#p = p + coords_fixed(1);p  ##https://mp.weixin.qq.com/s/f9X5yJAw_cA08g6Ygq9Itw

##不报warning
warnings('off') ##只是把红的变成黑色
options(warn=-1)
?options

ggsave("PCA.pdf", width = 4, height = 2.5,dpi = 600)
?ggsave
getwd()


#####################################################################DCA
rm(list=ls())
setwd("E:/桌面/test_data/")
suppressMessages(library(vegan))
x<-read.table(file="otu.tabular",sep="\t",header=T,row.names=1);x = t(x)
group = read.table("group1.txt", sep="\t", row.names=1 )

x.dca<-decorana(x)  ;x.dca

output = summary(x.dca) ;output

str(output) 
dca.sites = output$site.scores;  dca.sites

write.table(dca.sites, file="DCA_site.txt", sep="\t", col.names = NA)

##
sink("DCA.txt")
output
sink()

##plot
plot(x.dca,dis="sites",type="text")
points(x.dca, pch=21, col="red", bg="red", cex=0.5)

####ggplot2
dca.sites = dca.sites[,1:2]

##match group and analysis results
rowgroup = rownames(group)
rowsam = rownames(dca.sites)
mat = match(rowgroup,rowsam)
dca.sites = dca.sites[mat,];dca.sites

colnames(group) = "group"
data.plot = cbind(dca.sites,group)
data.plot
#ggplot2
library(ggplot2)
rowname = row.names(dca.sites)

p = ggplot(data.plot,aes(DCA1,DCA2))
p = p + geom_point(aes(colour = group,shape = group),alpha=0.5,size = 3);p
p = p + xlab("DCA1")+ylab("DCA2")+ labs(title = "DCA analysis") ;p
#p = p + geom_text(aes(label =rowname,colour = group),position = position_dodge(0.5),vjust=-1) ;p
p = p +  plot_theme + theme(plot.title=element_text(hjust=0.5));p
#p = p +  stat_ellipse(aes(DCA1,DCA2,fill=group),geom="polygon", level=0.95, alpha=0.2);p  ##椭圆
#p = p +  guides(color=guide_legend("Group"),fill=guide_legend("Group"));p
p = p +  stat_ellipse(aes(colour = group),level=0.95,linetype = 2);p 

ggsave("DCA.pdf", width = 4, height = 2.5,dpi = 600)
?ggsave
getwd()

#########################################################################NMDS
rm(list=ls())
setwd("E:/桌面/test_data/")
suppressMessages(library(vegan))

x<-read.table(file="otu.tabular",sep="\t",header=T,row.names=1)
group = read.table("group1.txt", sep="\t", row.names=1 )

x = read.table(file="Comnine-Galaxy296-resample_uparse_otu_ALS.txt",sep="\t",header=T,row.names=1)
group = read.table("group2.txt", sep="\t", row.names=1 )

?metaMDS
##bray  #trymax=100,maximum numbers of random starts in search of stable solution
bray.mds<-metaMDS(t(x), distance="bray", k=2, trymax=100) ;bray.mds

##jackard
x = decostand(x,"pa")
jaccard.mds<-metaMDS(t(x), distance="jaccard", k=2, trymax=100) ;jaccard.mds

str(bray.mds)  
bray.mds$species
outbray = bray.mds$points ;outbray
outjaccard = jaccard.mds$point ;outjaccard

write.table(outbray,file = "bray_NMDS.txt",sep="\t",col.names=NA)
write.table(outjaccard,file = "jaccard_NMDS.txt",sep="\t",col.names=NA)

##plot
#1
par(mfrow=c(2,1))
plot(jaccard.mds,dis="sites",type="text",main="jaccard NMDS")
points(jaccard.mds, pch=21, col="red", bg="red", cex=0.5)

plot(bray.mds,dis="sites",type="text",main = "bray NMDS")
points(bray.mds, pch=21, col="red", bg="red", cex=0.5)

#2
split.screen(c(2,1))
screen(1)
plot(jaccard.mds,dis="sites",type="text",main="jaccard NMDS")
points(jaccard.mds, pch=21, col="red", bg="red", cex=0.5)
screen(2)
plot(bray.mds,dis="sites",type="text",main = "bray NMDS")
points(bray.mds, pch=21, col="red", bg="red", cex=0.5)

####ggplot
outbray = bray.mds$points ;outbray
outjaccard = jaccard.mds$point ;outjaccard

out.bra = outbray[,1:2]
out.jac = outjaccard[,1:2]

stress.bra = bray.mds$stress;stress.bra
stress.jaccard = jaccard.mds$stress
##合并
# out = cbind(out.bray,out.jaccard)
# class(out)
# colnames(out) = c("bra_MDS1","bra_MDS2","jac_MDS1","jac_MDS2")
# head(out)

##但是感觉还是不合并，分别画图并输出比较好。利用facet画合并的图比较困难。
##match group and analysis results
rowgroup = rownames(group)
rowsam = rownames(out.jac)  ##out.jac替换为out.bra即可
mat = match(rowgroup,rowsam)
out.jac = out.jac[mat,];out.jac

colnames(group) = "group"
data.plot = cbind(out.jac,group)
data.plot
#ggplot2
library(ggplot2)
rowname = row.names(out.jac)

p = ggplot(data.plot,aes(MDS1,MDS2))
p = p + geom_point(aes(colour = group,shape = group),alpha=0.5,size = 3);p
p = p + xlab("MDS1")+ylab("MDS2")+ labs(title = paste("NMDS\n","stress=",round(stress.bra,3),sep="\t")) ;p
#p = p + geom_text(aes(label =rowname,colour = group),position = position_dodge(0.5),vjust=-1) ;p
p = p +  plot_theme + theme(plot.title=element_text(hjust=0.5));p
#p = p +  stat_ellipse(aes(MDS1,MDS2,fill=group),geom="polygon", level=0.95, alpha=0.2);p  ##椭圆
#p = p +  guides(color=guide_legend("Group"),fill=guide_legend("Group"));p
p = p +  stat_ellipse(aes(colour = group),level=0.95,linetype = 2);p  

ggsave("out.jac.pdf", width = 4, height = 2.5,dpi = 600)
?ggsave
getwd()

#########################################################################PD_PCoA
####### PD
rm(list=ls(all=TRUE))
setwd("E:/桌面/test.data/")
setwd("E:/桌面/Go-PCR实验/5.8 哀牢山森林土样本/数据分析6.5/做了ITSx/YN")
library("picante");library("ape");library("vegan")

phytree = read.tree("Galaxy33-[FastTree.nwk].nwk");phytree  ##FastTree结果文件
summary(phytree)
otu = read.table("Galaxy20-[resample1400_normalized_128-uparse_otu.txt].txt", header=T, row.names=1, sep="\t")
group = read.table("group.txt", sep="\t", row.names=1 )

phytree = read.tree("Galaxy429-[FastTree.nwk].nwk");phytree  ##FastTree结果文件
otu = read.table(file="Comnine-2fold-Galaxy296-resample_uparse_otu_ALS.txt",sep="\t",header=T,row.names=1)
group = read.table("group2.txt", sep="\t", row.names=1 )

#构建有根树=====
is.rooted(phytree) ;?root # whether the tree is rooted. Tree from FastTree is unrooted.FLASE
phytree2 = root(phytree, 1, r=TRUE) ;is.rooted(phytree2) # pick the the farthest OTU as a root.
#resolve.root:	 a logical specifying whether to resolve the new root as a bifurcating node.

##修剪树和OTU===
#?prune.sample #对树进行修剪，仅包含otu中出现的物种。Prune a phylogenetic tree to include only species present in a otuunity data set or with non-missing trait data
phy.tree = prune.sample(t(otu), phytree2) #delete useless OTUs in the tree

#?match.phylo.otu #pruning and sorting the two kinds of data to match one another for subsequent analysis.
match.otu <- match.phylo.comm(phy.tree,t(otu)) # match the OTUs in the tree with the OTUs in the OTU table

#otu = c(1,2,3,4,5,6,7); tree = c(3,4,5,6,7,8,9)

str(match.otu)
otu = match.otu$comm;otu  ##已经转至了
phy = match.otu$phy;phy

##计算PD值====
PD = pd(otu,phy, include.root=TRUE);PD;?pd
write.table(PD, file="PD.txt",sep="\t", row.names=TRUE, col.names=NA)

#####PCoA analysis
#install.packages("GUniFrac")
library(GUniFrac) ;?GUniFrac#用于计算Unifrac距离
unifracs <- GUniFrac(otu,phy,alpha=c(0, 0.5, 1))  ;str(unifracs)

du <- unifracs$unifracs[, , "d_UW"] # 计算Unweighted UniFrac距离
dw <- unifracs$unifracs[, , "d_1"]  # # 计算weighted UniFrac距离

?pcoa  ##ape package
PCOA_unweight <- pcoa(du, correction="none", rn=NULL) #Unweighted
str(PCOA_unweight)
eig_unweight = PCOA_unweight$values
sample_unweight =  PCOA_unweight$vectors

sink("PCoA_unweight.txt")
PCOA_unweight
sink()


PCOA_weight <- pcoa(dw, correction="none", rn=NULL) #weighted
str(PCOA_unweight)
eig_weight = PCOA_weight$values
sample_weight =  PCOA_weight$vectors

sink("PCoA_weight.txt")
PCOA_unweight
sink()

#####cmdscale
?cmdscale
pcoa_unweight = cmdscale(du, k=2, eig=T) # k is dimension, ; eig is eigenvalues
#str(pcoa_unweight)
#points = pcoa_unweight$points
pcoa_weight = cmdscale(dw, k=2, eig=T) # k is dimension, ; eig is eigenvalues


##plot
par(mfrow=c(2,1))
x = as.matrix(sample_weight[,1:2])
row=row.names(x)
pc1=round(eig_weight[1,2],2);pc1
pc2=round(eig_weight[2,2],2);pc2
plot(x,main="weighted PCoA",xlab=paste("pc1=",pc1),ylab=paste("pc2=",pc2));text(x,row)
points(x, pch=21, col="red", bg="red", cex=0.5)

y = as.matrix(sample_unweight[,1:2])
row=row.names(y)
pc1=round(eig_unweight[1,2],2);pc1
pc2=round(eig_unweight[2,2],2);pc2
plot(y,main="unweighted PCoA",xlab=paste("pc1=",pc1),ylab=paste("pc2=",pc2));text(y,row)
points(y, pch=21, col="red", bg="red", cex=0.5)

#####ggplot
wei = sample_weight[,1:2]
PCoA1=round(eig_weight[1,2],2);PCoA1
PCoA2=round(eig_weight[2,2],2);PCoA2

unwei = sample_unweight[,1:2]
PCoA1=round(eig_unweight[1,2],2);PCoA1
PCoA2=round(eig_unweight[2,2],2);PCoA2
##match group and analysis results
rowgroup = rownames(group)
rowsam = rownames(unwei)  ##wei替换为unwei即可
mat = match(rowgroup,rowsam)
unwei = unwei[mat,];unwei

colnames(group) = "group"
data.plot = cbind(unwei,group)
data.plot
#ggplot2
library(ggplot2)
rowname = row.names(unwei)

p = ggplot(data.plot,aes(Axis.1,Axis.2))
p = p + geom_point(aes(colour = group,fill=group),alpha=0.5,size = 3);p #,shape = group
#p = p + geom_point(aes(colour = group,shape = group),alpha=0.5,size = 3);p 
p = p + xlab(paste("PCoA1=",PCoA1*100,"%",sep=""))+ylab(paste("PCoA2=",PCoA2*100,"%",sep=""));p  #+ labs(title = "PCoA analysis") ;p
#p = p + geom_text(aes(label =rowname,colour = group),position = position_dodge(0.5),vjust=-1) ;p
#p = p +  plot_theme + theme(plot.title=element_text(hjust=0.5));p
p = p +theme_bw();p
#p = p +  stat_ellipse(aes(Axis.1,Axis.2,fill=group),geom="polygon", level=0.95, alpha=0.2);p  ##椭圆
#p = p +  guides(color=guide_legend("Group"),fill=guide_legend("Group"));p
p=p+theme(axis.text= element_text(size=20, color="black", family  = "serif", face= "bold", vjust=0.5, hjust=0.5))+
  theme(axis.title = element_text(size=20, color="black", family  = "serif",face= "bold", vjust=0.5, hjust=0.5))+
  theme(legend.text = element_text(colour = 'black', size = 20,  family  = "serif",face = 'bold'))+
  theme(legend.title=element_blank());p
  p=p+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank());p
  

mycolor = c("#000000","#999999","#0033FF","#9900FF","#003300","#FF3300","#003333","#00FF00")

p = p +scale_colour_manual(values=mycolor)
p
#有时候你想移除图例，使用 guides()。

p = p +  stat_ellipse(aes(colour = group),level=0.95,linetype = 2);p  

ggsave("unweight.pdf", width = 4, height = 2.5,dpi = 600); ?ggsave; getwd()


##########################################################################CCA_RDA
#DCA结果中Lengths of gradient 的第一轴的大小#
#如果大于4.0,就应选CCA；如果在3.0-4.0之间，选RDA和CCA均可#
#如果小于3.0, RDA的结果要好于CCA#
rm(list=ls())
setwd("E:/桌面/test_data/")
suppressMessages(library(vegan))

otu<-read.table("otu.tabular",sep="\t",header=T,row.names=1)

env.dat<-read.table("env.txt",sep="\t",header=T,row.names=1)
env.st = decostand(env.dat, method="standardize", MARGIN=2);?decostand
#standardize: scale x to zero mean and unit variance (default MARGIN = 2).
#Margin, if default is not acceptable. 1 = rows, and 2 = columns of x.按列（样本）标准化
group = read.table("group1.txt", sep="\t", row.names=1 )

# match env and fg datasets
samp.fg = colnames(otu)
samp.env= rownames(env.st)
my.env = match(samp.fg, samp.env)
env.st2 = na.omit(env.st[my.env, ])  # omit the NA rows if without fg data
samp.env= rownames(env.st2)
my.fg = match(samp.env, samp.fg)
otu = otu[, my.fg]

# for CCA calculation
otu = t(otu)
C.whole = cca(otu, env.st2) ;C.whole ; ?cca ##rda(otu, env.st2)

# for env selection by CCA inflation factors
inf_factor = vif.cca(C.whole) ;inf_factor ; ?vif.cca #Function vif.cca and alias.cca can be used to analyse linear dependencies among constraints and conditions.

# delete varable with max inflation factor
na_env = which(is.na(inf_factor))
if(isTRUE(length(na_env) > "0") ){
  inf_factor = inf_factor[-na_env]
}
inf_factor

max_env = which(inf_factor == max(inf_factor))
env.st3 = env.st2
while ( inf_factor[max_env] > 20){
  env.st3 = env.st3[,-max_env]
  C.reduced = cca(otu, env.st3)
  inf_factor = vif.cca(C.reduced)
  max_env = which(inf_factor == max(inf_factor))
}
output2 = inf_factor ;output2
head(env.st3)

# for F and p values
ind.p = array(0,dim=c(1,ncol(env.st3)))
ind.F = array(0,dim=c(1,ncol(env.st3)))
for(j in 1:ncol(env.st3)){
  ind.cca = cca(otu, env.st3[,j]) #ind.cca = cca(otu, env.st[,j], env.st[,-j])  #
  ind.sig = anova(ind.cca,step=1000)
  ind.p[1,j] = ind.sig$Pr[1]
  ind.F[1,j] = ind.sig$F[1]
}
ind.p ;ind.F

colnames(ind.p) = colnames(env.st3)

inf_Fp=rbind(output2,ind.F,ind.p)
row.names(inf_Fp)=c("inf_factor","F","p")
inf_Fp

##重新计算CCA
C.whole = cca(otu, env.st3) ; C.whole ##rda(otu, env.st3)

x.sig = anova(C.whole) ; x.sig

str(x.sig)
x.p = x.sig$Pr[1] ;x.p
x.F = x.sig$F[1]  ;x.F


output1 = summary(C.whole)
output1
str(output1)
a=output1$sites;a  ##样本坐标
b=output1$cont$importance;b ##特征值，解释度 #eigenvals(C.whole)
c=output1$biplot;c  ##环境因子坐标

write.table(a,file="cca_site.txt",sep="\t",col.names=NA)
write.table(b,file="cca_evale.txt",sep="\t",col.names=NA)
write.table(c,file="cca_env.txt",sep="\t",col.names=NA)
write.table(inf_Fp,file="cca_inf_Fp.txt",sep="\t",col.names=NA)

##所有结果
sink("cca.txt")
output1
inf_Fp
sink()

##plot
cca1=round(b[2,1],2);cca1
cca2=round(b[2,2],2);cca2


plot(C.whole,dis=c('wa','cn'),xlab=paste("cca1=",cca1),ylab=paste("cca2=",cca2), main = paste("(F= ",round(x.F,2)," , p=",x.p,")",sep=''))
points(C.whole, pch=21, col="red", bg="red", cex=0.5)

#####ggplot
cca1=round(b[2,1],2);cca1
cca2=round(b[2,2],2);cca2
sam = a[,1:2] ;head(sam) ##样本坐标
env = c[,1:2] ;env ##环境因子坐标

##match group and analysis results
rowgroup = rownames(group)
rowsam = rownames(sam)  
mat = match(rowgroup,rowsam)
sam = sam[mat,];sam

colnames(group) = "group"
data.plot = cbind(sam,group)
data.plot
#ggplot2
library(ggplot2)
rowname = row.names(sam)

p = ggplot(data.plot,aes(CCA1,CCA2))
p = p + geom_point(aes(colour = group,shape = group),alpha=0.5,size = 3);p
p = p + xlab(paste("CCA1=",cca1*100,"%",sep=""))+ylab(paste("CCA2=",cca2*100,"%",sep=""))+ labs(title = "CCA analysis") ;p
#p = p + geom_text(aes(label =rowname,colour = group),position = position_dodge(0.5),vjust=-1) ;p
p = p +  plot_theme + theme(plot.title=element_text(hjust=0.5));p
#p = p +  stat_ellipse(aes(CCA1,CCA2,fill=group),geom="polygon", level=0.95, alpha=0.2);p  ##椭圆
#p = p +  guides(color=guide_legend("Group"),fill=guide_legend("Group"));p
p = p + geom_text(aes(label = rownames(data.plot), vjust = 1.1, hjust = -0.5))

##加环境因子
#https://mp.weixin.qq.com/s?__biz=MzAwMDY0MzQ0Ng==&mid=2247483940&idx=1&sn=dfee5a1269f5984f2d83f411b50f60f3&chksm=9ae49a3dad93132b9b8e3b89a974df8fd847266bc69f6243b01130e57a98461659c4c217c2bd&mpshare=1&scene=1&srcid=0520kcKPgA1MhipSnVleJubX&pass_ticket=a%2FDZKnr7xDnM7txpzaFEK%2BPJtwU6W9%2BkB4QMG1yU6Lht0mbLenrBKIaxi9BbMLim#rd

rowenv = rownames(env);rowenv
env = data.frame(env)  ##矩阵做不了，必须转成数据框

p = p + geom_segment(data=env,aes(x=0,y=0,xend=CCA1,yend=CCA2),arrow = arrow(length = unit(0.3,"cm")), size = 0.5) ;p
   
p = p + geom_text(data = env,aes(CCA1,CCA2,label = rowenv),size = 4,nudge_x=0,nudge_y=-0.1);p

p = p + geom_hline(aes(yintercept = 0), linetype="dashed") + geom_vline(aes(xintercept = 0) ,linetype="dashed");p
ggsave("CCA.pdf", width = 4, height = 2.5,dpi = 600); ?ggsave; getwd()

#####autoplot
library(vegan);library(ggvegan)
??ggvegan
?autoplot
autoplot(C.whole)  ##样本和OTU都会显示出来。autoplot是ggplot object ,可以在此基础上继续添加图层。
f = fortify(C.whole)
head(f)
str(f)
#####

#########################################################################alpha diversity
rm(list=ls())
setwd("E:/桌面/test.data/")
suppressMessages(library(vegan))

x<-read.table(file="Galaxy295-[resample_normalized_unoise_otu_ALS.tabular].txt",sep="\t",header=T,row.names=1)
x[is.na(x)] = 0
x = t(x)

Shannon <- diversity(x)
Inv_Simpson <- diversity(x, "inv")
S <- specnumber(x)
Pielou_evenness <- Shannon/log(S)
Simpson_evenness <- Inv_Simpson/S
Observed_richness <- colSums(t(x)>0)
report1 = cbind(Shannon, Inv_Simpson,Observed_richness, Pielou_evenness, Simpson_evenness)

#write.table(report1, outputFile, sep="\t", col.names = NA)

##########ggplot2 barplot
library(ggplot2)
group = read.table("group2.txt", sep="\t", row.names=1 )
sam=report1

rowgroup = rownames(group)
rowsam = rownames(sam)  
mat = match(rowgroup,rowsam)  ##%in%
sam = sam[mat,]#;sam

colnames(group) = "group"
data.plot = cbind(sam,group) #data.plot

library(Rmisc)
shannon_sd <- summarySE(data.plot, measurevar="Shannon", groupvars="group")
Inv_Simpson_sd <- summarySE(data.plot, measurevar="Inv_Simpson", groupvars="group")
Observed_richness_sd <- summarySE(data.plot, measurevar="Observed_richness", groupvars="group")
Pielou_evenness_sd <- summarySE(data.plot, measurevar="Pielou_evenness", groupvars="group")

##散点图
# ggplot(shannon_SE, aes(x=group, y=Shannon, colour=group)) + 
#   geom_errorbar(aes(ymin=Shannon-sd, ymax=Shannon+sd), width=.1) + 
#   geom_line() + geom_point(size = 3) 

##柱状图
p1 = ggplot(shannon_sd, aes(x=group, y=Shannon, fill=group)) + 
  geom_bar(position=position_dodge(), stat="identity",size=0.3) + 
  geom_errorbar(aes(ymin=Shannon-sd, ymax=Shannon+sd), width=.2, size =.3, position=position_dodge(.9)) +
   plot_theme+ theme(legend.position="None",axis.title.x = element_blank())

p2 = ggplot(Inv_Simpson_sd, aes(x=group, y=Inv_Simpson, fill=group)) + 
  geom_bar(position=position_dodge(), stat="identity",size=0.3) + 
  geom_errorbar(aes(ymin=Inv_Simpson-sd, ymax=Inv_Simpson+sd), width=.2, size =.3, position=position_dodge(.9)) +
  plot_theme+ theme(legend.position="None",axis.title.x = element_blank())

p3 = ggplot(Observed_richness_sd, aes(x=group, y=Observed_richness, fill=group)) + 
  geom_bar(position=position_dodge(), stat="identity",size=0.3) + 
  geom_errorbar(aes(ymin=Observed_richness-sd, ymax=Observed_richness+sd), width=.2, size =.3, position=position_dodge(.9)) +
  plot_theme+ theme(legend.position="None",axis.title.x = element_blank())

p4 = ggplot(Pielou_evenness_sd, aes(x=group, y=Pielou_evenness, fill=group)) + 
  geom_bar(position=position_dodge(), stat="identity",size=0.3) + 
  geom_errorbar(aes(ymin=Pielou_evenness-sd, ymax=Pielou_evenness+sd), width=.2, size =.3, position=position_dodge(.9)) +
  plot_theme+ theme(legend.position="None",axis.title.x = element_blank())
#1.分面
library(gridExtra)
grid.arrange(p1, p2, p3, p4)

##2.用par设置ggplot2参数？这个可以有！ 
##https://mp.weixin.qq.com/s?__biz=MzI5NjUyNzkxMg==&mid=2247485566&idx=1&sn=5cb3c3f04163a2665d960a84d9955e12&scene=21#wechat_redirect
#devtools::install_github("GuangchuangYu/yyplot")
library(yyplot)
library(ggplot2)
?ggpar

ggpar(mar=rep(6,4))
d=data.frame(x=rnorm(10), y=rnorm(10))
plot(d$x, d$y)
X11()   ####打开一个新的画图窗口
ggplot(d, aes(x,y)) + geom_point() 
theme_set(theme_minimal())
X11()
ggpar(mar=rep(1,4))
ggplot(d, aes(x,y)) + geom_point() 
##结果并没有合到一张图。试了好久也不行。



##【r<-绘图|ggplot2】绘制均值的误差棒
##https://www.jianshu.com/p/003138ac593b

##ggplot2学习笔记系列之利用ggplot2绘制误差棒及显著性标记 
##https://mp.weixin.qq.com/s?__biz=MzA3MTM3NTA5Ng==&mid=2651057637&idx=1&sn=f69b192e01ebca087b6556f83a9ca5a3&chksm=84d9cc72b3ae4564b014dadd94c141e36fdf301ba14e6ecfb2ed0db0705c7220717fc6cdc71a&scene=21#wechat_redirect

##R语言可视化学习笔记之添加p-value和显著性标记 
##https://mp.weixin.qq.com/s?__biz=MzA3MTM3NTA5Ng==&mid=2651057507&idx=1&sn=ae611bd67a11ed03089b4ce53eefa6a8&chksm=84d9ccf4b3ae45e29b89b5f347a5407aca0f9bd3ad59575a2bd84b891c086f509a21264839b1&scene=21#wechat_redirect


##加显著性标记
library(ggpubr)
p1=p1 + stat_compare_means(method = "t.test")
p1

##2020.9.21
#alpha多样性之间显著性检验
setwd("E:\\桌面\\陈鹏数据\\")
x<-read.table(file="ITS结果/alpha_diversity.txt",sep="\t",header=T,row.names = 1)
group = read.table(file="group.txt",sep="\t",row.names=1)

sam=x

rowgroup = rownames(group)
rowsam = rownames(sam)  
mat = match(rowgroup,rowsam)  ##%in%
sam = sam[mat,]#;sam

colnames(group) = "group"
data.plot = cbind(sam,group) #data.plot

?t.test
n = ncol(sam)

result = c()
for (k in 1:2){  ###两种分组方式   #k=1
com = combn(unique(group[,k]),2)

resu = c()
for (m in 1:n){#数据每一列的值进行比较，共n列  m=1
  res = c()
  for (i in 1:dim(com)[2]){  #i = 1   ##不同的组两两比较
    r = t.test(data.plot[data.plot[,n+k] ==  com[1,i],m],
            data.plot[data.plot[,n+k] ==  com[2,i],m],
            paired = FALSE,conf.level = 0.95)
    re = cbind(G1 = com[1,i] ,G2 = com[2,i],p = round(r$p.value,4))
    res = rbind(res,re)  #每一列的结果
  }
  resu = rbind(resu,res)
}
result = rbind(result,resu)
}
result

#########################################################################chao
rm(list=ls())
setwd("E:/桌面/test.data/")
suppressMessages(library(vegan))
##得到chao值后画图
#注意原始文件需要改名字
x<-read.table(file="Galaxy328-[chao_unoise.txt].txt",sep="\t",header=T,row.names=1)
group = read.table("group2.txt", sep="\t", row.names=1 )

library(ggplot2)

sam=x

rowgroup = rownames(group)
rowsam = rownames(sam)  
mat = match(rowgroup,rowsam)  ##%in%
sam = sam[mat,]#;sam

colnames(group) = "group"
data.plot = cbind(sam,group) #data.plot

library(Rmisc)
chao_sd <- summarySE(data.plot, measurevar="sam", groupvars="group")

p = ggplot(chao_sd, aes(x=group, y=sam, fill=group)) + 
  geom_bar(position=position_dodge(), stat="identity",size=0.3) + 
  geom_errorbar(aes(ymin=sam-sd, ymax=sam+sd), width=.2, size =.3, position=position_dodge(.9)) +
  plot_theme+ theme(legend.position="None",axis.title.x = element_blank())+labs(y="Chao value")
p
ggsave(paste("chao.pdf", sep=""), p, width = 8, height = 5)
###显著性检验
p=p + stat_compare_means(method = "kruskal.test")
p

p=p+stat_compare_means(method = "anova", label.y = 1600)+ # Add global annova p-value
  stat_compare_means(label = "p.signif", method = "t.test", ref.group = ".all.")# Pairwise
p


#############################################################rarefaction curve
rm(list=ls())
setwd("E:/桌面/test.data/")
library("ggplot2") 
library("reshape2")
##得到rarefaction文件后画图
##注意原始文件需要改名字
x<-read.table(file="Galaxy327-[rarefaction_unoise.txt].txt",sep="\t",header=T,row.names=1)
group = read.table("group2.txt", sep="\t", row.names=1 )
sam=x;head(sam,2)
rowgroup = rownames(group)
colsam = colnames(sam)    ###col
mat = match(rowgroup,colsam)  
sam = sam[,mat]; head(sam,2)  ###col

colnames(group) = "group"
sam
# 转换宽表格为ggplot通用长表格格式
sam$x = rownames(sam) # 添加x轴列
sam_melt = melt(sam, id.vars=c("x")) # 转换为长表格
sam_melt$x = factor(sam_melt$x, levels=1:100) # 设置x轴顺序


data.plot = cbind(sam,group); head(data.plot)
data.plot2 = t(data.plot)
p = ggplot(data.plot2, aes(x = rownames(data.plot2), y = value, group = group, color = group )) + 
  geom_line()+
  xlab("Rarefraction Percentage")+ylab("Richness (Observed OTUs)")+
  scale_x_discrete(breaks = c(1:10)*10, labels = c(1:10)*10) + theme_classic()
p
ggsave(paste("rarefaction.pdf", sep=""), p, width = 8, height = 5)













