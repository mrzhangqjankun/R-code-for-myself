####2020.7.27

##测序数据差异分析
rm(list=ls(all=TRUE));gc()
getwd()
setwd("/Users/19021/Desktop/沈鑫数据/分析结果")

x = read.table(file="OTU.txt",header = T,row.names = 1,sep="\t")
group = read.table("group.txt", sep="\t", row.names=1 )

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

# 1. alpha多样性指数 ----------------------------------------------------------------
##源自ggplot.diversity
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

#显著性
#https://mp.weixin.qq.com/s?__biz=MzI3Mzc1MzczMA==&mid=2247484318&idx=1&sn=aeeb47d5f0cc6ce0971032f4709393ef&chksm=eb1f3073dc68b9651aa3fe1fed06db66ade6231c5f9790868ffcf680e76f67e329e04f823da3&scene=21
#install.packages("ggsignif")
library(ggsignif)
?geom_signif
compaired <- list(c("SH", "SP"))
#geom_signif(comparisons = compaired,step_increase = 0.1,map_signif_level = F,test = wilcox.test)  ##两组非参数比较


##柱状图
p1 = ggplot(shannon_sd, aes(x=group, y=Shannon, fill=group)) + 
  geom_bar(position=position_dodge(), stat="identity",size=0.3) + 
  geom_errorbar(aes(ymin=Shannon-sd, ymax=Shannon+sd), width=.2, size =.3, position=position_dodge(.9)) +
  plot_theme+ theme(legend.position="None",axis.title.x = element_blank())+
  geom_signif(comparisons = compaired,step_increase = 0.1,map_signif_level = F,test = wilcox.test) 

p2 = ggplot(Inv_Simpson_sd, aes(x=group, y=Inv_Simpson, fill=group)) + 
  geom_bar(position=position_dodge(), stat="identity",size=0.3) + 
  geom_errorbar(aes(ymin=Inv_Simpson-sd, ymax=Inv_Simpson+sd), width=.2, size =.3, position=position_dodge(.9)) +
  plot_theme+ theme(legend.position="None",axis.title.x = element_blank())+
  geom_signif(comparisons = compaired,step_increase = 0.1,map_signif_level = F,test = wilcox.test) 

p3 = ggplot(Observed_richness_sd, aes(x=group, y=Observed_richness, fill=group)) + 
  geom_bar(position=position_dodge(), stat="identity",size=0.3) + 
  geom_errorbar(aes(ymin=Observed_richness-sd, ymax=Observed_richness+sd), width=.2, size =.3, position=position_dodge(.9)) +
  plot_theme+ theme(legend.position="None",axis.title.x = element_blank())+
  geom_signif(comparisons = compaired,step_increase = 0.1,map_signif_level = F,test = wilcox.test) 

p4 = ggplot(Pielou_evenness_sd, aes(x=group, y=Pielou_evenness, fill=group)) + 
  geom_bar(position=position_dodge(), stat="identity",size=0.3) + 
  geom_errorbar(aes(ymin=Pielou_evenness-sd, ymax=Pielou_evenness+sd), width=.2, size =.3, position=position_dodge(.9)) +
  plot_theme+ theme(legend.position="None",axis.title.x = element_blank())+
  geom_signif(comparisons = compaired,step_increase = 0.1,map_signif_level = F,test = wilcox.test) 
#分面
library(gridExtra)
grid.arrange(p3, p1, p2, p4)
##值为显著性

# 2. beta多样性 --------------------------------------------------------------
##################### PCA
suppressMessages(library(vegan))
library(ggplot2)
options(warn=-1)

x.pca = rda(t(x));x.pca
outputpca = summary(x.pca)
str(outputpca)

a = outputpca$species ;a
b = outputpca$cont$importance ;b

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

rowname = row.names(sam.a)

p = ggplot(data.plot,aes(PC1,PC2))
p = p + geom_point(aes(colour = group,shape = group),alpha=0.5,size = 3);p
p = p + xlab(paste("PC1=",pc1*100,"%",sep=""))+ylab(paste("PC2=",pc2*100,"%",sep=""))+ labs(title = "PCA analysis") ;p
p = p +  plot_theme + theme(plot.title=element_text(hjust=0.5));p
p = p + theme_bw();p
p=p+theme(axis.text= element_text(size=16, color="black", family  = "serif", face= "bold", vjust=0.5, hjust=0.5))+
  theme(axis.title = element_text(size=16, color="black", family  = "serif",face= "bold", vjust=0.5, hjust=0.5))+
  theme(legend.text = element_text(colour = 'black', size = 16,  family  = "serif",face = 'bold'))+
  theme(legend.title=element_blank());p
p=p+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())
p
#p = p +  stat_ellipse(aes(colour = group),level=0.95,linetype = 2);p  
#p = p + coords_fixed(1);p  ##https://mp.weixin.qq.com/s/f9X5yJAw_cA08g6Ygq9Itw


#####################################################################DCA
suppressMessages(library(vegan))
x.dca<-decorana(t(x)) ;x.dca
output = summary(x.dca) ;output
dca.sites = output$site.scores;  dca.sites

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
p = p +  plot_theme ;p
#p = p +  stat_ellipse(aes(colour = group),level=0.95,linetype = 2);p 
p=p+theme(axis.text= element_text(size=16, color="black", family  = "serif", face= "bold", vjust=0.5, hjust=0.5))+
  theme(axis.title = element_text(size=16, color="black", family  = "serif",face= "bold", vjust=0.5, hjust=0.5))+
  theme(legend.text = element_text(colour = 'black', size = 16,  family  = "serif",face = 'bold'))+
  theme(legend.title=element_blank());p
p=p+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())
p

#########################################################################NMDS
suppressMessages(library(vegan))

?metaMDS
##bray  #trymax=100,maximum numbers of random starts in search of stable solution
bray.mds<-metaMDS(x, distance="bray", k=2, trymax=100) ;bray.mds

##jackard
x = decostand(x,"pa")
jaccard.mds<-metaMDS(x, distance="jaccard", k=2, trymax=100) ;jaccard.mds

str(bray.mds)  
bray.mds$species
outbray = bray.mds$points ;outbray
outjaccard = jaccard.mds$point ;outjaccard

outbray = bray.mds$points ;outbray
outjaccard = jaccard.mds$point ;outjaccard

out.bra = outbray[,1:2]
out.jac = outjaccard[,1:2]

stress.bra = bray.mds$stress;stress.bra
stress.jaccard = jaccard.mds$stress

rowgroup = rownames(group)
meth = out.bra  ##out.jac替换为out.bra即可
rowsam = rownames(meth)  
mat = match(rowgroup,rowsam)
meth = meth[mat,];meth

colnames(group) = "group"
data.plot = cbind(meth,group)
data.plot
#ggplot2
library(ggplot2)
rowname = row.names(meth)

p = ggplot(data.plot,aes(MDS1,MDS2))
p = p + geom_point(aes(colour = group,shape = group),alpha=0.5,size = 3);p
p = p + xlab("MDS1")+ylab("MDS2")+ labs(title = paste("NMDS\n","stress=",round(stress.bra,3),sep="\t")) ;p
p = p +  plot_theme;p
#p = p +  stat_ellipse(aes(colour = group),level=0.95,linetype = 2);p  
p=p+theme(axis.text= element_text(size=16, color="black", family  = "serif", face= "bold", vjust=0.5, hjust=0.5))+
  theme(axis.title = element_text(size=16, color="black", family  = "serif",face= "bold", vjust=0.5, hjust=0.5))+
  theme(legend.text = element_text(colour = 'black', size = 16,  family  = "serif",face = 'bold'))+
  theme(legend.title=element_blank());p
p=p+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())
p

##########################################################################CCA_RDA
#DCA结果中Lengths of gradient 的第一轴的大小#
#如果大于4.0,就应选CCA；如果在3.0-4.0之间，选RDA和CCA均可#
#如果小于3.0, RDA的结果要好于CCA#
#0.9,用RDA

#这个数据没用。给了脾脏和小肠的环境因子。但是只给了一个OTU，不知道是哪一个的。
suppressMessages(library(vegan))
otu<- x;head(x)
##按照环境因子标准化，不是按照样本标准化~~~~~~~~
env.dat = read.table(file="gfstz脾脏.txt",header = T,row.names = 1,sep="\t")
env.dat = read.table(file="gfstz小肠.txt",header = T,row.names = 1,sep="\t")

colnames(env.dat) = colnames(otu)
env.dat = t(env.dat)
env.st = decostand(env.dat, method="standardize", MARGIN=2);?decostand
#standardize: scale x to zero mean and unit variance (default MARGIN = 2).
#Margin, if default is not acceptable. 1 = rows, and 2 = columns of x.按列（样本）标准化
group = read.table("group.txt", sep="\t", row.names=1 )

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
###=====###==新增改进
while ( inf_factor[max_env] > 20){
  env.st3 = env.st3[,-max_env]
  C.reduced = cca(otu, env.st3)
  inf_factor = vif.cca(C.reduced)
    na_env = which(is.na(inf_factor))
      if(isTRUE(length(na_env) > "0") ){
        inf_factor = inf_factor[-na_env]
      }
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


##所有结果
sink("测序cca_小肠.txt")
output1
inf_Fp
sink()

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
p = p + geom_text(aes(label = rownames(data.plot), vjust = 1.6, hjust = 0.5))
p
##加环境因子
#https://mp.weixin.qq.com/s?__biz=MzAwMDY0MzQ0Ng==&mid=2247483940&idx=1&sn=dfee5a1269f5984f2d83f411b50f60f3&chksm=9ae49a3dad93132b9b8e3b89a974df8fd847266bc69f6243b01130e57a98461659c4c217c2bd&mpshare=1&scene=1&srcid=0520kcKPgA1MhipSnVleJubX&pass_ticket=a%2FDZKnr7xDnM7txpzaFEK%2BPJtwU6W9%2BkB4QMG1yU6Lht0mbLenrBKIaxi9BbMLim#rd

rowenv = rownames(env);rowenv
env = data.frame(env)  ##矩阵做不了，必须转成数据框

p = p + geom_segment(data=env,aes(x=0,y=0,xend=CCA1,yend=CCA2),arrow = arrow(length = unit(0.3,"cm")), size = 0.5) ;p

p = p + geom_text(data = env,aes(CCA1,CCA2,label = rowenv),size = 4,nudge_x=-0.15,nudge_y=0.15);p

p = p + geom_hline(aes(yintercept = 0), linetype="dashed") + geom_vline(aes(xintercept = 0) ,linetype="dashed");p
#ggsave("CCA.pdf", width = 4, height = 2.5,dpi = 600); ?ggsave; getwd()
p = p+theme(axis.text= element_text(size=16, color="black", family  = "serif", face= "bold", vjust=0.5, hjust=0.5))+
  theme(axis.title = element_text(size=16, color="black", family  = "serif",face= "bold", vjust=0.5, hjust=0.5))+
  theme(legend.text = element_text(colour = 'black', size = 16,  family  = "serif",face = 'bold'))+
  theme(legend.title=element_blank());p
p=p+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())
p

# 3. dissimilarity test ---------------------------------------------------
#源于培训班代码
# Dissimilarity test for one group 
suppressMessages(library(vegan))
fg.dat = x
list.dat = group

first.group = list.dat[, 1]
grp1 = unique(first.group);grp1

fg.dat2 = fg.dat[, 1:ncol(fg.dat)]#1 is for without taxonomy; 8 is for taxonomy;
fg.dat2[is.na(fg.dat2)] = 0
sample = colnames(fg.dat2)

# group cutting
samp.fg = colnames(fg.dat)
samp.env= rownames(list.dat)
my.fg = match(samp.env, samp.fg)  
fg.dat2 = fg.dat2[,my.fg]
rowS = rowSums(fg.dat2>0)
valid.row = which(rowS>0)
fg.dat2 = fg.dat2[valid.row, ]

#===
# for Dissimilarity test among group1
grp<-list()
for (i in c(1:length(grp1))){
  a<-as.vector(grp1[[i]])  ##组名
  grp[[i]]<-rownames(list.dat)[which(first.group==a)]
}
grp
names(grp)<-grp1 ; names(grp)
samp<-colnames(fg.dat2) ; samp
mrpp.re = matrix(0, nrow=length(grp), ncol=length(grp))
ado.re = matrix(0, nrow=length(grp), ncol=length(grp))
ano.re = matrix(0, nrow=length(grp), ncol=length(grp))

for (x in c(1:(length(grp)-1))) {
  for(y in c((x+1):length(grp))){
    list1 = grp[[x]]
    list2 = grp[[y]]
    
    # ?pmatch.pmatch(x, table) 
    col1 = pmatch(list1, samp)  ##在samp中找出list1对应的列数
    col2 = pmatch(list2, samp)
    grp.list = c(rep(as.vector(grp1[[x]]),length(list1)),rep(as.vector(grp1[[y]]), length(list2)))
    dat = fg.dat2[, c(col1, col2)]
    #===cut empty row=
    sum1 = rowSums(dat, na.rm=T) 
    valid.row = which(sum1 > 0)
    #
    dat = dat[valid.row,]
    dat[is.na(dat)] = 0
    dat1 = t(dat)  #注意转置
    
    #dat.mrpp$delta, dat.mrpp$Pvalue, dat.ano$statistic, dat.ano$signif, dat.ado$aov.tab[1,4], dat.ado$aov.tab[1,6]
    dat.dist = vegdist(dat1, method = "jaccard",binary=TRUE)  ##bray
    
    ?mrpp
    dat.mrpp = mrpp(dat.dist, grp.list)  
    #str(dat.mrpp)
    mrpp.re[x, y] = dat.mrpp$Pvalue  
    mrpp.re[y, x] = dat.mrpp$delta   #上三角是P，下三角是特征值
    
    ?anosim
    dat.ano = anosim(dat.dist, grp.list) #bray
    ano.re[x, y] = dat.ano$signif  
    ano.re[y, x] = dat.ano$statistic
    
    ?adonis
    grp.vector = list(V1 = grp.list)
    dat.ado = adonis(dat1 ~ V1, data=grp.vector, method = "jaccard",binary=TRUE)
    ado.re[y, x] = dat.ado$aov.tab[1,4]
    ado.re[x, y] = dat.ado$aov.tab[1,6] 
  }
}
colnames(mrpp.re) = rownames(mrpp.re) <- grp1
mrpp.re
colnames(ado.re) = rownames(ado.re) <- grp1
ado.re
colnames(ano.re) = rownames(ano.re) <- grp1
ano.re
write.table(mrpp.re,file="mrpp.txt",sep="\t",col.names=NA)
write.table(ado.re,file="adonis.txt",sep="\t",col.names=NA)
write.table(ano.re,file="anosim.txt",sep="\t",col.names=NA)

#==
# do dissimilarity for whole dataset based on the grp1 grouping profile
fg.dat2<-fg.dat2[,as.vector(unlist(grp))]
fg.dat2[is.na(fg.dat2)] = 0
grp.list = c()
for(i in 1:length(grp)){
  grp.list = c(grp.list, rep(paste("grp",i,sep=""),length(grp[[i]])))
}
grp.list

report=c()
grp.vector = list(V1 = grp.list)

dat.dist = vegdist(t(fg.dat2), method = "jaccard", binary=TRUE) #"bray"

dat.mrpp = mrpp(dat.dist, grp.list) 

dat.ano = anosim(dat.dist, grp.list) 

dat.ado = adonis(t(fg.dat2) ~ V1, data=grp.vector, method = "jaccard", binary=TRUE)
#dat.ado = adonis(t(fg.dat2) ~ V1, data=grp.vector, method = "bray")

report = cbind(report, c(dat.mrpp$delta, dat.mrpp$Pvalue, dat.ano$statistic, dat.ano$signif, dat.ado$aov.tab[1,4], dat.ado$aov.tab[1,6]))
rownames(report) <- c("MRPP.delta","MRPP.P","ANOSIM.r","ANOSIM.P","PERMANOVA.F","PERMANOVA.P")
colnames(report) = "Whole dataset"
report
write.table(report,file="whole_group.txt",sep="\t",col.names = NA)


# 3. phylum物种分布 -----------------------------------------------------------
rm(list=ls(all=TRUE));gc()
getwd()
setwd("/Users/19021/Desktop/沈鑫数据/分析结果")
df = read.table(file = "phylum.txt",header = T,sep="\t")

##删除空行
df = df[rowSums(df[,2:7])!=0,]

len = length(unique(df[,1]))
res = c()
for (i in 1:len){  # i = 1
  hang = df[  df[,1] == unique(df[,1])[i]  ,]
  res = rbind(res, apply(hang[,2:7],2,sum)  )
}
res

#算百分比
res2 = matrix(data=NA,nrow = nrow(res),ncol = ncol(res))
for (i in 1:ncol(res)){  #i = 1
  res2[,i] = res[,i]*100/apply(res,2,sum)[i]
}
apply(res2,2,sum)
colnames(res2) = colnames(res)
res2 = as.data.frame(res2)

##?arrange  #desc()降序   排序完phylum就对不上了
#res2 = arrange(res2,desc(res2[,2]))

#install.packages("rlang",version = "0.4.7")
library(dplyr)
library(tidyverse)
res2 = tibble::as_tibble(res2)
res2 = res2 %>% 
  mutate(Phylum = unique(df[,1]))
res2
write.table(res2,file = "phylum_for_ggplot.txt",col.names=NA,sep="\t")

res2 = read.table(file="phylum_for_ggplot.txt",header = T,sep="\t")
res2.long <- gather(res2,SH1,SH2,SH3,SP1,SP2,SP3,key = "sample",value ="value")

library("ggthemes")
library("RColorBrewer")
?scale_colour_brewer()
display.brewer.all() 
colourCount = length(unique(res2.long$Phylum))
getPalette = colorRampPalette(brewer.pal(9, 'Paired'))

library(ggplot2)
p = ggplot(res2.long,aes(x=sample,y=value,fill=Phylum)) +
  geom_bar(stat = 'identity', width = 0.5, position = 'stack')+
  xlab("Samples")+ylab("Abundance")+
  scale_fill_manual(values = getPalette(colourCount))
p
p = p+theme(axis.text= element_text(size=16, color="black", family  = "serif", face= "bold", vjust=0.5, hjust=0.5))+
  theme(axis.title = element_text(size=16, color="black", family  = "serif",face= "bold", vjust=0.5, hjust=0.5))+
  theme(legend.text = element_text(colour = 'black', size = 16,  family  = "serif",face = 'bold'))+
  theme(legend.title=element_blank());p
p = p+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())
p
