##2019.5.17
#heatmap
#https://www.cnblogs.com/xugege/p/7742263.html
#www.pianshen.com/article/1629355400/

rm(list=ls(all=TRUE));gc()
setwd("E:/桌面/R script 2017/何晴_热图/")
library(corrplot)

env = read.table(file="Env.txt",sep="\t",header=T,row.names= 1)
qpcr = read.table(file="qPCR.txt",sep="\t",header=T,row.names= 1)

##qpcr最后8行都是0 ，去掉
qpcr = qpcr[1:(nrow(qpcr)-8),]


#环境因子标准化。有两个样本大部分是NA，舍弃。直接na.rm=T忽略就行了，不用删掉两列了。
library(vegan);?decostand()
#env2 = env[,-c(which(colnames(env)=="GXS_SED"),which(colnames(env)=="JZL_SED"))]
env.scale = decostand(env,margin=1,method="normalize",na.rm=T)

##另一种方法，标准化之前NA 变成0
env[is.na(env)] = 0
env.scale = decostand(env,margin=1,method="normalize",na.rm=T)

##先检验数据的正态性
sh.p = c()
for (i in 1:nrow(env.scale)){  # i=1 
  sh = shapiro.test(as.vector(unlist(env[i,])))
  sh.p = cbind(sh.p,sh$p.value)
};sh.p

# ba.p = c()
# for (i in seq(1,11,2)){  # i=1 
#   ba = fligner.test(otu.table[,c(i,i+1)])  #fligner.test;bartlett.test
#   ba.p = cbind(ba.p,ba$p.value)
# };ba.p

###算相关性
?cor.test
cor.p=c()
cor.r=c()
for (i in 1:nrow(qpcr)){ #i=1    
    p = c()
    r = c()
  for (j in 1:nrow(env.scale)){ #j=1
    res = cor.test(as.vector(unlist(env.scale[j,])),as.vector(unlist(qpcr[i,])),method = "spearman") ##pearson  spearman
    p = cbind(p,res$p.value)
    r = cbind(r,res$estimate)
  }
  cor.p = rbind(cor.p,p)
  cor.r = rbind(cor.r,r)
}
rownames(cor.p) = rownames(qpcr);colnames(cor.p) = rownames(env)
rownames(cor.r) = rownames(qpcr);colnames(cor.r) = rownames(env)
cor.p
cor.r

###一起画太丑了，分7组。
library(customLayout)
lay = lay_new(
  mat = matrix(1:6,ncol = 3),
  widths = c(2,2,2),
  heights = c(2,2)
)
lay_show(lay)
#默认调色板，蓝色为1，红色为-1。
# col2 <- colorRampPalette(c("#67001F", "#B2182B", "#D6604D", "#F4A582",
#                            "#FDDBC7", "#FFFFFF", "#D1E5F0", "#92C5DE",
#                            "#4393C3", "#2166AC", "#053061"))
#倒过来，红色为1
col.rev = colorRampPalette(c("#053061", "#2166AC", "#4393C3","#92C5DE" ,
                             "#D1E5F0", "#FFFFFF", "#FDDBC7", "#F4A582",
                             "#D6604D","#B2182B" , "#67001F"))

p1 = corrplot(cor.r[1:11,],method = "color",col=col.rev(100),p.mat=cor.p[1:11,],insig = "label_sig",
         sig.level = c(.001, .01, .05), pch.cex = .9, pch.col = "yellow")   ##显著性加星号
p2 = corrplot(cor.r[12:22,],method = "color",col=col.rev(100),p.mat=cor.p[12:22,],insig = "label_sig",
              sig.level = c(.001, .01, .05), pch.cex = .9, pch.col = "yellow")   
p3 = corrplot(cor.r[23:33,],method = "color",col=col.rev(100),p.mat=cor.p[23:33,],insig = "label_sig",
              sig.level = c(.001, .01, .05), pch.cex = .9, pch.col = "yellow")   
p4 = corrplot(cor.r[34:44,],method = "color",col=col.rev(100),p.mat=cor.p[34:44,],insig = "label_sig",
              sig.level = c(.001, .01, .05), pch.cex = .9, pch.col = "yellow")   
p5 = corrplot(cor.r[45:55,],method = "color",col=col.rev(100),p.mat=cor.p[45:55,],insig = "label_sig",
              sig.level = c(.001, .01, .05), pch.cex = .9, pch.col = "yellow")   
p6 = corrplot(cor.r[55:73,],method = "color",col=col.rev(100),p.mat=cor.p[55:73,],insig = "label_sig",
              sig.level = c(.001, .01, .05), pch.cex = .9, pch.col = "yellow")   


##OTU和环境因子相关
setwd("E:\\桌面\\陈鹏数据\\")

env = read.table(file="env.txt",sep="\t",header=T,row.names= 1)
qpcr = read.table(file="16S结果/resample28741_UPARSE_otu_table.txt",sep="\t",header=T,row.names= 1)
env = t(env)
