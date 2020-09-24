##2020.7.26

##热图  左脾脏 & 右小肠
#对照组先取平均，数据再除以对照组作为标准化后的变化量。
#输入进来之后先按照行(因子)标准化，在计算spearman相关性。再画图。颜色为相关性，星号为显著性检验。
rm(list=ls(all=TRUE));gc()
getwd()
setwd("/Users/19021/Desktop/沈鑫数据/分析结果")

dat = read.table(file="gfstz脾脏.txt",header = T,row.names = 1,sep="\t")
dat = read.table(file="gfstz小肠.txt",header = T,row.names = 1,sep="\t")

##2019.5.17 何晴热图 
#heatmap
#https://www.cnblogs.com/xugege/p/7742263.html
#www.pianshen.com/article/1629355400/


#环境因子标准化。有两个样本大部分是NA，舍弃。直接na.rm=T忽略就行了，不用删掉两列了。
library(vegan);?decostand()
dat.scale = decostand(dat,margin=1,method="normalize",na.rm=T)

##另一种方法，标准化之前NA 变成0
# env[is.na(env)] = 0
# env.scale = decostand(env,margin=1,method="normalize",na.rm=T)

##先检验数据的正态性
sh.p = c()
for (i in 1:nrow(dat.scale)){  # i=1 
  sh = shapiro.test(as.vector(unlist(dat[i,])))
  sh.p = cbind(sh.p,sh$p.value)
};sh.p

# ba.p = c()
# for (i in seq(1,11,2)){  # i=1 
#   ba = fligner.test(otu.table[,c(i,i+1)])  #fligner.test;bartlett.test
#   ba.p = cbind(ba.p,ba$p.value)
# };ba.p

###算相关性
library(corrplot)
?cor.mtest
m=cor(dat.scale);m
##添加显著性标记
res1 <- cor.mtest(dat.scale, method = "spearman",conf.level = .95);res1

###
library(customLayout)
lay = lay_new(
  mat = matrix(1:2,ncol = 2),
  widths = c(2,2),
  heights = c(2)
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

p1 = corrplot(m,method = "color",col=col.rev(100),p.mat=res1$p,insig = "label_sig",
              sig.level = c(.001, .01, .05), pch.cex = .9, pch.col = "yellow")   ##显著性加星号
p2 = corrplot(m,method = "color",col=col.rev(100),p.mat=res1$p,insig = "label_sig",
              sig.level = c(.001, .01, .05), pch.cex = .9, pch.col = "yellow")   


# library(eoffice);?topptx()
# p = as.ggplot(~plot(cars, cex.lab=2, cex.main=2,
#                     xlab="biobabble", ylab="biobabble",
#                     main = "Y叔叔演示专用"))
# topptx(p, f)




###算相关性
library(corrplot)
?cor.mtest
m=cor(dat.scale[1:4,]);m
##添加显著性标记
res1 <- cor.mtest(dat.scale, method = "spearman",conf.level = .95);res1

###
library(customLayout)
lay = lay_new(
  mat = matrix(1:2,ncol = 2),
  widths = c(2,2),
  heights = c(2)
)
