##2018.3.13
##https://mp.weixin.qq.com/s?__biz=MzUzMjA4Njc1MA%3D%3D&mid=2247485147&idx=1&sn=36bde7eb3b8424eb7f7a8e77804fd4fc&scene=45#wechat_redirect
##corrplot绘图相关系数矩阵 

rm(list=ls(all=TRUE))
library(corrplot)

m=cor(mtcars);m  ##按照列两两进行相关性检验
corrplot(m,order = "hclust",addrect = 2)

corrplot.mixed(m,lower.col="black",number.cex = 0.7)


##添加显著性标记
res1 <- cor.mtest(mtcars, conf.level = .95);res1
corrplot(m, p.mat = res1$p, sig.level = .05)

#还可设置不显著的空白，或显示p值；更可以利用此方法显示所有p值，或用*数量代表显示性

## leave blank on no significant coefficient
corrplot(m, p.mat = res1$p, insig = "blank")

## add p-values on no significant coefficient
corrplot(m, p.mat = res1$p, insig = "p-value")

## add all p-values
corrplot(m, p.mat = res1$p, insig = "p-value", sig.level = -1)

## star level
corrplot(m, p.mat = res1$p, insig = "label_sig",
         sig.level = c(.001, .01, .05), pch.cex = .9, pch.col = "white")


#文章开篇的蓝月亮是如何画的呢？

#原理：生成一个1-15和15-1的30个数且每行10个的矩阵，利用corrplot可视，方法为饼形，矩阵为非对称，去除标签和图例，设置图例范围
dat = matrix(c(1:15,15:1), nrow = 10)
corrplot(t(dat), "pie", is.corr = F, cl.pos = "n", tl.pos = "n", cl.lim = c(1,15))
         
#corrplot(t(dat), "pie", is.corr = F)

setwd("E:/桌面/博三上工作/Heatmap-yangying")
lii = read.table("lii.txt",row.names =1, head = T,sep ="\t");head(lii)
corrplot(t(lii),method = "color",is.corr =F)  ##注意转置；is.corr=F 表示不是正方形的矩阵。行列数量不相同必须加上。

res1 <- cor.mtest(lii, conf.level = .95);res1   ##显著性矩阵

corrplot(t(lii),method = "color",is.corr =F,p.mat=res1$p,insig = "label_sig",
         sig.level = c(.001, .01, .05), pch.cex = .9, pch.col = "yellow")   ##显著性加星号


#################################################################################################
##ggcorrplot
##https://mp.weixin.qq.com/s?__biz=MzUzMjA4Njc1MA==&mid=2247484955&idx=1&sn=f494f3d82253c9e6cdb8d5f25532b409&scene=21#wechat_redirect

# 国内清华镜像快速安装包
site="https://mirrors.tuna.tsinghua.edu.cn/CRAN"
install.packages("ggcorrplot", repo=site)

library(ggcorrplot)

setwd("E:/桌面/博三上工作/Heatmap-yangying")
lii = read.table("lii.txt",row.names =1, head = T,sep ="\t");head(lii)

?ggcorrplot
ggcorrplot(t(lii))
ggcorrplot(t(lii),method="circle")

#重排矩阵，使用分等级聚类
ggcorrplot(t(lii), hc.order = TRUE, outline.color = "white") ##不是正方形矩阵会有一部分不显示，只显示正方形部分
#ggcorrplot没看到有类似is.corr=F的参数设置。所以很可能只适用于正方形矩阵。

#控制矩阵形状
ggcorrplot(t(lii), hc.order = TRUE, type = "lower", outline.color = "white")#下三角形,upper上三角形

#更改颜色以及主题
ggcorrplot(t(lii), hc.order = TRUE, type = "lower", outline.color = "white",
           
           ggtheme = ggplot2::theme_gray, colors = c("#6D9EC1", "white", "#E46726"))

#添加相关系数
ggcorrplot(t(lii), lab = TRUE)


##计算p-value ,cor_pmat()
p.mat = cor_pmat(lii);p.mat  ##等同于corrplot的cor.mtest

#增加显著性水平，不显著的话就不添加了
ggcorrplot(t(lii),hc.order = TRUE, type = "lower", p.mat = p.mat)


##2018.7.15
##R作图 相关性矩阵可视化包ggcorrplot和corrplot 
https://mp.weixin.qq.com/s?__biz=MzU4MzQ3NDExMw==&mid=2247483953&idx=1&sn=c2e2898625bb600dd04a56bbf79b2688&chksm=fda9cc9fcade458916d9592ef4795566584b5d7c079e9a883c630a1d33b00ac82308c92cf090&scene=0#rd


###2018.11.26
#技术贴│R语言13种相关矩阵图 
https://mp.weixin.qq.com/s?__biz=MzA3MTM3NTA5Ng==&mid=2651059394&idx=1&sn=fe50f8b874eea32ce2f7b161e6157937&chksm=84d9d555b3ae5c43fc7d2d89aa09229154b57cc71e2890cbedfe385a521a2da76984ddebf1bb&mpshare=1&scene=1&srcid=1126HUrVyD5K174qxI0hpJ0P&pass_ticket=1T2FqCMFav2it0yQQePH3nOazC%2F0aTA%2ByOHHjWTfYffYboJ9nT78pYcYordbgr7r#rd

# 安装corrplot包并调用
# 安装iterators包并调用
# 安装corrgram包并调用
# 安装ellipse包并调用
# 安装GGally包并调用
# 安装PerformanceAnalytics包并调用