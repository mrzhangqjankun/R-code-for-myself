###2018.12.14
#ggvegan
#微生物环境因子分析(RDA/db-RDA)-“ggvegan“介绍 
https://mp.weixin.qq.com/s?__biz=MzUzMjA4Njc1MA==&mid=2247487246&idx=2&sn=98d6ed989caaaaad026f98e035f7ba5c&chksm=fab9e5bfcdce6ca9af854612ffedf24b7198b05f35538048e682e73f2d647f9cf8e48721ff01&scene=0#rd

# 首先要安装devtools包，仅需安装一次
install.packages("devtools")
# 加载devtools包
library(devtools)
# 下载ggvegan包
devtools::install_github("gavinsimpson/ggvegan",force=TRUE)

setwd("E:/桌面/R script 2017/ggvegan")

library(ggvegan);?autoplot;??ggvegan
otu.tab <- read.csv("otutab.txt", row.names = 1, header=T, sep="\t")
env.data <- read.csv("new_meta.txt", row.names = 1, fill = T, header=T, sep="\t")
#transform data
otu <- t(otu.tab)
#data normolization (Legendre and Gallagher,2001)
##by log
env.data.log <- log1p(env.data)##
##delete NA
env <- na.omit(env.data.log)

###hellinger transform
otu.hell <- decostand(otu, "hellinger")

#DCA analysis  
sel <- decorana(otu.hell)
sel   ##axis lengths 0.64

otu.tab.0 <- rda(otu.hell ~ 1, env) #no variables
#Axis 第一项大于4应该用CCA分析,小于3用RDA
otu.tab.1<- rda(otu.hell ~ ., env)
#我们在筛选完RDA和CCA分析后，我们需要对所有环境因子进行共线性分析，利用方差膨胀因子分析
vif.cca(otu.tab.1)
#删除掉共线性的环境因子，删掉最大的变量，直到所有的变量都小于10
otu.tab.1 <- rda(otu.hell ~ N+P+K+Ca+Mg+pH+Al+Fe+Mn+Zn+Mo, env.data.log)

vif.cca(otu.tab.1)
#进一步筛选
otu.tab.1 <- rda(otu.hell ~ N+P+K+Mg+pH+Al+Fe+Mn+Zn+Mo, env.data.log)
vif.cca(otu.tab.1)
#test again
otu.tab.1 <- rda(otu.hell ~ N+P+K+Mg+pH+Fe+Mn+Zn+Mo, env.data.log)

#方差膨胀因子分析,目前所有变量都已经小于10
vif.cca(otu.tab.1)
##用step模型检测最低AIC值
?step   #An Information Criterion
mod.u <- step(otu.tab.0, scope = formula(otu.tab.1), test = "perm")# "perm"增加P值等参数
mod.d <- step(otu.tab.0, scope = (list(lower = formula(otu.tab.0), upper = formula(otu.tab.1))))
mod.d
##本处筛选的结果，找到一个Mg环境因子适合模型构建，为了下一步画图，我们
#保留所有非共线性的环境因子
#choose variables for best model and rda analysis again#
(otu.rda.f <- rda(otu.hell ~ N+P+K+Mg+pH+Fe+Mn+Zn+Mo, env))  ##从这往下怎么不用env.data.log了？

anova(otu.rda.f)
anova(otu.rda.f, by = "term")
anova(otu.rda.f, by = "axis")
#计算db-rda
otu.tab.bray <- vegdist(otu.hell, "bray")
#dbrda() for dbRDA # cca() for CCA

?capscale  #Distance-based redundancy analysis (dbRDA) .严格线性的方法
otu.tab.b<- capscale(otu.tab.bray ~ ., env)
##绘制db-RDA图
plot(otu.tab.b ,display=c("si","bp","sp"))

## 用ggvegan绘制RDA图
p<- autoplot(otu.rda.f, arrows = TRUE,axes = c(1, 2), geom = "text", layers = c( "species","sites", "biplot", "centroids"), legend.position = "right", title = "db-RDA")
## 添加图层
p + theme_bw()+theme(panel.grid=element_blank())
