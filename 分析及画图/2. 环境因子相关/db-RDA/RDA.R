##2018.3.21
##用db-RDA进行微生物环境因子分析-“ggvegan“介绍
##https://mp.weixin.qq.com/s?__biz=MzUzMjA4Njc1MA==&mid=2247485256&idx=1&sn=424905c5aa453f8eaedc9348f17fab74&chksm=fab9edf9cdce64efd1e4715be7f724cf5141a926fdee05bb262c9dbde0217b029b4261967f79&mpshare=1&scene=1&srcid=0320r3RpGMLajSCbtmisYcB7&pass_ticket=hcEBTmUB0j1N4poFG7EE0OS2rklY9BD1Qmvj2lHIynH0%2F5p32nw7CdVaBNZCuIU8#rd

rm(list=ls(all=TRUE))
setwd("E:/桌面/R script 2017/db-RDA")

# 首先要安装devtools包，仅需安装一次
install.packages("devtools")
# 加载devtools包
library(devtools)
# 下载ggvegan包
devtools::install_github("gavinsimpson/ggvegan")


library(ggvegan)
otu.tab <- read.csv("otutab.txt", row.names = 1, header=T, sep="\t");head(otu.tab)
env.data <- read.csv("new_meta.txt", row.names = 1, fill = T, header=T, sep="\t");head(env.data)
#transform data
otu <- t(otu.tab)
#data normolization (Legendre and Gallagher,2001)
##by log
?log1p  #log1p(x) computes log(1+x) accurately also for |x| << 1.
env.data.log <- log1p(env.data)##
##delete NA
env <- na.omit(env.data.log)

###hellinger transform
?decostand  #hellinger: square root of method = "total" (Legendre & Gallagher 2001).平方根
otu.hell <- decostand(otu, "hellinger");tail(otu.hell)

#DCA analysis  
sel <- decorana(otu.hell)
sel

otu.tab.0 <- rda(otu.hell ~ 1, env) #no variables
#Axis 第一项大于四应该用CCA分析
otu.tab.1<- rda(otu.hell ~ ., env)#若只指定一行或一列，可用.作为占位符，如row_var~.,创建单行多列的图形矩阵
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
?step
mod.u <- step(otu.tab.0, scope = formula(otu.tab.1), test = "perm")# "perm"增加P值等参数
mod.d <- step(otu.tab.0, scope = (list(lower = formula(otu.tab.0), upper = formula(otu.tab.1))))
mod.d
##本处筛选的结果，找到一个Mg环境因子适合模型构建，为了下一步画图，我们
#保留所有非共线性的环境因子
#choose variables for best model and rda analysis again#
(otu.rda.f <- rda(otu.hell ~ N+P+K+Mg+pH+Fe+Mn+Zn+Mo, env))

anova(otu.rda.f)
anova(otu.rda.f, by = "term")
anova(otu.rda.f, by = "axis")
#计算db-rda
## 用ggvegan绘图
p<- autoplot(otu.rda.f, arrows = TRUE,axes = c(1, 2), geom = "text", layers = c( "species","sites", "biplot", "centroids"), legend.position = "right", title = "db-RDA")
## 添加图层
p + theme_bw()+theme(panel.grid=element_blank())
