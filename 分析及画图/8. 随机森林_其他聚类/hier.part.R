##2019.3.22
##hier.part
#hierarchical partitioning method 拟合优度计算和层次划分
#hier.part函数使用all.regs函数计算所有N个自变量的组合对整个模型层次结构的拟合优度度量。
#它采用拟合优度测度列表，使用配分函数，应用Chevan and Sutherland(1991)的层次划分算法，
#返回一个简单的表，其中列出了每个变量及其独立贡献(I)及其与所有其他变量的联合贡献(J)。
#注意独立的变量不能超过12个。该过程依赖于gtools包。

#层次划分(HP)是一种多元回归分析方法，它在解决多重共线性问题的同时，
#识别出最可能的原因因素。由于其对多元回归分析的补充作用，
#它在生态和保护方面的应用正在增加。

#install.packages("hier.part")

library(hier.part)
?hier.part
?glm
#hier.part(y, xcan, family = "gaussian", gof = "RMSPE",barplot = TRUE)
#y 因变量的向量
#xcan 包含n个独立变量的数据
#family glm广义线性模型的参数
#gof 拟合优度度量.默认RMSPE为均方根。其他还有logLik对数似然函数；Rsqu决定系数（R2)
#barplot TRUE会对每个变量单独和综合贡献的总解释方差的百分比画图

##结果包含3个内容：
#gfs：每个独立变量的组合情况；以及拟合优度的度量
#IJ：I为变量独立的贡献；J为变量综合的贡献
#I.perc：I在总解释方差中的百分比

##Example
#linear regression of log(electrical conductivity) in streams against seven independent variables describing catchment characteristics (from Hatt et al. 2004)
#描述流域特征的七个自变量对河流中测井曲线(电导率)的线性回归
data(urbanwq)
env <- urbanwq[,2:5];env
hier.part(urbanwq$lec, env, fam = "gaussian", gof = "Rsqu")

##改变变量顺序结果不变
envv = cbind(unsealden=env[,4], selev=env[, 6],fimp=env[,1],
             amgeast=env[,7],sdensep=env[,3],sconn=env[,2],fcarea=env[,5])
hier.part(urbanwq$lec, envv, fam = "gaussian", gof = "Rsqu")  
#logistic regression of an amphipod species occurrence in streams against four independent variables describing catchment characteristics (from Walsh et al. 2004).
#根据描述流域特征的四个自变量，对河流中两足动物物种的出现进行逻辑回归
data(amphipod)
env1 <- amphipod[,2:5]
hier.part(amphipod$australis, env1, fam = "binomial", #二项分布
          gof = "logLik")

##改变变量顺序结果不变
env2 = cbind(densep=env1[,3],fconn=env1[,2],fimp=env1[,1],unseal=env1[,4])
hier.part(amphipod$australis, env2, fam = "binomial", #二项分布
          gof = "logLik")
##combos
#按升序列出所有n个变量的可能组合的层次结构，从1个变量开始，
#然后是2个变量的所有组合，以此类推，直到所有n个变量的一个组合。
combos(3)


##构造一个超过9个的解释变量。看不同顺序结果是否一致。结果不一样了！！！
envvv = cbind(urbanwq[,3:11],new1=1:15,new2=3:17)
hier.part(urbanwq$fimp, envvv, fam = "gaussian", gof = "Rsqu")

envvv = cbind(new1=1:15,new2=3:17,urbanwq[,3:11])
hier.part(urbanwq$fimp, envvv, fam = "gaussian", gof = "Rsqu")
#说明文档
#https://cran.r-project.org/web/packages/hier.part/hier.part.pdf