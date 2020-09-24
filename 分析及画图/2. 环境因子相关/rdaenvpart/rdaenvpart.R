##2019.9.25

##rdaenvpart

library(rdaenvpart)

# rdaenvpart(Y,X,pieplot = "tv",type="RDA")
# Y是响应变量矩阵
# X是解释变量矩阵，目前不能超过13个变量，因为超过13个，组合太多算不过来。超过9个会有四舍五入的误差
# pieplot = "tv"或pieplot = "tev"，选择tv是展示每个解释变量占总变化量（total variation）的比例的饼图，选择tev是展示每个解释变量占总被解释变化量（total explained variation）的比例的饼图。
# type="RDA"或type="CCA"

?rdaenvpart

require(vegan)
data(varespec)
data(varechem)
#see the "I" in $IJ, for the individual explain % of each variable
rdaenvpart(varespec,varechem[,1:5],pieplot = "tv",type="RDA")
#see the "I" in $IJ for the individual explain % of each variable
rdaenvpart(varespec,varechem[,1:5],pieplot = "tv",type="CCA")

# Y=varespec
# X=varechem[,1:5]

##########10.5  最新更新
##分解单个环境因子解释率的包rdacca.hp
#https://mp.weixin.qq.com/s/crFAezctLETlnweNkkteuw

