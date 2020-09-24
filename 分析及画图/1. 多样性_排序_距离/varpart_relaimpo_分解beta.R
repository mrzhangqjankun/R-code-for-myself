setwd("C:/Users/19021/Desktop/小白鱼/200328-普鲁克分析（Procrustes Analysis）评估物种-环境或功能关联度的一个示例/")

#两种变异分解的方法
#1. varpart

library(vegan)
?varpart

#使用RDA的adjusted R-squared或基于距离的冗余分析，通过2/3/4个解释矩阵将群落进行变异分解（VPA分析）
#注意解释矩阵中的共线性的变量不需要提前删除。

#Usage
varpart(Y, X, ..., data, chisquare = FALSE, transfo, scale = FALSE,
        add = FALSE, sqrt.dist = FALSE, permutations)
#Y:数据框或矩阵形式的OTU
#X：2~4个解释矩阵，如环境因子
#chisquare = FALSE使用RDA，=TRUE使用CCA
#transfo：对Y进行转化，方法同decostand
#scale:标准化为单位方差
#permutations：置换次数

#如果Y是一个变量，分解基于线性回归。如果Y是不相似度，分解基于db-RDA(capscale)。

#实例
data(mite) #OTU
data(mite.env) #环境因子
data(mite.pcnm) #地理距离PCNM

# 两个解释变量，数据做Hellinger转化
mod <- varpart(mite, mite.env, mite.pcnm, transfo="hel")
mod

##画图
showvarparts(2, bg = c("hotpink","skyblue"))
plot(mod, bg = c("hotpink","skyblue"))

## rda检测结果[a]的显著性：
aFrac <- rda(decostand(mite, "hel"), mite.env, mite.pcnm)
anova(aFrac)
## RsquareAdj结果中的adj.r.squared和varpart结果一致
RsquareAdj(aFrac)

##再用CCA试试
mod <- varpart(mite, mite.env, mite.pcnm, chisquare = TRUE)
mod
showvarparts(2, bg = c("hotpink","skyblue"))
plot(mod, bg = c("hotpink","skyblue"))


## 来个四个解释变量的~
mod <- varpart(mite, ~ SubsDens + WatrCont, ~Substrate + Shrub + Topo,
               mite.pcnm[,1:11], mite.pcnm[,12:22], data=mite.env, transfo="hel")
mod
plot(mod, cutoff = -Inf, cex = 0.7, bg=2:5)

##这种方法的局限性是最多只能做4个解释变量。如果是4个以上，CCA的方法可以使用partial CCA实现。这里不再赘述。


#2. lmg
library(relaimpo)
?relaimpo
#relaimpo计算线性模型的相对重要性指标.推荐两种方法lmg和pmvd
#主要函数calc.relimp
calc.relimp(object, x = NULL, ..., 
            type = "lmg", diff = FALSE, rank = TRUE, rela = FALSE, always = NULL, 
            groups = NULL, groupnames = NULL, weights=NULL, design=NULL)
#object:输入
#type:计算方法，包括lmg, pmvd, last, first, betasq, pratt, genizi。每种具体含义如下。


#实例
data(swiss)
res = calc.relimp(swiss, 
            type = c("lmg", "last", "first", "betasq", "pratt", "genizi", "car") )
res
plot(res)


