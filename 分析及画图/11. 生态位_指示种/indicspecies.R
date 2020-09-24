##2019.9.8
#Studying the statistical relationship between species and groups of sites
#评估物种发生/丰度与样本之间关系的强度和统计意义，并能够计算生态位宽度。
##indicspecies 

install.packages("indicspecies")

library(indicspecies)
?indicspecies

##strassoc 计算物种与样本之间联系的强度
strassoc(X, cluster, func = "r", group = NULL, nboot = 0, alpha = 0.05, c = 1)
#x:行为样本，列为OTU
#cluster:样本分组的标签
#cunf:计算联系的强度的指数："r", "r.g", "IndVal", "IndVal.g", "A", "A.g", "B", "cos", "cos.g", "r.ind", "r.ind.g", "s.ind", "s.ind.g" (lowercase values are also accepted).
#nboot:bootstrap数
#alpha:置信区间错误率
#c:每个样本丰度之和

## Load species data
data(wetland) 

## Create three clusters using
wetkm = kmeans(wetland, centers=3)

## Compute Dufrene and Legendre's IndVal
strassoc(wetland, wetkm$cluster, func="IndVal.g") 


##indicators 物种组合指标分析。探究同时发生的物种集合(即物种组合)的指标值

indicators(X, cluster, group, func="IndVal", max.order=5, max.indicators=NULL, 
           At=0, Bt=0, sqrtIVt=0, nboot=0, alpha=0.05, XC=TRUE, enableFixed = FALSE, verbose = FALSE)
#x:行为样本，列为OTU
#max.order：最大组合物种数
#max.indicators: 保留的有效指标的最大数量。NULL全部都保留
#func: 要使用的指示值变量。"IndVal" (non-equalized) or "IndVal.g" (group-equalized).
#XC: TRUE，输出发生率形式的矩阵。
#verbose：中间步骤是否显示
#At:最低的正阈值
#Bt:灵敏度阈值


## Determine sensitivity of individual species
B=strassoc(wetland, cluster=wetkm$cluster,func="B") ;?strassoc

## Select species with more than 20% of sensitivity for the first group
sel=which(B[,1]>0.2) 

## Run indicator analysis with species combinations for the first group
sc= indicators(X=wetland[,sel], cluster=wetkm$cluster, group=1, verbose=TRUE, 
               At=0.5, Bt=0.2)

#Prints the results
print(sc)

#A:物种组合的正预测能力
#B:灵敏度
#sqrtIV:物种组合的预测值的平方根

## Plots positive predictive power and sensitivity against the order of 
## combinations
plot(sc, type="A")
plot(sc, type="B")

##pruneindicators 确定预测结果中的最优子集
pruneindicators(x, At=0, Bt=0, sqrtIVt=0, max.indicators=4, verbose=FALSE) 
#x:indicators的结果
#max.indicators：保留的最大物种组合数
sc2=pruneindicators(sc, At=0.5, Bt=0.2, verbose=TRUE)
print(sc2)


##计算生态位宽度：nichevar；nichecentroid；nichepref
#nichepref从物种资源使用情况(以及给定的资源可用性)计算物种资源偏好。
#nichecentroid计算物种在资源空间上的质心。
#nichevar计算物种的多元资源方差。
#在所有函数中，资源之间的距离以距离矩阵D表示，物种资源利用以P表示，资源可用性以向量q表示
nichevar(P, D = NULL, q = NULL, mode="multiple", Np = NULL, 
         Nq = NULL, nboot = 1000, alpha=0.05)
#P：行为物种，列为资源使用情况
#mode:Either mode = "single" (rows of matrix P are individual observations to be pooled for a single niche) or mode = "multiple" (rows in P represent different niches).

# Loads example data
data(birds)

# The niche metrics using distances among resources and assuming equal availability of resources
nichepref(birdsbreed, D = resourceD) 
nichevar(birdsbreed, D = resourceD) 
nichecentroid(birdsbreed, D = resourceD) 

# The niche metrics using distances among resources and computes 
# 95 percent confidence intervals
nichepref(birdsbreed, D = resourceD, mode="multiple", 
          Np = rowSums(birdsbreed), Nq = 100) 
nichevar(birdsbreed, D = resourceD, mode="multiple", 
         Np = rowSums(birdsbreed), Nq = 100) 
nichecentroid(birdsbreed, D = resourceD, mode="multiple", 
              Np = rowSums(birdsbreed), Nq = 100) 

# Same computations with different resource availability
nichepref(birdsbreed, D = resourceD, 
          q = c(0.18, 0.24, 0.22, 0.21, 0.15), mode="multiple")
nichevar(birdsbreed, D = resourceD, 
         q = c(0.18, 0.24, 0.22, 0.21, 0.15), mode="multiple")
nichecentroid(birdsbreed, D = resourceD, 
              q = c(0.18, 0.24, 0.22, 0.21, 0.15), mode="multiple")

# The niche metrics using distances among resources and 
# computes 95 percent confidence intervals
nichepref(birdsbreed, D = resourceD, 
          q = c(0.18, 0.24, 0.22, 0.21, 0.15), mode="multiple", Np = rowSums(birdsbreed), Nq = 100)
nichevar(birdsbreed, D = resourceD, 
         q = c(0.18, 0.24, 0.22, 0.21, 0.15), mode="multiple", Np = rowSums(birdsbreed), Nq = 100)
nichecentroid(birdsbreed, D = resourceD, 
              q = c(0.18, 0.24, 0.22, 0.21, 0.15), mode="multiple",  Np = rowSums(birdsbreed), Nq = 100)

# The following example defines a function to calculate the area of the niche
# It requires package 'rgeos'
library(rgeos)
nichearea <- function (P, D = NULL, axes=c(1,2)) {
  if (is.null(D)) 
    D <- as.dist((matrix(1, ncol(P), ncol(P)) - diag(rep(1, ncol(P)))))
  cmd = cmdscale(D,eig=TRUE,k= ncol(P)-1)
  X = cmd$points
  V <- data.frame(Area=rep(0, nrow(P)))
  for (i in 1:nrow(P)) {
    pi = as.numeric(P[i,])
    if (is.na(sum(pi))) V[i, ] <- NA
    else if (sum(pi) < 1e-16) V[i, ] <- 0
    else if (sum(pi>0)==1) V[i,]<-0
    else {
      a =X[pi>0,axes]
      V[i,]=area.poly(as(a[chull(a),],"gpc.poly"))
    }        	
  }
  return(V)
}
nichearea(birdsbreed, D = resourceD)

#########################################################

##2019.10.24
##Framebot文章
#https://mbio.asm.org/content/4/5/e00592-13#F2
# Indicator species were identified using labdsv package version 1.5-0. 
library(labdsv)

#见indual_指示物种.R
