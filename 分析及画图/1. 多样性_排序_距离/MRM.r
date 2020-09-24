##2018.12.28
#MRM methods

library(ecodist)
?ecodist
?MRM  #Multiple regression on distance matrices (MRM) using permutation tests of significance for regression coefficients and R-squared.

# Usage
# 
# MRM(formula = formula(data), data = sys.parent(), nperm = 1000, method = "linear",
#     mrank = FALSE)

##nperm	  #number of permutations to use
##mrank	  #FALSE (the default option), Pearson correlations will be used. 
#TRUE, Spearman correlation (correlation ranked distances) will be used.
##method	#if "linear", the default, uses multiple regression analysis. 
#If "logistic", performs logistic regression with appropriate permutation testing. Note that this may be substantially slower.

#permutation test uses a pseudo-t test to assess significance, 
#rather than using the regression coefficients directly.

?lm()
#Examples
data(graze)
head(graze)
MRM(dist(LOAR10) ~ dist(sitelocation), data=graze, nperm=100)


# This grass is related to forest cover but not location
# + 表示排除这个因子的影响。!!!!错了。+不是排除，是添加另一个矩阵
MRM(dist(LOAR10) ~ dist(sitelocation) + dist(forestpct), data=graze, nperm=100)

# This legume is related to location but not forest cover
MRM(dist(TRRE3) ~ dist(sitelocation) + dist(forestpct), data=graze, nperm=100)



library("ecodist")
set.seed(9876)
sampleloc <- 1:20
species <- matrix(rnorm(100), nrow = 20, ncol = 5)
sampleloc.edist <- distance(sampleloc, "euclidean")
species.bcdist <- distance(species, "bray-curtis")
mantel(species.bcdist ~ sampleloc.edist)


mantel(species.bcdist ~ sampleloc.edist, mrank = TRUE)

?mantel
mantel(species.bcdist ~ sampleloc.edist, nperm=1000)  ##默认为1000


soil <- runif(20)
soil.edist <- distance(soil, "euclidean")
mantel(species.bcdist ~ sampleloc.edist + soil.edist)

##vf
# Example of multivariate analysis using built-in iris dataset
data(iris)
iris.d <- dist(iris[,1:4])

### nmds() is timeconsuming, so this was generated
### in advance and saved.
### set.seed(1234)
### iris.nmds <- nmds(iris.d, nits=20, mindim=1, maxdim=4)
### save(iris.nmds, file="ecodist/data/iris.nmds.rda")
data(iris.nmds)

# examine fit by number of dimensions
plot(iris.nmds)

# choose the best two-dimensional solution to work with
iris.nmin <- min(iris.nmds, dims=2)

# fit the data to the ordination as vectors
### vf() is timeconsuming, so this was generated
### in advance and saved.
### set.seed(1234)
### iris.vf <- vf(iris.nmin, iris[,1:4], nperm=1000)
### save(iris.vf, file="ecodist/data/iris.vf.rda")
data(iris.vf)
plot(iris.nmin, col=as.numeric(iris$Species), pch=as.numeric(iris$Species), main="NMDS")
plot(iris.vf)

rm(list=ls());gc()
###########################
#Backward Elimination方法
data(graze)
test = graze[,-c(1:3)]
head(test)
response = test[,1:5] #前5列为响应变量
predictor = test[,-c(1:5)] #剩余变量作为输入变量
all = MRM(dist(response) ~ dist(predictor), nperm=1000)
origin.p = all$coef[2,2];origin.p # p = 0.276
#设定一个阈值，如p=0.2时终止
p.set = 0.2
p.last = origin.p
while (p.last > p.set){  
#每一列分别进行MRM，最小的p去掉后再重新对整体做MRM
p.each = c()
 for (i in 1:ncol(predictor)) {
  each = MRM(dist(response) ~ dist(predictor[,-i]), nperm=1000)
  p.each = cbind(p.each,each$coef[2,2])
 }
p.each  ##去掉最小p值的变量，再重新对剩下来的整体做MRM。这里有个小缺陷，如果同时有多个最小值，则一次全去掉了。
predictor = predictor[,-c(which(p.each == min(p.each)))]

all.new = MRM(dist(response) ~ dist(predictor), nperm=1000)
p.last = all.new$coef[2,2]
print(ncol(predictor))
}
p.last
ncol(predictor)

##只去掉一个因子就满足了条件。

###############
#Forward Selection
data(graze)
test = (graze[,-c(1:3)])
head(test)
response = test[,1:5] #前5列为响应变量
predictor = test[,-c(1:5)] #剩余变量作为输入变量
#设定一个阈值，如p=0.2时终止
p.set = 0.2
p.last = 0.01
predictor.new = matrix(data=NA,nrow=nrow(test))
while (p.last < p.set){  
  #每一列分别进行MRM，取最小的p值加入predictor后重新对整体做MRM
  p.each = c()
  for (i in 1:ncol(predictor)) {
    each = MRM(dist(response) ~ dist(cbind(predictor.new,predictor[,i])), nperm=1000)
    p.each = cbind(p.each,each$coef[2,2])
  }
  p.each  ##取最小p值的变量，再重新对剩下来的整体做MRM。这里有个小缺陷，如果同时有多个最小值，则一次全加入模型。
  predictor.new = cbind(predictor.new, predictor[,c(which(p.each == min(p.each)))] )
  predictor = predictor[,-c(which(p.each == min(p.each)))]
  
  all = MRM(dist(response) ~ dist(predictor.new), nperm=1000)
  p.last = all$coef[2,2]
  print(ncol(predictor.new))
}
p.last
ncol(predictor.new)

#结果进行了7次。最终变量为8个,p为0.216。
#此结果再次证明了第二部分中，Backward Elimination更有优势的观点。其能够保留更多的变量。

##这两种方法的代码在小细节上有不少差别，需要仔细看~
#还有一个细节，如果nperm次数太少每次的结果都会不一样。nperm=1000每次的结果也会有差别。