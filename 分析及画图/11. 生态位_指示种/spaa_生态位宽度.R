

library(spaa)
?spaa
#用于分析物种关联和生态位重叠。

data(testdata)
testdata
data(splist)
splist

#add.col函数，把inputB中的一列添加到inputA中
## 把splist中genera这一列，按照species这一列名称添加到testdata中
add.col(inputA = testdata, inputB = splist, add = "genera",
        according = "species")
## 把splist中family这一列，按照species这一列名称添加到testdata中
add.col(inputA = testdata, inputB = splist, add = "family", 
        according = "species")

### 列表转化为矩阵形式
(spmatrix <- data2mat(testdata))

### 计算物种之间的关联
sp.assoc(spmatrix)

# 成对物种之间的关联
(result <- sp.pair(spmatrix))

# simple network with positive lines in red and negative lines 
# in blue
plotnetwork(result$Pearson)
title("Pearson Correlation Network")

# The lower matrix plot illustrating Pearson's correlation 
# between each pair of species Note the triangle didn't appeared
# in the plots, but have been added to the legend. This is due 
# to the distribution of data. Be carefull in seletion of intervals.

plotlowertri(result$Pearson, int = 0.5, cex=3, interval = 4, 
             pchlist = c(19, 17, 15, 1, 5), size = TRUE)
title("Pearson Correlation Lower Matrix Plot")

## plot lower matrix
## Using BCI data for lower matrix plot
library(vegan)
data(BCI)
## select the top 30 species according to relative frequeny.
sub <- sub.sp.matrix(BCI, common = 30)
## Set the digits to 1
plotlowertri(cor(sub), size = TRUE, cex = 3, digits = 1)

#### Niche width and niche overlap
data(datasample)
niche.overlap.boot(datasample[,1:3], method = "levins")


#niche.width
niche.width(mat, method = c("shannon", "levins"))
#mat:列为物种，行为样本
#method:计算方法
niche.width(datasample[,1:3], method = "shannon")

#每两个物种之间生态位的重叠
niche.overlap(mat, method = c("levins", "schoener", 
                              "petraitis", "pianka", "czech", "morisita"))
niche.overlap(datasample[,1:3], method = "levins")

#niche.overlap.boot也是计算生态位重叠，只是增加了bootstrap值，用法一样。


##example turnover()
plotlab1 <- XYname(4,6)
xxx <- 1:240
dim(xxx) <- c(24, 10)
rownames(xxx) <- plotlab1
### Distance between each pair of plots
ddd <- dist(xxx)
### label matrix
labmat1 <- lab.mat(plotlab1)
yyy <- turnover(labmat1, ddd)


## geodist() example
## Paris
L1 = deg2dec(-2,20,14)
phi1 = deg2dec(48, 50, 11)
## Washington DC
L2 = deg2dec(77,03,56)
phi2 = deg2dec(38,55,17)
##High precision Great Circle distance
geodist(L1, phi1, L2, phi2)
