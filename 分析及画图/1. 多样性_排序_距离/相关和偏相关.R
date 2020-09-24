##2020.2.29

##https://mp.weixin.qq.com/s/5gZ3LvQ3pN8RZyMkNAxlMQ

##Pearson、Spearman、Kendall、Polychoric、Polyserial相关系数简介及R计算 

#cor()可用于计算Pearson、Spearman和Kendall相关矩阵，cov()可用于获得协方差矩阵。

data(mtcars)

#标准化不影响相关系数计算值，但可以让数据服从均值 0，标准差 1 的等方差结构
mtcars <- scale(mtcars)

#协方差计算，cov()
cov_pearson <- cov(mtcars, method = 'pearson')
cov_pearson

cov_spearman <- cov(mtcars, method = 'spearman')
cov_spearman

cov_kendall <- cov(mtcars, method = 'kendall')
cov_kendall

#相关系数计算，cor()
cor_pearson <- cor(mtcars, method = 'pearson')
cor_pearson

cor_spearman <- cor(mtcars, method = 'spearman')
cor_spearman

cor_kendall <- cor(mtcars, method = 'kendall')
cor_kendall

#相关图，例如
library(corrplot)

corrplot(cor_pearson, method = 'number', number.cex = 0.8, diag = FALSE, tl.cex = 0.8)
corrplot(cor_pearson, add = TRUE, type = 'upper', method = 'pie', diag = FALSE, tl.pos = 'n', cl.pos = 'n')

#输出，例如
write.table(cor_pearson, 'cor_pearson.txt', sep = '\t', col.names = NA, quote = FALSE)

##################
#偏相关是指在控制一个或多个定量变量时，另外两个定量变量之间的相互关系。R包ggm中提供的命令pcor()可以计算偏相关系数。

##偏相关，ggm 包 pcor()
library(ggm)

#要计算相关系数的两个变量，或指定下标
x1 <- c('mpg', 'cyl')

#要控制的条件变量，或指定下标
x2 <- c('drat', 'wt', 'qsec')

#指定协方差矩阵，计算偏相关
pcor_pearson <- pcor(c(x1, x2), cov(mtcars, method = 'pearson'))
pcor_pearson

# Polychoric和Tetrachoric相关
# 
# psych包提供了计算这些相关系数的方法。
# psych包也能计算Polyserial和Biserial相关，但文档中没提供示例

##Polychoric、Tetrachoric
library(psych)

#Polychoric 相关
data(bock)

polyc <- polychoric(lsat6)
polyc

#Tetrachoric 相关
tetr <- tetrachoric(lsat6[ ,1:2])
tetr

# Polyserial和(Point-)Biserial相关
# 
# 以ltm包提供的方法为例。

##Polyserial、(Point-)Biserial
library(ltm)

#Polyserial 相关
mpg <- subset(ggplot2::mpg, class == 'midsize' | class == 'compact')
polys <- polyserial(mpg$cty, mpg$class, std.err = TRUE)
polys

#Point-Biserial 相关
poi_biser <- biserial.cor(mpg$cty, mpg$class)
poi_biser

#Biserial 相关
biser <- biserial.cor(mtcars$mpg, mtcars$vs)
biser

# 变量相关性的显著性检验
# 
# 通常来讲，相关性分析是一种用于描述变量关联程度的探索性分析方法，而非确立因果关系的模型，不涉及假设检验过程。但如果有必要，仍可以计算相关系数的显著性，评估哪些变量间的关联程度是更重要的。
# 一些R包提供了计算变量间相关系数显著性的方法。此外，也可以自写函数获得，见下文。  
# psych包的方法
# 
# 计算相关矩阵及显著性水平。
library(psych)

#所有变量间相关系数的对称矩阵
corr_matrix <- corr.test(mtcars, method = 'pearson')
corr_matrix$r    #相关矩阵
corr_matrix$p    #p 值矩阵

#相关图，只展示 p < 0.05 的相关系数
library(corrplot)

col1 <- colorRampPalette(c('blue4', 'blue', 'white', 'orange', 'red3'))
corrplot(corr_matrix$r, p.mat = corr_matrix$p, sig.level = 0.05, insig = 'blank', method = 'number',
         diag = FALSE, col = col1(21), tl.cex = 1)
corrplot(corr_matrix$r, p.mat = corr_matrix$p, sig.level = 0.05, insig = 'blank', method = 'circle',
         add = TRUE, type = 'upper', diag = FALSE, col = col1(21), tl.pos = 'n', cl.pos = 'n')

#自定义筛选，例如选择 |r| >=0.7，p < 0.05 的结果，将不满足条件的相关系数值赋值为 0 后输出
corr_matrix$p[corr_matrix$p >= 0.05] <- -1
corr_matrix$p[corr_matrix$p < 0.05 & corr_matrix$p >= 0] <- 1
corr_matrix$p[corr_matrix$p == -1] <- 0

corr_matrix$r[abs(corr_matrix$r) < 0.7] <- 0
corr_matrix$r <- corr_matrix$r * corr_matrix$p
write.table(corr_matrix$r, 'corr_matrix_select.txt', sep = '\t', col.names = NA, quote = FALSE)

#给定两组变量间相关系数的非对称矩阵
x <- as.matrix(mtcars[c('mpg', 'cyl', 'disp', 'hp')])
y <- as.matrix((mtcars[c('drat', 'wt', 'qsec')])
               
               corr_matrix <- corr.test(x, y, method = 'pearson')
               corr_matrix$r    #相关矩阵
               corr_matrix$p    #p 值矩阵
               
               #相关图，只展示 p < 0.05 的相关系数
               col1 <- colorRampPalette(c('blue4', 'blue', 'white', 'orange', 'red3'))
               corrplot(corr_matrix$r, p.mat = corr_matrix$p, sig.level = 0.05, insig = 'blank',
                        method = 'square', addCoef.col = 'black', col = col1(21), number.cex = 0.8, tl.cex = 1.2)
               
                 #Hmisc包的方法
                 
                 #计算相关矩阵及显著性水平。
                 library(Hmisc)
               
               #所有变量间相关系数的对称矩阵
               rcorr_matrix <- rcorr(as.matrix(mtcars), type = 'pearson')
               rcorr_matrix$r    #相关矩阵
               rcorr_matrix$P    #p 值矩阵
               
               #给定两组变量间相关系数的非对称矩阵
               x <- as.matrix(mtcars[c('mpg', 'cyl', 'disp', 'hp')])
               y <- as.matrix((mtcars[c('drat', 'wt', 'qsec')]))
               
               rcorr_matrix <- rcorr(x, y, type = 'pearson')
               rcorr_matrix$r    #相关矩阵
               rcorr_matrix$P    #p 值矩阵
               
               #相关图、自定义结果筛选等，参考上述
               
# 手写置换检验程序
#                
# 置换检验是个百搭的非参数检验方法，相关系数的显著性可根据置换检验的原理获得。
# 上述提到的所有相关系数，包括Polychoric、Tetrachoric、Polyserial、(Point-)Biserial等，如果找不到计算显著性的R包，不妨考虑手写函数计算，其实并不难。
#  
               
 #计算观测值的相关系数（cor0），还是以 Pearson 相关为例，其它类似
cor0 <- cor(mtcars, method = 'pearson')
               
#随机置换数据 999 次，计算每次置换后数据计算的相关系数（corN），并统计 |corN|>|cor0| 的频数
p_num <- cor0
p_num[abs(p_num)>0] <- 1
               
set.seed(123)
for (i in 1:999) {
                 random <- apply(mtcars, 2, sample)
                 corN <- cor(random, method = 'pearson')
                 
                 corN[abs(corN) >= abs(cor0)] <- 1
                 corN[abs(corN) < abs(cor0)] <- 0
                 p_num <- p_num + corN
               }
               
#p 值矩阵，即 |corN|>|cor0| 的概率
p <- p_num/1000
p
               
 #相关图比较，仅显著（p < 0.05）的相关系数标以背景色
#左图为手写的置换检验结果，右图为 psych 包获得的结果，二者是一致的
library(corrplot)
library(psych)
cor_psych <- corr.test(mtcars, method = 'pearson')
               
layout(matrix(c(1,2), 1, 2, byrow = TRUE))
 corrplot(cor0, method = 'square', type = 'lower', p.mat = p, sig.level = 0.05, insig = 'blank',
                 addCoef.col = 'black', diag = FALSE, number.cex = 0.8, tl.cex = 0.8)
        corrplot(cor_psych$r, method = 'square', type = 'lower', p.mat = cor_psych$p, sig.level = 0.05, insig = 'blank',
       addCoef.col = 'black', diag = FALSE, number.cex = 0.8, tl.cex = 0.8)
               