##2019.11.21

##https://mp.weixin.qq.com/s/Ro1rbcOjSEyXZIj-p5T7cA

###原文中有P value分布的含义，值得一看

library(qvalue)
data(hedenfalk)
pvalues <- hedenfalk$p
qobj <- qvalue(p=pvalues)
summary(qobj)

#估计原假设 (H0 null hypothesis)的整体比例 (π0)，q-value与p-value的关系, 
#qvalue即是定义某一个检验统计显著需要承受的最小假阳性率值。
#lfdr指在给定的p-value条件下，原假设 (H0)为真的后验概率值。

hist(qobj)

?qvalue


# import data
data(hedenfalk)
p <- hedenfalk$p

# get q-value object
qobj <- qvalue(p)
plot(qobj)
hist(qobj)

# options available
qobj <- qvalue(p, lambda=0.5, pfdr=TRUE)
qobj <- qvalue(p, fdr.level=0.05, pi0.method="bootstrap", adj=1.2)
