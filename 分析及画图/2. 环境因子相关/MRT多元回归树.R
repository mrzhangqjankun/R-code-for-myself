##2018.4.17
##多元回归树分析Multivariate Regression Trees，MRT 
##https://mp.weixin.qq.com/s?__biz=MzUzMjA4Njc1MA==&mid=2247485476&idx=1&sn=908a505054c22faae02a3af5f7d054a8&chksm=fab9e295cdce6b83d5c91c3d78b4934f6c90e8a5acbd12672167720c42aa4d907f8f5af7c75f&scene=0#rd

rm(list=ls(all=TRUE))
?mvpart
library("mvpart")
#调用程序包自带数据集spider
#spider有28行18列，前12列为不同种蜘蛛的多度数据，剩余的为环境数据
data(spider) 

#defaults,"."代表env中所有变量
fit<-mvpart(data.matrix(spider[,1:12])~herbs+reft+moss+sand+twigs+water,spider)

#设定xv="1se"，根据“1SE”准则自动选择最优分类方案，与默认结果相同，因为默认选择1SE
fit<-mvpart(data.matrix(spider[,1:12])~herbs+reft+moss+sand+twigs+water,spider,xv="1se") 

#xv="min",选择具有最小CVRE值的回归树
fit<-mvpart(data.matrix(spider[,1:12])~herbs+reft+moss+sand+twigs+water,spider,xv="min")

#xv="pick",允许通过人机交互方式从函数提供的误差图中选择自己认为合适的分组
fit<-mvpart(data.matrix(spider[,1:12])~herbs+reft+moss+sand+twigs+water,spider,xv="pick") 
#运行函数生成误差图，绿色为相对误差，蓝色为CVRE，红色水平线指示最小CVRE（大红点）的1个标准差范围。红点为具有最小CVRE的分类方案。橙点为最接近1SE准则的分组。

#根据“1SE”准则自动选择最优分类方案，以pca图的形式展示分组
fit <- mvpart(data.matrix(spider[,1:12])~herbs+reft+moss+sand+twigs+water,spider,xv="1se",pca=TRUE)
