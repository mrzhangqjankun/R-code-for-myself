##li 2017.7.5 
##http://www.omicshare.com/forum/thread-414-1-1.html
##[R语言] R语言绘图教程之频率直方图

rm(list=ls(all=TRUE))
setwd('D:/文件存放/galaxy pipeline/galaxy/自己整理代码/')

##set.seed(200)  ##设置随机�?

x = rnorm(1000)
hist(x)
hist(x,breaks=30)  ##设置区段数量,30个长方形
hist(x,breaks=c(-4,-3,-2,-1,0,1,2,3,4))  ##设置区段的位�?

curve(dnorm(x),add=T,col="red") #��������


##在使用ggplot2之前需要先导入ggplot2包，且同时需要将数据转换成数据框的格式�?
library(ggplot2)

data = data.frame(x)
ggplot(data,aes(x=x)) + geom_histogram()

ggplot(data,aes(x=x)) + geom_histogram(binwidth = 1) ##设置区间的大小，1为长度的长方�?

ggplot(data,aes(x=x)) + geom_histogram(bins = 20)    ##设置区间的数�? 20个长方形

##给频率直方图添加频数�?
res = hist(x,breaks=20,plot=FALSE)           ##plot=FALSE表示只进行运算，而不绘图
print(res)                 

hist(x,breaks=20,ylim=c(0,200))   ##ylim  y轴范�?
## 通过text()将上述res中的count值加到频率直方图�?,+10是为了防止字符与柱子重叠�?
text(res$mids,res$counts+10,labels=res$counts)  
##res$mids是长方形中间的位置�?

##ggplot2绘制含字符的柱状�?
##ggplot2调整字符的位置比较方便，不需要调整y轴的值，可以直接通过参数vjust实现�?
data = data.frame(mids=res$mids,counts=res$counts)
ggplot(data,aes(x=mids,y=counts)) + geom_bar(stat="identity") + geom_text(aes(label=counts),vjust=-1) + ylim(0,220)




