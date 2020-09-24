##2019.9.25

##只有相关系数，如何算显著性p值 

##http://wap.sciencenet.cn/blog-267448-1044024.html

?pt  #The Student t Distribution
#如果 r是负值，
pt(r*sqrt((n-2)/(1-r^2)),n-2)*2
#如果 r是正值
(1-pt(r*sqrt((n-2)/(1-r^2)),n-2))*2

##计算两个矩阵中不同变量相关系数和p值的函数 

##http://wap.sciencenet.cn/blog-267448-1128925.html

#有两个矩阵，一个物种矩阵，一个是环境因子矩阵，要算所有物种和所有环境因子之间的俩俩
#相关系数和p值，找不到现成的函数做，写循环又比较繁琐。我简单写了函数解决这个问题，供大家参考！
#以vegan包中vare物种数据和环境因子数据为例
library(vegan)
data(varespec)
data(varechem)
r=cor(varespec,varechem)#计算相关系数矩阵
r1=r[r>=0] #找出大于等于0的相关系数
r2=r[r<0]  #找出小于0的相关系数
n=nrow(varespec) #算样本量
p1=(1-pt(r1*sqrt((n-2)/(1-r1^2)),n-2))*2 #计算大于等于0的相关系数的p值
p2=pt(r2*sqrt((n-2)/(1-r2^2)),n-2)*2 #计算小0的相关系数的p值
pvalue=r  #构建pvalue矩阵，让它首先等于相关系数矩阵
pvalue[r>=0]=p1 #pvalue矩阵中r大于等于0地方等于p1
pvalue[r<0]=p2  #pvalue矩阵中r小于0地方等于p2
pvalue
#write.csv()写出相关系数矩阵r和p值矩阵pvalue