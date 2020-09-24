##2019.4.12
#https://mp.weixin.qq.com/s/p38EPM6rV0gmwOj5FpBf4A
##basicTrendline:CRAN发布的线性非线性拟合的R函数包介绍 

#添加线性或非线性拟合线，在图上显示回归方程及R2和回归模型的p值（不是参数的p值）

#install.packages("basicTrendline")
library(basicTrendline)

x<-c(1,30,90,180,360)
y<-c(4,8,10,11,11)
trendline(x,y,model="exp3P", summary=TRUE, paramDigit=10, legendPos="topleft",linecolor="red")  

#只需改变参数 model 的值，即可输出不同的回归模型的结果以及图。
# “line2P”    # y=a*x+b
# “line3P”    # y=a*x^2+b*x+c
# “log2P”     # y=a*ln(x)+b
# “exp3P”     # y=a*exp(b*x)+c
# “power3P”   # y=a*x^b+c)

?trendline()
