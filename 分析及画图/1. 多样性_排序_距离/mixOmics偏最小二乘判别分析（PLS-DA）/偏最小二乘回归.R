install.packages("pls")

library(pls)

n <- 50

x1 <- rnorm(n); xx1 <- scale(x1)

x2 <- rnorm(n); xx2 <- scale(x2)

y <- x1 + x2 + rnorm(n,0,0.1); yy <- scale(y)

p <- plsr(yy ~ xx1+xx2, ncomp=1)

#这一步是PLS的语句，xx1，xx2代表解释变量，这里是两个解释变量，也可以多个解释变量，yy是响应变量，可以是向量，也可以是矩阵，其实根本不必看前面这些代码， PLS就这个代码就能搞定，不必管上下这些代码，都是解释性的。

#

( w <- loading.weights(p) )#看这个主成分与原始xx1与xx2之间的回归系数

a <- w["xx1",]

b <- w["xx2",]

a^2+b^2

cor(y, a*xx1+b*xx2)#a,b是如何确定的，就是让这个回归系数最大，也就是带有主成分回归模型R方最大

p$coef #这样就获取了yy跟xx1与xx2之间的回归系数的回归系数，如何获取的，请看下面的分析

x <- a*xx1+b*xx2

coef(lm(yy~0+x))

coef(lm(yy~0+x))*a

coef(lm(yy~0+x))*b

#有了上面这个系数，可以顺利写出yy跟xx1与xx2之间的回归回归方程