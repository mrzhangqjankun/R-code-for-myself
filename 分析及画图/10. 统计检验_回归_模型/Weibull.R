##2019.3.9
#weibull3分布
#R画函数图像
#fitdistr极大似然拟合


##The Weibull distribution with shape parameter a and scale parameter b has density given by
#a是形状参数，b是尺度参数
#这个公式和文献里的不一样
f(x) = (a/b) (x/b)^(a-1) exp(- (x/b)^a)
x=-10:10
a = 2; b = 1
y = (a/b) * (x/b)^(a-1) * exp(- (x/b)^a)
plot(x,y,type='l',col='red',lwd=2)

###文献：S = a*[1-exp(-b*A^c)]  a为生态学中的c，b为生态学中的z，c为f
#这个公式的结果比较符合
A = 0:100
a = 1; b = 0.05;c = 1   
##c离1越远曲线都会平的越快；a对于曲线的形状无影响，是理论的最大值。
##b越大平的越快。
S = a*(1-exp(-b*A^c))
plot(A,S,type='l',col='red',lwd=2)



###两种画函数的方法
#1.curve
curve(x^2, from = -10, to = 10, n=1000)
curve(exp(x), from = -10, to = 10, n=1000)
#2.plot
x=-100:100 
y=x**3+sin(x)*cos(x) 
plot(x,y,type='l',col='red',lwd=2)
#3.ggplot也可以
library(ggplot2)
A = 0:100
data = as.data.frame(cbind(A,S));colnames(data) = c("A","S");data
p = ggplot(data,aes(x=A,y=S)) + geom_smooth(method="auto",formula = y ~ 1*(1-exp(-0.05*x^1)));p
#?geom_smooth

#mod<-loess(formula = S ~ a*(1-exp(-b*A^c)), start = list(a=1, b= 0.05,c=1))
mod<-loess(formula = S ~ 1*(1-exp(-0.05*A^1)))
#mod$
# coef(mod)
# ?coef

fit = fitdistr(c(1:10), "weibull")
fit$estimate;fit$sd;fit$loglik;fit$n;fit$vcov

#####################逻辑斯的曲线
A=0:100
a = 1
b = 0.1
c = 1
S = a/(1+exp(-A*b + c))
plot(A,S,type='l',col='red',lwd=2)


library(MASS)
?fitdistr  #Maximum-likelihood Fitting of Univariate Distributions
#单变量分布的极大似然拟合

#fitdistr(x, densfun, start, ...)

#"beta", "cauchy", "chi-squared", "exponential", "gamma", 
#"geometric", "log-normal", "lognormal", "logistic", "negative binomial",
#"normal", "Poisson", "t" and "weibull"

## avoid spurious accuracy
op <- options(digits = 3)
set.seed(123)
x <- rgamma(100, shape = 5, rate = 0.1)
fitdistr(x, "gamma")
## now do this directly with more control.
fitdistr(x, dgamma, list(shape = 1, rate = 0.1), lower = 0.001)

set.seed(123)
x2 <- rt(250, df = 9)
fitdistr(x2, "t", df = 9)
## allow df to vary: not a very good idea!
fitdistr(x2, "t")
## now do fixed-df fit directly with more control.
mydt <- function(x, m, s, df) dt((x-m)/s, df)/s
fitdistr(x2, mydt, list(m = 0, s = 1), df = 9, lower = c(-Inf, 0))

set.seed(123)
x3 <- rweibull(100, shape = 4, scale = 100) ;?rweibull
fitdistr(x3, "weibull")

set.seed(123)
x4 <- rnegbin(500, mu = 5, theta = 4)
fitdistr(x4, "Negative Binomial")
options(op)


##从多条回归线如何取得总的回归线？
#将每次的y平均，然后回归
#单独回归，系数平均