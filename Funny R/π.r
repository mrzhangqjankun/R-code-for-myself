##2018.8.13

##https://mp.weixin.qq.com/s?__biz=MzI5NjUyNzkxMg==&mid=2247486301&idx=1&sn=ca43d0eed8e2138ec9d967e27630b256&chksm=ec43ba1adb34330c752f0eba3df5184bdeb3b9da344287b019a33e8e8be9e6f636ceba2205f0&scene=0#rd
##使用蒙特-卡罗方法来估计Pi 

#圆和外接正方形的面积比，是π/4.
#通过这一比值，可以使用蒙特-卡罗方法来估计Pi，这是Monte Carlo方法的最经典的一个例子。

##?runif 均匀分布随机数。均匀分布和π有关
getPI <- function(N) {
  x <- runif(N)
  y <- runif(N)
  hits <- sum(sqrt(x^2+y^2) < 1)
  pi <- 4*hits/N    
  return(pi)
}

options(digits=15)
set.seed(12345)
n <- c(seq(from=100, to=1000, by=100),
       seq(from=1000, to=10000, by=1000)
)

require(ggplot2)
p <- ggplot() ;p
p = p+ aes(x=n, y=sapply(n, getPI));p
p = p+ geom_point() ;p
p = p+ geom_hline(aes(yintercept=pi, colour="red")) +
  xlab("")+ ylab("") ;p
p = p+ theme(legend.position="none");p

print(p)
