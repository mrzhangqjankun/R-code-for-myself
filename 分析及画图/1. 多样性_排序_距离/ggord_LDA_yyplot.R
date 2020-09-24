##2018.1.11
##比PCA更好用的监督排序—LDA分析、作图及添加置信-ggord
##https://mp.weixin.qq.com/s?__biz=MzUzMjA4Njc1MA==&mid=2247484846&idx=1&sn=488356de7f985bf929f1e31bc29affe7&chksm=fab9ef1fcdce660
##https://github.com/fawda123/ggord

rm(list=ls(all=TRUE))
# library(devtools)
# install_github('fawda123/ggord')
library(ggord)
?ggord

ord <- prcomp(iris[, 1:4])

p <- ggord(ord, iris$Species)
p

p <- ggord(ord, iris$Species, cols = c('purple', 'orange', 'blue'))
p

library(ggplot2)
p + scale_shape_manual('Groups', values = c(1, 2, 3))

p + theme_classic()

p + theme(legend.position = 'top')

# transparent ellipses椭圆透明
p <- ggord(ord, iris$Species, poly = FALSE)
p

# observations as labels from row names
p <- ggord(ord, iris$Species, obslab = TRUE)
p

# faceted by group
p <- ggord(ord, iris$Species, facet = TRUE, nfac = 1)
p

# 加载lda包
library(MASS)

# 按物种分组LDA排序
ord <- lda(Species ~ ., iris, prior = rep(1, 3)/3)

# 展示LDA分析
library(ggord)
p <- ggord(ord, iris$Species)
p

# 计算置信椭圆函数

get_lda_ell <- function(ord_in, grp_in, ellipse_pro = 0.97){
  ## adapted from https://github.com/fawda123/ggord/blob/master/R/ggord.R
  require(plyr)
  axes = c('LD1', 'LD2')
  obs <- data.frame(predict(ord_in)$x[, axes])
  obs$Groups <- grp_in
  names(obs)[1:2] <- c('one', 'two')
  theta <- c(seq(-pi, pi, length = 50), seq(pi, -pi, length = 50))
  circle <- cbind(cos(theta), sin(theta))
  ell <- ddply(obs, 'Groups', function(x) {
    if(nrow(x) <= 2) {
      return(NULL)
    }
    sigma <- var(cbind(x$one, x$two))
    mu <- c(mean(x$one), mean(x$two))
    ed <- sqrt(qchisq(ellipse_pro, df = 2))
    data.frame(sweep(circle %*% chol(sigma) * ed, 2, mu, FUN = '+'))
  })
  names(ell)[2:3] <- c('one', 'two')
  ell <- ddply(ell, .(Groups), function(x) x[chull(x$one, x$two), ])
  ell
}

# 计算置信椭圆，并添加至原图
anotherEll <- get_lda_ell(ord, iris$Species, 0.97)
## Loading required package: plyr
p + geom_polygon(data = anotherEll, 
                 aes_string(color = 'Groups', group = 'Groups'),
                 lty=2, fill = NA)

##https://mp.weixin.qq.com/s?__biz=MzI5NjUyNzkxMg==&mid=2247485343&idx=1&sn=81805d75eb0a05e15594ee7f7bc92d41&chksm=ec43b6d8db343fce343be9525d1da97ed17c8e4428e9f60cd010308382fdc838bffc56aece06&scene=0&pass_ticket=5axKwaX%2FyLltppczPECMViVqxBWcUl1KGgQIts8K7d3BBGcqhgqdVF7HZizpi61K#rd

##画个小圈圈 Y叔
library(MASS)
ord <- lda(Species ~ ., iris, prior = rep(1, 3)/3)
library(ggord)
p <- ggord(ord, iris$Species)
p

##geom_ord_ellipse这个图层呢，随便你加置信区间，想加几个就加几个。这个图层支持所有排序算法
library(yyplot)

p + geom_ord_ellipse(ellipse_pro = .96, color='firebrick', size=1, lty=3) +
  geom_ord_ellipse(ellipse_pro = .99, lty=2) 
##会报错could not find function "geom_ord_ellipse"，重新装一下yyplot就好了。这是新加的功能

?geom_ord_ellipse

yyplot::geom_ord_ellipse()

devtools::install_github("GuangchuangYu/yyplot")

###2019.1.8
#加环境因子和箭头
https://mp.weixin.qq.com/s/yFz9ILcv7zdYKdVQyK02Fg
p <- ggord(ord, iris$Species,arrow=0.3, txt=5,repel=T)  #iris除去species其余4列作为环境因子
p


