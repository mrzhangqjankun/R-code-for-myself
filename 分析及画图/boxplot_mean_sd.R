##2019.2.21
#图层中的统计量，你自己说的算！ 
#https://mp.weixin.qq.com/s/yvfdIvtN7aNHDgdSNVCIRw

library(ggplot2)

pups <- nlme::RatPupWeight %>% 
  janitor::clean_names() %>%
  dplyr::mutate(litter = as.integer(litter))

p <- ggplot(pups, aes(x = litter, y = weight, color = treatment))

#针对某一个x值，我们可以画出相应y的统计量，比如均值、方差等，
#这些都可以“手工”的方式通过图层叠加来实现。
p + geom_jitter(shape=1) + 
  stat_summary(fun.y = "mean", size=2, geom="point", color = 'black') + 
  stat_summary(fun.data = mean_cl_normal, geom="errorbar", width=.1, color='black')

##mean-sd, mean, mean+sd来画boxplot
f <- function(y) {
  r <- quantile(y, probs = c(0.05, 0.25, 0.5, 0.75, 0.95))
  r[2] <- mean(y) - sd(y) 
  r[3] <- mean(y)
  r[4] <- mean(y) + sd(y)
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}
#?quantile
p1 <- p + stat_summary(fun.data=f, geom='boxplot', width=.8) ;p1
p2 <- p + geom_boxplot(aes(group = litter));p2
cowplot::plot_grid(p1, p2, 
                   labels = c("mean +- sd", "quantile"))


#使用stat_smooth(method = "lm", formula = y ~ 1)，可以展示某一x取值区间的平均y值。
p + geom_point() + stat_smooth(method = "lm", formula = y ~ 1)

#如果要计算中位数的话，可以使用rlm：
library(MASS)
p + geom_point() + stat_smooth(method = "rlm", formula = y ~ 1)
??MASS
?rlm