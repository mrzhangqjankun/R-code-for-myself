##2019.11.2

##ggplot2小技巧-数据没有，函数倒是有一个

##https://mp.weixin.qq.com/s/isw1ZHkYIMseF6dK9CXA5g

set.seed(2019-10-23)
d <- data.frame(val=abs(rnorm(20)), 
                type=rep(c('A', 'B'), 10))
d

library(ggplot2)
ggplot(d, aes(type, val, colour=type)) + geom_point()

#position_nudge去调整一个位移
ggplot(d, aes(type, val, colour=type)) + geom_point() +
  geom_boxplot(aes(fill=type), colour = 'black', width=.2, 
               position=position_nudge(x=-.2))

#图层的data这个参数中定义一个函数，
#然后这个函数会去应用于ggplot2中传入的data，而计算出来的统计量，
#就会被用于这个图层去画图

library(ggplot2)
library(dplyr)
ggplot(d, aes(type, val, colour=type)) + geom_point() +
  geom_boxplot(aes(fill=type), colour = 'black', width=.2, 
               position=position_nudge(x=-.2)) +
  geom_pointrange(data = function(d) {
    group_by(d, type) %>%
      summarize(mean=mean(val), sd=sd(val))
  }, aes(y=mean, ymin=mean-sd, ymax=mean+sd), 
  position=position_nudge(x=.2), size=2)

