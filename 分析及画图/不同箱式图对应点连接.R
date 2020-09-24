##2020.7.4

#######################两组
##成对点连接
x <- rnorm(10)
y <- rnorm(10)

d <- data.frame(x=x, y=y)
d$g <- rownames(d)
#tidyr一条指令，变成长数据用于ggplot2画图：
require(tidyr)
dd <- pivot_longer(d, 1:2)  #gather的更新版函数
?pivot_longer
#
require(ggplot2)
ggplot(dd, aes(name, value)) + geom_boxplot() +
  geom_line(aes(group=g), color='firebrick') +
  geom_point(aes(color=name), size=3)

########################四组
t1 <- rnorm(10)
t2 <- rnorm(10) + 1
t3 <- rnorm(10) + 2
t4 <- rnorm(10) + 3

d <- data.frame(t1=t1, t2=t2, t3=t3, t4=t4)
d$g <- rownames(d)
require(tidyr)
dd <- pivot_longer(d, 1:4)

#画图代码，原封不动，完全不用改：
require(ggplot2)
ggplot(dd, aes(name, value)) + geom_boxplot() +
  geom_line(aes(group=g), color='firebrick') +
  geom_point(aes(color=name), size=3)

##更进一步，上升的线画红色，下降的画蓝色
##预先算好，分组上色就行了