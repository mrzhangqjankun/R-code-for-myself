##2019.11.27

##ggplot2扩展包gg.gap,截取x轴用facet()，截取y轴交给gg.gap

##https://mp.weixin.qq.com/s/hQwl7bni3hWU4VAOfgpA0A

install.packages("gg.gap")

#理论上，ggplot2的图都能切，想切几段切几段。
library(gg.gap);?gg.gap()

data(mtcars)
library(ggplot2)
p<-ggplot(data = mtcars, aes(x = gear, fill = gear)) +
  geom_bar() +
  ggtitle("Number of Cars by Gear") +
  xlab("Gears")
p
#single segments and missing tick_width
gg.gap(plot=p,
       segments=c(5,10),
       ylim=c(0,50))
#tick_width can be one or more numbers
gg.gap(plot=p,
       segments=c(5,10),
       tick_width = c(1,10),
       ylim=c(0,50))
#segments list cantains more than one number vectors
gg.gap(plot=p,
       segments=list(c(2.5,4),c(5,10)),
       tick_width = c(1,0.5,10),
       ylim=c(0,50))
## rel_heights can set the relative height for segments and segmented y-axis
gg.gap(plot=p,
       segments=list(c(2.5,4),c(5,10)),
       tick_width = c(1,0.5,10),
       rel_heights=c(0.2,0,0.2,0,1),
       ylim=c(0,50))
## reversed y-axis
p<-ggplot(data = mtcars, aes(x = gear, fill = gear)) +
  geom_bar() +
  ggtitle("Number of Cars by Gear") +
  xlab("Gears")+
  scale_y_continuous(trans = 'reverse')
p
## single segments and missing tick_width
gg.gap(plot=p,
       segments=c(10,5),
       ylim=c(15,0))
## for facet() 
#devtools::install_github("ChrisLou-bioinfo/gg.gap")
#library(ggplot2)
p<-ggplot(mtcars,aes(mpg,hp))+geom_point()
p
p1<-p+facet_wrap(~cyl,scales="free")
p1
gg.gap(plot = p1,ylim = c(60,200),segments = c(100,120))
