##2019.4.26
#画根号2
#https://mp.weixin.qq.com/s/jC4wfZI2TgZRiC2fS_s8PQ

dd <- data.frame(x=c(0,1,1,0), y=c(0,0,1,0))
pp <- ggplot(dd,aes(x,y))
pp <- pp +
  geom_point() + 
  geom_path() +
  xlim(-0.2,1.5) +
  ylim(-0.2,1.5)
pp

xc <- seq(1, sqrt(2), 0.001)
yc <- sqrt(2-xc^2)
xc <- c(xc,sqrt(2))
yc <- c(yc,0)
d2 <- data.frame(x = xc, y = yc)

pp <- pp + 
  geom_path(data=d2) + 
  annotate("line", x=c(-0.1,1.5),y=c(0,0)) + 
  geom_point(aes(x=sqrt(2),y=0)) +
  annotate("text",
           x=c(0, 1, sqrt(2)), 
           y =c(-0.06, -0.06, -0.06), 
           label=c(0, 1, as.character(expression(sqrt(2)))), 
           parse=TRUE) + 
  xlab("") + 
  ylab("") + 
  theme_bw() 

print(pp)
