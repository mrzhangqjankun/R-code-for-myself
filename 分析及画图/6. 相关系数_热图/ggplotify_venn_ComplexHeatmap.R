##2019.8.22

##ggplotify - version 0.0.4

##https://mp.weixin.qq.com/s/8nGnnV71ibftRHIzgXqbiw

library(eulerr)
library(magrittr)

df <- data.frame(A = c(1, 1, 0, 1, 0, 1),
                 B = c(0, 0, 1, 1, 1, 1),
                 C = c(1, 0, 1, 0, 1, 0))

u <- 
  df %>% 
  euler(.) %>%
  plot(.,
       main = "Test",
       quantities = TRUE,
       legend = list(side = "bottom")
  ) 
u

#devtools::install_github('jokergoo/ComplexHeatmap')
library(ComplexHeatmap)

p1 <- c(0.2, 0.2, 0.2, rep(0.02, 5), 0.15, 0.15)
p2 <- c(0.1, 0.1, 0.1, rep(0.02, 5), 0.3, 0.3)
n <- 10
dat1 <- rmultinom(n = n, size = 100, prob = p1)
dat2 <- rmultinom(n = n, size = 100, prob = p2)
dat <- cbind(dat1, dat2)

h <- Heatmap(dat, 
             cluster_rows = FALSE, 
             cluster_columns = FALSE, 
             show_column_names = FALSE,
             rect_gp = gpar(col = "white"),
             column_split = rep(LETTERS[1:2], each = 10))  
h

#remove.packages("ggplotify")
library(ggplotify);?as.ggplot()

cowplot::plot_grid(
  as.ggplot(u), as.ggplot(h),
  labels = LETTERS[1:2], rel_widths=c(1, .5))


##8.23

##https://mp.weixin.qq.com/s/1F3C-xt0n7wjdhSL0GEUqg

##让我们画个黑板报吧

require(ggplot2)
require(ggimage)

d = data.frame(x=LETTERS[1:3], y = 1:3)
d$y2 = rev(cumsum(rev(d$y)))

p = ggplot(d, aes(x=1, y, fill=x)) + geom_col(color='white') +  
  geom_bgimage("img/blackboard.jpg") + theme_void() +
  coord_polar("y") + theme(legend.position='none') + 
  geom_text(aes(y = y2 - y/2, label=x),family='xkcd', size=8) + 
  xlim(-1, NA) + scale_fill_viridis_d()


require(xkcd)
dataman <- data.frame( x= -1, y=0,
                       scale = 2,
                       ratioxy = 1,
                       angleofspine =  -pi/2 ,
                       anglerighthumerus = -pi/6,
                       anglelefthumerus = pi + pi/6,
                       anglerightradius = 0,
                       angleleftradius = pi/4,
                       angleleftleg = 3*pi/2  + pi / 12 ,
                       anglerightleg = 3*pi/2  - pi / 12,
                       angleofneck = 3 * pi / 2 + pi/10,
                       color = 'a'
)

mapping <- aes(x=x,
               y=y,
               scale=scale,
               ratioxy=ratioxy,
               angleofspine = angleofspine,
               anglerighthumerus = anglerighthumerus,
               anglelefthumerus = anglelefthumerus,
               anglerightradius = anglerightradius,
               angleleftradius = angleleftradius,
               anglerightleg =  anglerightleg,
               angleleftleg = angleleftleg,
               angleofneck = angleofneck,
               color = color)

g = ggplot() + xkcdman(mapping,dataman) + theme_void() + theme(legend.position="none") 
