

#2019.6.6

#画点之组内再分组
#https://mp.weixin.qq.com/s/id9ZKQXn3UFtwaY1QKix8A

library(ggplot2)

d = subset(diamonds, color %in% c("D", "E"))
ggplot(d, aes(cut, carat, fill = color, color = color)) +
  geom_violin(alpha = .3) +
  geom_jitter(aes(group = color),
              position = position_jitterdodge(jitter.width = 0.5,
                                              dodge.width = 0.9),
              alpha = .3) +
  theme_bw() +
  scale_color_discrete()
