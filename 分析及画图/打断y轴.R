#2019.12.9

#打断y轴  R语言-断轴图(二)

#https://mp.weixin.qq.com/s/c3Rw_s6gzZbAit-Wp3r9NQ

##gg.gap也可以！


mtcars
library(ggplot2)
#绘制barplot图
bar_plot <- ggplot(mtcars, aes(cyl, disp, group = cyl)) +
  scale_fill_manual(values = c("red","green","blue"))+
  geom_col(position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = disp - wt, ymax = disp + wt), width = 0.5, position=position_dodge(0.8)) +
  labs(x = NULL, y = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank())
bar_plot
#去掉背景等操作
bar_plot+ theme_bw() +
  theme(panel.border = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),axis.line = element_line(colour = "black"))


#使用coord_cartesian() 分割bar_plot为两部分split1，split2
split1 <- bar_plot + theme_bw() +
  theme(panel.border = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),axis.line = element_line(colour = "black")) +coord_cartesian(ylim = c(0,300)) + theme(legend.position='none')
split2 <- bar_plot + theme_bw() +
  theme(panel.border = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +coord_cartesian(ylim = c(400, 500)) + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.line.y.left =element_line(colour = "black"),legend.position = c(0.95, 0.7))


#使用 grid 包组合分割的split1，split2
library(grid)
#保存为bar_plot.split.png图片
png('bar_plot.split.png', width = 2500, height = 1700, res = 300, units = 'px')
#新建组合画板
grid.newpage()
plot_site1 <- viewport(x = 0, y = 0, width = 1, height = 0.4, just = c('left','bottom'))
plot_site2 <- viewport(x = 0, y = 0.4, width = 1, height = 0.3, just = c('left','bottom'))
#输出
print(split1, vp = plot_site1)
print(split2, vp = plot_site2)
dev.off()

