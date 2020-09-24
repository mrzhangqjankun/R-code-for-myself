library(ggplot2)

#data(diamonds)

#small <- diamonds[sample(nrow(diamonds), 1000), ]
#head(small)
eff_size <- read.table("E:/桌面/forestdata.txt", header = T,sep = "\t")
eff_size
p <- ggplot(eff_size, mapping=aes(x=A,y=y),shape=3) 
#p+geom_point()

#p <- ggplot(data=small, mapping=aes(x=carat, y=price, shape=cut, colour=color))
sp <- p+geom_point()
sp<- sp+geom_point(eff_size, mapping=aes(x=B,y=y),shape=1)
sp<- sp+geom_point(eff_size, mapping=aes(x=C,y=y),shape=2)
sp

for (i in 1:nrow(eff_size)){ 
  sp <- sp + geom_segment(aes_string(x = eff_size$Lower[i], y = eff_size$y[i], 
                              xend = eff_size$Upper[i], yend = eff_size$y[i]),
                          color="black")
}
sp

###aes只会画循环中的最后一个，而aes_string可以全部都画出来！

###优化
library(reshape2)
eff.ggplot = melt(eff_size,
                     id.vars = c("species","Lower","Upper","y"),#需要保留不参与聚合的变量,
                     measure.vars = c("A","B","C"),#用于聚合的变量,
                     variable.name='group',
                     value.name='values')  
head(eff.ggplot)

plot_theme = theme(panel.background=element_blank(),
                   panel.grid=element_blank(),
                   axis.line.x=element_line(size=.5, colour="black"),
                   axis.line.y=element_line(size=.5, colour="black"),
                   axis.ticks=element_line(color="black"),
                   axis.text=element_text(color="black", size=20),
                   legend.position="right",
                   legend.background=element_blank(),
                   legend.key=element_blank(),
                   legend.text= element_text(size=20),
                   text=element_text(family="sans", size=20)
)

p = ggplot(data=eff.ggplot,aes(values,y,col=group))+
    geom_point(shape=group,size=3);p
for (i in 1:nrow(eff.ggplot)){ 
  p <- p + geom_segment(aes_string(
                x = eff.ggplot$Lower[i], y = eff.ggplot$y[i], 
                xend = eff.ggplot$Upper[i], yend = eff.ggplot$y[i]),
                color="tomato3",size=1)
};
p = p + geom_vline(xintercept=0,color="tomato3");p
#p = p + xlim(-1,max(eff.ggplot$values));p
left = eff_size[eff_size$Lower>0,]
right = eff_size[eff_size$Upper<0,]

#?annotate
#hjust=0将左对齐，0.5将居中，1将右对齐。
p = p + annotate("text",x=0.15,y=right$y,label=right$species,hjust=0)+
        annotate("text",x=-0.1,y=left$y,label=left$species,hjust=1)+
          annotate("text", x=-0.01, y=22, label="Control enriched", size=5,hjust=1)+
          annotate("text", x=0.01, y=22, label="Carcinoma enriched", size=5,hjust=0);p
p = p + labs(title="Forest plot", 
              subtitle="Multi- effect size for each species",
              caption="By 水岸风堤",
              x="Effect size",
              y="Species")+
        plot_theme;p
dev.off()

#?geom_text
