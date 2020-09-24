##2019.6.12
##听说你想画好几颗树在一起
#https://mp.weixin.qq.com/s/Hx9fI-JaMN7gY_vElQiQKg

#用foritfy方法，它调用了tidytree::as_tibble把树对象转化为tidy data frame，
#再加入用于画图的坐标轴信息，另外你也可以用ggtree(tree)$data来获取相同的数据，
#有了这数据，就可以随便玩了。

#下面是一个示例，画两颗树面对面，如同cophyloplot一般。

library(dplyr)
library(ggtree)

x <- rtree(30)
y <- rtree(30)
p1 <- ggtree(x);p1
p2 <- ggtree(y);p2

d1 <- p1$data
d2 <- p2$data

## reverse x-axis and 
## set offset to make the tree in the right hand side of the first tree
d2$x <- max(d2$x) - d2$x + max(d1$x) + 1

pp <- p1 + geom_tiplab() + geom_tree(data=d2) + geom_tiplab(data = d2, hjust=1);pp
?geom_tiplab #add tip label layer
?geom_tree #add tree layer

dd <- bind_rows(d1, d2) %>%   #合并
  filter(!is.na(label))

pp + geom_line(aes(x, y, group=label), data=dd, color='grey')

#如果你想画好几颗树呢，比如把流感8段基因分别做树，画在一起，
#把相同的病毒株用线条连接起来，像问题中的图那样，这也是挺容易做到的。

z <- rtree(30)
d2 <- fortify(y)
d3 <- fortify(z)
d2$x <- d2$x + max(d1$x) + 1
d3$x <- d3$x + max(d2$x) + 1

pp <- p1 + geom_tree(data = d2) + geom_tree(data = d3);pp

dd = bind_rows(d1, d2, d3) %>% 
  filter(!is.na(label))

pp + geom_line(aes(x, y, group=label, color=node < 15), data=dd, alpha=.3)
