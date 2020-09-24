##2019.11.21

##Maptree-层级结构数据展示的绝佳尝试

##https://mp.weixin.qq.com/s/7d3CeLvPKhEJgl0sH9cskA

# 载入包
library(ggraph);?ggraph  ##等同于ggplot2::ggplot()
library(igraph)
library(tidyverse)

# 提取边文件
edges <- flare$edges

# 提取节点文件
vertices <- flare$vertices

# 构建igraph对象
mygraph <- graph_from_data_frame( edges, vertices=vertices )

# 出图
ggraph(mygraph, layout = 'circlepack') + #该布局将层次结构显示为圆圈中的圆圈。在概念上等同于treemaps
  geom_node_circle() +
  theme_void()

#布局转化，只需要修改layout参数。
ggraph(mygraph, layout='dendrogram', circular=TRUE) + #在树形图中以树状图的形式显示节点，叶节点设置为0，父节点位于其最高的子节点之上1个单元。
  geom_edge_diagonal() +
  theme_void() +
  theme(legend.position="none")

#circular布局转换为径向表示
ggraph(mygraph, layout='dendrogram', circular=FALSE) +
  geom_edge_diagonal() +
  theme_void() +
  theme(legend.position="none")

#将圈树转化为水平展示
ggraph(mygraph, 'treemap', weight = 'size') +
  geom_node_tile(aes(fill = depth), size = 0.25) +
  theme_void() +
  theme(legend.position="none")

ggraph(mygraph, 'partition', circular = TRUE) +
  geom_node_arc_bar(aes(fill = depth), size = 0.25) +
  theme_void() +
  theme(legend.position="none")

#使用igraph网络布局展示
ggraph(mygraph) +
  geom_edge_link() +
  geom_node_point() +
  theme_void() +
  theme(legend.position="none")

#配色和标签设置
library(tidyverse)
library(viridis)

# 同样导入边和节点数据
edges=flare$edges
vertices = flare$vertices
str(edges)
head(edges)
##构造网络对象
mygraph <- graph_from_data_frame( edges, vertices=vertices )

#出图函数是ggraph，提供将igraph对象通过layout的定义转换为相应的ggplot坐标并输出数据框。
#这很重要，因为后面的geom都是基于这个数据框来出图的。
#出图函数geom_node_circle：同ggplot2中的geom对象一样的用法。
#不过这里的depth似乎不太好理解，因为这是转化后的自带列名。
#作用时将maptreee按照登记分为几个部分。填充上对应的数字。
#从0开始逐渐递增代表了层级越来越低。本次通过depth来对图形进行填充颜色。#
#scale_fill_manual用于指定填充颜色，values指定颜色，这是一组向量，
#需要使用目标填充列对颜色进行署名，来指定对应的颜色。
#scale_color_manual来定义边框颜色，这里除了最外层，都被设置成黑色了。

# Hide the first level (right)
ggraph(mygraph, layout = 'circlepack', weight="size") +
  geom_node_circle(aes(fill = as.factor(depth), color = as.factor(depth) )) +
  scale_fill_manual(values=c("0" = "white", "1" = viridis(4)[1], "2" = viridis(4)[2], "3" = viridis(4)[3], "4"=viridis(4)[4])) +
  scale_color_manual( values=c("0" = "white", "1" = "black", "2" = "black", "3" = "black", "4"="black") ) +
  theme_void() +
  theme(legend.position="FALSE")
# Second one: hide 2 first levels

###报错
#Error in tree_to_hierarchy(graph, direction, sort.by, weight) : 
#Weight must be numeric
??tree_to_hierarchy

#更改配色我们就很轻易得到了下面的图形：
ggraph(mygraph, layout = 'circlepack', weight="size") +
  geom_node_circle(aes(fill = as.factor(depth), color = as.factor(depth) )) +
  scale_fill_manual(values=c("0" = "white", "1" = "white", "2" = magma(4)[2], "3" = magma(4)[3], "4"=magma(4)[4])) +
  scale_color_manual( values=c("0" = "white", "1" = "white", "2" = "black", "3" = "black", "4"="black") ) +
  theme_void() +
  theme(legend.position="FALSE")


#geom_node_text用于添加标签，使用方法和geom_text类似。
# 重现导入边和节点数据，方便重复
edges <- flare$edges %>%
  filter(to %in% from) %>%
  droplevels()
vertices <- flare$vertices %>%
  filter(name %in% c(edges$from, edges$to)) %>%
  droplevels()
vertices$size <- runif(nrow(vertices))

# 构造对象
mygraph <- graph_from_data_frame( edges, vertices=vertices )

ggraph(mygraph, layout = 'circlepack', weight="size" ) +
  geom_node_circle(aes(fill = depth)) +
  geom_node_text( aes(label=shortName, filter=leaf, fill=depth, size=size)) +
  theme_void() +
  theme(legend.position="FALSE") +
  scale_fill_viridis()
