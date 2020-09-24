##2019.8.24

##ggraph V1.0.0 发布了-R语言网络可视化的崛起 

##https://mp.weixin.qq.com/s/uRmZmuBMTt1hd4A-_6Udmw

#ggraph是ggplot2的扩展，旨在支持网络，图形和树等表示关系的数据的结构。
#虽然它建立在ggplot2及其API的基础之上，但它带有自己的一套geom，facets等，并且在语法中添加了许多布局。

library(ggraph)
#> Loading required package: ggplot2
library(tidygraph)
#>
#> Attaching package: 'tidygraph'
#> The following object is masked from 'package:stats':
#>
#>     filter

# Create graph of highschool friendships
graph <- as_tbl_graph(highschool) %>%
  mutate(Popularity = centrality_degree(mode = 'in'))

# plot using ggraph
ggraph(graph, layout = 'kk') +
  geom_edge_fan(aes(alpha = stat(index)), show.legend = FALSE) +
  geom_node_point(aes(size = Popularity)) +
  facet_edges(~year) +
  theme_graph(foreground = 'steelblue', fg_text_colour = 'white')

?ggraph
#ggraph可以访问igraph中可用的所有布局功能，并且还提供了大量自己的布局功能，例如蜂巢图，树图和圆形图。

library(ggraph)
library(igraph)
graph <- graph_from_data_frame(highschool)

p <- ggraph(graph, layout = 'kk') +
  geom_edge_link(aes(colour = factor(year))) +
  geom_node_point() +
  ggtitle('An example')

p
p + theme_graph()

p + theme_graph(background = 'grey20', text_colour = 'white')

#设置默认主题 方便全部网络图绘制
set_graph_style()

p

#可以使用分面绘制不同处理网络喽
# Assign each node to a random class
V(graph)$class <- sample(letters[1:4], gorder(graph), TRUE)
# Make year a character
E(graph)$year <- as.character(E(graph)$year)

p <- ggraph(graph, layout = 'kk') +
  geom_edge_fan(aes(alpha = ..index.., colour = year)) +
  geom_node_point(aes(shape = class)) +
  scale_edge_alpha(guide = 'none')

p + facet_edges(~year)

### 想怎么分面就怎么分面
p + facet_nodes(~class) + th_foreground(foreground = 'grey80', border = TRUE)

### 来一张震撼的八分面
p + facet_graph(year ~ class)

### 或许更加震撼的15分面
p + facet_graph(year ~ class, margins = TRUE)

#更多功能参考：https://www.data-imaginist.com/2017/announcing-ggraph/