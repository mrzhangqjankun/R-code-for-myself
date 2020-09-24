##2020.5.1

##https://mp.weixin.qq.com/s/1QoPxzf-1fYuGDbQEW33Eg

##弧形图（Arc Diagram）

##https://www.data-to-viz.com/#portfolioanchor

library(tidyverse)
dataUU <- read.table("https://raw.githubusercontent.com/holtzy/data_to_viz/master/Example_dataset/13_AdjacencyUndirectedUnweighted.csv", header=TRUE)
#将dataUU转化成所需的格式
connect <- dataUU %>% 
  gather(key="to", value="value", -1) %>% #将matrix变成三列，一列from，一列to，一列共现文献数
  mutate(to = gsub("\\.", " ",to)) %>% #将to这一列中姓名.去掉变成空格
  na.omit() #剔除NA
head(connect)#第一列的数字是剔除NA之前的行号

# 计算每个作者的共现条数
c( as.character(connect$from), as.character(connect$to)) %>%
  as.tibble() %>% #将数据的type转变为tbl_df，tibble可以理解成是一种加强版的数据框
  group_by(value) %>% #按照value进行分组
  summarize(n=n()) -> coauth
colnames(coauth) <- c("name", "n")#修改列名
head(coauth)

library(igraph)
mygraph <- graph_from_data_frame( connect, vertices = coauth, directed = FALSE )

#我们可以使用igraph包中的walktrap.community函数。这是一个用来进行社区划分的函数。
#当然，igraph包中还提供了许多其他划分函数比如fastgreedy.community，spinglass.community,edge.betweenness.community,leading.eigenvector.community等

# 鉴定社区
com <- walktrap.community(mygraph)

#重新整理数据
coauth <- coauth %>% 
  mutate( grp = com$membership) %>%
  arrange(grp) %>%
  mutate(name=factor(name, name))

# 保留排名前15的社区
coauth <- coauth %>% 
  filter(grp<16)

# 只保留排名前15的社区涉及的作者
connect <- connect %>%
  filter(from %in% coauth$name) %>%
  filter(to %in% coauth$name)

mygraph <- graph_from_data_frame( connect, vertices = coauth, directed = FALSE )

#如何作图接着，我们将使用ggraph函数进行画图。

library(RColorBrewer)
library(ggraph)
mycolor<-colorRampPalette(brewer.pal(9, "Paired"))
ggraph(mygraph, layout="linear") + 
  geom_edge_arc(edge_colour="black", edge_alpha=0.2, edge_width=0.3, fold=TRUE) +
  geom_node_point(aes(size=n, color=as.factor(grp), fill=grp), alpha=0.5) +
  scale_size_continuous(range=c(0.5,8)) +
  scale_color_manual(values=mycolor(15)) +
  geom_node_text(aes(label=name), angle=65, hjust=1, nudge_y = -1.1, size=2.3) +
  theme_void() +
  theme(
    legend.position="none",
    plot.margin=unit(c(0,0,0.4,0), "null"),
    panel.spacing=unit(c(0,0,3.4,0), "null")
  ) +
  expand_limits(x = c(-1.2, 1.2), y = c(-5.6, 1.2)) 
