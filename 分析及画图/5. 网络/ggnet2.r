##2019.3.12
#ggnet2: network visualization with ggplot2

#https://mp.weixin.qq.com/s/Xmun156WcDtV-baD_Ey55Q

#install.packages("GGally")
library(GGally); ??GGally

# devtools::install_github("briatte/ggnet")
# library(ggnet)

#install.packages("network")
#install.packages("sna")
library(network) ; ?network
library(sna) ; ??sna  #Tools for Social Network Analysis
#Error: 找不到‘sna’所需要的程辑包‘statnet.common’  这个包需要R>=3.5.2019-01-08发布
#https://cran.r-project.org/web/packages/statnet.common/index.html
library(ggplot2)


# random graph
net = rgraph(10, mode = "graph", tprob = 0.5) #伯努利随机图；sna里的函数
net = network(net, directed = FALSE)

# vertex names
network.vertex.names(net) = letters[1:10]

#visualized
ggnet2(net)  #net可以使任何网络性质的对象

#节点颜色和大小
ggnet2(net, node.size = 6, node.color = "black", edge.size = 1, edge.color = "grey")

#node.color and node.size可以简写
ggnet2(net, size = 6, color = "black", edge.size = 1, edge.color = "grey")

ggnet2(net, size = 6, color = rep(c("tomato", "steelblue"), 5))

##下面还有很多内容略
