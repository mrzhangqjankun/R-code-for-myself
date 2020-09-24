##2019.8.16

##ggplot版本的网络图终于可以替代igraph了

##https://mp.weixin.qq.com/s/fX4n3KJ1ov81qSBd8lSIfw

通过ggplot做出基本网络图形

suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(network))
suppressPackageStartupMessages(library(sna))
suppressPackageStartupMessages(library(ergm))
library(network)
library(ggplot2)
library(sna)
library(ergm)

plotg <- function(net, value = NULL) {
  
  m <- as.matrix.network.adjacency(net)  # get sociomatrix
  # get coordinates from Fruchterman and Reingold's force-directed placement
  # algorithm.
  plotcord <- data.frame(gplot.layout.fruchtermanreingold(m, NULL))
  # or get it them from Kamada-Kawai's algorithm: plotcord <-
  # data.frame(gplot.layout.kamadakawai(m, NULL))
  colnames(plotcord) = c("X1", "X2")
  edglist <- as.matrix.network.edgelist(net)
  edges <- data.frame(plotcord[edglist[, 1], ], plotcord[edglist[, 2], ])
  plotcord$elements <- as.factor(get.vertex.attribute(net, "elements"))
  colnames(edges) <- c("X1", "Y1", "X2", "Y2")
  edges$midX <- (edges$X1 + edges$X2)/2
  edges$midY <- (edges$Y1 + edges$Y2)/2
  pnet <- ggplot() + geom_segment(aes(x = X1, y = Y1, xend = X2, yend = Y2),
                                  data = edges, size = 0.5, colour = "grey") + geom_point(aes(X1, X2,
                                                                                              colour = elements), data = plotcord) + scale_colour_brewer(palette = "Set1") +
    scale_x_continuous(breaks = NULL) + scale_y_continuous(breaks = NULL) +
    # discard default grid + titles in ggplot2
    theme(panel.background = element_blank()) + theme(legend.position = "none") +
    theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
    theme(legend.background = element_rect(colour = NA)) + theme(panel.background = element_rect(fill = "white",
                                                                                                 colour = NA)) + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())
  return(print(pnet))
}

g <- network(50, directed = FALSE, density = 0.03)
classes <- rbinom(50, 1, 0.5) + rbinom(50, 1, 0.5) + rbinom(50, 1, 0.5)
set.vertex.attribute(g, "elements", classes)
g
plotg(g)


#尝试：基于ggplot出图储存于list后批量拼图

#ggplot拼图函数有很多，但是这里我们批量出图储存于list中，这里使用下面这个函数做拼图。google上有人写的：

multiplot <- function(..., plotlist=NULL, cols) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # Make the panel
  plotCols = cols                          # Number of columns of plots
  plotRows = ceiling(numPlots/plotCols) # Number of rows needed, calculated from # of cols
  
  # Set up the page
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(plotRows, plotCols)))
  vplayout <- function(x, y)
    viewport(layout.pos.row = x, layout.pos.col = y)
  
  # Make each plot, in the correct location
  for (i in 1:numPlots) {
    curRow = ceiling(i/plotCols)
    curCol = (i-1) %% plotCols + 1
    print(plots[[i]], vp = vplayout(curRow, curCol ))
  }
  
}

#基于微生物大量的OTU，我尝试了18中layout

#大家使用cor.test计算得到的相关矩阵即可作为输入

# 确定物种间存在相互作用关系的阈值，将相关性R矩阵内不符合的数据转换为0
occor.r[occor.p>p.threshold|abs(occor.r)<r.threshold] = 0 
library(network)
library(ggplot2)
library(sna)
library(ergm)
library(igraph)

g <- network(occor.r, directed=FALSE,vertex.attrnames=T)

summary(g)
net  = g

m <- as.matrix.network.adjacency(net)  # get sociomatrix

plotcord = list()

plotcord[[1]] <- data.frame(gplot.layout.fruchtermanreingold(m, NULL));names(plotcord[[1]]) = "fruchtermanreingold"
plotcord[[2]] <- data.frame(gplot.layout.kamadakawai(m, NULL));names(plotcord[[2]]) = "kamadakawai"
plotcord[[ 3]] <- data.frame(gplot.layout.adj(m, NULL));names(plotcord[[3]]) = "adj"
plotcord[[ 4]] <- data.frame(gplot.layout.circle(m, NULL));names(plotcord[[4]]) = "circle"
plotcord[[ 5]] <- data.frame(gplot.layout.circrand(m, NULL));names(plotcord[[5]]) = "circrand"
plotcord[[ 6]] <- data.frame(gplot.layout.eigen(m, NULL));names(plotcord[[6]]) = "eigen"
plotcord[[ 7]] <- data.frame(gplot.layout.fruchtermanreingold(m, NULL));names(plotcord[[7]]) = "fruchtermanreingold"
plotcord[[ 8]] <- data.frame(gplot.layout.geodist(m, NULL));names(plotcord[[8]]) = "geodist"
plotcord[[ 9]] <- data.frame(gplot.layout.hall(m, NULL));names(plotcord[[9]]) = "hall"
plotcord[[ 10]]<- data.frame(gplot.layout.kamadakawai(m, NULL));names(plotcord[[10]]) = "kamadakawai"
plotcord[[11]] <- data.frame(gplot.layout.mds(m, NULL));names(plotcord[[11]]) = "mds"
plotcord[[12]]  <- data.frame(gplot.layout.princoord(m, NULL));names(plotcord[[12]]) = "princoord"
plotcord[[13 ]] <- data.frame(gplot.layout.random(m, NULL));names(plotcord[[13]]) = "random"
plotcord[[14 ]] <- data.frame(gplot.layout.rmds(m, NULL));names(plotcord[[14]]) = "rmds"
plotcord[[15 ]] <- data.frame(gplot.layout.segeo(m, NULL));names(plotcord[[15]]) = "segeo"
plotcord[[16 ]] <- data.frame(gplot.layout.seham(m, NULL));names(plotcord[[16]]) = "seham"
plotcord[[17 ]] <- data.frame(gplot.layout.spring(m, NULL));names(plotcord[[17]]) = "spring"
plotcord[[18 ]] <- data.frame(gplot.layout.springrepulse(m, NULL));names(plotcord[[18]]) = "springrepulse"
#计算花费很长时间，所以不计算了
# plotcord[[19 ]] <- data.frame(gplot.layout.target(m, NULL))
plots = list()
ii = 1
for (ii in 1:18) {
  plotcor = plotcord[[ii]]
  colnames(plotcor) = c("X1", "X2")
  head(plotcor)
  plotcor$elements <- colnames(occor.r)
  edglist <- as.matrix.network.edgelist(net)
  edglist = as.data.frame(edglist)
  # aaaa = as.matrix.network(net)
  # 构建igraph对象构建邻接矩阵
  igraph <- graph_from_adjacency_matrix(occor.r,mode="undirected",weighted=TRUE,diag=FALSE)
  E(igraph)$weight
  edglist$weight = E(igraph)$weight
  
  edges <- data.frame(plotcor[edglist[, 1], ], plotcor[edglist[, 2], ])
  head(edges)
  edges$weight = E(igraph)$weight
  ##这里将边权重根据正负分为两类
  aaa = rep("a",length(edges$weight))
  for (i in 1:length(edges$weight)) {
    if (edges$weight[i]> 0) {
      aaa[i] = "+"
    }
    if (edges$weight[i]< 0) {
      aaa[i] = "-"
    }
    
  }
  #添加到edges中
  edges$wei_label = aaa
  
  colnames(edges) <- c("X1", "Y1","OTU_1", "X2", "Y2","OTU_2","weight","wei_label")
  edges$midX <- (edges$X1 + edges$X2)/2
  edges$midY <- (edges$Y1 + edges$Y2)/2
  head(edges)
  # library(ggrepel)
  
  
  pnet <- ggplot() + geom_segment(aes(x = X1, y = Y1, xend = X2, yend = Y2,colour = edges$wei_label),
                                  data = edges, size = 0.5) +
    geom_point(aes(X1, X2), size=3, pch = 21, data = plotcor, fill = "#8DD3C7") + scale_colour_brewer(palette = "Set1") +
    scale_x_continuous(breaks = NULL) + scale_y_continuous(breaks = NULL) +
    labs( title = names(plotcord[[ii]])[1])+
    # geom_text(aes(X1, X2,label=elements),size=4, data = plotcor)+
    # discard default grid + titles in ggplot2
    theme(panel.background = element_blank()) +
    theme(legend.position = "none") +
    theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
    theme(legend.background = element_rect(colour = NA)) +
    theme(panel.background = element_rect(fill = "white",  colour = NA)) +
    theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())
  
  pnet
  
  plots[[ii]] = pnet
  
  
}


pdf(file = "cs.pdf",width = 12,height = 18)
multiplot(plotlist=plots,cols=3)
dev.off()