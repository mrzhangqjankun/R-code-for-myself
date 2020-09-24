##2019.8.19

##https://mp.weixin.qq.com/s/1TCHJWJ96D19dnqA7uEeQg

##一文构建微生物群落sparcc网络 
library(igraph)
library(ggplot2)

# 作者改变工作路径，这里我们不进行改变
# setwd("~/Desktop/EBI2014/Thursday/coocurrences")

# 导入环境，里面有环境变量和一张OTU表格
load("OTU_env_forAntonio_1.RData")

# Check how many OTUs we have
dim(OTU_table)

# 过滤序列数量少于50条的的OTU
OTU.table.filtered <- OTU_table[,colSums(OTU_table) >= 50]

# 查看行列
dim(OTU.table.filtered)
#修改列名
OTU.table.filtered.colnames <- colnames(OTU.table.filtered)
OTU.table.filtered.sparcc <- cbind(rownames(OTU.table.filtered), OTU.table.filtered)
colnames(OTU.table.filtered.sparcc) <- c("OTU_id", OTU.table.filtered.colnames)

# 输出文件，作为sparCC的输入
# We need to transpose the table

write.table(t(OTU.table.filtered.sparcc), file = "otu_table_filtered_50.tsv", row.names = T, sep = "\t", quote = F, col.names = F)

#######################################
# 这个过程省略，因为800多个OTU也得一会
#######################################

# Occupancy–abundance relationship (Endemism-cosmopolitanism)
# Example from http://ecology.msu.montana.edu/labdsv/R/labs/lab1/lab1.html

# For comodity
otus <- (OTU.table.filtered)

# We set a minimum abundance to be present
minabu <- 50

# 设定阈值为50，统计序列数量大于50条的OTU有多少个
otu.site <- apply(otus > minabu, 1, sum)

# 统计每个OTU在多少个样品中含量超过了50个。
site.otu <- apply(otus > minabu, 2, sum)

## 下面这个循环求取超过50条序列的样品中，每个样品平均有多少条
if (minabu == 0) {
  # 
  mean.abu <- apply(otus, 2, sum)/site.otu
} else {
  mean.abu <- rep(0, ncol(otus))
  # If not we have to find where there are the OTU with a value larger than minabu
  for (i in 1:ncol(otus)) {
    mask <- otus[, i] > minabu
    # And calculate the mean abundance
    mean.abu[i] <- sum(otus[mask, i])/max(1, site.otu[i])
  }
}
mean.abu[is.na(mean.abu)] <- 0

# Let's plot the Abundance vs. Occurrence

#l列一张表，表明每个OTU多少个样品中平均有多少条序列
distr.otus<-as.data.frame(cbind(site.otu[mean.abu > minabu], mean.abu[mean.abu > minabu]))
colnames(distr.otus)<-c("counts", "mean")
head(distr.otus)
# 这里根据OTU出现的频率，对OTU进行了定义
#第一个就是广泛存在的
distr.otus$com[distr.otus$counts >= 20]<-"Cosmopolitan"
#虽然在少数样品中存在，但是丰度较高的，定义为地域性的OTU。
distr.otus$com[distr.otus$counts <= 8 & distr.otus$mean >= 500]<-"Endemic"
distr.otus$com[is.na(distr.otus$counts)]<-"NA"

##下面就是一张丰度大于50条的全部OTU在多少个样品中出现的分布散点图
ggplot(data=distr.otus) +
  geom_point(aes(counts, mean, colour=com)) +
  xlab("Number of sites") +
  ylab("Mean abundance") +
   xlim(c(1,39)) +
   ylim(c(min(mean.abu[mean.abu > minabu]),2000)) +
  coord_trans(y = "log10") +
  guides(colour=FALSE) +
  theme(legend.position = "bottom") +
  theme_bw()

#拿到这些定义好的OTU的编号
cosmopolitan<- rownames(distr.otus)[distr.otus$counts >= 20]
endemic <- rownames(distr.otus)[distr.otus$counts <= 8 & distr.otus$mean >= 500]


#######################################
# Back from sparCC
#######################################


# 让我们构建共线性网络
# 首先定义一些功能
makeNet <- function(X){
  a<-matrix(nrow=1,ncol=4)
  a[1,1] <- rownames(sparcc$r)[X[1]]
  a[1,2] <- colnames(sparcc$r)[X[2]]
  a[1,3] <- sparcc$r[X[1], X[2]]
  a[1,4] <- sparcc$P[X[1],X[2]]
  return(a)
}

graph.transform.weights <- function (X) {
    require(igraph)
    data.tmp <- matrix(0, nrow=nrow(X), ncol=2)
    dimnames(data.tmp)[[2]] <- c("i", "j")
    data.tmp[,1] <- X[,1]
    data.tmp[,2] <- X[,2]
    data <- data.frame(data.tmp)
    graph.data <- graph.data.frame(data, directed=F)
    E(graph.data)$weight <- abs(as.numeric(X[,3]))
    E(graph.data)$cor <- as.numeric(X[,3])
    E(graph.data)$pvalue <- as.numeric(X[,3])
    summary(graph.data)
    cat("Average degree:",ecount(graph.data)/vcount(graph.data)*2)
    return(graph.data)
}

# 提取sparcc相关性矩阵
sparcc.cor <- read.table(file = "icomm_cor_sparcc_5.out", sep = "\t", header = T, row.names = 1)
dim(sparcc.cor)

# 提取sparCC pseudo p-values
sparcc.pval <- read.table(file = "icomm_pvals_sparcc_5.out", sep = "\t", header = T, row.names = 1)
dim(sparcc.pval)

# 构建一个列表同时包含这两个文件
sparcc <- structure(list(r = sparcc.cor, P = sparcc.pval))

# 设定显著性阈值为0.01，相关性阈值为0.7
pval <- 0.01
r <- 0.7
#选择符合要求的行列
selrp<-which((abs(sparcc$r) > r & sparcc$P < pval) & lower.tri(sparcc$r == TRUE), arr.ind = TRUE)

# 使用makeNet功能构建邻接矩阵
sparcc.graph.df <- t(apply(selrp,1, makeNet))

# 构建igraph网络对象
sparcc.graph <- graph.transform.weights(sparcc.graph.df)

# We will add the distribution information to the nodes
sparcc.graph.names <- as.data.frame(V(sparcc.graph)$name, stringsAsFactors = F)
colnames(sparcc.graph.names) <- "names"

otu.distr <- rbind(cbind(cosmopolitan, "cosmopolitan"),cbind(endemic, "endemic"))
colnames(otu.distr)<-c("names","distribution")
head(otu.distr )
#合并网络中的OTU名称和我们上面定义的标签
sparcc.graph.atr <- as.data.frame(merge(sparcc.graph.names, otu.distr, by="names", all.x=T, all.y = F), stringsAsFactors = F)
head(sparcc.graph.atr)
sparcc.graph.atr <- data.frame(lapply(sparcc.graph.atr, as.character), stringsAsFactors=FALSE)
sparcc.graph.atr[is.na(sparcc.graph.atr)]<-'FALSE'

# 这条命令调整顺序
sparcc.graph.atr.sorted <- sparcc.graph.atr[match(sparcc.graph.names$names,sparcc.graph.atr$names),]
head(sparcc.graph.atr.sorted)
# 将上面编号的节点注释文件编入igraph网络的节点属性中。
V(sparcc.graph)$distribution <- sparcc.graph.atr.sorted$distribution

# 保存网络
write.graph(sparcc.graph, "sparcc_graph_50.graphml", format="graphml")


# 提取自网络，这里我们根据上上面分组，提取包含这每个分组otu的子网络 注意这里的联系可能包含这个类别之外的节点

# We combine the node names from the graph and for each type of distribution
sparcc.graph.ids.cosm <- unlist(list(V(sparcc.graph)$name, unique(cosmopolitan)))
sparcc.graph.ids.end <- unlist(list(V(sparcc.graph)$name, unique(endemic)))

# We keep those present in both
sparcc.graph.ids.cosm <- sparcc.graph.ids.cosm[duplicated(sparcc.graph.ids.cosm)]
sparcc.graph.ids.end <- sparcc.graph.ids.end[duplicated(sparcc.graph.ids.end)]

# We extract the first order neighbors
sparcc.graph.cosm.nei<-neighborhood(sparcc.graph, 1, nodes=sparcc.graph.ids.cosm)
sparcc.graph.end.nei<-neighborhood(sparcc.graph, 1, nodes=sparcc.graph.ids.end)

# We create the subgraph only containing the first order neighbors
sparcc.graph.cosm<-induced.subgraph(sparcc.graph, unlist(sparcc.graph.cosm.nei))
sparcc.graph.end<-induced.subgraph(sparcc.graph, unlist(sparcc.graph.end.nei))

# We write the network to a file
write.graph(sparcc.graph.cosm, "sparcc_graph_cosm_nei_50.graphml", format="graphml")
write.graph(sparcc.graph.end, "sparcc_graph_end_nei_50.graphml", format="graphml")


# 同样挑选自网络，但是只保留同一个类别网络中的连线
# We extract the first order neighbors
sparcc.graph.cosm.nei<-neighborhood(sparcc.graph, 0, nodes=sparcc.graph.ids.cosm)
sparcc.graph.end.nei<-neighborhood(sparcc.graph, 0, nodes=sparcc.graph.ids.end)

# 提取自网络
sparcc.graph.cosm<-induced.subgraph(sparcc.graph, unlist(sparcc.graph.cosm.nei))
sparcc.graph.end<-induced.subgraph(sparcc.graph, unlist(sparcc.graph.end.nei))

# 去除孤立的节点
sparcc.graph.cosm <- delete.vertices(sparcc.graph.cosm, which(degree(sparcc.graph.cosm) < 1))
sparcc.graph.end <- delete.vertices(sparcc.graph.end, which(degree(sparcc.graph.end) < 1))

# 文件写入
write.graph(sparcc.graph.cosm, "sparcc_graph_cosm_50.graphml", format="graphml")
write.graph(sparcc.graph.end, "sparcc_graph_end_50.graphml", format="graphml")



###改变网络节点边框宽度
mycircle <- function(coords, v=NULL, params) {
  vertex.color <- params("vertex", "color")
  if (length(vertex.color) != 1 && !is.null(v)) {
    vertex.color <- vertex.color[v]
  }
  vertex.size  <- 1/200 * params("vertex", "size")
  if (length(vertex.size) != 1 && !is.null(v)) {
    vertex.size <- vertex.size[v]
  }
  vertex.frame.color <- params("vertex", "frame.color")
  if (length(vertex.frame.color) != 1 && !is.null(v)) {
    vertex.frame.color <- vertex.frame.color[v]
  }
  vertex.frame.width <- params("vertex", "frame.width")
  if (length(vertex.frame.width) != 1 && !is.null(v)) {
    vertex.frame.width <- vertex.frame.width[v]
  }
  
  mapply(coords[,1], coords[,2], vertex.color, vertex.frame.color,
         vertex.size, vertex.frame.width,
         FUN=function(x, y, bg, fg, size, lwd) {
           symbols(x=x, y=y, bg=bg, fg=fg, lwd=lwd,
                   circles=size, add=TRUE, inches=FALSE)
         })
}

add.vertex.shape("fcircle", clip=igraph.shape.noclip,
                 plot=mycircle, parameters=list(vertex.frame.color=1,
                                                vertex.frame.width=1))


igraph = sparcc.graph.cosm
# igraph = sparcc.graph.end

l <- layout_nicely(igraph)
l <- norm_coords(l, ymin=-1, ymax=1, xmin=-1, xmax=1)
plot(igraph)

  plot(igraph,
     layout=l,
     vertex.shape="fcircle",
     vertex.label=NA ,
     vertex.frame.color="#984EA3",
     vertex.color="#984EA3",
     vertex.size =2,
     vertex.frame.alpha =0.5,
     edge.width=0.5,
     edge.lty=2,
     edge.curved=F,
     margin=c(0,0,0,0),
     vertex.frame.width=5
)

  
  
  igraph = sparcc.graph.end
  
  l <- layout_nicely(igraph)
  l <- norm_coords(l, ymin=-1, ymax=1, xmin=-1, xmax=1)
  plot(igraph)
  
  plot(igraph,
       layout=l,
       vertex.shape="fcircle",
       vertex.label=NA ,
       vertex.frame.color="#984EA3",
       vertex.color="#984EA3",
       vertex.size =2,
       vertex.frame.alpha =0.5,
       edge.width=0.5,
       edge.lty=2,
       edge.curved=F,
       margin=c(0,0,0,0),
       vertex.frame.width=5
  )
  