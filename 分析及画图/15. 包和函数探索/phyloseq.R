##2019.3.7
#https://mp.weixin.qq.com/s/sk-BksZ-CJq4eONScu3baw
#https://mp.weixin.qq.com/s/NzNM61NzZgPMKYzWTpEZug
# phylsoeq使用S4类方法

rm(list=ls())
# source('http://bioconductor.org/biocLite.R')
# biocLite('phyloseq')

library("phyloseq"); packageVersion("phyloseq")
library("ggplot2"); packageVersion("ggplot2")

#导入文件
?import  #c("mothur", "pyrotagger", "QIIME", "RDP")

# Create a pretend OTU table that you read from a file, called otumat
otumat = matrix(sample(1:100, 100, replace = TRUE), nrow = 10, ncol = 10)
otumat
rownames(otumat) <- paste0("OTU", 1:nrow(otumat))
colnames(otumat) <- paste0("Sample", 1:ncol(otumat))
otumat
# Create tax file
taxmat = matrix(sample(letters, 70, replace = TRUE), nrow = nrow(otumat), ncol = 7)
rownames(taxmat) <- rownames(otumat)
colnames(taxmat) <- c("Domain", "Phylum", "Class", "Order", "Family", "Genus", "Species")
taxmat

##otu_table和tax_table函数构造phyloseq对象并加入phyloseq
OTU = otu_table(otumat, taxa_are_rows = TRUE);?otu_table
TAX = tax_table(taxmat);?tax_table
OTU
TAX 
#加入phyloseq体系
physeq = phyloseq(OTU, TAX)
physeq

#以科水平做柱状图
plot_bar(physeq, fill = "Family") ; ?plot_bar()

#构建样品及其分组文件（mapping）
sampledata = sample_data(data.frame(
  Location = sample(LETTERS[1:4], size=nsamples(physeq), replace=TRUE),
  Depth = sample(50:1000, size=nsamples(physeq), replace=TRUE),
  row.names=sample_names(physeq),
  stringsAsFactors=FALSE
))
sampledata

#构建随机树合并phyloseq
library("ape")
random_tree = rtree(ntaxa(physeq), rooted=TRUE, tip.label=taxa_names(physeq));?rtree
plot(random_tree)
physeq1 = merge_phyloseq(physeq, sampledata, random_tree);?merge_phyloseq()
physeq1
#可重新构建元素
physeq2 = phyloseq(OTU, TAX, sampledata, random_tree);?phyloseq
physeq2
#两者是否相同，很明显是一样的
identical(physeq1, physeq2)

#使用phyloseq展示进化树和count丰度热图
plot_tree(physeq1, color="Location", label.tips="taxa_names", ladderize="left", plot.margin=0.3) ; ?plot_tree()

plot_tree(physeq1, color="Depth", shape="Location", label.tips="taxa_names", ladderize="right", plot.margin=0.3)

plot_heatmap(physeq1) ; ?plot_heatmap()

plot_heatmap(physeq1, taxa.label="Phylum")


####################boim格式文件导入
#基于biom格式数据导入
rich_dense_biom  = system.file("extdata", "rich_dense_otu_table.biom",  package="phyloseq")
rich_sparse_biom = system.file("extdata", "rich_sparse_otu_table.biom", package="phyloseq")
min_dense_biom   = system.file("extdata", "min_dense_otu_table.biom",   package="phyloseq")
min_sparse_biom  = system.file("extdata", "min_sparse_otu_table.biom",  package="phyloseq")
treefilename = system.file("extdata", "biom-tree.phy",  package="phyloseq")
refseqfilename = system.file("extdata", "biom-refseq.fasta",  package="phyloseq")
import_biom(rich_dense_biom, treefilename, refseqfilename, parseFunction=parse_taxonomy_greengenes)
myData = import_biom(rich_dense_biom, treefilename, refseqfilename, parseFunction=parse_taxonomy_greengenes)
myData
sample_data(myData)
plot_tree(myData, color="Genus", shape="BODY_SITE", size="abundance")
plot_richness(myData, x="BODY_SITE", color="Description")
plot_bar(myData, fill="Genus")
refseq(myData)

#####################基于qiime输出文件导入
otufile = system.file("extdata", "GP_otu_table_rand_short.txt.gz", package="phyloseq")
mapfile = system.file("extdata", "master_map.txt", package="phyloseq")
trefile = system.file("extdata", "GP_tree_rand_short.newick.gz", package="phyloseq")
rs_file = system.file("extdata", "qiime500-refseq.fasta", package="phyloseq")
qiimedata = import_qiime(otufile, mapfile, trefile, rs_file)
qiimedata
plot_bar(qiimedata, x="SampleType", fill="Phylum")

##########################使用phyloseq的例子
#使用phyloseq
data(GlobalPatterns);?GlobalPatterns #Global patterns of 16S rRNA diversity at a depth of millions of sequences per sample (2011)
data(esophagus);?esophagus   #Small example dataset from a human esophageal community (2004)
data(enterotype);?enterotype #Enterotypes of the human gut microbiome (2011)
data(soilrep);?soilrep  #Reproducibility of soil microbiome data(2011)

example(enterotype, ask=FALSE)
?make_network #Make microbiome network (igraph)
ig <- make_network(enterotype, "samples", max.dist=0.3)
plot_network(ig, enterotype, color="SeqTech", shape="Enterotype", line_weight=0.3, label=NULL)

##报错Error in .C("veg_distance", x = as.double(x), nr = N, nc = ncol(x), d = double(N *  : 
#"veg_distance" not available for .C() for package "vegan"
##根据https://github.com/vegandevs/vegan/issues/272
ig = make_network(enterotype, max.dist=0.3, distance=function(x){vegan::vegdist(x, "jaccard")})
plot_network(ig, enterotype, color="SeqTech", shape="Enterotype", line_weight=0.3, label=NULL)


#########################phyloseq按照一定要求合并数据
##phyloseq合并数据
data(GlobalPatterns)
GP = GlobalPatterns
#数据过滤，保留count大于0的OTU
GP = prune_taxa(taxa_sums(GlobalPatterns) > 0, GlobalPatterns)
humantypes = c("Feces", "Mock", "Skin", "Tongue")
sample_data(GP)$human <- get_variable(GP, "SampleType") %in% humantypes

#按照SampleType合并样品
mergedGP = merge_samples(GP, "SampleType")
SD = merge_samples(sample_data(GP), "SampleType")
print(SD[, "SampleType"])
sample_data(mergedGP)
OTUnames10 = names(sort(taxa_sums(GP), TRUE)[1:10])
GP10  = prune_taxa(OTUnames10,  GP)
mGP10 = prune_taxa(OTUnames10, mergedGP)
ocean_samples = sample_names(subset(sample_data(GP), SampleType=="Ocean"))
print(ocean_samples)
otu_table(GP10)[, ocean_samples]

rowSums(otu_table(GP10)[, ocean_samples])
otu_table(mGP10)["Ocean", ]

plot_richness(GP, "human", "SampleType", title="unmerged")

sample_data(mergedGP)$SampleType = sample_names(mergedGP)
sample_data(mergedGP)$human = sample_names(mergedGP) %in% humantypes
plot_richness(mergedGP, "human", "SampleType", title="merged")

########################根据进化关系对OTU进行合并
load("example-data.RData")
plot_tree(closedps, color="Treatment", size="abundance", sizebase=2, label.tips="taxa_names")

x1 = merge_taxa(closedps, taxa_names(closedps)[3:27], 2)
plot_tree(x1, color="Treatment", size="abundance", sizebase=2, label.tips="taxa_names")
#######################基于phyloseq数据合并
#合并phyloseq对象
data(GlobalPatterns)
tree = phy_tree(GlobalPatterns)
tax  = tax_table(GlobalPatterns)
otu  = otu_table(GlobalPatterns)
sam  = sample_data(GlobalPatterns)
otutax = phyloseq(otu, tax)
otutax

GP2 = merge_phyloseq(otutax, sam, tree)
identical(GP2, GlobalPatterns)
##更加复杂的方式

otusamtree = phyloseq(otu, sam, tree)
GP3 = merge_phyloseq(otusamtree, otutax)
GP3
identical(GP3, GlobalPatterns)

GP4 = merge_phyloseq(otusamtree, tax_table(otutax))
GP4
identical(GP4, GlobalPatterns)

#######################phyloseq系统内数据查看
##访问数据及其数据处理
data("GlobalPatterns")
GlobalPatterns
#查看OTU数量
ntaxa(GlobalPatterns)
#查看样品数量
nsamples(GlobalPatterns)
#样品名查看，此处查看前五个样品名称
sample_names(GlobalPatterns)[1:5]
#查看tax的分类等级信息
rank_names(GlobalPatterns)
#查看mapping文件表头（样品信息文件列名）
sample_variables(GlobalPatterns)
#查看部分OTU表格矩阵
otu_table(GlobalPatterns)[1:5, 1:5]
#查看部分注释（tax）文件矩阵
tax_table(GlobalPatterns)[1:5, 1:4]
#进化树查看，注意不是可视化
phy_tree(GlobalPatterns)
#查看OTU名称，此处查看前十个
taxa_names(GlobalPatterns)[1:10]

#按照丰度提取前十个OTU，并可视化进化树
myTaxa = names(sort(taxa_sums(GlobalPatterns), decreasing = TRUE)[1:10])
ex1 = prune_taxa(myTaxa, GlobalPatterns)
plot(phy_tree(ex1), show.node.label = TRUE)


############################数据预处理

#转化OTUcount数为相对丰度
GPr  = transform_sample_counts(GlobalPatterns, function(x) x / sum(x) )
otu_table(GPr)[1:5][1:5]
#提取丰度大于十万份之一的OTU
GPfr = filter_taxa(GPr, function(x) mean(x) > 1e-5, TRUE)
#提取指定分类的OTU
GP.chl = subset_taxa(GlobalPatterns, Phylum=="Chlamydiae")
#提取总count数量大于20的样品
GP.chl = prune_samples(sample_sums(GP.chl)>=20, GP.chl)
#合并指定OTU： taxa_names(GP.chl)[1:5]为一个OTU
GP.chl.merged = merge_taxa(GP.chl, taxa_names(GP.chl)[1:5])
# gpsfbg = tax_glom(gpsfb, "Family")
# plot_tree(gpsfbg, color="SampleType", shape="Class", size="abundance")

transform_sample_counts(GP.chl, function(OTU) OTU/sum(OTU) )
#去除至少20％样本中未见过3次以上的OTU,注意编写函数的形式
GP = filter_taxa(GlobalPatterns, function(x) sum(x > 3) > (0.2*length(x)), TRUE)
#对样品分组文件mapping添加一列
sample_data(GP)$human = factor( get_variable(GP, "SampleType") %in% c("Feces", "Mock", "Skin", "Tongue") )

#测序深度进行统一
total = median(sample_sums(GP))
standf = function(x, t=total) round(t * (x / sum(x)))
gps = transform_sample_counts(GP, standf)
sample_sums(gps)
#变异系数过滤OTU，减少一些意外OTU误差
gpsf = filter_taxa(gps, function(x) sd(x)/mean(x) > 3.0, TRUE)
#提取指定门类OTU
gpsfb = subset_taxa(gpsf, Phylum=="Bacteroidetes")
title = "plot_bar; Bacteroidetes-only"
plot_bar(gpsfb, "SampleType", "Abundance", title=title)

#######################基于phyloseq的微生物群落聚类的计算
library("plyr"); packageVersion("plyr")
data(enterotype)
enterotype <- subset_taxa(enterotype, Genus != "-1")
#我们进行查看发现有很多的距离算法
dist_methods <- unlist(distanceMethodList)
print(dist_methods)
#删除需要tree的距离算法
# Remove them from the vector
dist_methods <- dist_methods[-(1:3)]
# 删除需要用户自定义的距离算法
dist_methods["designdist"]
dist_methods = dist_methods[-which(dist_methods=="ANY")]

plist <- vector("list", length(dist_methods))
names(plist) = dist_methods
for( i in dist_methods ){
  # Calculate distance matrix
  iDist <- distance(enterotype, method=i)
  # Calculate ordination
  iMDS  <- ordinate(enterotype, "MDS", distance=iDist)
  ## Make plot
  # Don't carry over previous plot (if error, p will be blank)
  p <- NULL
  # Create plot, store as temp variable, p
  p <- plot_ordination(enterotype, iMDS, color="SeqTech", shape="Enterotype")
  # Add title to each plot
  p <- p + ggtitle(paste("MDS using distance method ", i, sep=""))
  # Save the graphic to file.
  plist[[i]] = p
}

df = ldply(plist, function(x) x$data)
names(df)[1] <- "distance"
p = ggplot(df, aes(Axis.1, Axis.2, color=SeqTech, shape=Enterotype))
p = p + geom_point(size=3, alpha=0.5)
p = p + facet_wrap(~distance, scales="free")
p = p + ggtitle("MDS on various distance metrics for Enterotype dataset")
p


df = ldply(plist, function(x) x$data)
names(df)[1] <- "distance"
p = ggplot(df, aes(Axis.1, Axis.2, color=Enterotype, shape=SeqTech))
p = p + geom_point(size=3, alpha=0.5)
p = p + facet_wrap(~distance, scales="free")
p = p + ggtitle("MDS on various distance metrics for Enterotype dataset")
p

#jsd距离查看效果
print(plist[["jsd"]])

print(plist[["jaccard"]])

print(plist[["bray"]])


################################差异统计

library("phyloseq"); packageVersion("phyloseq")
library("cluster"); packageVersion("cluster")
library("ggplot2"); packageVersion("ggplot2")
##统计差异
theme_set(theme_bw())
# Load data
data(enterotype)
#jsd距离进行MDS排序
exord = ordinate(enterotype, method="MDS", distance="jsd")

pam1 = function(x, k){list(cluster = pam(x,k, cluster.only=TRUE))}
x = phyloseq:::scores.pcoa(exord, display="sites")
# gskmn = clusGap(x[, 1:2], FUN=kmeans, nstart=20, K.max = 6, B = 500)
gskmn = clusGap(x[, 1:2], FUN=pam1, K.max = 6, B = 50)

gap_statistic_ordination = function(ord, FUNcluster, type="sites", K.max=6, axes=c(1:2), B=500, verbose=interactive(), ...){
  require("cluster")
  #   If "pam1" was chosen, use this internally defined call to pam
  if(FUNcluster == "pam1"){
    FUNcluster = function(x,k) list(cluster = pam(x, k, cluster.only=TRUE))     
  }
  # Use the scores function to get the ordination coordinates
  x = phyloseq:::scores.pcoa(ord, display=type)
  #   If axes not explicitly defined (NULL), then use all of them
  if(is.null(axes)){axes = 1:ncol(x)}
  #   Finally, perform, and return, the gap statistic calculation using cluster::clusGap  
  clusGap(x[, axes], FUN=FUNcluster, K.max=K.max, B=B, verbose=verbose, ...)
}

plot_clusgap = function(clusgap, title="Gap Statistic calculation results"){
  require("ggplot2")
  gstab = data.frame(clusgap$Tab, k=1:nrow(clusgap$Tab))
  p = ggplot(gstab, aes(k, gap)) + geom_line() + geom_point(size=5)
  p = p + geom_errorbar(aes(ymax=gap+SE.sim, ymin=gap-SE.sim))
  p = p + ggtitle(title)
  return(p)
}

gs = gap_statistic_ordination(exord, "pam1", B=50, verbose=FALSE)
print(gs, method="Tibs2001SEmax")

plot_clusgap(gs)

plot(gs, main = "Gap statistic for the 'Enterotypes' data")
mtext("Looks like 4 clusters is best, with 3 and 5 close runners up.")


#############################################################################
###Performs a double principal coordinate analysis：dpcoa
data(esophagus)
eso.dpcoa <- DPCoA(esophagus)
eso.dpcoa
plot_ordination(esophagus, eso.dpcoa, "samples")
plot_ordination(esophagus, eso.dpcoa, "species")
plot_ordination(esophagus, eso.dpcoa, "biplot")
#
#
# # # # # # GlobalPatterns
data(GlobalPatterns)
# subset GP to top-150 taxa (to save computation time in example)
keepTaxa <- names(sort(taxa_sums(GlobalPatterns), TRUE)[1:150])
GP       <- prune_taxa(keepTaxa, GlobalPatterns)
# Perform DPCoA
GP.dpcoa <- DPCoA(GP)
plot_ordination(GP, GP.dpcoa, color="SampleType")

###Summarize alpha diversity:estimate_richness
#### Default is all available measures
estimate_richness(esophagus) #c("Observed", "Chao1", "ACE", "Shannon", "Simpson", "InvSimpson", "Fisher").

###Unifrac
#UniFrac(physeq, weighted=FALSE, normalized=TRUE, parallel=FALSE, fast=TRUE)
################################################################################
# Perform UniFrac on esophagus data
################################################################################
data("esophagus")
(y <- UniFrac(esophagus, TRUE))
UniFrac(esophagus, TRUE, FALSE)
UniFrac(esophagus, FALSE)
# ################################################################################
# # Now try a parallel implementation using doParallel, which leverages the 
# # new 'parallel' core package in R 2.14.0+
# # Note that simply loading the 'doParallel' package is not enough, you must
# # call a function that registers the backend. In general, this is pretty easy
# # with the 'doParallel package' (or one of the alternative 'do*' packages)
# #
# # Also note that the esophagus example has only 3 samples, and a relatively small
# # tree. This is fast to calculate even sequentially and does not warrant
# # parallelized computation, but provides a good quick example for using UniFrac()
# # in a parallel fashion. The number of cores you should specify during the
# # backend registration, using registerDoParallel(), depends on your system and
# # needs. 3 is chosen here for convenience. If your system has only 2 cores, this
# # will probably fault or run slower than necessary.
# ################################################################################
library(doParallel)
data(esophagus)
# For SNOW-like functionality (works on Windows):
cl <- makeCluster(3)
registerDoParallel(cl)
UniFrac(esophagus, TRUE)
# Force to sequential backed:
registerDoSEQ()
# For multicore-like functionality (will probably not work on windows),
# register the backend like this:
registerDoParallel(cores=3)
UniFrac(esophagus, TRUE)
################################################################################

rm(list=ls())
##################有用的数据集
data(GlobalPatterns);?GlobalPatterns #Global patterns of 16S rRNA diversity at a depth of millions of sequences per sample (2011)
data(soilrep);?soilrep  #Reproducibility of soil microbiome data (2011)

GlobalPatterns
#查看OTU数量
ntaxa(GlobalPatterns)    #19216
#查看样品数量
nsamples(GlobalPatterns)  #26
#样品名查看
sample_names(GlobalPatterns)
#查看部分OTU表格矩阵
otu_Global = otu_table(GlobalPatterns)
reads = sort(apply(otu_Global,2,sum)) ##单个样本reads从58688-2357181


soilrep
#查看OTU数量
ntaxa(soilrep)    #16825
#查看样品数量
nsamples(soilrep)  #56
#样品名查看
sample_names(soilrep)
#查看部分OTU表格矩阵
otu_soilrep = otu_table(soilrep)
reads = sort(apply(otu_soilrep,2,sum)) ##单个样本reads从889-4352
reads

estimate_richness(soilrep)
