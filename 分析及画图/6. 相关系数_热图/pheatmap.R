##2018.1.11
##https://mp.weixin.qq.com/s?__biz=MzUzMTEwODk0Ng==&mid=2247484763&idx=1&sn=56b5e5183b19d9399efc9707e503e931&chksm=fa46c066cd3149706739d5683135035cddc7324845a49edd8b7a5695674bad7e6c9307c17021&scene=0#rd
##热图最佳实践-pheatmap 

rm(list=ls(all=TRUE))

# Create test matrix
## Just replace the test matrix with your own data.

test = matrix(rnorm(200), 20, 10)
?seq
#test[1:10, seq(1, 10, 2)]  1-10行，列为1-10，间隔为2，即1,3,5,7,9列。
test[1:10, seq(1, 10, 2)] = test[1:10, seq(1, 10, 2)] + 3  ##所有值+3
#2,4,6,8,10列
test[11:20, seq(2, 10, 2)] = test[11:20, seq(2, 10, 2)] + 2

test[15:20, seq(2, 10, 2)] = test[15:20, seq(2, 10, 2)] + 4

colnames(test) = paste("Test", 1:10, sep = "")
rownames(test) = paste("Gene", 1:20, sep = "")
test


# Draw heatmaps
library(pheatmap)
#update.packages("pheatmap")
packageVersion("pheatmap")  ##1.0.10
pheatmap(test)
pheatmap(test, kmeans_k = 2)
pheatmap(test, scale = "row", clustering_distance_rows = "correlation")
pheatmap(test, color = colorRampPalette(c("navy", "white", "firebrick3"))(50) )
pheatmap(test, cluster_row = FALSE)
pheatmap(test, legend = FALSE)


# Show text within cells
pheatmap(test, display_numbers = TRUE)
pheatmap(test, display_numbers = TRUE, number_format = "%.1e")
pheatmap(test, display_numbers = matrix(ifelse(test > 5, "*", ""), nrow(test)))
pheatmap(test, cluster_row = FALSE, legend_breaks = -1:4, legend_labels = c("0","1e-4", "1e-3", "1e-2", "1e-1", "1"))


# Fix cell sizes and save to file with correct size

pheatmap(test, cellwidth = 15, cellheight = 12, main = "Example heatmap")
#输出到桌面
pheatmap(test, cellwidth = 15, cellheight = 12, fontsize = 8, filename = "test.pdf")

# Generate annotations for rows and columns
annotation_col = data.frame(CellType = factor(rep(c("CT1", "CT2"), 5)),Time = 1:5)
rownames(annotation_col) = paste("Test", 1:10, sep = "")
annotation_row = data.frame(
GeneClass = factor(rep(c("Path1", "Path2", "Path3"), c(10, 4, 6))))
rownames(annotation_row) = paste("Gene", 1:20, sep = "")

# Display row and color annotations
pheatmap(test, annotation_col = annotation_col)
pheatmap(test, annotation_col = annotation_col, annotation_legend = FALSE)
pheatmap(test, annotation_col = annotation_col, annotation_row = annotation_row)


# Specify colors
ann_colors = list(
Time = c("white", "firebrick"),
CellType = c(CT1 = "#1B9E77", CT2 = "#D95F02"),
GeneClass = c(Path1 = "#7570B3", Path2 = "#E7298A", Path3 = "#66A61E"))
pheatmap(test, annotation_col = annotation_col, annotation_colors = ann_colors, main = "Title")
pheatmap(test, annotation_col = annotation_col, annotation_row = annotation_row,annotation_colors = ann_colors)
pheatmap(test, annotation_col = annotation_col, annotation_colors = ann_colors[2])


# Gaps in heatmaps
pheatmap(test, annotation_col = annotation_col, cluster_rows = FALSE, gaps_row = c(10, 14))
pheatmap(test, annotation_col = annotation_col, cluster_rows = FALSE, gaps_row = c(10, 14),cutree_col = 2)

# Show custom strings as row/col names
labels_row = c("", "", "", "", "", "", "", "", "", "", "", "", "", "", "","", "", "Il10", "Il15", "Il1b")
pheatmap(test, annotation_col = annotation_col, labels_row = labels_row)


# Specifying clustering from distance matrix
drows = dist(test, method = "minkowski")
dcols = dist(t(test), method = "minkowski")
pheatmap(test, clustering_distance_rows = drows, clustering_distance_cols = dcols)

# Modify ordering of the clusters using clustering callback option

callback = function(hc, mat){
sv = svd(t(mat))$v[,1]
dend = reorder(as.dendrogram(hc), wts = sv)
as.hclust(dend)}
pheatmap(test, clustering_callback = callback)

## Not run:

# Same using dendsort package

library(dendsort)
callback = function(hc, ...){dendsort(hc)}
pheatmap(test, clustering_callback = callback)

## End(Not run)

#2019.6.18
#获取pheatmap聚类后和标准化后的结果 
#https://mp.weixin.qq.com/s/ZJJSnwupETia7K_NDP5vAA

#生成测试数据
mat <- matrix(rnorm(30), nrow=5)
colnames(mat) <- paste("sample", 1:6, sep="_")
rownames(mat) <- paste("gene", 1:5, sep="_")
mat
#绘图
library(pheatmap)
# 绘图同时存储绘图结果
(a <- pheatmap(mat, cluster_rows = T, cluster_cols = T))

#提取聚类后的原始矩阵
# 查看绘图数据的结构
# 直接查看会很大，这里只展示其前2层
# str: structure
str(a, max.level = 2)

#重新组合行和列，完成提取
mat_cluster <- mat[a$tree_row$order, a$tree_col$order]
mat_cluster

#提取聚类后的标准化矩阵
(a <- pheatmap(mat_cluster, scale="row", display_numbers = T))

#直接提取不太方便。这可以自己先对数据scale标准化处理，再排序。
mat_scale <- round(t(apply(mat, 1, scale)),2)
colnames(mat_scale) <- colnames(mat)
mat_scale
#最终结果
mat_cluster <- mat_scale[a$tree_row$order, a$tree_col$order]
mat_cluster
(a <- pheatmap(mat_cluster, scale="row", display_numbers = T))
