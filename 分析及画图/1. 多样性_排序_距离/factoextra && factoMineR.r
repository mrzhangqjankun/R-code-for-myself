##2018.12.23
#factoextra && factoMineR
http://www.sthda.com/english/wiki/factoextra-r-package-easy-multivariate-data-analyses-and-elegant-visualization#principal-component-analysis

# factoextra is an R package making easy to extract and visualize the output of exploratory multivariate data analyses, including:
# Principal Component Analysis (PCA), which is used to summarize the information contained in a continuous (i.e, quantitative) multivariate data by reducing the dimensionality of the data without loosing important information.
# Correspondence Analysis (CA), which is an extension of the principal component analysis suited to analyse a large contingency table formed by two qualitative variables (or categorical data).
# Multiple Correspondence Analysis (MCA), which is an adaptation of CA to a data table containing more than two categorical variables.
# Multiple Factor Analysis (MFA) dedicated to datasets where variables are organized into groups (qualitative and/or quantitative variables).
# Hierarchical Multiple Factor Analysis (HMFA): An extension of MFA in a situation where the data are organized into a hierarchical structure.
# Factor Analysis of Mixed Data (FAMD), a particular case of the MFA, dedicated to analyze a data set containing both quantitative and qualitative variables.

#CA是PCA的延伸，可对大的列联表进行分析。MCA是CA的改良版本，可分析2个以上的变量。
#MFA处理分组的变量。HMFA是MFA的扩展，可处理分层结构的分组变量。FAMD是MFA一个特殊形式，可分析同时包含定性和定量的数据。

#其他可以做PCA的包。
#FactoMineR, ade4, stats, ca, MASS and ExPosition.

######################################安装。FactoMineR进行分析。factoextra进行可视化。
#install.packages("FactoMineR")
library("FactoMineR")

#install.packages("factoextra")
##Or, install the latest version from Github
# if(!require(devtools)) install.packages("devtools")
# devtools::install_github("kassambara/factoextra")
library("factoextra")

############################################################重要参数
##1.可视化结果
# fviz_eig (or fviz_eigenvalue)： 提取特征向量
# fviz_pca;fviz_ca,fviz_mca ：画图
# fviz_ellipses： 画椭圆
# fviz_cos2： Visualize the quality of representation of the row/column variable from the results of PCA, CA, MCA functions.
# fviz_contrib： 贡献

##2.提取结果
# get_eigenvalue：提取并可视化特征值
# get_pca;get_ca;get_mca;get_mfa;get_famd;get_hmfa: 提取结果
# facto_summarize：Subset and summarize the output of factor analyses.

##3.聚类和可视化
# dist(fviz_dist, get_dist)：计算和可视化距离矩阵
# get_clust_tendency：评估聚类趋势
# fviz_nbclust(fviz_gap_stat)：确定和可视化最优的聚类数
# fviz_dend：增强树状图的可视化
# fviz_cluster：可视化聚类结果
# hcut：计算层及聚类
# hkmeans (hkmeans_tree, print.hkmeans)：层级K-means聚类
# eclust:聚类分析的可视化增强

######################################################例子
# Principal component analysis
# Data: decathlon2 [in factoextra package]
# PCA function: FactoMineR::PCA()
# Visualization factoextra::fviz_pca()
data("decathlon2")
df <- decathlon2[1:23, 1:10];head(df)
res.pca <- PCA(df,  graph = FALSE)  ##结果中包含individuals (行)和 variables(列)的结果
res.pca <- PCA(df,  graph = TRUE)
# Extract eigenvalues/variances
get_eig(res.pca)

# Visualize eigenvalues/variances
fviz_screeplot(res.pca, addlabels = TRUE, ylim = c(0, 50))

# Extract the results for variables   提取列的结果
var=get_pca_var(res.pca)
var
# Contribution of variables   ##每个变量（列）的贡献
head(var$contrib)

# Graph of variables: default plot
fviz_pca_var(res.pca, col.var = "black")

# Control variable colors using their contributions
fviz_pca_var(res.pca, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
)

# Contributions of variables to PC1
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)

# Extract the results for individuals  提取行的结果
ind <- get_pca_ind(res.pca)
ind

# Graph of individuals
# 1. Use repel = TRUE to avoid overplotting
# 2. Control automatically the color of individuals using the cos2
# cos2 = the quality of the individuals on the factor map
# Use points only
# 3. Use gradient color
fviz_pca_ind(res.pca, col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping (slow if many points)
)

# Biplot of individuals and variables
fviz_pca_biplot(res.pca, repel = TRUE)

#Color individuals by groups:
  # Compute PCA on the iris data set
  # The variable Species (index = 5) is removed before PCA analysis
  iris.pca <- PCA(iris[,-5], graph = FALSE)
# Visualize
# Use habillage to specify groups for coloring
fviz_pca_ind(iris.pca,
             label = "none", # hide individual labels
             habillage = iris$Species, # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE # Concentration ellipses
)


# Correspondence analysis
# Data: housetasks [in factoextra]
# CA function FactoMineR::CA()
# Visualize with factoextra::fviz_ca()
data("housetasks")
res.ca <- CA(housetasks, graph = FALSE)  #ca的结果分为row和col。和pca有所不同。

#Extract results for row/column variables:
  # Result for row variables
  get_ca_row(res.ca)
# Result for column variables
get_ca_col(res.ca)
#Biplot of rows and columns
fviz_ca_biplot(res.ca, repel = TRUE)

#To visualize only row points or column points, type this:
  # Graph of row points
  fviz_ca_row(res.ca, repel = TRUE)
# Graph of column points
fviz_ca_col(res.ca)
# Visualize row contributions on axes 1
fviz_contrib(res.ca, choice ="row", axes = 1)
# Visualize column contributions on axes 1
fviz_contrib(res.ca, choice ="col", axes = 1)


# Multiple correspondence analysis
# Data: poison [in factoextra]
# MCA function FactoMineR::MCA()
# Visualization factoextra::fviz_mca()

#Computing MCA:
library(FactoMineR)
data(poison);head(poison,3)
res.mca <- MCA(poison, quanti.sup = 1:2,  ##前两列为数字，为定量数据。3-4列为字符，为定性数据。
               quali.sup = 3:4, graph=FALSE)
#Extract results for variables and individuals:
  # Extract the results for variable categories
  get_mca_var(res.mca)
# Extract the results for individuals
get_mca_ind(res.mca)
#Contribution of variables and individuals to the principal axes:
  # Visualize variable categorie contributions on axes 1
  fviz_contrib(res.mca, choice ="var", axes = 1)
# Visualize individual contributions on axes 1
# select the top 20
fviz_contrib(res.mca, choice ="ind", axes = 1, top = 20)
#Graph of individuals
# Color by groups
# Add concentration ellipses
# Use repel = TRUE to avoid overplotting
grp <- as.factor(poison[, "Vomiting"])
fviz_mca_ind(res.mca,  habillage = grp,
             addEllipses = TRUE, repel = TRUE)
#Graph of variable categories:

#Biplot of individuals and variables:
    fviz_mca_biplot(res.mca, repel = TRUE)
 
###################################Cluster analysis and factoextra     聚类分析
#####Partitioning clustering
    # 1. Loading and preparing data
    data("USArrests")
    df <- scale(USArrests)
    # 2. Compute k-means
    set.seed(123)
    km.res <- kmeans(scale(USArrests), 4, nstart = 25)
    # 3. Visualize
    library("factoextra")
    fviz_cluster(km.res, data = df,
                 palette = c("#00AFBB","#2E9FDF", "#E7B800", "#FC4E07"),
                 ggtheme = theme_minimal(),
                 main = "Partitioning Clustering Plot"
    )
    
#####Hierarchical clustering
    library("factoextra")
    # Compute hierarchical clustering and cut into 4 clusters
    res <- hcut(USArrests, k = 4, stand = TRUE)
    # Visualize
    fviz_dend(res, rect = TRUE, cex = 0.5,
              k_colors = c("#00AFBB","#2E9FDF", "#E7B800", "#FC4E07"))
    
#####Determine the optimal number of clusters
    # Optimal number of clusters for k-means
    library("factoextra")
    my_data <- scale(USArrests)
    fviz_nbclust(my_data, kmeans, method = "gap_stat")    
#    Suggested number of cluster: 3    
    
    
# Reference：
# Sebastien Le, Julie Josse, Francois Husson (2008). FactoMineR: An R Package for Multivariate Analysis. Journal of Statistical Software, 25(1), 1-18. 10.18637/jss.v025.i01    
   
    
    
##一个绝佳的学习R的网站：
  http://www.sthda.com/english/
    
##非常详细的讲解了PCA的原理，过程及结果的含义。 并结合factoMineR和factoextra详细讲解相关代码。
 http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/112-pca-principal-component-analysis-essentials/
   
##CA
 http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/113-ca-correspondence-analysis-in-r-essentials/
    
##MCA
 www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/114-mca-multiple-correspondence-analysis-in-r-essentials/
    
##Clustering methods
 https://www.datanovia.com/en/blog/types-of-clustering-methods-overview-and-quick-start-r-code/

    
    
    
    