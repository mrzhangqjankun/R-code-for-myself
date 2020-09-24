##2019.7.11

##一个群落beta多样性分析的完整R脚本 

##https://mp.weixin.qq.com/s/YNwctUlA9WcVZw27wT2-lw

# 参考文献：
# Abbas, S., Nichol, J. E., Zhang, J., & Fischer, G. A. (2019). The accumulation of species and recovery of species composition along a 70-year succession in a tropical secondary forest. Ecological Indicators, 106, 105524. DOI:10.1016/j.ecolind.2019.105524
# R脚本各部分内容如下：
# 1 稀疏化曲线，用于比较个体数不同样方的物种丰富度
# 2 Mantel检验两个距离矩阵的相关性是否显著
# 3 指示种分析、PCNM、环境因子的前向筛选、物种组成的方差分解以及显著性检验
# 4 betadisper检验beta多样性各组的离散度是否有统计差异，Mantel Correlogram展示距离矩阵相关的精细结构
# 5 分析不同演替阶段生活型组成的变化
# 6 提取丰富度最高的种
# 7 NMDS分析，用以展示不同演替阶段样方物种组成相似性的关系

#为了用我们的数据，把原来代码里的AGE_MEDIAN全换为了H2


# 1 稀疏化曲线，用于比较个体数不同样方的物种丰富度 -----------------------------------------------

#1 稀疏化曲线，用于比较个体数不同样方的物种丰富度
setwd("E:/桌面/R script 2017/一个群落beta多样性分析的完整R脚本/")

rm(list = ls())
library(vegan)
dat <- read.table("otu.txt",
                row.names = 1, header = TRUE,sep='\t')
dat[is.na(dat)] <- 0
tdat <- t(dat)
env <- read.table("env.txt", header = TRUE,sep='\t',row.names = 1)
env
colors_new <- c("red", "pink", "orange", "lightgreen", "blue")

library(plyr)
coll <- factor(env$H2)
coll <- mapvalues(coll, from = c("1", "1.1","1.3","1.4","1.5"), to = colors_new) ##分组用
?mapvalues #用向量或因子中的新值替换指定值。
#### Using Minimum
S <- specnumber(tdat) # observed number of species
(raremax <- min(rowSums(tdat))) ### Number of individuals to be sampled

?rarefy #Rarefaction Species Richness
res_rarefy <- rarefy(tdat, raremax)
res_rarefy
ndat <- data.frame(env, spn = S, rare_spn = res_rarefy);ndat
summary(aov(spn ~ H2, data = ndat))
summary(aov(res_rarefy ~ H2, data = ndat))
# tiff(filename = "rarecurve.tiff",width = 5000, height = 4800, res = 800,  compression = "lzw")
f=rarecurve(tdat, step = 20, sample = raremax,
          col = as.character(coll),
          cex = 0.6,
          main = "Rarefaction Curve (107 individuals)",
          xlab = "Number of individuals",
          ylab = "Number of species")
class(f)
ff = as.data.frame(unlist(f))
legend(lty = c(1,1,1),
       col = colors_new,
       legend = c("1", "1.1","1.3","1.4","1.5"), x = "topleft", title = "H2",  lwd = 2)
?rarecurve

######################虫虫代码
raremax <- min(rowSums(tdat))
inter=seq(0,raremax,by=1000)

drare <- rarefy(tdat,inter)

rarepoint <- as.data.frame(t(rbind(inter,drare)))
#colnames(rarepoint)[2:ncol(rarepoint)]

library(reshape2)
point<-melt(rarepoint,id.vars=c("inter"),variable.name="group",value.name="reads")
point
#write.csv(point,'rarecurve')
library(ggplot2)
plot_theme = theme(panel.background=element_blank(),
                   panel.grid=element_blank(),
                   axis.line.x=element_line(size=.5, colour="black"),
                   axis.line.y=element_line(size=.5, colour="black"),
                   axis.ticks=element_line(color="black"),
                   axis.text=element_text(color="black", size=20),
                   legend.position="right",
                   legend.background=element_blank(),
                   legend.key=element_blank(),
                   legend.text= element_text(size=20),
                   text=element_text(family="sans", size=20)) 
p = ggplot()+geom_line(dat=point,aes(x=inter,y=reads,group=group),color=as.numeric(point$group),size=2)+plot_theme
p
#################虫虫代码over

# dev.off()

# tiff(filename = "rarified species richness.tiff", width = 5000, height = 4800, res = 800, compression = "lzw")
boxplot(res_rarefy ~ env$H2,
        col = colors_new,
        xlab = "H2",
        ylab = "Rarified Number of Species",
        main = "Rarified species richness (107 individuals)")

# dev.off()

####################################################
#### Density, Number of Individuals of each plot
den <- apply(dat, 2, sum) #每个样本序列数
age <- env$H2

# tiff(filename = "density_species.tiff", width = 5000, height = 4800, res = 800, compression = "lzw")
boxplot(den ~ env$H2, col = colors_new,
        xlab = "Age (Years)",
        ylab = "Number of individuals per plot",
        main = "Number of individuals"
)

# dev.off()

#### legend(lty = c(1,1,1),
####        col = colors_new,
####        legend = c("7","20","39","61","70"),
####        x = "topleft",
####        title = "Age (Years)",
####        lwd = 2)
# dev.off()


res_den <- data.frame(den = den, age = as.factor(env$H2))
summary(res_aov_den <- aov(den ~ age, data = res_den))
res_tukeyhsd <- TukeyHSD(res_aov_den)
par(las = 2)
plot(res_tukeyhsd)

?TukeyHSD #Compute Tukey Honest Significant Differences

tapply(res_den$den, res_den$age, median)

#X11()  ##打开一个新的画图窗口


# 2 Mantel检验两个距离矩阵的相关性是否显著 ------------------------------------------------

#2 Mantel检验两个距离矩阵的相关性是否显著
rm(list = ls())
library(vegan)
library(ape)
library(simba)
library(ecodist)
spmat <- dat
spmat[is.na(spmat)] <- 0
tspmat <- t(spmat)
colors_new <- c("red", "pink", "orange", "lightgreen", "blue") #### Colors for different age classes

### 4.1 Compute species dissimilarity between each pair of plots

##############################################
########### Species Similarity ###############
##############################################

#### Jaccard
dist_jaccard <- vegdist(tspmat, method="jaccard")

#### Morisita
dist_Morisita <- vegdist(tspmat, method="morisita")

#### Horn–Morisita
dist_horn_morisita <- vegdist(tspmat, method="horn")

#### Bray
dist_bray <- vegdist(tspmat, method="bray")

#### Chao
dist_chao <- vegdist(tspmat, method="chao")

### 4.2 Compute topographical and edaphic distance between each pair of plots
##############################################
########### Environmental Distance############
##############################################

envdat <- env
head(envdat, 3)
#### Transformation, allowing the aspect could be used in the analysis
envdat$H2_SIN <- sin(envdat$H2/(180/pi))

?attach #这个数据加入到路径中。如下，直接用就行，不用再enddat$ 这种形式
attach(envdat)
topo <- data.frame(Long, Lat)
soil <- data.frame(H2,Ce,H2Rec,RT,H2_SIN)
topo_and_soil <- cbind(topo, soil)
hk80 <- data.frame(EneRec, SubRec)
age  <- data.frame(pH)
detach(envdat)

rownames(topo) <- rownames(envdat)
rownames(soil) <- rownames(envdat)
rownames(hk80) <- rownames(envdat)
rownames(age)  <- rownames(envdat)

topo_dist <- dist(scale(topo))
soil_dist <- dist(scale(soil))
env_dist  <- dist(scale(topo_and_soil))
hk80_dist <- dist(hk80)
age_dist  <- dist(age)
### 4.3 Mantel Test between each pair of the distance matrices

#############################################
################## Jaccard ##################
#############################################
#### use 9999 instead of 9


## dist_jaccard
vegan::mantel(xdis = dist_jaccard, ydis = topo_dist, permutations = 9)
vegan::mantel(dist_jaccard, soil_dist, permutations = 9)
vegan::mantel(dist_jaccard, hk80_dist, permutations = 9)
vegan::mantel(dist_jaccard, age_dist , permutations = 9)
vegan::mantel(dist_jaccard, env_dist , permutations = 9)

## dist_Morisita
vegan::mantel(dist_Morisita, topo_dist, permutations = 9)
vegan::mantel(dist_Morisita, soil_dist, permutations = 9)
vegan::mantel(dist_Morisita, hk80_dist, permutations = 9)
vegan::mantel(dist_Morisita, age_dist , permutations = 9)
vegan::mantel(dist_Morisita, env_dist , permutations = 9)

## dist_horn_morisita
vegan::mantel(dist_horn_morisita, topo_dist, permutations = 9)
vegan::mantel(dist_horn_morisita, soil_dist, permutations = 9)
vegan::mantel(dist_horn_morisita, hk80_dist, permutations = 9)
vegan::mantel(dist_horn_morisita, age_dist , permutations = 9)
vegan::mantel(dist_horn_morisita, env_dist , permutations = 9)

## dist_bray
vegan::mantel(dist_bray, topo_dist, permutations = 9)
vegan::mantel(dist_bray, soil_dist, permutations = 9)
vegan::mantel(dist_bray, hk80_dist, permutations = 9)
vegan::mantel(dist_bray, age_dist , permutations = 9)
vegan::mantel(dist_bray, env_dist , permutations = 9)

## dist_chao
vegan::mantel(dist_chao, topo_dist, permutations = 9)
vegan::mantel(dist_chao, soil_dist, permutations = 9)
vegan::mantel(dist_chao, hk80_dist, permutations = 9)
vegan::mantel(dist_chao, age_dist , permutations = 9)
vegan::mantel(dist_chao, env_dist , permutations = 9)

## Correlation environmental variables
#################################################################
#############Correlation environmental variables ################
#################################################################
vegan::mantel(hk80_dist,  soil_dist, permutations = 9)
vegan::mantel(age_dist ,  soil_dist, permutations = 9)
vegan::mantel(topo_dist,  soil_dist, permutations = 9)
vegan::mantel(age_dist ,  hk80_dist, permutations = 9)
vegan::mantel(topo_dist,  hk80_dist, permutations = 9)
vegan::mantel(topo_dist,  age_dist , permutations = 9)


# 3 指示种分析、PCNM、环境因子的前向筛选、物种组成的方差分解以及显著性检验 ---------------------------------

#3 指示种分析、PCNM、环境因子的前向筛选、物种组成的方差分解以及显著性检验
## 1. Preparation of dataset

rm(list = ls())
library(vegan)
library(ggplot2)
library(labdsv);?labdsv::chullord.nmds()
library(corrplot)
library(vegan)
library(reshape2)
library(stringi)
library(openxlsx)
### PCA of the environmental data
##
## library(devtools)
## install_github("vqv/ggbiplot")
library(ggbiplot)
## Loading required package: scales
## Loading required package: grid
#### load and transform the data
spmat <- dat
spmat[is.na(spmat)] <- 0
tspmat <- t(spmat)

#### Read the environment data
envdat <- envdat

envdat$H2_SIN <- sin(envdat$H2/(180/pi))

attach(envdat)
env <- data.frame(H2,Ce,H2Rec,RT,H2_SIN)
spatial <- data.frame(EneRec, SubRec)
age     <- data.frame(pH)
detach(envdat)

colors_new <- c("red", "pink", "orange", "lightgreen", "blue")

## 3. Multi Response Permutation Procedure and Mean Dissimilarity Matrix
#### MRPP (using the classes not merged)


col1 <- colorRampPalette(c("black", "darkblue", "purple", "red", "#FF7F00",
                           "yellow", "#007FFF"))

#### jaccard
mrpp(vegdist(tspmat, method = "jaccard"), grouping = envdat$H2, permutations = 9999)
#### MEAN DIST AMONG THE GROUPS SHOWING THE DETAILS
mean_dist_jaccard <- meandist(vegdist(tspmat, method = "jaccard"), grouping = envdat$H2, permutations = 9999)
#### tiff(filename = "mean_dist_jaccard.tiff", width = 4800, height = 4800, res = 600, compression = "lzw")
corrplot(mean_dist_jaccard,type = "lower", is.corr = FALSE,  col = col1(100), cl.lim = c(0, 1),tl.srt = 0, tl.offset = 1.2)
title("Mean Jaccard Distance")
?meandist

#### dev.off()

#### morisita
mrpp(vegdist(tspmat, method = "morisita"), grouping = envdat$AGE_MEDIAN, permutations = 9999)

## 4. Indicator Value for each age class  #age为分组。不同组中指示物种的丰度
res_ind <- data.frame(indval(tspmat,  envdat$H2)$pval)
significant_sp <- row.names(res_ind)[res_ind[,1] < 0.05]
?indval #计算集群或类型中物种的指标值(保真度和相对丰度)。
indval_res <- indval(tspmat,  envdat$H2)$indval

ind_dat <- indval_res[row.names(indval_res) %in% significant_sp,]
ind_dat2 <- data.frame(species = row.names(ind_dat),ind_dat)


ind_dat_metl <- melt(ind_dat2)
ind_dat_metl$age <- stri_pad_left((gsub("X", "", ind_dat_metl$variable)), width = 2, pad = "0")
ind_dat_metl$species <- gsub("_", " ", ind_dat_metl$species)

?stri_pad_left
#### tiff(filename = "indicator_species_class.tiff", width = 8000, height = 4800, res = 700, compression = "lzw")
ggplot(data=ind_dat_metl, aes(x=age, y=value, fill=age)) +
  geom_bar(stat="identity") + facet_wrap(~ species, ncol = 6) + ylab("Indicator Value")+ theme_bw()  + scale_fill_manual(values=colors_new)+
  xlab("H2")+labs(fill="H2")

#### dev.off()


### write.xlsx(indval_res[row.names(indval_res) %in% significant_sp,], "Indicator_value_of_species_each_age_class_significant.xlsx", row.names = TRUE)


## 5. PCNM and Variation Partitioning
##### Generate PCNM vectors
pcnm_plots <- pcnm(dist(spatial))
pcnm_plots_vectors <- data.frame(pcnm_plots$vectors)
mod0 <- rda(tspmat ~ 1, pcnm_plots_vectors)  # Model with intercept only
mod1 <- rda(tspmat ~ ., pcnm_plots_vectors)  # Model with all explanatory variables

### With scope present, the default direction is "both"
### forward_selection <- ordistep(mod0, scope = formula(mod1), perm.max = 1000)
?ordistep
### backward selection
### backward_selection <- ordistep(mod1, perm.max = 1000)

### Both backward and forward selection selected the PCNM vector 1,2 and 7 to represent the spatial structure.
spat <- subset(pcnm_plots_vectors, selected = c("PCNM2", "PCNM1", "PCNM7"))

env.pca <- princomp(env, cor = TRUE)
summary(env.pca)
#### devtools::install_github("vqv/ggbiplot") #### To install
#### Visualization of the PCA for environment
ggbiplot(env.pca, obs.scale = 1, var.scale = 1,
         groups = factor(age$pH), ellipse = TRUE, circle = TRUE) +
  theme(legend.direction = 'horizontal', legend.position = 'top')


#### Using forward selection to identify the most important components in explaining species composition to avoid over fitting.
env_pca_scores <- data.frame(env.pca$scores)
env_pca_model0 <- rda(tspmat ~ 1, env_pca_scores )  # Model only with intercept
env_pca_model  <- rda(tspmat ~ ., env_pca_scores )   # Model with all explanatory variables

#### Forward Selection based on R-Square explained using RDA
pca_selection_env_r2 <- ordiR2step(env_pca_model0, scope = env_pca_model)
#### Only the first two components selected, therefore only use the first two components

res_part <- varpart(tspmat, env.pca$scores[,c(1,2)], pcnm_plots_vectors, age)

### Visualize the contribution of X1: the environment; X2 the spatial distance; X3:age.
plot(res_part, Xnames = c("Env","Spatial","Age"), bg=2:4)



# 4 betadisper检验beta多样性各组的离散度是否有统计差异，Mantel Correlogram展示距离矩阵相关的精细 --------

#4 betadisper检验beta多样性各组的离散度是否有统计差异，Mantel Correlogram展示距离矩阵相关的精细结构
spmat <- dat
spmat[is.na(spmat)] <- 0
tspmat <- t(spmat)

### 4.1 Compute species dissimilarity between each pair of plots
##############################################
########### Species Similarity ###############
##############################################

#### Jaccard
dist_jaccard <- vegdist(tspmat, method="jaccard", binary = TRUE)

#### Morisita
dist_Morisita <- vegdist(tspmat, method="morisita")

#### Horn–Morisita
dist_horn_morisita <- vegdist(tspmat, method="horn")

#### Bray-Curtis
dist_bray <- vegdist(tspmat, method="bray")

#### Chao
dist_chao <- vegdist(tspmat, method="chao")


### 4.2 Compute topographical and edaphic distance between each pair of plots

##############################################
########### Environmental Distance############
##############################################

envdat <- envdat

#### Transformation, allowing the aspect could be used in the analysis
envdat$H2_SIN <- sin(envdat$H2/(180/pi))

attach(envdat)
topo <- data.frame(Long, Lat)
soil <- data.frame(H2,Ce,H2Rec,RT,H2_SIN)
hk80 <- data.frame(EneRec, SubRec)
age  <- data.frame(pH)
detach(envdat)

rownames(topo) <- rownames(envdat)
rownames(soil) <- rownames(envdat)
rownames(hk80) <- rownames(envdat)
rownames(age)  <- rownames(envdat)

topo_dist <- dist(scale(topo))
soil_dist <- dist(scale(soil))
hk80_dist <- dist(hk80)
age_dist  <- dist(age)
### 4.3 Mantel Test between each pair of the distance matrices

#############################################
################## Jaccard ##################
#############################################

mantel.test(as.matrix(dist_jaccard), as.matrix(topo_dist), nperm = 999)
mantel.test(as.matrix(dist_jaccard), as.matrix(soil_dist), nperm = 999)
mantel.test(as.matrix(dist_jaccard), as.matrix(hk80_dist), nperm = 999)
mantel.test(as.matrix(dist_jaccard), as.matrix(age_dist ), nperm = 999)

mantel.test(as.matrix(dist_Morisita), as.matrix(topo_dist), nperm = 999)
mantel.test(as.matrix(dist_Morisita), as.matrix(soil_dist), nperm = 999)
mantel.test(as.matrix(dist_Morisita), as.matrix(hk80_dist), nperm = 999)
mantel.test(as.matrix(dist_Morisita), as.matrix(age_dist ), nperm = 999)

mantel.test(as.matrix(dist_horn_morisita), as.matrix(topo_dist), nperm = 999)
mantel.test(as.matrix(dist_horn_morisita), as.matrix(soil_dist), nperm = 999)
mantel.test(as.matrix(dist_horn_morisita), as.matrix(hk80_dist), nperm = 999)
mantel.test(as.matrix(dist_horn_morisita), as.matrix(age_dist ), nperm = 999)

mantel.test(as.matrix(dist_bray), as.matrix(topo_dist), nperm = 999)
mantel.test(as.matrix(dist_bray), as.matrix(soil_dist), nperm = 999)
mantel.test(as.matrix(dist_bray), as.matrix(hk80_dist), nperm = 999)
mantel.test(as.matrix(dist_bray), as.matrix(age_dist ), nperm = 999)

mantel.test(as.matrix(dist_chao), as.matrix(topo_dist), nperm = 999)
mantel.test(as.matrix(dist_chao), as.matrix(soil_dist), nperm = 999)
mantel.test(as.matrix(dist_chao), as.matrix(hk80_dist), nperm = 999)
mantel.test(as.matrix(dist_chao), as.matrix(age_dist ), nperm = 999)

?mantel.test  ##ape
?mantel  ##vegan
?mantel #ecodist
################################################################
#################################################################
#############Correlation environmental variables ################
#################################################################
mantel.test(as.matrix(hk80_dist), as.matrix( soil_dist), graph = FALSE)
mantel.test(as.matrix(age_dist ), as.matrix( soil_dist), graph = FALSE)
mantel.test(as.matrix(topo_dist), as.matrix( soil_dist), graph = FALSE)
mantel.test(as.matrix(age_dist ), as.matrix( hk80_dist), graph = FALSE)
mantel.test(as.matrix(topo_dist), as.matrix( hk80_dist), graph = FALSE)
mantel.test(as.matrix(topo_dist), as.matrix( age_dist ), graph = FALSE)
### 4.4 Convert the distance matrices to vectors
#############################################
#############################################
?liste #将dist和矩阵转换为database格式
#as.vector(dist_jaccard)
vector_dist_jaccard        <- liste(dist_jaccard)[,3]
vector_dist_Morisita       <- liste(dist_Morisita)[,3]
vector_dist_horn_morisita  <- liste(dist_horn_morisita)[,3]
vector_dist_bray           <- liste(dist_bray)[,3]
vector_dist_chao           <- liste(dist_chao)[,3]

vector_topo_dist           <- liste(topo_dist)[,3]
vector_soil_dist           <- liste(soil_dist)[,3]
vector_hk80_dist           <- liste(hk80_dist)[,3]
vector_age_dist            <- liste(age_dist )[,3]

#############################################
beta_dat <- data.frame(vector_dist_jaccard,
                       vector_dist_Morisita,
                       vector_dist_horn_morisita,
                       vector_dist_bray,
                       vector_dist_chao,
                       vector_topo_dist,
                       vector_soil_dist,
                       vector_hk80_dist,
                       vector_age_dist)

### 4.5 The relationship between Jaccard Dissimilarity and the topographical, soil, spatial and age

###############################################################
########## Jaccard Dissimilarity, Presence-Absence ############
###############################################################

#### topo_dist
plot(vector_dist_jaccard ~ vector_topo_dist, data = beta_dat,
     col = "grey", xlab = "Topographical Distance",
     ylab = "Species Dissimilarity (1-Jaccard)",
     main = "Jaccard Dissimilarity vs. Topographical Distance")
jaccard_topo_loess <- loess(vector_dist_jaccard ~ vector_topo_dist,
                            data = beta_dat, span = 1, degree = 1);?loess
predict_jaccard_topo_loess <- predict(jaccard_topo_loess,
                                      data.frame(vector_topo_dist =
                                                   seq(min(vector_topo_dist),
                                                       max(vector_topo_dist),
                                                       length.out = 200)),
                                      se = TRUE);?predict
lines(x = seq(min(vector_topo_dist), max(vector_topo_dist), length.out = 200),
      y = predict_jaccard_topo_loess$fit, col = "blue", lwd = 2)


########################################################################################

### 4.9 The analysis of dispersion of variances within groups, all the plots being grouped by age

res_anosim_jaccard           <- anosim(dist_jaccard, grouping = envdat$H2, permutations = 9999)
res_anosim_horn_morisita     <- anosim(dist_horn_morisita, grouping = envdat$H2, permutations = 9999)
res_anosim_Morisita          <- anosim(dist_Morisita, grouping = envdat$H2, permutations = 9999)
res_anosim_Morisita          <- anosim(dist_Morisita, grouping = envdat$H2, permutations = 9999)
res_anosim_bray              <- anosim(dist_bray, grouping = envdat$H2, permutations = 9999)
res_anosim_chao              <- anosim(dist_chao, grouping = envdat$H2, permutations = 9999)

summary(res_anosim_jaccard       )
plot(res_anosim_jaccard  , main = "Jaccard Dissimilarity")
?anosim

?adonis
res_adonis2_jaccard           <- adonis2(dist_jaccard ~ H2, data = envdat, permutations = 9999)
res_adonis2_horn_morisita     <- adonis2(dist_horn_morisita ~ H2,data = envdat, permutations = 9999)
res_adonis2_Morisita          <- adonis2(dist_Morisita ~ H2, data = envdat, permutations = 9999)
res_adonis2_bray              <- adonis2(dist_bray ~ H2, data = envdat, permutations = 9999)
res_adonis2_chao              <- adonis2(dist_chao ~ H2, data = envdat, permutations = 9999)

res_adonis2_jaccard
# res_adonis2_horn_morisita
# res_adonis2_Morisita
# res_adonis2_bray
# res_adonis2_chao

?betadisper #群分散的多元均匀性(方差)
res_betadisper_jaccard       <- betadisper(dist_jaccard,       group = envdat$H2)
res_betadisper_horn_morisita <- betadisper(dist_horn_morisita, group = envdat$H2)
res_betadisper_Morisita      <- betadisper(dist_Morisita,      group = envdat$H2)
res_betadisper_bray          <- betadisper(dist_bray,          group = envdat$H2)
res_betadisper_chao          <- betadisper(dist_chao,          group = envdat$H2)
###
plot(res_betadisper_jaccard,
     ellipse = TRUE, hull = FALSE, xlim = c(-.4, .4), ylim = c(-.4, .4),
     main = "Distribution of Jaccard dissimilarity", col = c("red", "orange", "gold", "green", "blue"))

boxplot(res_betadisper_jaccard,
        col = c("red", "orange", "gold", "green", "blue"),
        main = "Jaccard dissimilarity",
        xlab = "Age of vegetation (Years)")


#### Comparison of different groups
anova(res_betadisper_jaccard         )
# anova(res_betadisper_horn_morisita   )
# anova(res_betadisper_Morisita         )
# anova(res_betadisper_bray         )
# anova(res_betadisper_chao         )

scores(res_betadisper_jaccard       )
# scores(res_betadisper_horn_morisita  )
# scores(res_betadisper_Morisita        )
# scores(res_betadisper_bray        )
# scores(res_betadisper_chao        )

#### Pairwise comparison
TukeyHSD(res_betadisper_jaccard      )
# TukeyHSD(res_betadisper_horn_morisita)
# TukeyHSD(res_betadisper_Morisita     )
# TukeyHSD(res_betadisper_bray     )
# TukeyHSD(res_betadisper_chao     )
?TukeyHSD

par(las = 2)
plot(TukeyHSD(res_betadisper_jaccard      ))


#### Figure  Multiple comparisons of the deviation between each pair of age classes for Horn Morisita dissimilairity

###################################################################################
dist_jaccard       <- as.dist(as.matrix(dist_jaccard      ))
dist_horn_morisita <- as.dist(as.matrix(dist_horn_morisita))
dist_Morisita      <- as.dist(as.matrix(dist_Morisita     ))
dist_bray          <- as.dist(as.matrix(dist_bray         ))
dist_chao          <- as.dist(as.matrix(dist_chao         ))

topo_dist          <- as.dist(as.matrix(topo_dist         ))
soil_dist          <- as.dist(as.matrix(soil_dist         ))
hk80_dist          <- as.dist(as.matrix(hk80_dist         ))
age_dist           <- as.dist(as.matrix(age_dist          ))

##### dist_jaccard
?mantel.correlog
mantel.correlog(dist_jaccard,  topo_dist)
# mantel.correlog(dist_jaccard,  soil_dist)
# mantel.correlog(dist_jaccard,  hk80_dist)
# mantel.correlog(dist_jaccard,  age_dist )

plot(mantel.correlog(dist_jaccard,  topo_dist))


# 5 分析不同演替阶段生活型组成的变化 ------------------------------------------------------

#5 分析不同演替阶段生活型组成的变化
############################################################
##### R script for Analysing change of growth forms ####
############################################################
######### Author: Jinlong Zhang ############################
######### Date: 18 MAY 2018 ################################
######### Email: jinlongzhang01@gmail.com #################
############################################################


rm(list = ls())
library(openxlsx)
library(reshape2)
library(ggplot2)
library(ggthemes)
library(stringr)

##这个表我没有，整个这部分略过
growth_form <- read.xlsx("SI Table 4. Species growth forms.xlsx")
pie(table(growth_form$growth_form))


# 6 提取丰富度最高的种 -------------------------------------------------------------

#6 提取丰富度最高的种
##略过


# 7 NMDS分析，用以展示不同演替阶段样方物种组成相似性的关系 -----------------------------------------

#7 NMDS分析，用以展示不同演替阶段样方物种组成相似性的关系
?metaMDS  ##vegan
#略过
colors_new <- c("red", "pink", "orange", "lightgreen", "blue")

ggplot(NMDS, aes(x=MDS1, y=MDS2, col=age, label = NMDS$LAB)) +
  geom_point() +
  stat_ellipse(level = 0.60, show.legend = TRUE,  type = "norm", lwd = 1.2) +
  theme_bw() +
  labs(title = "Nonmetric Multidimensional Scaling (Chao)")+
  scale_color_manual(values= colors_new) +
  xlab("NMDS 1") +
  ylab("NMDS 2") +
  theme_base() +
  geom_text(check_overlap = TRUE, aes(y = MDS2 + 0.035), show.legend=FALSE)



