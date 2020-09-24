##2018.1.7
#mobsim: An r package for the simulation and measurement of biodiversity across spatial scales

#计算物种稀释曲线和积累曲线，种面积关系（TAR）及距离衰减（DDR）

#install.packages("mobsim")

library(mobsim)
?mobsim

##########1. 模拟物种丰度及分布
# sim_sad  ##模拟SAD

#可选模型c("lnorm", "bs", "gamma", "geom", "ls",
#"mzsm", "nbinom", "pareto", "poilog", "power", "powbend", "weibull")
#Simulate log-normal species abundance distribution
sad_lnorm1 <- sim_sad(s_pool = 100, n_sim = 10000, sad_type = "lnorm", #s_pool 物种数; n_sim个体数
                      sad_coef = list("meanlog" = 5, "sdlog" = 0.5))
plot(sad_lnorm1, method = "octave")
plot(sad_lnorm1, method = "rank")

# Alternative parameterization of the log-normal distribution
sad_lnorm2 <- sim_sad(s_pool = 100, n_sim = 10000, sad_type = "lnorm",
                      sad_coef = list("cv_abund" = 0.5))  #cv,变异系数。cv_abund即丰度/平均丰度的标准差。与均匀度负相关
plot(sad_lnorm2, method = "octave")

# Fix species richness in the simulation by adding rare species
sad_lnorm3a <- sim_sad(s_pool = 500, n_sim = 10000, sad_type = "lnorm",
                       sad_coef = list("cv_abund" = 5), fix_s_sim = TRUE) #是否应该限制模拟的本地群落的物种数量
sad_lnorm3b <- sim_sad(s_pool = 500, n_sim = 10000, sad_type = "lnorm",
                       sad_coef = list("cv_abund" = 5))

plot(sad_lnorm3a, method = "rank")
points(1:length(sad_lnorm3b), sad_lnorm3b, type = "b", col = 2)
legend("topright", c("fix_s_sim = TRUE","fix_s_sim = FALSE"),
       col = 1:2, pch = 1)

# Different important SAD models

# Fisher's log-series
sad_logseries <- sim_sad(s_pool = NULL, n_sim = 10000, sad_type = "ls",
                         sad_coef = list("N" = 1e5, "alpha" = 20))

# Poisson log-normal
sad_poilog <- sim_sad(s_pool = 100, n_sim = 10000, sad_type = "poilog",
                      sad_coef = list("mu" = 5, "sig" = 0.5))

# Mac-Arthur's broken stick
sad_broken_stick <- sim_sad(s_pool = NULL, n_sim = 10000, sad_type = "bs",
                            sad_coef = list("N" = 1e5, "S" = 100))

# Plot all SADs together as rank-abundance curves
plot(sad_logseries, method = "rank")
lines(1:length(sad_lnorm2), sad_lnorm2, type = "b", col = 2)
lines(1:length(sad_poilog), sad_poilog, type = "b", col = 3)
lines(1:length(sad_broken_stick), sad_broken_stick, type = "b", col = 4)
legend("topright", c("Log-series","Log-normal","Poisson log-normal","Broken stick"),
       col = 1:4, pch = 1)

# sim_poisson_coords  ##SAD增加空间上的随机坐标
abund <- sim_sad(s_pool = 100, n_sim = 1000)
sim_com1 <- sim_poisson_coords(abund)
plot(abund)
plot(sim_com1)
summary(sim_com1)

# sim_thomas_coords #向SAD中添加聚集的位置。方法为Poisson cluster process
abund <- c(10,20,50,100)
sim1 <- sim_thomas_coords(abund, sigma = 0.02)
plot(sim1)

# Simulate species "ranges"
sim2 <- sim_thomas_coords(abund, sigma = 0.02, mother_points = 1)
plot(sim2)

# Equal numbers of points per cluster
sim3 <- sim_thomas_coords(abund, sigma = 0.02, cluster_points = 5)
plot(sim3)

# With large sigma the distribution will be essentially random (see Details)
sim4 <- sim_thomas_coords(abund, sigma = 10)
plot(sim4)

# sim_poisson_community  #模拟确定的SAD和随机的空间坐标。是 sim_sad and sim_poisson_coords的结合
com1 <- sim_poisson_community(s_pool = 20, n_sim = 500, sad_type = "lnorm",
                              sad_coef = list("meanlog" = 2, "sdlog" = 1))
plot(com1)

# sim_thomas_community   #模拟确定的SAD和种内聚集
com1 <- sim_thomas_community(s_pool = 20, n_sim = 500, sad_type = "lnorm",
                             sad_coef = list("meanlog" = 2, "sdlog" = 1),
                             sigma = 0.01)
plot(com1)


#########2. 分析物种丰度及分布
# rare_curve  #输入为SAD
sad1 <- sim_sad(100, 2000, sad_type = "lnorm", sad_coef = list("meanlog" = 2,
                                                               "sdlog" = 1))
rc1 <- rare_curve(sad1)
plot(rc1, type = "l", xlab = "Sample size", ylab = "Expected species richness")

#和vegan包的rarefy类似。
require(vegan)
data(BCI)
S <- specnumber(BCI) # observed number of species
(raremax <- min(rowSums(BCI)))
Srare <- rarefy(BCI, raremax)
plot(S, Srare, xlab = "Observed No. of Species", ylab = "Rarefied No. of Species")
abline(0, 1)
rarecurve(BCI, step = 20, sample = raremax, col = "blue", cex = 0.6)


# spec_sample_curve  ##稀释或累计曲线
sim_com1 <- sim_thomas_community(s_pool = 100, n_sim = 1000)
sac1 <- spec_sample_curve(sim_com1, method = c("rare","acc"))

head(sac1)
plot(sac1)


# divar
#Estimate diversity indices in subplots of different sizes. 
#This includes the well-known species-area and endemics-area relationships. 
# Usage
# 
# divar(comm, prop_area = seq(0.1, 1, by = 0.1), n_samples = 100,
#       exclude_zeros = T)
# Arguments
# 
# comm	
# community object
# prop_area	
# Subplot sizes as proportion of the total area (numeric)
# n_samples	
# Number of randomly located subplots per subplot size (single integer)
# exclude_zeros	
# Should subplots without individuals be excluded? (logical)
sim1 <- sim_thomas_community(100, 1000)
divar1 <- divar(sim1, prop_area = seq(0.01, 1.0, length = 20))
plot(divar1)
?divar

# dist_decay
# Usage
# community(x, y, spec_id, xrange = c(0, 1), yrange = c(0, 1))
# Arguments
# x, y Coordinates of individuals (numeric)
# spec_id Species names or IDs; can be integers, characters or factors
# xrange Extent of the community in x-direction (numeric vector of length 2)
# yrange Extent of the community in y-direction (numeric vector of length 2) 
sim_com1 <- sim_thomas_community(100, 10000, sigma = 0.1, mother_points = 2)
dd1 <- dist_decay(sim_com1, prop_area = 0.005, n_samples = 20)
plot(dd1)
?dist_decay

# sample_quadrats
library(vegan)
sim_com1 <- sim_poisson_community(100, 10000)
comm_mat1 <- sample_quadrats(sim_com1, n_quadrats = 100,
                             quadrat_area = 0.002, method = "grid")
specnumber(comm_mat1$spec_dat)
diversity(comm_mat1$spec_dat, index = "shannon")

##########其他
#community
# Usage
# community(x, y, spec_id, xrange = c(0, 1), yrange = c(0, 1))
# Arguments
# x, y Coordinates of individuals (numeric)
# spec_id Species names or IDs; can be integers, characters or factors
# xrange Extent of the community in x-direction (numeric vector of length 2)
# yrange Extent of the community in y-direction (numeric vector of length 2)
x <- runif(100)
y <- runif(100)
species_names <- rep(paste("species",1:10, sep = ""), each = 10)
com1 <- community(x,y, species_names)
plot(com1)
summary(com1)

#div_rand_rect    计算多样性指数及标准偏差
# Usage
# div_rand_rect(prop_area = 0.25, comm, n_rect = 100, exclude_zeros = F)
# Arguments
# prop_area Size of subplots as proportion of the total area
# comm community object
# n_rect Number of randomly located subplots
# exclude_zeros Should subplots without individuals be excluded? (logical)
sim1 <- sim_poisson_community(100,1000)
div_rand_rect(prop_area = 0.1, comm = sim1)
