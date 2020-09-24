##2019.8.24

##Rarefaction toolkit (RTK) 稀释曲线

install.packages("rtk")
library(rtk);?rtk

# Usage
# 
# rtk(input, repeats = 10, depth = 1000, ReturnMatrix = 0, margin = 2,
#     verbose = FALSE, threads = 1, tmpdir = NULL )

#input可以使一个矩阵或路径
#repeats重复计算次数
#depth重采样的深度
#verbose运行过程是否输出
#tmpdir临时文件储存位置

data <- matrix(sample(x = c(rep(0, 1500),rep(1:10, 500),1:1000),
                                 size = 120, replace = TRUE), 10)
data
# find the column with the lowest aboundance
samplesize <- min(colSums(data)); samplesize
# rarefy the dataset, so each column contains the same number of samples
data.rarefied <- rtk(input = data, depth = samplesize, ReturnMatrix = 1)

get.diversity(data.rarefied)
get.median.diversity(data.rarefied)
get.mean.diversity(data.rarefied)
richness <- get.diversity(data.rarefied, div = "richness")
eveness <- get.diversity(data.rarefied, div = "eveness")


 
#物种积累曲线
data  <- matrix(sample(x = c(rep(0, 15000),rep(1:10, 100)),
                            size = 10000, replace = TRUE), ncol = 80)
data.r  <- rtk(data, ReturnMatrix = 1, depth = min(colSums(data)))
# collectors curve on dataframe/matrix
collectors.curve(data, xlab = "No. of samples", ylab = "richness")
# same with rarefaction results (one matrix recommended)
collectors.curve(data.r, xlab = "No. of samples (rarefied data)", ylab = "richness")

# if you want to have an accumulated order, t compare various studies to one another:
cls <- rep_len(c("a","b","c","d"), ncol(data))  # study origin of each sample
accumOrder <- c("b","a","d","c")      # define the order, for the plot
colors     <- c(1,2,3,4)
names(colors) <- accumOrder # names used for legend
collectors.curve(data, xlab = "No. of samples",
                 ylab = "richness", col = colors, bin = 1,cls = cls, 
                 accumOrder = accumOrder)


##稀释曲线
data  <- matrix(sample(x = c(rep(0, 1500),rep(1:10, 500),1:1000),
                                 size = 120, replace = TRUE), 40)
# find the column with the lowest aboundance
samplesize  <- min(colSums(data))
# rarefy the dataset, so each column contains the same number of samples
d1  <- rtk(input = data, depth = samplesize)
# rarefy to different depths between 1 and samplesize
d2  <- rtk(input = data, depth = round(seq(1, samplesize, length.out = 10)))

# just the richness of all three samples as boxplot
plot(d1, div = "richness")
#rarefaction curve for each sample with fit
plot(d2, div = "richness", fit = "arrhenius", pch = c(1,2,3))
# Rarefaction curve with boxplot, sampels pooled together (grouped)
plot(d2, div = "richness", fit = FALSE, boxplot = TRUE, col = 1, groups = rep(1, ncol(data)))

