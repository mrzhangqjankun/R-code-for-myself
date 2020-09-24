##2019.6.27
#SourceTracker
#https://mp.weixin.qq.com/s/LgGDR6Tl6eTOBwIXbcBUzw

#该软件中目标样本为Sink，微生物污染源或来源的样品为Source；基于贝叶斯算法，探究目标样本（Sink）中微生物污染源或来源（Source）的分析。
#根据Source样本和Sink样本的群落结构分布，来预测Sink样本中来源于各Source样本的组成比例。

# This runs SourceTracker on the original "contamination" data set
# (data included in 'data' folder)

getwd()
# load sample metadata
metadata <- read.table('./sourcetracker-1.0/data/metadata.txt',sep='\t',h=T,row.names=1,check=F,comment='')

# load OTU table
# This 'read.table' command is designed for a
# QIIME-formatted OTU table.
# namely, the first line begins with a '#' sign
# and actually _is_ a comment; the second line
# begins with a '#' sign but is actually the header
# 读取行、列名，跳过一行，注释为空即读#号行
otus <- read.table('./sourcetracker-1.0/data/otus.txt',sep='\t', header=T,row.names=1,check=F,skip=1,comment='')
# 读入数据框变矩阵，且转置为样本为行的旧格式
otus <- t(as.matrix(otus))

# extract only those samples in common between the two tables
common.sample.ids <- intersect(rownames(metadata), rownames(otus))
otus <- otus[common.sample.ids,]
metadata <- metadata[common.sample.ids,]
# double-check that the mapping file and otu table
# had overlapping samples
# 判断是否存在共有样品，否则退出
if(length(common.sample.ids) <= 3) {
  message <- paste(sprintf('Error: there are %d sample ids in common ',length(common.sample.ids)),
                   'between the metadata file and data table')
  stop(message)
}

# extract the source environments and source/sink indices
# 筛选哪些是来源或目标真假T/F，which转化为位置编号
# 共筛选训练集180个，测试集125个
train.ix <- which(metadata$SourceSink=='source')
test.ix <- which(metadata$SourceSink=='sink')
# 测试集太多，只保留6个样品做演示
test.ix = head(test.ix)
envs <- metadata$Env
# 判断是否存在Description列，存在列保存于desc
if(is.element('Description',colnames(metadata))) desc <- metadata$Description

# load SourceTracker package
# 加载软件包
source('./sourcetracker-1.0/src/SourceTracker.r')

# tune the alpha values using cross-validation (this is slow!)
# 使用交叉验证调整alpha值，非常耗时
# tune.results <- tune.st(otus[train.ix,], envs[train.ix])
# alpha1 <- tune.results$best.alpha1
# alpha2 <- tune.results$best.alpha2
# note: to skip tuning, run this instead:
# 跳过优化alpha值步骤，直接设置为0.001继续计算
alpha1 <- alpha2 <- 0.001

# train SourceTracker object on training data
# 基于训练集和对应描述获得预测模型
st <- sourcetracker(otus[train.ix,], envs[train.ix])

# Estimate source proportions in test data
# 估计测试集中来源比例
results <- predict(st,otus[test.ix,], alpha1=alpha1, alpha2=alpha2)

# Estimate leave-one-out source proportions in training data
# 在训练集中留一法（一种交叉验证方法）估计来源比例，计算次数等于训练集样本数量，极耗时
# results.train <- predict(st, alpha1=alpha1, alpha2=alpha2)

# plot results
# 结果绘图, 将环境和描述列合并作为标签展示
labels <- sprintf('%s %s', envs,desc)
# 绘制饼形图比例
plot(results, labels[test.ix], type='pie')

# other plotting functions
plot(results, labels[test.ix], type='bar')
plot(results, labels[test.ix], type='dist')
# plot(results.train, labels[train.ix], type='pie')
# plot(results.train, labels[train.ix], type='bar')
# plot(results.train, labels[train.ix], type='dist')

# plot results with legend
# 添加图例，并人工指定颜色
plot(results, labels[test.ix], type='pie', include.legend=TRUE, env.colors=c('#47697E','#5B7444','#CC6666','#79BEDB','#885588'))
plot(results, labels[test.ix], type='pie', include.legend=TRUE, env.colors=rainbow(5))

##github: sourcetracker2   基于python3
#https://github.com/biota/sourcetracker2
#Documentation里有介绍：
#This script replicates and extends the functionality of Dan Knights's SourceTracker R package.
##加入了并行，速度更快~