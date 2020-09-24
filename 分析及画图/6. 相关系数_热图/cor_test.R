##2018.5.20

####1.
##https://mp.weixin.qq.com/s?__biz=MzAwMDY0MzQ0Ng==&mid=2247483697&idx=1&sn=4ed85d06ff8bfab3c407193a58763e26&chksm=9ae49928ad93103e02861a9ad4e232c14558dbea352e4185c3d1e20493a00cb56159ba15a95e&mpshare=1&scene=1&srcid=0520f9QEVWm4FPsuLQ5V1Q3Y&pass_ticket=a%2FDZKnr7xDnM7txpzaFEK%2BPJtwU6W9%2BkB4QMG1yU6Lht0mbLenrBKIaxi9BbMLim#rd
##如何选择合适的相关性分析方法 ：

# Pearson相关系数呈现连续型正太分布变量之间的线性相关关系
# Spearman相关系数不要求正太连续，但至少是有序的，呈现非线性相关。
# 
# 误区一：必须是正太分布的数据才可以计算Pearson相关系数。
# 解读：只要两变量的协方差和方差存在，就可以计算这两变量的Pearson相关系数，并不需要这两变量的数据符合正太分布。
# 样本双正态分布，样本的相关性能很好地反映整体的相关性。若不是正态分布，样本的相关性能够表示你的样本变量直接是相关的，但不能反应整体中这两变量是否依然相关。
# 
# 误区二：同样的数据计算Spearman相关系数要大于Pearson相关系数。
# 当存在离群值的情况下，Pearson相关系数有可能要大于Spearman相关系数，但一般在有离群值存在的条件下应该选用Spearman相关系数。
# 
# 选择方法原则：
# 正态-pearson;
# 不符合或者不知道-查看离群值-有离群值用spearman,无离群值用pearson;
# 截断的数据-pearson.


####2.
##https://mp.weixin.qq.com/s?__biz=MzAwMDY0MzQ0Ng==&mid=2247483712&idx=2&sn=2ba32659cddd63a6ae0e9cd7dbf07f3b&chksm=9ae49959ad93104ff288e60face81893c67ab7aaaff109473dd0fb36b9ece5e24df6d366175f&mpshare=1&scene=1&srcid=0520cjDoRp4SYk5xgEiKtzKK&pass_ticket=a%2FDZKnr7xDnM7txpzaFEK%2BPJtwU6W9%2BkB4QMG1yU6Lht0mbLenrBKIaxi9BbMLim#rd
##双变量相关性分析及R作图 
rm(list=ls())
library(ggplot2)

data = cars
head(data);dim(data)
p = ggplot(data)+
  geom_point(aes(speed,dist))+
  theme_bw()+
  geom_smooth(aes(speed,dist,method = "lm"));p

# 查看数据是否有缺失值：
?complete.cases
sum(!complete.cases(data)) ##sum计空值（FALSE）的个数
#sum(c(TRUE,FALSE)) #1

## 缺失值数量为0,如果有缺失值，则去掉缺失值：data[!complete.cases(data),]

## 然后查看数据是否符合正太分布
?shapiro.test
shapiro.test(data$speed)
shapiro.test(data$dist)
#p-value均大于0.05，拒绝原假设，即数据和正太分布不存在显著性差异，数据分布属于正太分布。

# 再作图验证是否是正太分布：
library(ggpubr) #没有安装则install.packages("ggpubr")
?ggqqplot
ggqqplot(data$speed, ylab = "speed")
ggqqplot(data$dist, ylab = "dist")

#从图上来看也符合正态分布。
#是否应该用person相关系数检验相关性呢，先要看数据是否存在离群值。
#为了简单期间，使用普通变换，不适用melt函数将宽格式变为长格式。
data_1 = data.frame(value = append(data$speed,data$dist),variable = c(rep("speed",50),rep("dist",50)))
data_1

#箱线图查看是否存在离群点（大于1.5倍的Q4-Q1）大于1.5被的3/4分位数-1/4分位数的值
q = ggplot(data_1)+
    geom_boxplot(aes(variable,value,fill = variable),outlier.color = "red",outlier.size = 3)+
    theme_bw();q

##正态分布，但是存在离群值，应该用spearman相关性系数：
?cor.test
cor.test(data$speed,data$dist,method = "spearman") #rho为相关系数

####3.
##https://mp.weixin.qq.com/s?__biz=MzAwMDY0MzQ0Ng==&mid=2247483747&idx=2&sn=3030c8073c6a76bf66588e1f12c75129&chksm=9ae4997aad93106c5afb78d828aaaa57d62d35f867229e4001b328bdd21c31e1f1696f79462e&mpshare=1&scene=1&srcid=05203k7DyT3K3ej9LRyvmbq3&pass_ticket=a%2FDZKnr7xDnM7txpzaFEK%2BPJtwU6W9%2BkB4QMG1yU6Lht0mbLenrBKIaxi9BbMLim#rd
##多变量相关性分析及R作图 
rm(list = ls())
data(attitude)
head(attitude)

for(i in 1:ncol(attitude)){
  # 使用shapiro.test函数检验每个变量是否与正太分布具有显著性差异
  p_value <- apply(attitude,2,shapiro.test)[[i]]$p.value
  # 输出结果变量名和p值。
  print(paste(names(attitude)[i],p_value,sep=" "))
}
##或者不用apply函数：
for(i in 1:ncol(attitude)){
 p=shapiro.test(attitude[,i])$p.value 
 print(paste(names(attitude)[i],p,sep=" "))
}
# 通过shapiro.test检验结果p值都>0.01，则不能拒绝原假设，即这7个变量都符合正太分布。
# 接下来做箱线图看看变量是否有离群值：

library(reshape2)
library(ggplot2)
# 宽格式变为长格式：
?melt
attitude_melt <- melt(attitude)  
# 作图查看每个变量的离群值：
ggplot(attitude_melt)+
  geom_boxplot(aes(x=variable,y=value,fill=variable),outlier.colour = "red",outlier.size = 3)+
  theme_bw()+
  theme(
    axis.text.x = element_text(angle = 30,hjust=1,vjust=1)
  )

##有两个变量中存在离群点，其他变量中并没有离群点存在。
#有多变量的情况下，我们计算cook距离衡量某个单变量中离群点在所有变量拟合过程中产生的影响。
#从几何角度看，Cook距离度量的是使用第i个观测值计算的拟合值与不使用第i个观测值计算的似合值之间的距离。
#一般来说，如果某个观测的Cook距离比平均距离大4倍，我们就可以认为这个点是离群点。
# 构建函数cook_plot寻找并可视化每个变量的cook距离。
cook_plot<-function(attitude_col){
  # 建立线性模型
  ?eval
  mod <- lm(eval(parse(text = attitude_col)) ~ ., data=attitude)
  # 计算cook距离
  ?cooks.distance
  cooksd = cooks.distance(mod)
  # 构建作图所需数据框：
  cooksd_d <- data.frame(ID=names(cooks.distance(mod)),cooksd_distance = cooks.distance(mod))
  # 获得离群点：
  outline_point <- as.numeric(ifelse(cooksd>4*mean(cooksd_d$cooksd, na.rm=T),cooksd,""))
  outline_point[is.na(outline_point)]<-0
  # 作图：
  ggplot(cooksd_d)+
    geom_point(aes(x=ID,y=cooksd_distance))+
    geom_hline(yintercept=4*mean(cooksd, na.rm=T),color = "red")+
    theme_bw()+
    ylab(attitude_col)+
    theme(
      
    )+
    annotate("text",x=names(cooksd),y=as.numeric(cooksd),label=ifelse(cooksd>4*mean(cooksd_d$cooksd, na.rm=T),"outlier",""),vjust=1,color="red")
  }
# 调用函数生成每个变量的cooksd值：
datalist_p <- lapply(names(attitude),cook_plot)
# 批量显示图片：
#install.packages("cowplot")
library(cowplot)
# 调用cowplot包的plot_grid函数同时显示多个图：
plot_grid(plotlist = datalist_p,nrow=4)  

#从图中可以看出每个变量中都存在对于拟合结果影响较大（cook距离大于平均距离的四倍）的值，
#如果我们想综合考虑在这些变量群体中哪个点的影响最大，则可以使用car包的outlierTest函数：

#install.packages("car")
library(car)
car::outlierTest(mod)

##综上所述，attitude数据集的变量都符合正太分布，但cook距离结果表明每个变量都存在离群值，
#所以我们使用Spearman相关性系数来计算这个数据集的相关性矩阵。



#####5.27
####3.#工具杂谈#相关性分析花式作图大法 
##https://mp.weixin.qq.com/s?__biz=MzAwMDY0MzQ0Ng==&mid=2247483767&idx=1&sn=65ad841cda9f261e8c09bd54b250ff86&chksm=9ae4996ead9310789bd96741910840bbd4fef52ef083e1eaffe6a931f5eef642fefed5f99cb1&mpshare=1&scene=1&srcid=0520x9dZkQbEr5ybhI2EPDSW&pass_ticket=%2F%2BJ%2FBQolZYhT5Uk7DPRwSmOB2z%2BQWVSUzkzzDLi0Gevgjm0aub%2F2b95K0RRIsppy#rd
rm(list=ls())
# 载入corrplot包，没有安装的请install.packages("corrplot")
library(corrplot)
#使用example函数查看corrplot包中两个关键函数的作图实例，按ENTER查看下个实例,按ESC退出观看实例。
# 输入example("corrplot)查看corrplot中corrplot函数的实例
example("corrplot",package = "corrplot")
# 查看corrplot包中corrplot.mixed函数的实例。
example("corrplot.mixed",package = "corrplot")

# 使用Spearman方法计算相关性矩阵：data("attitude")
cor_attitude <- cor(attitude,method="spearman")
# 将相关性系数保留三位小数
cor_attitude<-round(cor_attitude,3)
head(cor_attitude)

corrplot(cor_attitude,    #相关性矩阵
method="color",  #填充方法，可以是颜色或图形,circle,square,ellipse,number
type="upper",    #只显示上半部分矩阵三角
order = "hclust",  #排序方式
addCoef.col = "#ff0099")   #设置相关性系数的字体颜色

# 圆圈显示：
corrplot(cor_attitude,method = "circle")
# 矩形显示：
corrplot(cor_attitude,method = "square")
# 椭圆显示：
corrplot(cor_attitude,method = "ellipse")
# 显示相关性系数：
corrplot(cor_attitude,method = "number")
# 纯色显示：
corrplot(cor_attitude,method = "color")
# 饼图显示：
corrplot(cor_attitude,method = "pie",addCoef.col = "#ff0099")
# 只显示上面的相关性三角
corrplot(cor_attitude, type="upper")
# 只显示下面的相关性三角
corrplot(cor_attitude, type="lower")
# 混合显示圆圈和相关性系数：
corrplot.mixed(cor_attitude) ##corrplot.mixed()用来混合显示样式
# 混合显示上面是圆圈，下面是矩形。
corrplot.mixed(cor_attitude, lower="square", upper="circle")
# 混合显示上面是圆圈，下面是数字。
corrplot.mixed(cor_attitude, lower="number", upper="circle")

#当然还可以使用GGally的ggcorr来实现。
install.packages("GGally")
library(GGally)
# 不以色条形式展示，而是以颜色区间展示，如果想以色条展示，去掉nbreaks参数。
ggcorr(cor_attitude[,],
       nbreaks=6, 
       low = "steelblue", 
       mid = "white",
       high = "darkred",label= T)

#ggplo2来实现相关性热图
#http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization








