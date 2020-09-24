##2020.8.3

##zetadiv

library(zetadiv)
?Zeta.decline.mc

#蒙特卡罗抽样方法计算zeta多样性随阶数的下降
Zeta.decline.mc(
  data.spec,  ##行为样本，列为物种。发生率数据
  xy = NULL,  ##样本坐标
  orders = 1:10, ##zeta的阶数，即计算的样点量
  sam = 1000,  ##对于每个群落或样点计算的样本数
  confint.level = 0.95, ##回归的置信区间
  rescale = FALSE, ##是否除以z1进行标准化
  normalize = FALSE,  ##Sorensen或Jaccard标准化
  empty.row = "empty", ##空值的处理
  plot = TRUE  ##是否画图
)

##例子
utils::data(bird.spec.coarse)
xy.bird <- bird.spec.coarse[,1:2]
data.spec.bird <- bird.spec.coarse[,3:193]

dev.new(width = 12, height = 4)
zeta.bird <- Zeta.decline.mc(data.spec.bird, xy.bird, orders = 1:5, sam = 100,
                             NON = TRUE)
zeta.bird

###zeta.ddecay
##zeta的距离衰减
Zeta.ddecay(
  xy,  ##行为样本，列为坐标。
  data.spec, ##行为样本，列为物种。
  order = 2, ##阶数
  sam = 1000, ##计算的样本数
  distance.type = "Euclidean", ##距离计算方法
  method = "mean", #3个地点以上时如何计算距离，可选mean或max
  reg.type = "glm" #回归模型
)
#例子
utils::data(Marion.species)
xy.marion <- Marion.species[,1:2]
data.spec.marion <- Marion.species[,3:33]

dev.new()
zeta.ddecay.marion <- Zeta.ddecay(xy.marion, data.spec.marion, sam = 100, order = 3,
                                  method.glm = "glm.fit2", confint.level = 0.95, trsf = "log", normalize = "Jaccard")



###?zeta.varpart
##通过距离和环境变量对zeta多样性进行分解。

###Predict.msgdm
##对于新的环境和距离数据，预测其zeta值。

