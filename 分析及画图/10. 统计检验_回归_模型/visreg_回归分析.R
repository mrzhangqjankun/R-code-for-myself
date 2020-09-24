##2019.3.23

##visreg：带你玩遍模型可视化 
#https://mp.weixin.qq.com/s/boJra1Fzc8ICGOh8cWFoeg
#几乎支持了所有的回归分析模型，
#同时支持各种各样的ggplot2包提供的geom_*图层和其他扩展主题，
#是进行模型可视化的利器
if(!require(visreg)){
  install.packages("visreg")
}


library(visreg);?visreg
library(ggplot2)

##随机数据
set.seed(20180730)
x1 <- rnorm(100)
x2 <- rnorm(100, sd = 2)
x3 <- rnorm(100, sd = 1.7) + 0.3
cat <- sample(c(0, 1), 100, replace = TRUE)
e <- rnorm(100, mean = 2, sd = 1)
y0 <- 2 + 0.5 * x1 + 2.4 * x2 + 1.2 * x1 * x2 + e
y1 <- 0.8 + 1.7 * x1 + 1.4 * x2 + 0.5 * x1 * x2 + e
y <- ifelse(cat == 0, y0, y1)
data <- data.frame(y, x1, x2, x3, cat = as.factor(cat))
data
#变量之间的关系
GGally::ggpairs(data)
?ggpairs


#visreg进行模型可视化需要先对模型进行估计，
#然后把模型估计结果作为参数传入visreg()函数。
#下面我们简单估计了四个模型，后文会断断续续用到。
install.packages('texreg')
reg01 <- lm(y ~ x1 + x2 + x3 + cat, data = data)
reg02 <- lm(y ~ x1 + x2 + x1 * x2 + x3 + cat, data = data)
reg_cat0 <- lm(y ~ x1 + x2 + x1 * x2 + x3, data = data[data$cat == 0, ])
reg_cat1 <- lm(y ~ x1 + x2 + x1 * x2 + x3, data = data[data$cat == 1, ])
texreg::htmlreg(list(reg01, reg02, reg_cat0, reg_cat1), single.row = TRUE)
?htmlreg #将回归输出转换为LaTeX或HTML表

##可视化
#visreg包对单变量可视化时非常简洁，visreg()一条命令解决所有烦恼。
#默认在图中会显示出因变量、自变量之间的拟合直线、的置信区间以及原数据散点图。
#visreg()前两个参数分别为fit和xvar，前者为模型估计结果，
#后者为需要绘图的自变量。当不指定xvar参数时，会依次绘制因变量和所有自变量之间的关系图

par(mfrow = c(2, 2))  ##reg01模型中有四个变量，设置为2 x 2的图
visreg(reg01)

#通过设置xvar参数来绘制指定自变量关系图。
visreg(reg01, "x1")  ##注意，变量名一定要用引号括起来，不然会报错

visreg(reg01, "x1", by = "cat")

#breaks参数控制该变量被分成的组数。
#layout=c(2,2)和par(mfrow = c(2,2))作用类似，控制作图排列方式。
visreg(reg01, "x1", by = "x2", breaks = 4, layout = c(2, 2))

#overlay = TRUE在同一幅图中通过图层叠加进行展示。
visreg(reg01, "x1", by = "cat", overlay = TRUE)


#双变量可视化
#此处的双变量是指两个自变量的情况，visreg包默认采用2.5D热图呈现的，颜色差别代表了因变量值得差异，横纵坐标分别表示两个自变量。
#下面我们在一幅图上展现y和x1、x2变量之间的关系。此时需要用到visreg2d()函数，与visreg()类似，第一个参数任然是估计模型，第二、三个参数分别为xvar、yvar，分别表示横纵坐标
visreg2d(reg02, "x1", "x2")

#visreg2d()函数有个plot.type参数来控制绘图类型。设置参数plot.type="persp"时是绘制3D表面图。
visreg2d(reg02, "x1", "x2", plot.type = "persp")

#也可以通过设置参数plot.type="rgl"使用rgl包绘制3D图。
visreg2d(reg02, "x1", "x2", plot.type = "rgl")

####使用ggplot2绘图引擎

#visreg包默认底层使用的是R语言基本绘图函数plot()作为引擎，但可以设置参数gg = TRUE来使用ggplot2绘图引擎。
visreg(reg01, "x1", gg = TRUE)

visreg(reg01, "x1", gg = TRUE) + geom_smooth(method = "loess", col = "blue", 
                                             fill = "#FF4E37") 


###visreg包不仅可以做普通最小二乘回归结果的可视化，
#同时还支持广义最小二乘回归、随机森林模型、支持向量机等其它模型。

#广义最小二乘和普通最小二乘回归可视化几乎一样，下面通过一个简单的例子进行说明。

data("birthwt", package = "MASS")
fit <- glm(low ~ age + race + smoke + lwt, data = birthwt, family = "binomial")
visreg(fit, "lwt", gg = TRUE, xlab = "Mother's weight", ylab ="Log odds (low birthweight)")




