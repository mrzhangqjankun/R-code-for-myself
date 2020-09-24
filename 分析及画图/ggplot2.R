##ggplot2:数据分析与图形艺术
##2018.1.22
##2018.5.15  ggplot2 ~ 合集 
##https://mp.weixin.qq.com/s?__biz=MzU4MzQ3NDExMw==&mid=2247483905&idx=1&sn=ea51e8b8b0ea2eaf4c2abf8165eed86f&chksm=fda9ccafcade45b9a68f568edfee9ec4553180a62ef2c7657ba7376c85f748613488d336b356&mpshare=1&scene=1&srcid=0515EfwkaUWq7MLhhiqlrfg4&pass_ticket=ve5jn1iA2lehdH2jhV0qvyS0Dq0WHN7yFxffg0DHoBGAnOEN%2B8o%2BtoPmeLmIK344#rd

rm(list=ls(all=TRUE))
library(ggplot2)
packageVersion("ggplot2")

#图形的语法告诉我们，一张图形就是从数据（data）到几何对象的图形属性的一个映射（mapping）。图形中还包含数据的统计变换，最终绘制在某个特定的坐标系中。
##几何对象geom：点，线，多边形等
##图形属性aes：颜色，形状，大小等
##统计变换stats,对原始数据进行某种计算和汇总，例如二元散点图加上一条回归线
##图层（Layer）由几何对象和统计变换组成
##标度（scale）是将数据的取值映射到图形空间，例如颜色、大小和形状表示不同的数值。展现标度的常见做法是绘制图例和坐标轴
##坐标系coord：数据如何映射到图形所在平面，提供坐标轴和网格线
##分面facet：绘图窗口划分为几个子窗口.也称条件作图，控制分组方法和排列，描述如何将数据分解为各个子集，以及如何对子集作图

head(mpg) #大众汽车燃油经济性
#cty和hwy：城市和高速公路行驶记录每加仑行驶的英里数（miles per gallon，mpg）
#displ：发动机排量（L）
#drv：动力传动系统（前轮f，后轮r，四轮4）
#model：汽车模型（由于1999年至2008年期间每年都有新版本，因此选择了38款）
#class：描述汽车种类的变量（双座，SUV，紧凑型等）


#################################################第二章 qplot ；  quick plot
data(diamonds)
head(diamonds)
#抽100个随机样本
set.seed(1410) #样本可重复
?set.seed
##set.seed：值相同，每次得到的随机数就相同。值不同随机数也不同。值可以自己随便取，没有实际意义。伪随机数
##线性同余法：http://bbs.pinggu.org/thread-336973-1-1.html
##随机种子：https://d.cosx.org/d/15925-15925

dsmall<-diamonds[sample(nrow(diamonds),100),];head(dsmall,10) ##随机取100个数据
nrow(dsmall)

qplot(carat,price,data=diamonds)
qplot(log(carat),log(price),data=diamonds)
##colour 颜色，shape外形
qplot(carat,price,data=dsmall,colour=color,shape=cut)
##alpha 透明度设置。0全透明，1全不透明
qplot(carat,price,data=dsmall,alpha=I(0.5))

##geom=point 散点图；smooth拟合平滑曲线，包含曲线和标准误；
qplot(carat,price,data=dsmall,geom=c("point","smooth"))
qplot(carat,price,data=dsmall,colour=color,geom=c("point","smooth"))##每一个颜色都会做一条线
qplot(carat,price,data=dsmall,shape=cut,geom=c("point","smooth"))##每一个形状都会做一条线
qplot(carat,price,data=dsmall,geom="smooth")#只有一条线
?qplot

qplot(carat,price,data=dsmall,geom=c("point","smooth"),se=F) ##去掉标准误
?loess  ##local fitting,局部拟合。样本数小于1000的默认算法
#span曲线平滑程度。1最平滑，0最不平滑
qplot(carat,price,data=dsmall,geom=c("point","smooth"),span=0.2) 

##广义可加模型gam.大数据使用y=s(x,bs="cs"),数据超过1000的默认选择
library(mgcv)
qplot(carat,price,data=dsmall,geom=c("point","smooth"),method="gam",formula = y ~ s(x)) 
?gam

##lm线性模型    rlm与之类似，需要加载MASS包
library(splines)
qplot(carat,price,data=dsmall,geom=c("point","smooth"),method="lm")
qplot(carat,price,data=dsmall,geom=c("point","smooth"),method="lm",formula=y~ns(x,10)) #10位自由度，值越大曲线波动越大。


##扰动点图jitter,箱线图boxplot
#透明度，点大小，颜色，形状。  注意连续变量不能有shape
qplot(color,price/carat,data=diamonds,geom="jitter",alpha=0.5,size=0.05,colour=color)
#线框颜色，full内部颜色，size线粗细
qplot(color,price/carat,data=diamonds,geom="boxplot",colour=color)
?boxplot

##直方图和密度图histogram,density
qplot(carat,data=dsmall,geom="histogram",binwidth=0.01) #binwidth，组距调整平滑度
qplot(carat,data=dsmall,geom="density",adjust=0.05)  #adjust调整平滑度
qplot(carat,data=dsmall,geom="density",adjust=0.05,xlim=c(0,4)) ##坐标范围0-4

#图形映射。一个分类变量被映射到某个图形属性(颜色、形状等)上，几何对象（colour）会自动按这个变量进行拆分。
#每个颜色都会做一条密度曲线。
qplot(carat,data=dsmall,geom="density",adjust=0.05,colour=color) 

##条形图bar
qplot(color,data=dsmall,geom="bar")  
##weight几何对象。按carat进行加权 y轴名字由count变为carat
qplot(color,data=dsmall,geom="bar",weight=carat) + scale_y_continuous("carat")
?scale_y_continuous

##分面facets,将数据分割成若干子集，创建一个图形矩阵，将子集绘制到图形矩阵的窗格中。
##形式：row_var~col_var,行变量~列变量。 若只指定一行或一列，可用.作为占位符，如row_var~.,创建单行多列的图形矩阵。
qplot(carat,data=diamonds,geom="histogram",facets = color~.,binwidth=0.1,xlim=c(0,3))  
##..density..映射密度到y轴而不是数量
qplot(carat,..density..,data=diamonds,geom="histogram",facets = color~.,binwidth=0.1,xlim=c(0,3))  

##main图题，xlab,ylab坐标轴题。log=x,x轴取对数。

#####################################################ggplot2读书笔记2：ggplot()基本用法以及如何绘制几何对象 
#由ggplot2所制得图形有三个重要的组成部分
#数据
#数据和视觉变量属性之间的映射（aesthetic mappings）
#呈现数据结果的图层（一般使用 geom()函数）

ggplot(mpg,aes(displ,hwy)) + #第一排包括数据集和映射 aes()，放在 ggplot()中
       geom_point()            #通过 + 添加图层 geom_point()（散点图）

#图形的颜色、大小和形状等都是给绘图添加的额外变量，即标度（scale），添加在函数 aes() 的参数中
ggplot(mpg,aes(displ,hwy,colour= class,shape = drv,size = cyl))+
      geom_point()
#所有点一个颜色
ggplot(mpg,aes(displ,hwy))+geom_point(colour = "blue")
ggplot(mpg,aes(displ,hwy,colour= "124"))+geom_point() ##这样出来的颜色是红色，图例是124.

##分面facetting是比较不同分组的另一种方法：可以将数据分割成若干子集，然后创建一个图形矩阵，将每个子集绘制到图形矩阵的窗格中。
##分面有两种主要类型：网格（grid）和包装（wrapped）。相较而言，Wrapped更常用。
#使用方法是添加 facet_wrap()函数，参数写法是 ~+变量
ggplot(mpg,aes(displ,hwy))+geom_point()+facet_wrap(~class)

##各种图形
#geom_smooth()：拟合一条平滑曲线，并显示标准误
#geom_boxplot()：绘制箱线图，概括一系列点的分布情况
#geom_histogram()：绘制直方图
#geom_bar()：绘制条形图
#geom_path()：在数据之间连线

ggplot(mpg,aes(displ,hwy))+geom_point() + geom_smooth()
#其中灰色部分是置信区间，如果不需要标明标准误，可以添加参数 geom_smooth(se=FALSE)，即可取消灰色部分。
#通过改变 method可以设置曲线平滑度等多种参数。而 method则包括几种类型：在数据量n较小时，系统默认 method="loess"算法；而在数据量n>1000时，则采用 method="gam"算法；此外还有 method="lm" 和 method="rlm" 算法。
#method="loess"：在数据量较小时默认使用。 span参数控制曲线的平滑程度，其取值范围是0~1（从很不平滑到很平滑）
#method="gam"：此算法通过调用mgcv包拟合一个广义可加模型，使用formula = y ˜ s(x) 或y ˜ s(x, bs = "cs")公式。
#method="lm" 和 method="rlm"：这两种算法拟合的是线性模型，默认条件下回生成一条直线。可以通过添加二项式改变自由度使曲线波动变大。后者对异常值不敏感，但在使用前需要先加载MASS包。

##箱式图
#当数据集中包含了分类变量和连续变量时，我们想了解连续变量是怎样随着不同的分类变量水平变化而变化，这时散点图中则会出现大量重叠，而箱式图则可以更清晰的展示这类数据。
#以耗油量的数据集为例，我们想了解不同类别汽车的耗油量有什么差别，就可以使用箱式图。
#主要有三种函数
#1 geom_jitter()扰动点图，和普通散点图相比减少数据点的重叠
ggplot(mpg,aes(drv,hwy))+geom_jitter()
#2 geom_boxplot()箱式图，可以相对直观的看出数据分布特点。箱式图用于多组数据平均水平和变异程度的直观分析比较。每组数据均可呈现其最小值、最大值、平均水平，最小值、最大值形成间距都可以反映数据的变异程度
ggplot(mpg,aes(drv,hwy))+geom_boxplot()
#3 geom_violin()小提琴图，在普通箱式图的基础上可以从形状表示出点的“密度”
ggplot(mpg,aes(drv,hwy))+geom_violin()

##直方图和密度图
#geom_histogram()：直方图又称质量分布图，由一系列高度不等的纵向条纹或线段表示数据分布的情况。其中 binwidth参数可以用来设置组距，通过组距调节平滑度。
ggplot(mpg,aes(hwy))+geom_histogram(binwidth = 0.6)
ggplot(mpg,aes(displ,fill =drv))+geom_histogram(binwidth = 0.6)+facet_wrap(~drv,ncol = 1)

#geom_freqpoly()：密度曲线图
ggplot(mpg,aes(hwy))+geom_freqpoly(binwidth = 0.6)

##条形图geom_bar
ggplot(mpg,aes(manufacturer)) + geom_bar()

##线条图和路径图geom_lines,geom_path  用于时间序列

##坐标轴。xlab()和 ylab()是两个最常用的修改和补充坐标轴名称的函数。可以添加在 ggplot()第三行-第四行的位置。例如：
#xlim()和 ylim() 可用来限制横轴或纵轴的宽度/量程/范围/最大值最小值等
ggplot(mpg,aes(drv,hwy)) + 
  geom_jitter(width = 0.25,na.rm = TRUE) +
  xlab("dry")+
  ylab("hwy")+
  ylim(NA,50)

#####################################################第三章，ggplot语法
#####################################################ggplot2读书笔记6：语法（一）基础理论 
data(mpg);head(mpg)
qplot(displ,hwy,data=mpg,colour=factor(cyl)) ##颜色为离散型
qplot(displ,hwy,data=mpg,colour=cyl)  ##颜色为连续型
?factor
?colour
#factor把连续性变量编程离散型变量
qplot(displ,hwy,data=mpg,colour=factor(cyl),shape=factor(cyl))
#geom_smooth 与 geom("smooth")作用相同
qplot(displ,hwy,data=mpg,facets = .~year) + geom_smooth() 

#图表包括了数据、映射、统计变换、几何图形以及位置调整（position adjustment）。


#####################################################第四章，图层构建图像
##使用图层的三个主要目的：
#展示数据：绘制原始数据时唯一的一层（数据层）
#展示数据的统计摘要：在数据背景下展示模型的统计预测效果，模型层通常绘制在数据层之上
#添加额外的元数据（metadata）、上下文信息和注释：也称背景层，了解数据的背景信息或强调数据中的某些特征，一般在最后绘制。

##1.基本图形类型
#几何对象是ggplot2的基本组成部分，可以独立构建图形。他们都是二维的，主要函数有 geom_area()， geom_bar()， geom_line， geom_point()， geom_polygon()， geom_tile()等等。
#这些函数包括x，y两个主要属性，另外也可以接受 color 和 size两个图形属性，他们构成了基本的数据层。使用 +来添加图层。

#2.添加标签label
#geom_text()：和散点类似，就是将point换成了文字。
#size：设置字体大小;angle：设置倾斜角度;family：可设置字体，下面代码中的 "sans", "serif", "mono"代表三种字体， "sans"是默认字体。
df = data.frame(x = 1,y = 3:1, family = c("sans","serif","mono"))
ggplot(df,aes(x,y)) + geom_text(aes(label = family, family = family))
#fontface：可设置粗体或斜体， “plain”默认普通值, “bold” 粗体、 “italic”斜体。
#vjust 和 hjust 可以设置字体对齐方式。 vjust (“bottom”, “middle”, “top”, “inward”, “outward”)；
#hjust (“left”, “center”, “right”, “inward”, “outward”)。最常用的路线之一是 “inward”：它将文本对齐到主画面的中间：
#check_overlap：查找重复值。当注释中有大量重复时，设置 check_overlap=TRUE可以自动删除重复标签。
#另外，与 geom_text() 类似的是 geom_label()，它与geom_text的区别自动在文字后方绘制一个圆角矩形标签，当需要在复杂的背景上标注文字时可以使用。

#3.注释
#注释可以在你的图上添加一些额外的元数据，可以使用以下函数：
#geom_text()：在指定点添加标签（见上文）
#geom_rect()：可强调图形中感兴趣的矩形区域。包括 xmin, xmax, ymin, ymax
#geom_line(); geom_path(); geom_segment()：在图形中添加线条； arrow()可以用来添加箭头
#geom_vline(); geom_hline()：向图形添加垂直线或水平线
#geom_abline()：向图形添加任意斜率和截距地直线

##ggplot2读书笔记4：图层的使用（二）群组几何对象和曲面图 
#4.群组几何对象（Collective Geoms）
#group（分组）这种图形属性可以用来设置：哪些观测值控制哪种图形元素
#举例：纵向数据集Oxboys（nlme包）记录了26个男生（subject）在9个不同时期（occasion）中测定的身高（height）和中心化年龄（age）。
data(Oxboys,package = "nlme")
head(Oxboys)
ggplot(Oxboys,aes(age,height,group = Subject,colour=Subject)) + geom_point() + geom_line()

#想根据所有男孩的年龄和身高在图中添加一个平滑线条
ggplot(Oxboys,aes(age,height,group = Subject,colour=Subject)) + geom_line() + geom_smooth(method = "lm",se=F) #每个人一条线
ggplot(Oxboys,aes(age,height)) + geom_line(aes(group=Subject)) + geom_smooth(method = "lm",se = F) #只有一条线

library(ggplot2)

##离散变量：指变量值可以按一定顺序一一列举，通常以整数位取值的变量。如职工人数、工厂数、机器台数等。
#连续变量：在一定区间内可以任意取值的变量叫连续变量，其数值是连续不断的，相邻两个数值可作无限分割，即可取无限个数值。
#当离散变量存在时，一般就会将其认为是默认分组变量。
#如果图像中含有离散型变量，而你却想绘制连接所有分组的线条。这时就要修改默认分组。或者在新图层中设定一个新的分组，就能将二者结合起来。
ggplot(Oxboys,aes(Occasion,height)) + geom_boxplot() + geom_line(aes(group = Subject),colour = "red")

#气泡图
faithfuld
small = faithfuld[seq(1,nrow(faithfuld),by = 10),]
ggplot(small,aes(eruptions,waiting)) +
  geom_point(aes(size = density),alpha =1/3)+
  scale_size_area()
##########################################

#图层中的数据必须为数据框data frame，绘图结束后可以被修改。
#qplot自动创建了一个图形对象，添加图层并展示结果。而ggplot需要手动创建图形对象。
##ggplot包含两个主要的参数：数据和映射。数据是绘图的默认数据集（数据框）；映射需要将图形属性和变量名放到aes()中。
#图形对象：
p <- ggplot(diamonds,aes(carat,price,colour=cut))
#加入几何对象：
geom_histogram(binwidth = 2,fill = "steelblue")

##通用格式
geom_xxx(mapping,data,...,stat,position)
stat_xxx(mapping,data,...,geom,position)
#mapping(可选):图形属性映射，用aes()设定。
#data(可选):数据集
#...:参数。
#geom和stat(可选)：修改默认的stat或者geom。
#position：选择一种调整对象重合的方式。

#summary函数查看图形对象的结构
summary(p)

#创建一个半透明深蓝色回归线的图层：
library(scales)
bestfit<-geom_smooth(method="lm",se=F,colour=alpha("steelblue",0.5),size=2)
qplot(sleep_rem,sleep_total,data=msleep)+bestfit
qplot(awake,brainwt,data=msleep,log="xy")+bestfit

## %+% 添加新的数据集来代替原来的数据集

##aes()函数用来将数据变量映射到图形中，从而使变量成为可以被感知的图形属性。
aes(x,y,colour=age,...)

##设定和映射
p <- ggplot(mtcars,aes(mpg,wt)) 
p + geom_point(colour = "darkblue")  ##颜色设定为深蓝
p + geom_point(aes(colour = "darkblue"))  ##颜色为红色。“darkblue”被看做普通的字符串，使用默认的颜色标度进行标度转换，结果为红色。
#qplot使用I（）进行映射
colour = I("darkblue")


##分组
data(Oxboys);head(Oxboys)
p1 <- ggplot(Oxboys,aes(age,height))+geom_line();p1 ##没有分组，得到的结果无意义，得到的是通过每一个点的一条线
p2 <- ggplot(Oxboys,aes(age,height,group = Subject))+geom_line();p2 ##每个人一条线
#不同图层上分组
p2 + geom_smooth(method="lm",se=F) #每个人都有一条线
p2 + geom_smooth(group=1,method="lm",se=F,size=2)
p2 + geom_smooth(aes(group=1),method="lm",se=F,size=2) #只作一条线。和上面一条结果相同。

##统计变换 stat
#生成变量需要用..围起来。
ggplot(diamonds,aes(carat))+geom_histogram(aes(y=..density..),binwidth = 0.1)
#stat_bin常用语绘制直方图。得到如下变量：count,每组观测值的数目；density，观测值的密度；x，组中心位置

##位置调整
#多用于离散数据。jitter添加扰动；stack元素堆叠；dodge避免重叠，并排放置；identify不做调整；fill堆叠元素并将高度标准化为1
#position=“ ”表示

##整合
d <- ggplot(diamonds,aes(carat))+xlim(0,3)
?stat_bin
d + stat_bin(aes(ymax=..count..),binwidth = 0.1,geom = "area")##，默认是柱状图，改为面积图
d + stat_bin(aes(size = ..density..),binwidth = 0.1,geom = "point",position = "identity") ##点图，点的大小是密度，位置不作调整
d + stat_bin2d(aes(y = 1,fill=..count..),binwidth = 0.1,geom = "tile", position = "identity") ##瓦片图
?stat_bin2d
#用stat_bin会报错。Error: stat_bin() must not be used with a y aesthetic.
#http://wenwen.sogou.com/z/q766867583.htm
#stat_bin表明统计变换是计数，技术会被投射到y轴，与y=1冲突了。
d + stat_bin(aes(y = 1,fill=..count..),binwidth = 0.1,geom = "tile", position = "identity")

##改变图形属性和数据集

#############################2018.1.23
########################################################第5章 工具箱
#############################2018.5.17
#ggplot2读书笔记5：工具箱——误差线、加权数、展示数据分布 


##6.添加误差线
#离散型变量+区间： geom_errorbar(), geom_linerange()
#离散型变量+区间+中间值： geom_crossbar(), geom_pointrange()
#连续型变量+区间： geom_ribbon()
#连续型变量+区间+中间值： geom_smooth(stat="identity")
#以上函数默认，我们对给定x时y的值域和分布情况感兴趣，所以使用了图形属性 ymin和 ymax来确定y的值域。
y = c(18,11,16)
df = data.frame(x=1:3,y=y,se = c(1.2,0.5,1.0))
base = ggplot(df,aes(x,y,ymin=y-se,ymax=y+se))

## 箱线图的形式，离散型变量的区间和中间值
base+geom_crossbar()

## 点线图的形式，离散型变量的区间和中间值
base+geom_pointrange()

## 连续型变量，展示区间和中间值
base+ geom_smooth(stat="identity")

## 箱线图的形式，离散型变量只显示区间
base+geom_errorbar()

## 点线图的形式，离散型变量只显示区间
base+geom_linerange()

## 连续型变量，只显示区间
base+geom_ribbon()

##7.加权数据（Weighted Data）
#以2000年美国人口普查，东西部各州的统计数据集（midwest）为例。
#此数据主要包括的是比例型数据（eg. 白种人比例，贫困线下人口比例，大学以上学历人口比例）以及每个地区的信息（面积、人口总数、人口密度等）。
midwest
class(midwest)
#首先，对于点、线这些简单的几何对象，可以根据 size改变点的大小：
ggplot(midwest,aes(percwhite,percbelowpoverty)) + 
  geom_point(aes(size = poptotal/1e6))+
  scale_size_area("population\n(millions)",breaks = c(0.5,1,2,4))
  
#对于更加复杂、涉及到统计变换的情况，我们通过修改weight图形属性来表现权重。这些权重将被传递给统计汇总计算函数。
#在权重有意义的情况下，各种元素基本都支持权重的设定，例如各类smooth平滑器、箱线图、分位回归、直方图以及密度图等等。
ggplot(midwest,aes(percwhite,percbelowpoverty)) + 
  geom_point(aes(size = poptotal/1e6))+
  scale_size_area("population\n(millions)",breaks = c(0.5,1,2,4))+
  geom_smooth(aes(weight = poptotal),method =lm,size = 1)

##################################
#geom_area()面积图
#geom_bar(stat="identity").条形图。需要指定stat。
#geom_line()线条图。group决定了哪些点连在一起。geom_path根据在数据中出现的顺序进行连接。
#geom_point()散点图
#geom_polygon()多边形
#geom_text()添加标签。hjust和vjust调整位置，angle控制角度。
#geom_tile()瓦片图
ggplot() + geom_point() + labs(title = "geom_point")

##一维连续分布，最重要的几何对象是直方图。
depth <- ggplot(diamonds,aes(depth))+xlim(58,68)
depth + geom_histogram(aes(y = ..density..),binwidth = 0.1) + facet_grid(cut~.)
depth + geom_histogram(aes(fill = cut),binwidth = 0.1,position = "fill")
depth + geom_freqpoly(aes(y = ..density..,colour = cut),binwidth = 0.1) ##频率多边形

##箱式图geom_boxplot = geom_boxplot + stat_boxplot
library(plyr)
qplot(cut,depth,data=diamonds,geom = "boxplot")
qplot(carat,depth,data=diamonds, geom = "boxplot",group = round_any(carat,0.1,floor),xlim = c(0,3))##针对carat，以0.1为步长画箱式图
#改写为ggplot:

p <- ggplot(diamonds,aes(cut,depth))
p + geom_boxplot()
p + geom_boxplot(aes(group = round_any(carat,0.1,floor)))  ##图不对。注意group要放在aes中。

##扰动图geom_jitter = position_jitter + geom_point
##密度图geom_density = stat_density + geom_area

##两个连续变量
#遮盖绘制overplotting。数据量太大导致点重叠在一起。
#小规模的遮盖，使用更小的点或者中空符号。
df <- data.frame(x = rnorm(2000),y = rnorm(2000));head(df)
norm = ggplot(df,aes(x,y))
norm + geom_point()
norm + geom_point(shape = 1)
norm + geom_point(shape = ".") ##点的大小为像素级别
#利用alpha透明度
norm + geom_point(colour= "black",alpha = 0.1)
#利用jitter

##二维密度问题，还可用stat_bin2d或stat_density2d来解决。P80-81，略。

##绘制地图5.7
library(maps)
data(us.cities);head(us.cities)
big_cities <- subset(us.cities,pop>500000)
qplot(long,lat,data = big_cities) + borders("state",size = 0.5) ##按照州对美国进行划分
tx_cities <- subset(us.cities,country.etc == "TX") ##德克萨斯州
p=ggplot(tx_cities,aes(long,lat)) + borders("county","texas",colour = "grey70") + geom_point(colour = "black",alpha = 0.5) #county，郡级

states = map_data("state")

##揭示不确定性
#边际效应；条件效应

##图形注解5.10
#geom_text标签
#geom_vline, geom_hline,添加垂直线或水平线
#geom_abline,任意斜率截距的直线
#geom_rect,强调图形中感兴趣的矩形区域
#geom_line,geom_path,geom_segment,添加直线。

##权重 weight

###ggplot2读书笔记7：第五章 通过图层构建图像 
##https://mp.weixin.qq.com/s?__biz=MzU4MzQ3NDExMw==&mid=2247483717&idx=1&sn=3ea18fe9b7ae3ddaf2527bed913fc337&chksm=fda9cfebcade46fd8f056bdd1e2e0dde142a216165d2d79d3d8d2cd5c74923fe8907a4c0d722&scene=21#wechat_redirect

ggplot(mpg,aes(displ,hwy)) +  ##只是创建了一个图层,但是由于没有命令几何图形，数据无法形成映射，所以就是一块只有横纵坐标的白板
  geom_point()  ##括号是空的就是各种参数都是默认值，于是有了确切的几何图形命令，白板上就有图了
#geom_point()这个命令的背后，隐藏着一个叫做 layer()的命令，他的意思是创造一个图层，下面的代码里显示了藏在 layer()里面的各种参数：
layer(
  mapping=NULL,
  data=NULL,
  geom="point",
  stat="identity",
  position "identity"
)
#这里面就包括了图层（layer）的五个重要组成部分：
#mapping：映射，就是 aes()函数，通常省略，NULL就是指默认的 ggplot()函数中的映射
#data：数据，覆盖默认数据集，通常省略，NULL就是默认取 ggplot()中的数据集
#geom：几何对象，包含很多美学参数，例如颜色等，在这里设置不会成为标度。
#stat：统计变换，执行一些统计汇总，默认设置为 "identity"时保持不变。这个参数在直方图和平滑曲线图中应用较多。 geom和 stat设置一个即可。
#position：位置，调整遮盖情况

#ggplot2的作用只是将数据框可视化。在使用之前应该提前整理你的数据框，使其整齐、易于操作
#映射的函数就是 aes()。设置数据是怎样对应在到图表上的
#aes()中可以进行一些简单地运算，比如， aes(log(carat),log(price))，但不能出现 美元符号$（例如：diamonds$carat）

##当存在多个图层时，参数的位置就很重要。
ggplot(mpg,aes(displ,hwy,colour = class)) +  ##结果中有很多线
  geom_point() +
  geom_smooth(method = "lm",se = FALSE) +
  theme(legend.position = "none")  ##隐藏图例

ggplot(mpg,aes(displ,hwy)) +
  geom_point(aes(colour = class))+
  geom_smooth(method="lm",se = FALSE)

##设定和映射 （Setting vs. Mapping）
#除了将图形属性和变量映射（mapping）起来，我们也可以在图层的参数里将其设定（setting）成一个单一值（例如，colour = "read"）。
#Setting和Mapping是不同的。
#例如将上述例子中的图层颜色设置成统一的颜色（"darkblue"）：
#colour="darkblue"直接填写在 point()里即可，不需要在 aes()里面设定。

#否则，如果添加在 aes()里面，它的意义就变成了，将颜色映射到"darkblue"（普通的字符串）上，是用默认的颜色（桃红色）标度进行了转换，就图不达意了

##几何图形函数小结
#geom_blank(): 空白，可以用来限制轴的范围
#geom_point(): 散点
#geom_path(): 路径
#geom_ribbon(): 色带图，连续的x值所对应的y值范围
#geom_segment(): 添加线段或者箭头
#geom_rect(): 二维矩形图
#geom_polyon(): 填充多边形
#geom_text(): 文本注释

#geom_crossbar(): 带水平中心线的盒子图
#geom_errorbar(): 误差线+条形图
#geom_linerange(): 一条代表一个区间的竖直线
#geom_pointrange(): 以一条中心带点的竖直线代表一个区间

##统计变换（stat）
#统计变换是指原始数据通过一定的计算或者汇总，以另一种方式呈现。

##位置调整（Position Adjustments）
#位置调整主要用于调整图层中微小元素的微调。
#position_stack(): 将图形（条形、面积）元素堆叠起来
#position_fill(): 堆叠元素，并将其填充，高度标准化为1
#position_dodge(): 避免堆叠，并排放置
dplot = ggplot(diamonds,aes(color,fill=cut))
dplot + geom_bar()                  ##position stack is the default for bars, so `geom_bar()` is equivalent to `geom_bar(position = "stack")`
dplot + geom_bar(position = "fill")
dplot + geom_bar(position = "dodge")
##叠就是相同的x坐标，一个条形放到另一个上面.积累柱状图
##填充就是在堆叠的基础上高度标准化，100%。
##dodge并列，普通分组柱形图

####################################################第6章，标度、坐标轴和图例
##标度scale，控制数据到图形属性的映射。
#三步：变换、训练、映射。
#标度的类型依赖于变量的类型：连续型（变量为数值）；离散型（变量为因子、逻辑值、字符等）

#################################################ggplot2读书笔记8：第六章 标度（一） 
##标度（scale）是将数据的取值映射到图形空间，例如颜色、大小和形状表示不同的数值。展现标度的常见做法是绘制图例和坐标轴。
##每一种标度都是从数据空间的某个区域（标度的定义域）到图层属性空间的某个值域（标度的值域）的一个函数。标度的定义域对应着提供给这个标度的变量的取值范围。
##定义域（变量）可以是连续型、离散型、有序或者无序型。值域则包括我们可以感知的图形属性（颜色、形状、大小、线条等等）

#执行标度的过程分为：变换，训练，映射
#标度可以粗略地分为四个类别：位置标度，颜色标度，手动离散型标度，同一型标度
#scale的“命名方案”是在后面添加下划线 _，然后添加要修改的相应对象和属性
#xlab()， ylab()可以用来更改x轴和y轴的名称
#如果要将图例名称也更改，就直接用 labs()
df = data.frame(x = 1:2,y = 1,z = "a")
p = ggplot (df,aes(x,y)) + geom_point(aes(colour = z));p
p + xlab("X") + ylab("Y") + labs(colour = "Colour\nlegend") ##\n换行
#如果想去掉坐标轴名称：
p + labs(x="",y="")

#位置标度和标签（Breaks and Labels)
#breaks控制坐标轴上刻度线的单位间隔。每个 breaks都有一个对应的标签 labels。
ggplot(mpg,aes(displ,hwy)) + 
  geom_point()  +
  scale_x_continuous(breaks=c(3,6),labels = c("a","b"))

#改填充颜色
  scale_fill_continuous(breaks=c(3,6),labels = c("a","b"))
#改y轴
  scale_y_discrete(labels = c(a="a",b="b",c="c"))

###图例legends
#图例可以有多个图层。show.legend参数是设置是否显示在图例中，在默认状态下，设定值是为 FALSE，意思是不显示；
# 创建数据框，产生1000个服从正态分布的随机数
norm = data.frame(x=rnorm(1000),y=rnorm(1000))
#设置z，整个数据框取子集分成三个部分
norm$z = cut(norm$x,3,labels=c("a","b","c"))
#只在函数中设置透明度参数，图例透明度默认为0.5
ggplot(norm,aes(x,y)) +
  geom_point(aes(colour = z),alpha = 0.5)

#使用 guides()函数，设置 guide_legend()中的覆盖映射 override.aes：
ggplot(norm,aes(x,y)) +
  geom_point(aes(colour = z),alpha = 0.5)+
  guides(colour =guide_legend(override.aes =list(alpha = 1)))

##点的形状
ggplot(norm,aes(x,y)) + geom_point(aes(colour=z,shape=z))

##图例位置theme()中的 legend.position
#默认状态是 theme(legend.position="right")  ##right”, “left”, “top”, “bottom”，把他放在上/下/左，或者去掉“none”
#还可以通过坐标点直接指定位置

##guide函数
#guide()函数可以提供很多额外的功能，比如 guide_colourbar()（用于连续型变量）； guide_legend()（连续型或离散型变量均可）
#可以通过 ncol， byrow调整图例的各种排列
df = data.frame(x=1,y=1:4,z = letters[1:4])
p = ggplot(df,aes(x,y)) + geom_raster(aes(fill = z))
p
p + guides(fill = guide_legend(ncol = 2,byrow = TRUE))
p + guides(fill = guide_legend(reverse = TRUE)) #通过 reverse调整图例方向

##limits
#xlim(10,20)：设置x轴10-20连续的范围
#ylim(20,10)：设置y轴20-10连续的范围
#xlim("a","b","c")：设置x轴三个离散的字符点
#xlim(as.Date(c("2008-05-01","2008-08-01")))：设置x轴为日期

##标度工具箱（Scales Toolbox）
#除了调整标度的默认选项之外，你也可以重新创建新的标度，主要分为以下四类：
#连续型位置标度：用于将整数、数值、日期/时间数据映射到x轴或者y轴的位置上；
#颜色标度：用于将连续型或离散型变量映射到颜色；
#手动离散型标度：将离散型变量映射到你选择的大小、形状、颜色、线条等；
#同一型标度：当你的数据能被R中的绘图函数理解时，数据空间和图形属性空间相同时，可以使用同一型标度，此时默认不绘制图例的

##1. 连续性标度（Continuous Position Scales）
#scale_x_continuous()和 scale_y_continuous()，它们可以将数据映射到x轴和y轴。

##2. 颜色标度

##3. 手动离散型标度（The Manual Discrete Scales）
#离散型标度 scale_linetype()、 scale_size_discrete()和 scale_shape()基本没有选项。
#这些标度按照一定的顺序将因子的水平映射到一系列取值中。

##4.同一型标度（The Identity Scale)
#当你的数据能被R中的绘图函数理解时，即数据空间和图形属性空间相同，能使用同一型标度 scale_identity

##############################################ggplot2读书笔记10：定位（分面和坐标系） 
#定位由四个部分组成，前两个在前期讲过，而后两个部分在这章中会详细说到：
#位置调整：调整每个图层中出现重叠的对象的位置
#位置标度：控制数据到图形的映射
#分面：在一个页面上自动摆放多个图形。将数据分为多个子集，然后将每个子集依次绘制到不同面板上
#坐标系：控制两个独立的位置标度形成一个二维坐标系，如笛卡尔坐标系

##分面facet
#每个小图都代表不同的数据子集。分面可以快速地分析数据各子集模式的异同。
#facet_null()：默认，无分面，单一图形。
#face_wrap()：封装型，本质上是1维，为节省空间封装成二维
#face_grid()：网格型，生成一个二维的面板由两个独立的部分组成，面板的行和列可以通过变量来定义
base = ggplot(mpg,aes(displ,hwy)) +
  geom_blank()

base + facet_wrap(~class,ncol = 3)
### 设置as.table。  as.table决定各个分面的排列顺序。
base + facet_wrap(~class,ncol = 3,as.table = FALSE)
### 设置dir，水平或垂直排列
base + facet_wrap(~class,ncol = 3,dir = "v") ##h

####facet_grid
#网格分面，在二维网格中展示图形。
#需要设定哪些变量作为分面绘图的行，哪些变量作为列，中间用 ~短波浪线做标记。
base + facet_grid(drv~.)  ##行
base + facet_grid(.~cyl)  ##列
base + facet_grid(drv~cyl) #行+列

#控制标度
#• scales="fixed": x 和 y的标度在所有分面中都相同（默认）
#• scales="free_x": x 的标度可变，y 固定
#• scales="free_y": y 的标度可变，x 固定
#• scales="free": x 和 y 的标度都可变

##在 facet_grid()中，有个参数叫 space，当 space="free"时，每行的高度和该行的标度范围一致，这对分类标度很有用

##group_by()用于对数据集按照给定变量分组，返回分组后的数据集。对返回后的数据集使用以上介绍的函数时，会自动的对分组数据操作。
##%>%，可以直接把数据传递给下一个函数调用或表达式。

##library
library(magrittr)
library(dplyr)  #分面和颜色结合起来：select()参数，选择子数据集， -z是指除去 z。
df = data.frame(
  x = rnorm(120,c(0,2,4)),
  y = rnorm(120,c(1,2,1)),
  z = letters[1:3]
)
df
df2 = dplyr::select(df,-z)
df2
ggplot(df,aes(x,y)) +
  geom_point(data = df2,colour = "grey70") +
  geom_point(aes(colour  = z)) +
  facet_wrap(~z)

##连续变量
#连续变量也可以用来分面，我们需要先给变量规定区间，转变成离散型：
#cut_interval(x,n)：把数据分割成n和相同长度的部分
#cut_width(x,width)：按宽度分
#cut_number(x,n=10)：将数据划分为n个相同数目点的部分，每个分面上点数相同（x代表数据集）

###坐标系（Coordinate Systems)
#坐标系是将两种位置标度结合在一起的二维定位系统
#主要有俩功能：
#将两个位置图形属性（位置1、位置2；例如x, y）结合起来在图形中形成二维方位系统
#配合方面：绘制坐标轴和面板背景。标度控制坐标轴的数值，映射到图形上的位置，然后通过坐标系将他们绘制出来。

#线性坐标系：
#coord_cartesian()：即默认笛卡尔坐标系
#coord_flip()：x轴y轴互换后的笛卡尔坐标系
#coord_fixed()：具有固定长宽比的笛卡尔坐标系
#非线性坐标系：
#coord_map()/ coord_quickmap(): 地图投影
#coord_polar(): 极坐标
#coord_trans(): 变换后的笛卡尔坐标系


########ggplot2读书笔记11：第八章 主题设置、存储导出 
#主题元素，指的是非数据元素， plot.title控制标题的外观， axis.ticks.x控制x轴的刻度， legend.key.height控制图例中按键的高度。
#元素函数，描述元素的视觉属性，例如 element text()可以设置字体大小、颜色和文本外观如 plot.title。
#theme()函数，用来覆盖默认的主题元素，如 theme(plot.title=element text(colour="red"))
#完整主题，如 theme_grey()，用来把所有主题元素协调一致。

#theme_bw(): 是 theme_grey()的变体，白色背景和灰色网格线
#theme_linedraw(): 白色背景黑色线条
#theme_light(): 和 theme_linedraw()很像，区别是线条为灰色
#theme_dark():黑色背景的 theme_light()，可以用来画薄彩色线条
#theme_minimal():简约主题
#theme_classic(): 只有x、y轴没有背景和网格线
#theme_void(): 完全空白的主题

##ggthemes 包

##元素函数有四种基本类型：字体（text）、线条（line）、矩形（rectangles）和空白（blank）。
#plot +theme(element.name =element(function())  #修改主题中的个别部分，使用相关的元素函数。
#element_text()：修改图标题的位置和字体，包括 family、 face、 colour、 size、 hjust、 vjust、 angle、 lineheight这些参数
theme(plot.title=element_text(hjust=0.5))
#margin()参数可以设置标题和图表之间的距离，默认值是0，左右上下均可设置：
#plot.title  axis.title.y  axix.title.x
theme(plot.title=element_text(margin = margin(t = 10,b = 10)))

#element_line：修改网格线，颜色、粗细、虚实等，如 colour， size以及 linetype
theme(panel.grid.major = element_line(colour = "black",size = 2, linetype = "dotted"))

#element_rect：添加矩形图层，如 fill（修改背景颜色）， colour， size以及 linetype
theme(plot.background = element_rect(fill = "linen",colour =NA,size = 2 ))

#element_blank()：清空画板
theme(panel.grid.minor = element_blank())
theme(panel.grid.major = element_blank())

##总结了控制主题的主要元素
#https://mp.weixin.qq.com/s?__biz=MzU4MzQ3NDExMw==&mid=2247483780&idx=1&sn=8abaca12a9eeee4619dcc4f68cdb1929&chksm=fda9cf2acade463c7ca90b58a1ed897483c281c94e78e6870749b616b3945bb1ae2f2e0ca1df&scene=21#wechat_redirect


##储存和导出（Saving Your Output）
#当保存图片时，你有两种基本选择：矢量型图片和栅格型图片
#矢量型：图形可以无限缩放没有细节的损失；但如果包含数千个对象，矢量渲染过程会很慢
#栅格形：以像素阵列形式存储，有固定的最优观测大小，对于图形印刷，分辨率（600dpi）是较好的方案。

#第一种：
pdf("output.pdf" , width  = 6 , height = 6)
ggplot(mpg,aes(displ, cty ))+geom_point()
dev.off()
#第二种使用 ggsave：
ggplot(mpg,aes( displ, cty))+geom_point()
ggsave("output.pdf")

#显然第二种方法更加方便简洁，不过我们需要设置以下参数：
#path设定图形储存路径。 ggsave() 可以生成以下格式： .eps, .pdf, .svg, .wmf,.png, .jpg, .bmp, and .tiff.
#width和 height设置绝对尺寸的大小，可以精确控制尺寸
#分辨率 dpi默认值300，你可以修改为600。

#########################ggplot2读书笔记12：第九章 数据分析 
#整理数据（Tidy Data）
#整理数据的原则很简单：用一致的方式存储数据。（storing your data in a consistent way）
#所以整理数据的目的是为了创造一个数据框的统计学结构（变量和观测数据）和物理结构（列和行）之间的映射。
#变量放在列中（Variables go in columns）
#观测数据放在行中（Observations go in rows)
#我们需要先安装几个R包， dplyr、 tidyr、 magrittr。用于时间数据处理的r包 lubridate
library(dplyr)
library(tidyr)
library(magrittr)
library(lubridate)
ec2 = ggplot2::economics %>%
      tbl_df() %>%
      transmute(year = year(date),month = month(date),rate = uempmed)%>%
      filter(year > 2005) %>%
      spread(year,rate)
ec2
#这个数据集的混乱之处在于，变量 year、 month分别是列名和行名，观测结果在每个小单元里。
#然后我们需要用两对工具：
#Spread & gather
#Separate & unite

#tidyr包包含两个函数 gather()、 spread() 可以用来执行以下操作：

#gathering：从笛卡尔数据翻译成索引数据
#spreading：从索引数据翻译成笛卡尔数据

#gather()函数包含以下四个参数：
#data：需要转换的数据集
#key：将从列名创建出的变量名称
#value：将从每个单元创建出的变量名
#...：要收集哪些变量，可以一个一个列出来，也可以使用简写形式 ..:..
#所以我们要整理上面的ec2数据集，就要把所有变量放在列中。其中只有 month在列中， year和 rate还是“笛卡尔数据形式”，所以我们要把他们转换成“索引形式”，可生成以下数据集：

gather(ec2,key = year, value = unemp, "2006":"2015")
#或：gather除了month之外的数据
gather(ec2,key = year, value = unemp,-month)
#增加两种参数，可以更加严谨：
gather(ec2,year, rate, "2006":"2015",convert = TRUE,na.rm=TRUE)
#convert=TRUE自动将年份从字符串转换为数字
#na.rm=TRUE自动删除没有数据的月份

#当多个变量被放在了一列中，或者很多列中有一个变量时，我们使用 separate()和 unite()函数。
trt = dplyr::data_frame(
  var = paste0(rep(c("bed","end"),each = 3),"_",rep(c("a","b","c"))),
  val = c(1,4,2,10,5,11)
  
)
trt
#separate()函数包括：
#data：要修改的数据框
#col：要分隔的列
#into：给新变量命名的字符向量
#sep：描述如何分开这些变量
separate(trt,var,c("time","treatment"),"_")

?paste
?paste0  ##paste0("a", "b") == paste("a", "b", sep="")

####################ggplot2读书笔记13：第十章 数据变换 
##通常情况下，除了整理数据之外，我们还需要把原始数据做一些数据变换（聚合等），这时就要使用到 dplyr包。
#本章中我们学习 dplyr中四个重要的函数的用法：
#filter()
#mutate()
#group_by()
#summarise()
##将数据框作为第一个参数，然后返回一个修改后的数据框
#本章中还会用到 %>%创建数据转换管道， %>%和ggplot2中的 +有相似的作用。

##filter()函数，按给定的逻辑判断筛选出符合要求的子数据集。
diamonds
ggplot(diamonds,aes(x,y))+
  geom_bin2d()

diamonds_ok = filter(diamonds,x>0,y>0,y<20)
ggplot(diamonds_ok,aes(x,y))+
  geom_bin2d()

# filter()的第一个参数是数据集（数据框），第二个参数必须是逻辑向量。 filter()选择逻辑表达为TRUE的每一行，所以逻辑向量的长度必须和数据框的长度相同，否则就会报错。
# 通常我们使用比较运算符来创建逻辑向量：
# x==y：x等于y
# x!=y：x不等于y
# x%in%c("a","b","c")：x是右侧的值之一
# x>y, x>=y, x<y, x<=y：大于，大于等于，小于，小于等于
# 和逻辑运算符结合：
# !x ：不是x，逻辑FALSE
# x&y：如果x和y均为TRUE，则为TRUE
# x|y：如果x或y（或两者）为TRUE，则为TRUE
# xor(x,y)：如果x或y（不是两者都）是TRUE，则为TRUE

##创建新变量的函数是 mutate()
diamonds_ok2 = mutate(diamonds_ok,
                      sam = x-y,
                      size = sqrt(x^2+y^2)
  
)
diamonds_ok2

###分组和摘要（Group-wise Summaries）
# dplyr包分两步，从不同的分组变量输出摘要列表：
# group_by()：创建分组变量
# summarise()：描述每行的摘要

#####ggplot2读书笔记14：第十一章 可视化建模（一）拟合回归曲线 
#####ggplot2读书笔记15：第十一章 可视化建模（二）将统计模型的结果整理成数据框 
#####ggplot2读书笔记16：第十二章 Programming with ggplot2 

data(mpg)
mpg
ggplot(mpg,aes(displ,cyl))+
  geom_point()


###2018.6.7
##https://mp.weixin.qq.com/s?__biz=MzI5NjUyNzkxMg==&mid=2247486026&idx=1&sn=8c615849b5d064a431c47a7be14735d6&chksm=ec43bb0ddb34321be5ff1376b19c987f4048ff646cfd48e576c092b594728fcd7380b5b9e23c&scene=0#rd
##代码无感者的福音，小白也能ggplot2画出高大上的图 

###点鼠标就可以画图，这就是RStudio的插件 esquisse。
rm(list = ls())

#1
devtools::install_github("dreamRs/esquisse")
#3
source("https://install-github.me/dreamRs/esquisse")
#2
library(devtools)
install_github("dreamRs/esquisse")

esquisse::esquisser()
options("esquisse.display.mode" = "browser") ##打开新的浏览器.#pane : RStudio's Viewer
esquisse::esquisser()

###2018.6.8
##https://mp.weixin.qq.com/s?__biz=MzI5NjUyNzkxMg==&mid=2247486024&idx=1&sn=6a34900c759327755e3fabff4713fbe6&chksm=ec43bb0fdb343219ca6d7638bdbaf4f9658cc3af4d782e0ccb834e22a2a8713f17968e0c2649&mpshare=1&scene=1&srcid=0608ioXytT1FMdmVkmBMQ84K&pass_ticket=kMch7gBgGPki75NP%2FtSwMEnqtGbtXcK57CtBKlnyuiGs2XI5Nxalc9ga6qh%2BSua4#rd
##不需要花时间去学ggplot2主题系统 

##点鼠标来调细节ggplot2
install.packages("ggThemeAssist")
#devtools::install_github("calligross/ggthemeassist")

##2018.6.22
##https://mp.weixin.qq.com/s?__biz=MzI5NjUyNzkxMg%3D%3D&mid=2247483941&idx=1&sn=bb352b5d74797715a9759f64765e49f6&scene=45#wechat_redirect
##Use ggplot2 
require(ggplot2)
data(diamonds)
set.seed(42)
small <- diamonds[sample(nrow(diamonds), 1000), ]
head(small)

#坐标轴翻转由coord_flip()实现
ggplot(small)+geom_bar(aes(x=cut, fill=cut))
ggplot(small)+geom_bar(aes(x=cut, fill=cut))+coord_flip()
#转换成极坐标可以由coord_polar()实现：
ggplot(small)+geom_bar(aes(x=factor(1), fill=cut))+coord_polar(theta="y")
#饼图实际上就是柱状图，只不过是使用极坐标而已，柱状图的高度，
#对应于饼图的弧度，饼图并不推荐，因为人类的眼睛比较弧度的能力比不上比较高度（柱状图）
#还可以画靶心图：
ggplot(small)+geom_bar(aes(x=factor(1), fill=cut))+coord_polar()
#以及风玫瑰图(windrose)
ggplot(small)+geom_bar(aes(x=clarity, fill=cut))+coord_polar()

#图层（Layer）
#做为图层的一个很好的例子是蝙蝠侠logo，batman
#logo由6个函数组成，在下面的例子中，我先画第一个函数，
#之后再加一个图层画第二个函数，不断重复这一过程，直到六个函数全部画好。
require(ggplot2)
f1 <- function(x) {
  y1 <- 3*sqrt(1-(x/7)^2)
  y2 <- -3*sqrt(1-(x/7)^2)
  y <- c(y1,y2)
  d <- data.frame(x=x,y=y)
  d <- d[d$y > -3*sqrt(33)/7,]    
  return(d)
}

x1 <- c(seq(3, 7, 0.001), seq(-7, -3, 0.001))
d1 <- f1(x1)
p1 <- ggplot(d1,aes(x,y)) + geom_point(color="red") +xlab("") + ylab("") + theme_bw()
p1
x2 <- seq(-4,4, 0.001)
y2 <- abs(x2/2)-(3*sqrt(33)-7)*x2^2/112-3 + sqrt(1-(abs(abs(x2)-2)-1)^2)
d2 <- data.frame(x2=x2, y2=y2)
p2 <- p1 + geom_point(data=d2, aes(x=x2,y=y2), color="yellow")
p2
x3 <- c(seq(0.75,1,0.001), seq(-1,-0.75,0.001))
y3 <- 9-8*abs(x3)
d3 <- data.frame(x3=x3, y3=y3)
p3 <- p2+geom_point(data=d3, aes(x=x3,y=y3), color="green")
p3
x4 <- c(seq(0.5,0.75,0.001), seq(-0.75,-0.5,0.001))
y4 <- 3*abs(x4)+0.75
d4 <- data.frame(x4=x4,y4=y4)
p4 <- p3+geom_point(data=d4, aes(x=x4,y=y4), color="steelblue")
p4
x5 <- seq(-0.5,0.5,0.001)
y5 <- rep(2.25,length(x5))
d5 <- data.frame(x5=x5,y5=y5)
p5 <- p4+geom_point(data=d5, aes(x=x5,y=y5))
p5
x6 <- c(seq(-3,-1,0.001), seq(1,3,0.001))
y6 <- 6 * sqrt(10)/7 +(1.5 - 0.5 * abs(x6)) * sqrt(abs(abs(x6)-1)/(abs(x6)-1)) - 6 * sqrt(10) * sqrt(4-(abs(x6)-1)^2)/14
d6 <- data.frame(x6=x6,y6=y6)
p6 <- p5+geom_point(data=d6,aes(x=x6,y=y6), colour="blue")
p6

multiplot(p1,p2,p3,p4,p5,p6, cols=2)



#主题（Theme）
f <- function(x) 1/(x^2-1)
x <- seq(-3,3, by=0.001)
y <- f(x)
d <- data.frame(x=x,y=y)

p <- ggplot()
p <- p+geom_rect(fill = "white",color="black",size=3,
                 aes(NULL, NULL,xmin=-3, xmax=3,
                     ymin=-3,ymax=3, alpha=0.1))  ##先画了一个正方形
p
p <- p + geom_line(data=d, aes(x,y), size=3)+ylim(-3,3) ##囧
p
theme_null <- function() {
  theme_bw() %+replace%
    theme(axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          legend.position="none",
          panel.grid.minor=element_blank(),
          panel.grid.major=element_blank(),
          panel.background=element_blank(),
          axis.ticks=element_blank(),
          panel.border=element_blank())
}

p+theme_null()+xlab("")+ylab("")


####蝴蝶图:
theta <- seq(0,24*pi, len=2000)
radius <- exp(cos(theta)) - 2*cos(4*theta) + sin(theta/12)^5
dd <- data.frame(x=radius*sin(theta), y=radius*cos(theta))
ggplot(dd, aes(x, y))+geom_path()+theme_null()+xlab("")+ylab("")


##最后以生物界中常用的柱状图+误差图为实例，展示ggplot2非常灵活的图层。
#以我2011年发表的文章Phosphoproteome profile of human lung cancer cell line A549中
#的westernblot数据为例。这个实例展示了图层，标尺，主题，注释和各种细节微调多种元素。
Normal <- c(0.83, 0.79, 0.99, 0.69)
Cancer <- c(0.56, 0.56, 0.64, 0.52)
m <- c(mean(Normal), mean(Cancer))
s <- c(sd(Normal), sd(Cancer))
d <- data.frame(V=c("Normal", "Cancer"), mean=m, sd=s)
d$V <- factor(d$V, levels=c("Normal", "Cancer"))

p <- ggplot(d, aes(V, mean, fill=V, width=.5))
p <- p+geom_errorbar(aes(ymin=mean, ymax=mean+sd, width=.2), 
                     position=position_dodge(width=.8));p
p <- p + geom_bar(stat="identity", position=position_dodge(width=.8), colour="black");p #这个colour是边框的颜色
p <- p + scale_fill_manual(values=c("grey80", "white"));p
p <- p + theme_bw() +theme(legend.position="none") + xlab("") + ylab("");p
p <- p + theme(axis.text.x = element_text(face="bold", size=12), 
               axis.text.y = element_text(face="bold", size=12));p
p <- p+scale_y_continuous(expand=c(0,0), limits=c(0, 1.2), breaks=seq(0, 1.2, by=.2));p
p <- p+geom_segment(aes(x=1, y=.98, xend=1, yend=1.1));p
p <- p+geom_segment(aes(x=2, y=.65, xend=2, yend=1.1));p
p <- p+geom_segment(aes(x=1, y=1.1, xend=2, yend=1.1));p
p <- p + annotate("text", x=1.5, y=1.08, label="*");p













