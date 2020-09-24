##https://zhuanlan.zhihu.com/p/28337816
##li 2.17.9.2 start

#####9.2
#####常用数据导入方式简介
##https://mp.weixin.qq.com/s?__biz=MzA3Njc0NzA0MA==&mid=2653189801&idx=1&sn=b7661534f1bfd148dfaaf6b61135bb67&scene=21#wechat_redirect

data<-read.csv("C:\\Users\\Administrator\\Desktop\\myfile.csv")  #CSV数据读入
#如果文件内数据第一行无标题，需要在括号内路径后指定顶行非标题，("******"，header = F),默认参数为header=T,即顶行为变量名称。

data<-read.table("C:\\Users\\Administrator\\Desktop\\myfile.txt",header=T)#TXT读入

#剪切板直接复制
#这种方法比较粗暴，当然也较容易出现问题，先在excel或者其他数据文件中复制数据区域，在Rstudio中输入：
data <- read.table("clipboard", header = T, sep = '\t')#直接复制
#然后回车或者“Ctrl+L”运行即可导入。

#关于包的安装、加载及更新、卸载:
update.packages()#查看可更新包

install.packages("ggplot2")#安装下载工具包
library(ggplot2)#加载下载工具包
detach("ggplot2")#分离包（从内存空间中移除）
remove.packages("ggplot2")#删除（相当于卸载）

#关于R语言软件的更新：
install.packages("installr")#下载安装工具包
library(installr) #加载安装工具包
check.for.updates.R()#检测是否有最新版的R软件
installr()#下载并安装新版R软件
copy.packages.between.libraries()#复制旧版R中的包到新版R中


#####9.2
#####数据重塑及导出操作 
#导入xlsx。excel数据文件属于富文本类型，结构相对复杂，需要解除特殊包的支持以及java环境
#导入数据之前，最好先配置好你系统内的java环境，确保其与你的R语言版本一致。
#导入xlsx数据所需用到的包：
install.packages("rJava")
install.packages("xlsx")
install.packages("xlsxjars")
library("rJava")
library("xlsx")
library("xlsxjars")

#以下是导入代码：
#括号内第一个参数是路径及文件名，sheetName="file"是指定要导入的excel工作薄内的工作表对象，
#如果你对工作表有命名，一定要指定名称，如果没有，指定为默认的工作表名称（Sheet1、2、3），
#第三个参数指定导入数据文件的编码方式（UTF-8）。
data<-read.xlsx("E:桌面/emi13775-sup-0002-suppinfo2.xlsx",sheetName="Soil Properties",header=T,encoding='UTF-8')
data



#####9.2
#####数据合并与追加 
#横向追加（无需匹配字段） cbind
ID<-c(1,2,3,4)
Name<-c("A","B","C","D")
Score<-c(60,70,80,90)
Sex<-c("M","F","M","M")
One<-data.frame(ID,Name)
Two<-data.frame(Score,Sex)
Total<-cbind(One,Two);Total

#纵向合并,名字必须相同才能合并
ID<-c(1,2,3,4)
Name<-c("A","B","C","D")
Student1<-data.frame(ID,Name)
ID<-c(5,6,7,8)
Name<-c("E","F","G","H")
Student2<-data.frame(ID,Name)
Total_student3<-rbind(Student1,Student2)

#merge函数：
#merge函数主要针对横向（列字段）合并，而且可以针对主字段（主键）进行匹配，如果主字段名称不同，还可以指定前后相匹配的主字段。
#基本语法如下：
merge(x, y, by = , by.x = , by.y = , all = , all.x = , all.y = , sort = , suffixes = , incomparables = , ...)

x<-data.frame(name=c("John","Paul","George","Ringo","Stuart","Pete"),instrument=c("guitar","bass","guitar","drums","bass","drums"))
y<-data.frame(name=c("John","Paul","George","Ringo","Brian"),band=c("TRUE","TRUE","TRUE","TRUE","FALSE"))
x;y

#inner（内部链接），交集
m1 <- merge(x,y, by.x = "name", by.y = "name");m1

#left join（左连接）,共有的和x有的
m2 <- merge(x,y, by.x = "name", by.y = "name",all.x=TRUE);m2

#right join（右连接），共有的和y有的
m3 <- merge(x,y, by.x = "name", by.y = "name",all.y=TRUE);m3

#all_join（外连接），并集
m4 <- merge(x,y, by.x = "name", by.y = "name",all=TRUE);m4

#plyr::join函数
library("plyr")
#下面四个作用同上
#left_join，共有的和x有的
data1<-join(x,y,by="name",type = "left")

#right_join，共有的和y有的
data2<-join(x,y,by="name",type = "right")

#inner_join，交集
data3<-join(x,y,by="name",type = "inner")

#full_join，并集
data4<-join(x,y,by="name",type = "full")

#dplyr::inner_join/full_join/left_join/right_join,作用同上



#####9.2
#####信息可视化-文字云
install.packages("wordcloud2")
library(wordcloud2)
?wordcloud2
wordcloud2(demoFreq)
wordcloud2(demoFreq, size = 1,shape = 'pentagon')
wordcloud2(demoFreq, size = 1,shape = 'star')
wordcloud2(demoFreqC, size = 2, minRotation = -pi/6, maxRotation = -pi/6,
           rotateRatio = 1)

letterCloud(demoFreq, word = "傻虫子", wordSize = 2,color = 'random-dark',backgroundColor = "snow") 

Emotion <-system.file("E:/桌面/aaa.png",package = "wordcloud2")  #调用图片路径
wordcloud2(demoFreq, figPath = Emotion, size = 1,color = "random-light")  #使用默认案例数据集并结合选定的图片呈现文字云效果

###11.22
x <- 1:20 
y <- sapply(x, function(i) 1-0.9^i) ;y
#sapply返回值是和一个和X有相同的长度的list对象，这个list对象中的每个元素是将函数FUN应用到X的每一个元素。其中X为List对象（该list的每个元素都是一个向量）
 xv <- seq(1,20, 0.1) 
 x2 <- x^2 
 qualm <- lm(y ~ x+x2) ##x和x2为自变量。lm线性回归
 ##predict(model,newdata,type)其中model就是你之前通过训练数据拟合的,newdata是你用来预测结果的数据集
 #type大部分是分类问题才要提供，一般你是要显示结果为概率还是直接给你分类。 
 #如果你的模型是y~x,(x是自变量），首先估计模型Model=lm(y~x,data=...)
 #估计出来后，进行预测，例如预测x=2时候的y值，则应当输入命令
 #predict(Model,newdata=data.frame(x=2),interval="confidence")
 ##即先用lm做线性回归，再根据回归线进行预测。
 yv <- predict(qualm, list(x=xv, x2=xv^2)) 
 lines(xv,yv)
 summary(qualm)
plot(xv,yv,type="o")
plot(x2,y,type="o")

##11.23ggtree
##https://mp.weixin.qq.com/s/Uhx3l3lKQS88OJ4SHntkNg
install.packages("ggtree")
devtools::install_github("GuangchuangYu/ggtree")
 require(ggtree)
 nhxfile <- system.file("extdata/NHX", "ADH.nhx", package="treeio")
 nhx <- read.nhx(nhxfile)
 p = ggtree(nhx)
 as.phylo(p)

 ##11.23  swirl听说你想学R-swirl
 #https://guangchuangyu.github.io/cn/2017/07/swirl/
 install.packages("swirl")
??swirl
 require(swirl)
 

#https://mp.weixin.qq.com/s?__biz=MzUzMTEwODk0Ng==&mid=2247485365&idx=1&sn=8000ad9eb07785ed706ba6204ea92169&chksm=fa46c288cd314b9e152d102e33d6444a7a5f4dbc5987c9e8b192ce3b7f5809ae0c3d56c078cd&scene=0#rd
 ##select
 data = cars
 head(data);dim(data)
 library(dplyr)
 ?select
 data = cars
 head(data);dim(data)
 ss = data;head(ss)
 sss = select(ss,dist,everything());head(sss) ##把dist作为第一列。调整列的位置。
 
 ##mutate 代表重新生成一个新的变量,且变量在最后
 sss = mutate(ss,percent = dist/speed)
 head(sss)
 ##如果我们只想保留我们生成的变量，就要使用transmute()函数
 sss= transmute(ss,percent = dist/speed)
 head(sss)
 
 #%/% 表示取整数部分
 #%% 取余数部分
 #cumsum(x) 连续取和
 #cummin(x) 连续取最小值
 x = c(1:10)
 cumsum(x)
 cummean(x)
 
 ?merge
 ## example of using 'incomparables'
 x1 = c(1,2,3)
 x2 = c("a","b","c")
 merge(x1,x2)
 
 x <- data.frame(k1 = c(NA,NA,3,4,5), k2 = c(1,NA,NA,4,5), data = 1:5);x
 y <- data.frame(k1 = c(NA,2,NA,4,5), k2 = c(NA,NA,3,4,5), data = 1:5);y
 merge(x, y, by = c("k1","k2")) # NA's match
 merge(x, y, by = "k1") # NA's match, so 6 rows
 merge(x, y, by = "k2", incomparables = NA) # 2 rows
 