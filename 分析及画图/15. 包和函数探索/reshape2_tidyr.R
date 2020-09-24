##2018.6.30

##长宽数据转换
# 
# 什么是长数据？ 
# 长数据一般是指数据集中的变量没有做明确的细分，
#即变量中至少有一个变量中的元素存在值严重重复循环的情况（可以归为几类），
#表格整体的形状为长方形，即 变量少而观察值多。 
# 
# 什么是宽数据？ 
# 宽数据是指数据集对所有的变量进行了明确的细分，
#各变量的值不存在重复循环的情况也无法归类。数据总体的表现为 变量多而观察值少。
# 
# 为什么需要转换？ 
# 长数据与宽数据之间的转换通常为以下两个原因：
# 时间序列数据想要观察多个种类的变量在一段时间内的变化，宽数据格式无法利用ggplot做出图形。
# 当数据清洗完成后，导入某些软件时，例如导入SPSS软件时宽数据格式会更好。

library(tidyr)# 使用的gather & spread
library(reshape2) # 使用的函数 melt & dcast 
##gather,melt 宽数据转换为长数据。
##spread,dcast 长数据转换为宽数据。

mydata<-data.frame(
  Name = c("苹果","谷歌","脸书","亚马逊","腾讯"),
  Conpany = c("Apple","Google","Facebook","Amozon","Tencent"),
  Sale2013 = c(5000,3500,2300,2100,3100),
  Sale2014 = c(5050,3800,2900,2500,3300),
  Sale2015 = c(5050,3800,2900,2500,3300),
  Sale2016 = c(5050,3800,2900,2500,3300)
)
mydata

###reshape2:melt
mydata2<-melt(
  mydata,                       #待转换的数据集名称
  id.vars=c("Conpany","Name"),  #要保留的主字段
  variable.name="Year",         #转换后的分类字段名称（维度）
  value.name="Sale"             #转换后的度量值名称
)
mydata2

###tidyr:gather
data1<-gather(
  data=mydata,      #待转换的数据集名称
  key="Year",       #转换后的分类字段名称（维度）
  value="Sale" ,    #转换后的度量值名称
  Sale2013:Sale2016 #选择将要被拉长的字段组合
)               #（可以使用x:y的格式选择连续列，也可以以-z的格式排除主字段）
data1

###reshape2:dcast
dcast(
  data=data1,         #数据集名称
  Name+Conpany~Year   #x1+x2+……~class 
  #这一项是一个转换表达式，表达式左侧列       
  #出要保留的主字段（即不会被扩宽的字段，右侧则是要分割的分类变量，扩展之后的       
  #宽数据会增加若干列度量值，列数等于表达式右侧分类变量的类别个数
)

###tidyr:spred
spread(
  data=data1,   #带转换长数据框名称
  key=Year,     #带扩宽的类别变量（编程新增列名称）  
  value=Sale)   #带扩宽的度量值 （编程新增列度量值）


##参考
https://blog.csdn.net/ray_zhu/article/details/78679913
https://www.cnblogs.com/zlslch/p/8644627.html

