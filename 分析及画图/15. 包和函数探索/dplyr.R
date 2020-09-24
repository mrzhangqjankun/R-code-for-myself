##2018.8.6
##使用dplyr进行数据操作（30个实例） 
https://mp.weixin.qq.com/s?__biz=MzUzMjA4Njc1MA==&mid=2247486149&idx=1&sn=c9438a01599660c1e5a9c80c6da0e8c6&chksm=fab9e074cdce696261d4f7a49105b7a4258a8bd249a697f836a2cc09a3e568579a1e2e98662a&mpshare=1&scene=1&srcid=0806V8ONiNVfoC0ZErZ9eTE6&pass_ticket=zLn7FeLELfcDfG80ufyPIuExsSgd%2BRmegWlXWwsyZJbGgb6dhnfM1Fhvnab%2BuXV8#rd

#install.packages("dplyr")
library(dplyr)

data(mtcars)
head(mtcars)
length(mtcars)
#实例1：随机选择N行
#sample_n函数从数据框（或表）中随机选择行。 函数的第二个参数告诉R要选择的行数。
sample_n(mtcars,3)

# 实例2：随机选择总行的N%
# sample_frac函数随机返回N％的行。 在下面的例子中，它随机返回10％的行。
sample_frac(mtcars, 0.1)

# 实例3：基于所有变量（完整行）删除重复行
# distinct函数用于消除重复行

y = rbind(head(mtcars), head(mtcars))
dim(y)
x1 <-  dplyr::distinct(y)
dim(x1)
#在此数据集中，我们取前6行重复一次共12行，去冗余返回6行

# 实例6：选择变量（或列）
# 假设你被要求只选择几个变量。
cars = select(mtcars,mpg,cyl:drat)
cars
# 
# 实例7：删除变量
# 变量前面的减号表示R放弃变量。
cars = select(mtcars,-mpg,-cyl:drat)
# 
# 实例8：选择或删除以”m”开始的变量
# starts_with（）函数用于选择以字母开头的变量。
cars = select(mtcars,starts_with("m"))
cars
#在starts_with（）之前添加一个负号表示将删除以’m’开始的变量

# 实例11：变量重命名
# rename函数可用于重命名变量。
# 在下面的代码中，我们将’Index’变量重命名为’Index1’。
mydata9 <- rename(mydata, Index1=Index)

###2019.6.26
#https://mp.weixin.qq.com/s/FODoG8U5HMvS8nGgtRbb9g
#rbind合并要求字段名称相同，这里我们可以使用dplyr包中的bind_rows函数，这个函数会对字段名称不相同的数据进行判断，NA自动补全。
library(dplyr)
class1<-data.frame(name=c("Tom","Mary","Bob","Mike","Lily"),hight=c(170,165,175,180,158))
class3<-data.frame(name=c("Jackson","Iris","Edison","Rose","Annie","Julie"),score=c(90,85,75,80,95,60))
class1.1<-data.frame(name=c("Jackson","Owen","Bob","Mike","Lily"),score=c(90,85,75,80,95))
bind_rows(class1,class3)

#cbind函数无需匹配主字段，仅将数据进行横向拼接。
#若我们想将两个数据的共有字段“name”合并在一起，而不是简单的横向拼接，可以使用merge函数。
#merge函数对数据进行横向合并，可针对主字段进行匹配，如果主字段名称不一致，可以指定匹配的主字段名称。
#基本语法：
#merge(x,y,by = , by.x = , by.y = , all = , all.x = , all.y = ,...)
merge(class1,class1.1,by="name",all=TRUE)
merge(class1,class1.1,by="name",all=FALSE)
merge(class1,class1.1,by="name",all.x=TRUE)
merge(class1,class1.1,by="name",all.y=TRUE)

#如果我们合并的两个数据是主字段不相同时，使用by.x和by.y参数指定列名称，修改class1.1的列名称来进行演示：
colnames(class1.1)=c("studentname","score")
merge(class1,class1.1,by.x="name",by.y="studentname",all=TRUE)
