##2019.9.25

##合并多个文件为一个

##http://wap.sciencenet.cn/blog-267448-1166363.html

#在数据处理过程，我们经常要将很多独立的文件拼接起来，然后在组合到一个文件里面！其实干这个事情简单一个循环就可以搞定！

#批量读入数据并合并为一个文件
#在当前工作目录是下新建一个文件夹叫"myname"
#把要读入的数据存储在myname文件夹里面，这里只能是1层不能再有下一层文件夹
#先用dir()函数获取目录中的文件名
doc.names <- dir("myname")
#更改工作目录至"myname"下面
setwd("./myname")

#确定读入文件的个数
n=length(doc.names)
#首先读入第一个文件
a=read.table(doc.names[1],header=T)
#然后读入后面的文件并与前面的文件行合并
for (i in 2:n){a=rbind(a,read.table(doc.names[i],header=T))}
#当前案例是读入文本文件，如果是别的数据，修改为相应函数即可
#a就是你要的文件
a