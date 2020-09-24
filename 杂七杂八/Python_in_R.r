##2019.3.7
#https://mp.weixin.qq.com/s/UIpGD7YF8kAaqluBoe8iWA
#手把手 | 哇！用R也可以跑Python了 

install.packages("reticulate")
library(reticulate);?reticulate

#检查您的系统是否安装过Python
py_available()   #False,要先安装python

#使用函数import（）来导入特定的包或模块。
os <- import("os")
os$getcwd()  #返回工作目录

#使用os包中的listdir（）函数来查看工作目录中的所有文件。
os$listdir()

#安装Python包
#第一步：创建新的工作环境；
conda_create("r-reticulate")   ##需要有anaconda

#第二步：在conda环境下安装“r-reticulate”和“numpy”；
conda_install("r-reticulate”,“numpy")

#如果“numpy”已经安装，您不必再次安装这个包。上面的代码只是给个例子而已。
#第三步：加载包。
numpy <- import("numpy")

########后面略