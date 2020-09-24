##2018.5.10
##http://www.pzhao.org/zh/post/rmickey/
##9分钟写个R语言包：菜鸟致简速成

###谢益辉的开发R程序包之忍者篇，2011年。反复读了好几遍。
#https://cosx.org/2011/05/write-r-packages-like-a-ninja
###黄俊文的极简R包建立方法, 2013年。反复读了好几遍。
#https://cosx.org/2013/11/building-r-packages-easily
###Hadley Wickham 的R packages, 2015年。反复扫了好几眼。
#http://r-pkgs.had.co.nz/


install.packages(c("devtools", "roxygen2", "testthat", "knitr"))

##黄俊文
library("devtools")
has_devel()  ##诊断环境。true
create("E:/desktop/R script 2017/Write R packages/abdiv") ##创建文件夹，以包的名字命名
setwd("E:/R/writing R packages/abdiv")
dir()
file.edit("DESCRIPTION")  ##编辑说明文档

rm(list=ls())

load_all()  ##每次修改完函数后都要运行一遍
getwd()
dir()
library("vegan")
x<-read.table("otu.txt",sep="\t",header=T,row.names=1)
adiv(x)
bdiv(x)

##测试完成无错误后，编写函数上面的注释信息，再进行下一步

document() ##就会生成对应的 *.Rd 文件在 man 文件夹中。

build()##与包文件夹平行的文件夹中生成 somebm_0.1.tar.gz 类似的打包文件。可以在 R 环境中使用 install.packages('~/somebm_0.1.tar.gz', type='source') 来安装

check() #尽量排除所有的 errors notes。

install.packages("~/abdiv_0.0.1.tar.gz",type="source")
library(abdiv)
?abdiv
?hello
adiv(x)
install(".")

devtools::install_github("你的账号/你的项目名称")
library(abdiv)
foo()
update.packages()
