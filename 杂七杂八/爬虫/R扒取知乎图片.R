###lii2017.9.1
##同时用R语言和Python爬取知乎美图
##https://zhuanlan.zhihu.com/p/28400461

install.packages("rvest")
install.packages("downloader")

library(rvest)
library(downloader)
url<-"https://www.zhihu.com/question/35931586/answer/206258333"
#url<-"https://www.zhihu.com/question/35931586"
link<- read_html(url)%>% html_nodes("div.RichContent-inner>span")%>%html_nodes("img")%>%html_attr("data-original")%>%na.omit   
link<-link[seq(1,length(link),by=2)]                 #剔除无效网址
Name<-sub("https://pic\\d.zhimg.com/v2-","",link)    #提取图片名称
dir.create("E:/桌面/zhihu picture")             #建立存储文件夹
setwd("E:/桌面/zhihu picture")                  #锁定临时目录
for(i in 1:length(link)){
  download(link[i],Name[i], mode = "wb")
}  #下载过程：


rm(list=ls(all=TRUE))


