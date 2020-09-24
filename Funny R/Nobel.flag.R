##2019.5.20
#https://github.com/johannesbjork/LaCroixColoR
#https://mp.weixin.qq.com/s/jPnXAnnna1VFV9Oht3SToA
#看代码学画图 - Nobel化学奖的获得者多半投胎在那些国家？

devtools::install_github("johannesbjork/LaCroixColoR")
#LaCroixColoR用来配色，library(ggthemes)用来设主题，用了华尔街日报的主题theme_wsj，ggimage画小国旗。tidyverse处理数据。
library(LaCroixColoR)
library(tidyverse)
library(ggthemes)
library(ggimage)


#nobel_winners <- read_csv("data/data_2019-05-14.csv",col_types = "dccccdccDccccccDcc")
##自己改进一下，从网页中爬下来表格
#install.packages("XML")
library(XML) #载入XML包
u<-"https://github.com/abichat/tidytuesday/blob/master/data/data_2019-05-14.csv"; #写入表格所在的网址
tbls<-readHTMLTable(u) #分析网页中的表格，如果网页包含多个表格，需要确定读取哪个表。
str(tbls)
pop<-readHTMLTable(u,which=1) #读取网页中的第一张表
head(pop)
write.csv(pop,file="d:/pop.csv") #存储pop为CSV文档至D盘中

#出现Error: XML Content does not seem to be XML
##XML包不支持https网页的抓取。需要用Rcurl
library (RCurl)
curlVersion()$features
curlVersion()$protocol
## These should show ssl and https.
temp <- getURL("https://github.com/abichat/tidytuesday/blob/master/data/data_2019-05-14.csv",ssl.verifyPeer=FALSE)
DFX <- xmlTreeParse(temp,useInternal = TRUE)
#这个也会报错。原因是git版本太老了已经不支持了。

#再换一种方法,还是报错无法连接
devtools::install_github("Displayr/flipAPI")
library(flipAPI)
data = DownloadXLSX("https://github.com/abichat/tidytuesday/blob/master/data/data_2019-05-14.csv",skip=1,want.col.names = T,want.row.names = T,want.data.frame = T,want.factors = F)

#再换一种方法。RSelenium/Rwebdriver/rdom包抓取表格数据
#https://blog.csdn.net/joyliness/article/details/78665139
install.packages("RSelenium")
# 直接从CRAN下载RSelenium包
library(devtools)
install_github(repo = "Rwebdriver", username = "crubba")
# 从github下载Rwebdriver包
install_github("cpsievert/rdom")
# 从github下载rdom包

#nobel.url <- "https://github.com/abichat/tidytuesday/blob/master/data/data_2019-05-14.csv"

library(dplyr)
library(xml2)
# 管道操作符/网页转码函数
nobel.url <- "https://github.com/abichat/tidytuesday/blob/master/data/data_2019-05-14.csv"%>% url_escape(reserved = "][!$&'()*+,;=:/?@#")

library(RSelenium)
remDr <- remoteDriver(browserName = "phantomjs")
remDr$open()
# 用无头浏览器模拟访问，创建一个remoteDriver对象并打开
remDr$navigate(nobel.url)
# 访问指定页面

library(XML)
table <- remDr$getPageSource()[[1]] %>% htmlParse(encoding = "UTF-8") %>% readHTMLTable(header = FALSE, which = 1)
# 法一：用XML包的函数获取内容-解析结构-抓取表格（同rvest）
library(rvest)
table <- remDr$getPageSource()[[1]] %>% read_html(encoding = "UTF-8") %>% html_table(header = FALSE) %>% .[[1]]
# 法二：用rvest包的函数获取内容-解析结构-抓取表格（同XML）

library(rdom)
library(XML)
table <- rdom(nobel.url) %>% readHTMLTable(header = FALSE)
###还是都报错Undefined error in httr call. httr output: length(url) == 1 is not TRUE

##直接从github上下载也下不下来。好像设置了不能下载。