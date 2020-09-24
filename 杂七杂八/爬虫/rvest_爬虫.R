##2018.11.24
#我把我用R写的第一个爬虫就献给了国家 
https://mp.weixin.qq.com/s?__biz=MzA3MTM3NTA5Ng==&mid=2651056093&idx=2&sn=41e24e7dcc8808a65f48f83e1c6bd5cd&chksm=84d9c64ab3ae4f5c8dfaf6e990ccf628765cf4aeb482cdf56925ce4c0c5e218fc9784751812c&scene=21#wechat_redirect

# （1）rvest 爬取文章内容
# （2）jiebaR 用于分词，统计词频
# （3）wordcloud2 用于对文本进行可视化

library(rvest)
url<-'https://mp.weixin.qq.com/s?__biz=MzUzMjA4Njc1MA==&mid=2247487035&idx=1&sn=c195b3dddb216cb10ab95bdb024692df&chksm=fab9e48acdce6d9cfef69a0bf4c3f8ac52b4dfaefdc3b609cebb8e90352cf796d07a18349049&scene=0#rd'
web<-read_html(url,encoding="utf-8") #读取数据，规定编码
position<-web %>% html_nodes("div.rich_media_area_primary") %>% html_text()
#position<-web %>% html_nodes("div.pages_content") %>% html_text()
##查看元素。div.固定，后面是class里面的内容



# %>%  为管道函数，将左边的值赋给右边函数作为第一个参数的值。
# web  就是存储网页信息的变量。
# html_nodes() 函数获取网页里的相应节点。
# html_text() 函数获取标签内的文本信息。

getwd()
setwd("E:/桌面/R script 2017/")
#使用jiebaR进行分词，统计词频
library(jiebaR)
engine_s<-worker(stop_word = "stopwords.txt")#初始化分词引擎并加载停用词。
seg<-segment(position,engine_s)#分词
f<-freq(seg) #统计词频
f<-f[order(f[2],decreasing=TRUE),] #根据词频降序排列
f
#使用wordcloud2包进行可视化。
library(wordcloud2)#加载包
f2<-f[1:150,]     #总共有2000多个词，为了显示效果，我只提取前150个字
wordcloud2(f, size = 0.8 ,shape='star')    #形状设置为一颗五角星
