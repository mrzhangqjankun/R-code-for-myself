
rm(list=ls(all=TRUE))
library(wordcloud2)

setwd('E:/桌面/R script 2017/')
data=read.table("word.txt",sep="\t")
head(data)

data("demoFreqC")
head(demoFreqC)

letterCloud(data,"Listen", wordSize = 0.1,color="blue",backgroundColor = "grey")
#letterCloud(data, word ="L", wordSize = 0.1,color = 'random-light', backgroundColor = "grey",size=0.1)
wordcloud2(data, color = "random-light", backgroundColor = "grey",size=0.5)
wordcloud2(data, size = 0.2, minRotation = -pi/2, maxRotation = -pi/2) 

?wordcloud2
#javasript回调函数: 用js撰写任意的颜色生成函数，如
js_color_fun = "function (word, weight) {

return (weight > 80) ? '#f02222' : '#c09292';

}"

wordcloud2(data, color = htmlwidgets::JS(js_color_fun), backgroundColor = 'black',size=0.2)
#

#
js_color_fun = "function (word, weight) {

return (weight > 2000) ? '#f02222' : '#c09292';

}"

wordcloud2(data, color = htmlwidgets::JS(js_color_fun), backgroundColor = 'black')
#


#如何用R语言做词云图，以某部网络小说为例 
https://mp.weixin.qq.com/s?__biz=MzA3MTM3NTA5Ng==&mid=2651059877&idx=1&sn=026b7b69ac5442eb2c34322c6f1d78d3&chksm=84d9d732b3ae5e241e76bd0684fd8d8207e0f86b75d072cd65395427a3a2f08c556bf941aa04&scene=0#rd

#分词
#install.packages("jiebaR")
#install.packages("jiebaRD")
library(jiebaRD) 
library(jiebaR) 

#jiebaR包里有一个叫segment的函数，它可以用来分词，主要的输入格式如下：
#segment(code,jieba)
#code是文件的内容，jieba是用来分词的工具，我们首先设置下分词的工具，输入：
engine = worker() 

#########################################
#读取PDF
# install.packages("pdftools")
# install.packages("pdftables")
library(pdftools)
library(pdftables)
?pdftools
text <- pdf_text("E:/桌面/西北典型旱地生态系统细菌和真菌Beta多样性的模式和驱动因素/betapart/Betapart-2012-Methods_in_Ecology_and_Evolution.pdf")
#这一步特别慢,大概10分钟
#另一个例子
pdf_file <- file.path(R.home("doc"), "NEWS.pdf")
info <- pdf_info(pdf_file)
text <- pdf_text(pdf_file)
fonts <- pdf_fonts(pdf_file)
files <- pdf_attachments(pdf_file)
########################################

#然后把文件也写进去：
text_segment = segment(text,engine);head(text_segment)
getwd()
#这时它就会反馈给你一个新的，已经分好的txt文件，直接将那个文件导入到R中即可。输入：
# word <- scan(file = text_segment,sep='\n',what='',encoding="UTF-8") 
# ?scan

word <- qseg[text_segment] 
freq = freq(text_segment)
freq = freq[order(freq[,2],decreasing = T),];head(freq,30)

#应该先去掉没有意义的一些词
delete_word = c("of","the","and","The","2011","for","two","is","A","b",
                "c","2007","in","can","to","a","s","n","be","as","2",
                "di","or","from","2012","S","are","x","between","ff")
l = length(delete_word)
freq2 = freq
for (i in 1:l){
  delete = which(freq2[,1]==delete_word[i])
  freq2 = freq2[-delete,]
} 
head(freq2,30)
freq2 <- freq2[which(freq2[,2]>15),] ;freq2
wordcloud2(freq2,color = "random-light", backgroundColor = "grey",shape="star")
letterCloud(freq2,"b", wordSize = 0.1,color="blue",backgroundColor = "grey")
