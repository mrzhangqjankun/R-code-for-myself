##2018.11.6


###biostar问题
https://mp.weixin.qq.com/s?__biz=MzAxMDkxODM1Ng==&mid=2247487922&idx=1&sn=510e5b0792432a73269857aea556c813&chksm=9b485109ac3fd81fec9f3e436ad84ece512b093cf57036ef45378571b3751af9f79838d657f5&scene=0&xtrack=1#rd

##1.加载相关的R包
library(rvest)

##2.爬取biostar所有问题、点赞数、阅读数、以及问题链接

biostars_inf<-data.frame()
for (i in 1:2163) {
  #2163是总的页面个数
  print(i)
  #循环构建url
  web<-read_html(paste0("https://www.biostars.org/?page=",i,"&sort=update&limit=all%20time&q="))
  #爬取问题
  question<-web %>% html_nodes(xpath = "//*[@id=\"post-list\"]/div/div[3]/div/a") %>% html_text()
  #爬取点赞数
  vote<-web %>% html_nodes(xpath = "//*[@id=\"post-list\"]/div/div[1]/div[1]/div[1]") %>% html_text()
  vote<-gsub("k", "000", vote)
  vote<-as.numeric(as.character(vote))
  #爬取阅读数
  views<-web %>% html_nodes(xpath = "//*[@id=\"post-list\"]/div/div[1]/div[3]/div[1]") %>% html_text()
  views<-gsub("k", "000", views)
  views<-as.numeric(as.character(views))
  #构建问题链接qustion_url
  short_question_url<-as.character(web %>% html_nodes(xpath = "//*[@id=\"post-list\"]/div/div[3]/div[1]/a") %>% html_attrs())
  question_url<-paste0("https://www.biostars.org",short_question_url)
  biostars<-data.frame(question,vote,views,question_url)
  biostars_inf<-rbind(biostars_inf,biostars)
}

#3.根据阅读数和点赞数对问题排序。
topviews<-biostars_inf[order(as.numeric(biostars_inf$views),decreasing=T),]
topvote<-biostars_inf[order(as.numeric(biostars_inf$vote),decreasing=T),]


##peerJ期刊探索 
https://mp.weixin.qq.com/s?__biz=MzAxMDkxODM1Ng==&mid=2247486001&idx=1&sn=44671cf07caaa2aef580603057f081c0&chksm=9b484a8aac3fc39c7f23d4b5a6a872fe33e4461dc314c13ce13e3514e9301ad5f086de804a0a&scene=21#wechat_redirect

##R：rvest包总结
https://blog.csdn.net/weixu22/article/details/79237512