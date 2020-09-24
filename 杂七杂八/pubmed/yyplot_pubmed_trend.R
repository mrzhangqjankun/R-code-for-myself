##pubmed_trend  2017.11.21
##https://mp.weixin.qq.com/s?__biz=MzI5NjUyNzkxMg==&mid=2247485134&idx=1&sn=a0fde8d20727c304d564d89dd98b7f2a&chksm=ec43b789db343e9faca5d6055f328058d6651c8253b84eeffd2422a81fb7016df1e73542fcca&scene=0#rd
##pubmed年度趋势，你的开题报告需要它！ 

##install yyplot
##http://www.360doc.com/content/17/0629/11/43722561_667427243.shtml

rm(list=ls(all=TRUE))

install.packages('ggimage')
install.packages('devtools')
install.packages('qrcode')
devtools::install_github("GuangchuangYu/yyplot")

?yyplot
require(ggplot2)
require(ggimage)
require(yyplot)

term <- c('"H7N9"', '"H5N1"', '"RSV"')
pm <- pubmed_trend(term, year=2001:2014)
plot(pm)
yu=pubmed_trend("Yu Guangchuang[Full Author Name]", 2010:2016)
plot(yu)
ye=pubmed_trend("Deng Ye[Full Author Name]", 2010:2016)
plot(ye)
?pubmed_trend
zhou=pubmed_trend("Zhou Jizhong[Full Author Name]", 2010:2016)
plot(zhou)
phy=pubmed_trend("microecology", 2010:2016)
plot(phy)
li=pubmed_trend("Li Shuzhen[Full Author Name]", 2010:2016)
plot(li)
fe=pubmed_trend("Feng Kai", 2010:2016)
plot(fe)
liu=pubmed_trend("Yangying Liu[Full Author Name]", 2010:2016)
plot(liu)


##mock community
mock=pubmed_trend("mock community[Text Word]",2000:2017)
mock
plot(mock)

##2018.7.9
##scholar包检索期刊影响因子（2018年最新版本） 
https://mp.weixin.qq.com/s?__biz=MzI5NjUyNzkxMg==&mid=2247486131&idx=1&sn=f28dca056737debb8c620eb361a40eb3&chksm=ec43bbf4db3432e2734c3b4f2f37a7e16cb8c0faad3a787b0df695103c92711b5550af72ecfa&scene=0&pass_ticket=LqX56VIZzw%2Fgf0sfgiCGXJQG0ck174U7sriR2VmYpL3ndoi5%2Bvded0xYHRAwiJ1l#rd

#install.packages("scholar")
library(scholar)
jn = c("bioinformatics", "methods in ecology and evolution",
             "molecular biosystems", "molecular biology and evolution")
get_impactfactor(jn)

#检索到名字含有bioinformatics的期刊，然后就可以检索影响因子了，最高分竟然是国内的GPB！！！
 jn <- agrep("bioinformatics", scholar:::impactfactor$Journal, ignore.case=T, value=T, max.distance=0.05)
 jn
 get_impactfactor(jn)
 
 #再来一点实战，抓取我的profile，选取我要的信息，再追加期刊的影响因子，过滤出我做为第一作者的文章：
 #install.packages("dplyr") #加载dplyr包
 library(dplyr)
  require(scholar)

  x = get_publications("DO5oG40AAAAJ")
  ?get_publications
  y = x %>% select(year, author, journal, title) %>% 
    mutate(impactFactor = get_impactfactor(journal)$ImpactFactor) %>% 
    filter(grepl("^G Yu", author)) %>% group_by(year)
  y
  
  y %>% summarize(total_IF = sum(impactFactor, na.rm=T), 
                   mean_IF = mean(impactFactor, na.rm=T))
  
  ###如何查找某个人的谷歌学术ID？？？
  
  
#2019.8.11
#https://mp.weixin.qq.com/s/xqoJpiIicWgsrycED6muEw
#如何获取本领域科研发表文章的趋势图-pubmed
#解决install github方式下载包出错的方法