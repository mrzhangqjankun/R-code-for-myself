##2019.6.10
#https://mp.weixin.qq.com/s/XSjviDilYGQIMItSTWA_HA
#R包安利 ① easyPubMed—PubMed利器

#easyPubMed 可以查询 NCBI Entrez，以 XML 或 文本 格式获得 PubMed 信息，
#可以提取、整合数据，可以 轻 而 易 举 地下载一大堆记录信息，比如单独得到 作者、单位、题目、关键词、摘要、发表时间……
install.packages("easyPubMed")
library("easyPubMed");?easyPubMed

query <- "Damiano Fantini[AU]"  
query
#用 get_pubmed_ids() 得到一个 list 文件，包含后续操作需要的信息。
entrez_id <- get_pubmed_ids(query)
entrez_id

#用 fetch_pubmed_data() 得到 PubMed 数据。
abstracts_txt <- fetch_pubmed_data(entrez_id, format = "abstract")
print(abstracts_txt[1:16])

#"format" 允许这些选项："asn.1", "xml", "medline", "uilist", "abstract"
#还可以用 fetch_pubmed_data() 结合 XML包，得到所有文章的标题。
#install.packages("XML")
library(XML)
abstracts_xml <- fetch_pubmed_data(entrez_id,format = "xml")
class(abstracts_xml)

#下面会报错
my_titles <- unlist(xpathApply(abstracts_xml, "//ArticleTitle", saveXML))
my_titles <- gsub("(^.{5,10}Title>)|(<\\/.*$)", "", my_titles)
my_titles[nchar(my_titles)>75] <- paste(substr(my_titles[nchar(my_titles)>75], 1, 70), 
                                        "...", sep = "")
print(my_titles)

#换一种方法
titles <- custom_grep(abstracts_xml, "ArticleTitle", "char")  
## format，c("list", "char"):
print(titles) 

#以 TXT 或 XML 格式下载并保存信息 
#通过 batch_pubmed_download() 将数据保存为 txt 或 xml 文件。
## 搜索标题里有 APE1 或 OGG1 这两个基因——在2012-2016年间发表的文章
new_query <- "(APE1[TI] OR OGG1[TI]) AND (2012[PDAT]:2016[PDAT])"

## 设置输出文件格式、文件名前缀
outfile <- batch_pubmed_download(pubmed_query_string = new_query, 
                                 format = "xml", 
                                 batch_size = 150,
                                 dest_file_prefix = "easyPM_example")
outfile

#从单独的 PubMed 记录里提取信息
#custom_grep 函数可以将 XML 转换为字符串，从特定的 PubMed 记录中提取相关信息，返回 list 或 character.
PM_list <- articles_to_list(abstracts_xml)
## 任意选其中的一条
custom_grep(PM_list[13], tag = "DateCompleted")

###后面略过


#https://mp.weixin.qq.com/s/bndecTSABox2dcr7aoheig
#R包安利 ② pubmed.mineR—又一个PubMed利器 
#致力于从 Pubmed Abstarct 文件挖掘文本/数据的包

#内容略过

###2019.8.11
#https://mp.weixin.qq.com/s/xqoJpiIicWgsrycED6muEw
#如何获取本领域科研发表文章的趋势图-pubmed