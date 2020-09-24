##2018.12.24
#如何用R分析CNKI文献关键词词频？
#https://www.jianshu.com/p/4478a03bad62?from=timeline&isappinstalled=0

##关键词之间用两个分号来分割
rm(list=ls())

#设置时区
Sys.setenv(TZ="Asia/Beijing")

library(tidyverse)
library(readxl)
library(tidytext)

getwd()
setwd("E:/桌面/R script 2017/分析CNKI词频/")

df <- read_excel("双加氧酶_微生物降解_58篇.xlsx")

##挑出题目和关键词的两列
df1 <- df %>%
  select(starts_with('Keyword'), starts_with('Title'))

#原先的Excel里面列名中英文混合，这里我们修改为英文名称，便于后续使用。
colnames(df1) <- c('keyword', 'title')

#然后我们就需要对关键词这一列进行处理了。我们拆分一下，把关键词拆分，每一行保留一个关键词。
df1 %>%
  unnest_tokens(word, keyword, token = stringr::str_split, pattern = ";;")

#下面我们需要设置停用词。
#先看一下默认的停用词
data(stop_words)
stop_words

#原来是个数据框，那我们仿照这个样子，也设置自己的停用词表。
#这一段里面，我们先建立两个向量，分别是停用词和词典。因为我们不涉及词典的属性设置，所以统一设置为UNKOWN
my_stop_words_list = c('双加氧酶')
my_lexicon_list = c('UNKNOWN')
my_stop_words = data.frame(my_stop_words_list, my_lexicon_list, stringsAsFactors=FALSE)
colnames(my_stop_words) <- c('word', 'lexicon')
my_stop_words

#下面我们把刚才的内容串起来，先拆关键词，然后停用词过滤，最后统计停用词词频，并且排序：
df1 %>%
  unnest_tokens(word, keyword, token = stringr::str_split, pattern = ";;") %>%
  anti_join(my_stop_words) %>%
  count(word, sort = TRUE)

##可视化。
df1 %>%
  unnest_tokens(word, keyword, token = stringr::str_split, pattern = ";;") %>%
  anti_join(my_stop_words) %>%
  count(word, sort = TRUE) %>%
  filter(n > 2) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()


##停用词就是结果中出现的这个词都去掉。其实我们可以不加停用词，结果中才会有“双加氧酶”。
df1 %>%
  unnest_tokens(word, keyword, token = stringr::str_split, pattern = ";;") %>%
  count(word, sort = TRUE)

##但是还有一个问题，unnest_tokens这一步只会显示第一个关键词。后面的都被丢弃了。
?unnest_tokens # splitting the table into one-token-per-row. 一行只保留一个结果

keywords = df1[,1]

key = df1 %>%
  unnest_tokens(word, keyword) %>%  ##word是给输出的列命名；keyword是输入的那一列
  count(word, sort = TRUE)
head(key,20)
##得到的结果不对。很多词被拆开了。
#这个问题留待以后解决吧

