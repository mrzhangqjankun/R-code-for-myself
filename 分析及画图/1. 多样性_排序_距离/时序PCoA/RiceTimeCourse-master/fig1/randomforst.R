##2018.6.26
##水稻微生物组时间序列分析4-随机森林回归 
https://mp.weixin.qq.com/s?__biz=MzUzMjA4Njc1MA==&mid=2247485919&idx=1&sn=ca3943e2291b7a57e52544840baf33e2&chksm=fab9e36ecdce6a78e06cf1fae213b709c3ccd3889c1be770b02f4256edde931a4174f6005c90&mpshare=1&scene=1&srcid=0626vAlAZrEH7llhHx2U4wcv&pass_ticket=dsh7qIb5mj6erisMOzyYS4xK%2BhQmceJy6mGXI9YplY9ghTMfNUqgZDDxs5GNrAOU#rd


##下载数据
 https://github.com/YongxinLiu/RiceTimeCourse 

 # 读取实验设计、和物种分类文件
 tc_map =read.table("../data/design.txt",header = T, row.names = 1)
 # 物种分类文件，由qiime summarize_taxa.py生成，详见扩增子分析流程系列
 # 本研究以纲水平进行训练，其实各层面都可以，具体那个层面最优，需要逐个测试寻找。推荐纲、科，不建议用OTU，差异过大
 otu_table =read.table("../data/otu_table_tax_L3.txt",header = T, row.names = 1)
 # 筛选品种作为训练集
 sub_map = tc_map[tc_map$genotype %in% c("A50"),] # ,"IR24"
 # 筛选OTU
 idx = rownames(sub_map) %in% colnames(otu_table)
 sub_map = sub_map[idx,]
 sub_otu = otu_table[, rownames(sub_map)]  