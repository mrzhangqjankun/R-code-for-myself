##2018.10.17

##根据基因名批量下载蛋白质序列 

##http://www.dxy.cn/bbs/thread/38832197#38832197
##http://www.dxy.cn/bbs/topic/38817160

rm(list=ls())

# source("http://bioconductor.org/biocLite.R")
# biocLite("biomaRt")

# 加载biomaRt包
library(biomaRt)
?biomaRt
# 定义数据库
listMarts()  ##列出数据库
mart <- useMart("ensembl", dataset="hsapiens_gene_ensembl") ##人类基因的ensembl数据集
# 根据基因名，批量提取蛋白质序列
seq = getSequence(id = c("BRCA1","TP53"), 
                  type = "hgnc_symbol", 
                  seqType = "peptide", 
                  mart = mart)
# 显示基因名和蛋白质序列
show(seq)
# 默认目录下输出fasta格式seq
getwd()
exportFASTA(seq, file="seq.fa")


###https://www.jianshu.com/p/0dbd5528ce3d
#biomart
install.packages('DT')
library(DT)
ensembl=useMart("ENSEMBL_MART_ENSEMBL") 
all_datasets <- listDatasets(ensembl) #看看选择的数据库里面有多少数据表，这个跟物种相关 
datatable(all_datasets, 
          options = list( 
            searching = FALSE, pageLength = 5, lengthMenu = c(5, 10, 15, 20) 
            )
          ) 
###好像没有微生物数据
