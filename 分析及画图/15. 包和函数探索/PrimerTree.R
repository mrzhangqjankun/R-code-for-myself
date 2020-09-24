#2018.12.17
#https://github.com/MVesuviusC/primerTree
#primerTree
#Automatically query a primer pair and generate a tree of the products.
#need Clustal Omega

#install.packages('primerTree')
#devtools::install_github("jimhester/primerTree")

rm(list=ls())

library(primerTree)
##特别慢。需要用多核并行运算如下。
mammals_16S = search_primer_pair(name='Mammals 16S',
                                 'CGGTTGGGGTGACCTCGGA', 
                                 'GCTGTTATCCCTAGGGTAACT',  
              clustal_options=c(exec='E:/Clustal_Omega/clustal-omega-1.2.2-win64/clustalo.exe'))

plot(mammals_16S)

#curl  这个包卸载了重新装一下就好了。
#之前一直会报错
# Error in curl::handle_setform(handle, .list = req$fields) : 
#   Unsupported value type for form field 'PRIMER_LEFT_INPUT'.
# In addition: Warning messages:
# 1: In GET_retry(url) : request failed, retry attempt 1
# 2: In GET_retry(url) : request failed, retry attempt 2
# 3: In GET_retry(url) : request failed, retry attempt 3
# 4: In GET_retry(url) : request failed, retry attempt 4
# 5: In GET_retry(url) : request failed, retry attempt 5



##多核并行运算
#Using the parallel features with the multicore package using the doMC backend, with 8 threads.
#install.packages("doMC", repos="http://R-Forge.R-project.org")
library(doMC)
registerDoMC(8)
library(primerTree)
mammals_16S = search_primer_pair(name='Mammals 16S',
                                 'CGGTTGGGGTGACCTCGGA', 'GCTGTTATCCCTAGGGTAACT',
              clustal_options=c(exec='E:/Clustal_Omega/clustal-omega-1.2.2-win64/clustalo.exe'),
                                 .parallel=T,    #并行需要加这个参数
                                  num_aligns=20) #序列数。默认为500
plot(mammals_16S)
summary(mammals_16S)
str(mammals_16S)

#our 16S primer   跑不出结果。一直卡在blast上。
##'GTGYCAGCMGCCGCGGTAA', 
##'GGACTACNVGGGTWTCTAAT',                                                      
bacteria_16S = search_primer_pair(name='bacteria',
                                 'GTGYCAGCMGCCGCGGTAA', 'GGACTACNVGGGTWTCTAAT',
                                 clustal_options=c(exec='E:/Clustal_Omega/clustal-omega-1.2.2-win64/clustalo.exe'),
                                 .parallel=T,    #并行需要加这个参数
                                 num_aligns=5) #序列数。默认为500
plot(bacteria_16S)