##2019.3.7
##多核运算
##PrimerTree.R ; phyloseq.R

##############1. PrimerTree.R
#Using the parallel features with the multicore package using the doMC backend, with 8 threads.
#install.packages("doMC", repos="http://R-Forge.R-project.org")
library(doMC) ; ?doMC
registerDoMC(3)
library(primerTree)
mammals_16S = search_primer_pair(name='Mammals 16S',
                                 'CGGTTGGGGTGACCTCGGA', 'GCTGTTATCCCTAGGGTAACT',
                                 clustal_options=c(exec='E:/Clustal_Omega/clustal-omega-1.2.2-win64/clustalo.exe'),
                                 .parallel=T,    #并行需要加这个参数
                                 num_aligns=20) #序列数。默认为500
plot(mammals_16S)
summary(mammals_16S)
str(mammals_16S)

##############2.phyloseq.R
library(doParallel) ; ?doParallel
data(esophagus)
# For SNOW-like functionality (works on Windows):
#registerDoParallel(cl, cores=NULL, ...)
cl <- makeCluster(5)  ##设置集群数。默认为3
registerDoParallel(cl)
UniFrac(esophagus, TRUE)
# Force to sequential backed:
registerDoSEQ()
# For multicore-like functionality (will probably not work on windows),
# register the backend like this:
registerDoParallel(cores=5)
library(phyloseq)
UniFrac(esophagus, TRUE)


library(snow);??snow
#library(multicore);?multicore
