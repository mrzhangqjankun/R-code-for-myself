###2017.17.7
##DADA2
#http://www.bioconductor.org/packages/release/bioc/html/dada2.html
#http://www.bioconductor.org/packages/release/bioc/vignettes/dada2/inst/doc/dada2-intro.html
#https://www.nature.com/articles/nmeth.3869#methods
#http://bioconductor.org/install/#install-bioconductor-packages

rm(list=ls(all=TRUE))
##1
source("https://bioconductor.org/biocLite.R") 
biocLite()
biocLite(c("GenomicFeatures", "AnnotationDbi"))
biocLite("dada2")
##2   ###这个方法可以
library("devtools")  
devtools::install_github("benjjneb/dada2")
library(devtools)
install_github("benjjneb/dada2")
reshape2  ##先安装这个包再装dada2


##3
setwd('E:/桌面')
install.packages("dada2-1.6.zip",  ##下载了本地安装也不行
                 repos = NULL,
                 type = "source",
                 dependencies = c("Depends", "Suggests","Imports"))


biocLite("GenomeInfoDbData")

#3开始  步骤见下面网址
#http://www.bioconductor.org/packages/release/bioc/vignettes/dada2/inst/doc/dada2-intro.html
#更完整的流程
#http://benjjneb.github.io/dada2/tutorial.html
biocLite("GenomeInfoDbData")

library(dada2)
packageVersion("dada2")
?filterAndTrim
#1.	Filter and trim: filterAndTrim()
fnF1 <- system.file("extdata", "sam1F.fastq.gz", package="dada2") #读取
fnR1 <- system.file("extdata", "sam1R.fastq.gz", package="dada2")
filtF1 <- tempfile(fileext=".fastq.gz")  #质量信息
filtR1 <- tempfile(fileext=".fastq.gz")  
plotQualityProfile(fnF1)  #画图
plotQualityProfile(fnR1) 
filterAndTrim(fwd=fnF1, filt=filtF1, rev=fnR1, filt.rev=filtR1,  ##读取序列及其质量信息
              trimLeft=10, truncLen=c(240, 200),   ##trim每条序列前10个碱基。前后饭别截取240和200个碱基长度
              maxN=0, maxEE=2,                ##最多模糊碱基为0，read最多允许两个错误
              compress=TRUE, verbose=TRUE)  #输出文件格式为gzipped fastq files 
#2.	Dereplicate: derepFastq()  #去重复  $quals中包含了每条序列的质量信息
derepF1 <- derepFastq(filtF1, verbose=TRUE)
derepR1 <- derepFastq(filtR1, verbose=TRUE)

#3.	Learn error rates: learnErrors()  检测PCR扩增与测序产生的错误，得到一个矩阵
errF <- learnErrors(derepF1, multithread=FALSE)
errR <- learnErrors(derepR1, multithread=FALSE)

#4.	Infer sample composition: dada()  这是data2的核心方法。利用文章中的算法进行sample inference 
dadaF1 <- dada(derepF1, err=errF, multithread=FALSE)
dadaR1 <- dada(derepR1, err=errR, multithread=FALSE)
print(dadaF1)
#5.	Merge paired reads: mergePairs()   得到一个数据框
merger1 <- mergePairs(dadaF1, derepF1, dadaR1, derepR1, verbose=TRUE)

#6.	Remove chimeras: removeBimeraDenovo() 去除嵌合体
merger1.nochim <- removeBimeraDenovo(merger1, multithread=FALSE, verbose=TRUE)


#7.	Make sequence table: makeSequenceTable()
seqtab <- makeSequenceTable(list(merger1, merger2))
seqtab <- makeSequenceTable(list(merger1))
seqtab.nochim <- removeBimeraDenovo(seqtab, verbose=TRUE)   ##OTU table
seq=t(seqtab.nochim)    ##转置一下
seq
dim(seq)


seqtab = makeSequenceTable(merger1)
seqtab.nochim <- removeBimeraDenovo(seqtab, verbose=TRUE) 


##12.28张照婧数据
##去掉barcode和primer之后开始
#setwd('E:/桌面/zhaojing_DADA2/')
rm(list=ls(all=TRUE))
#1.	Filter and trim: filterAndTrim()
#library(ShortRead)  
#fl=readFastq("Galaxy5-[TrimPrimer_merge_R1.fastq].fastq")
#fnF1=fl
#fnR1=readFastq("Galaxy7-[TrimPrimer_merge_R2.fastq].fastq")
#plotQualityProfile(fnF1)  #画图   报错循环无效
#plotQualityProfile(fnR1) 

library(dada2); packageVersion("dada2")
path <- "E:/桌面/zhaojing_DADA2/" # CHANGE ME to the directory containing the fastq files after unzipping.
list.files(path)
# Forward and reverse fastq filenames have format: SAMPLENAME_R1_001.fastq and SAMPLENAME_R2_001.fastq
fnFs <- sort(list.files(path, pattern="_R1.fastq", full.names = TRUE));fnFs
fnRs <- sort(list.files(path, pattern="_R2.fastq", full.names = TRUE));fnRs
plotQualityProfile(fnFs[1:2])  ##画图
plotQualityProfile(fnRs[1:2])
# Extract sample names, assuming filenames have format: SAMPLENAME_XXX.fastq
sample.names <- sapply(strsplit(basename(fnFs), "_"), `[`, 1);sample.names
filt_path <- file.path(path, "filtered") # Place filtered files in filtered/ subdirectory
filtFs <- file.path(filt_path, paste0(sample.names, "_F_filt.fastq.gz"))
filtRs <- file.path(filt_path, paste0(sample.names, "_R_filt.fastq.gz"))
out <- filterAndTrim(fnFs, filtFs, fnRs, filtRs,  ##读取序列及其质量信息
                     maxN=0, maxEE=c(2,2), truncQ=2, rm.phix=TRUE,minQ=20,##最多模糊碱基为0，read最多允许两个错误,Q>20
                     compress=TRUE, multithread=FALSE,verbose=TRUE) # On Windows set multithread=FALSE
?filterAndTrim
#输出文件格式为gzipped fastq files 

out <- filterAndTrim(fnFs, filtFs, fnRs, filtRs,compress=TRUE, multithread=FALSE,verbose=TRUE) 



#2.	Dereplicate: derepFastq()  #去重复  $quals中包含了每条序列的质量信息
derepF1 <- derepFastq(filtF1, verbose=TRUE)
derepR1 <- derepFastq(filtR1, verbose=TRUE)

#3.	Learn error rates: learnErrors()  检测PCR扩增与测序产生的错误，得到一个矩阵
errF <- learnErrors(derepF1, multithread=FALSE)
errR <- learnErrors(derepR1, multithread=FALSE)

#4.	Infer sample composition: dada()  这是data2的核心方法。利用文章中的算法进行sample inference 
dadaF1 <- dada(derepF1, err=errF, multithread=FALSE)
dadaR1 <- dada(derepR1, err=errR, multithread=FALSE)
print(dadaF1)
#5.	Merge paired reads: mergePairs()   得到一个数据框
merger1 <- mergePairs(dadaF1, derepF1, dadaR1, derepR1, verbose=TRUE)

#6.	Remove chimeras: removeBimeraDenovo() 去除嵌合体
merger1.nochim <- removeBimeraDenovo(merger1, multithread=FALSE, verbose=TRUE)


#7.	Make sequence table: makeSequenceTable()
seqtab <- makeSequenceTable(list(merger1, merger2))
seqtab <- makeSequenceTable(list(merger1))
seqtab.nochim <- removeBimeraDenovo(seqtab, verbose=TRUE)   ##OTU table
seq=t(seqtab.nochim)    ##转置一下
seq
dim(seq)