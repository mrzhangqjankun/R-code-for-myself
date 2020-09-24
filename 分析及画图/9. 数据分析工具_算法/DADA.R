#! /usr/bin/env Rscript
# install.packages("installr")
# library(installr) 
# updateR() 

#rm(list=ls());
gc()

# library("devtools")  
# devtools::install_github("benjjneb/dada2", ref="v1.16")

# if (!requireNamespace("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# BiocManager::install("ShortRead", version = "3.11") 
# BiocManager::install("dada2", version = "3.11")

library("dada2")
#update.packages("dada2")
#packageVersion("dada2")
#?filterAndTrim
setwd("/home/lishuzhen/Duolun_combine/OTU_807/DADA/")
#setwd("E:/桌面/test/")
#setwd("/home/lishuzhen/DADA/")

path <- "temp_dir"
fnFs <- sort(list.files(path, pattern="__forward.fastq", full.names = TRUE))
fnRs <- sort(list.files(path, pattern="__reverse.fastq", full.names = TRUE))
sample.names <- sapply(strsplit(basename(fnFs), "__"), '[', 1)	

# filter and trim
filtFs <- file.path(path, "filtered", paste0(sample.names, "_F_filt.fastq.gz"))
filtRs <- file.path(path, "filtered", paste0(sample.names, "_R_filt.fastq.gz"))
out <- filterAndTrim(fnFs, filtFs, fnRs, filtRs,matchIDs=TRUE,compress=TRUE, multithread=TRUE)
#out <- filterAndTrim(fnFs, filtFs, fnRs, filtRs,truncLen=c(200,200),maxN=0, maxEE=c(2,2), truncQ=2, rm.phix=TRUE,matchIDs=TRUE,compress=TRUE, multithread=TRUE)
#out <- filterAndTrim(fnFs, filtFs, fnRs, filtRs, truncLen=c(200,200),maxN=0, maxEE=c(2,2), truncQ=2, rm.phix=TRUE,matchIDs=TRUE,compress=TRUE, multithread=TRUE)

# out = fastqPairedFilter(fn=c(fnFs,fnRs),fout=c(filtFs,filtRs), rm.phix=TRUE,matchIDs=TRUE,compress=TRUE, multithread=TRUE)
# ?dada
# ?filterAndTrim
###去掉N，S，W1,W3,E1,E3

# Learn the error rates
errF <- learnErrors(filtFs, multithread=TRUE);
errR <- learnErrors(filtRs, multithread=TRUE)

#save.image()

# Dereplication
derepFs <- derepFastq(filtFs);derepRs <- derepFastq(filtRs)
names(derepFs) <- sample.names;names(derepRs) <- sample.names

# sample inference   pool=   样本是否先合并,默认FALSE
dadaFs <- dada(derepFs, err=errF, multithread=TRUE,pool="pseudo");
dadaRs <- dada(derepRs, err=errR, multithread=TRUE,pool="pseudo")
?dada
# Merge paired reads
mergers <- mergePairs(dadaFs, derepFs, dadaRs, derepRs, verbose=TRUE)

# Construct sequence table
seqtab <- makeSequenceTable(mergers)
#seqtab2 <- seqtab[,nchar(colnames(seqtab)) %in% seq(245,260)]
#write.table(nchar(colnames(seqtab)),file="table_pooled.txt",sep="\t")

seqtab2 = seqtab
# Remove chimeras with denovo methods
seqtab.nochim <- removeBimeraDenovo(seqtab2, method="consensus", multithread=TRUE, verbose=TRUE)
table(nchar(getSequences(seqtab.nochim)))
dim(seqtab.nochim)
sum(seqtab.nochim)/sum(seqtab)

# Track reads through the pipeline
getN <- function(x) sum(getUniques(x))
track <- cbind(out, sapply(dadaFs, getN), sapply(dadaRs, getN), sapply(mergers, getN),rowSums(seqtab2),rowSums(seqtab.nochim))	
colnames(track) <- c("Input", "Filtered", "DenoisedForward", "DenoisedReverse", "Merged","LengthRange", "Nonchimera")
rownames(track) <- sample.names
head(track)

# Output
write.table(t(seqtab.nochim), "dada2_ASV_table_unpooled.txt", sep="\t", quote=F, row.names = T)
write.table(track , "dada2_processing_report_unpooled.txt", sep="\t", quote=F, row.names = T)

