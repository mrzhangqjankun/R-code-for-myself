##2018.10.30
rm(list=ls())

library("Biostrings") 
library("tidyverse") 
#fastaFile <- readDNAStringSet("my.fasta") 
# 或 
dna <- DNAStringSet(c("AGCCTCCGCTTATTGATATGCTTAART") )
# 反向 
dna %>% reverse 
# 互补 
dna %>% complement 
# 反向互补 
dna %>% reverse %>% complement 

##或直接用下面的函数即可
reverse(dna)
complement(dna)
reverseComplement(dna)
