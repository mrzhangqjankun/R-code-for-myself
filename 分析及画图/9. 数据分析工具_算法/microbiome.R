#2019.11.16

library(devtools) 
install_github("microbiome/microbiome") 

library(BiocManager)
#source("https://bioconductor.org/install")
#useDevel()
BiocManager::install(version='devel')
BiocManager::install("microbiome")
BiocManager::install("phyloseq")
#
library(microbiome)  ;?microbiome
library(knitr)
data(dietswap)
pseq <- dietswap

tab <- alpha(pseq, index = "all")
kable(head(tab))

tab <- dominance(pseq, index = "all")
head(tab)

tab <- rarity(pseq, index = "all")
head(tab)
