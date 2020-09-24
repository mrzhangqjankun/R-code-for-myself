rm(list=ls());gc()
getwd()
dat = read.table(file="C:/Users/19021/Desktop/李燕data/venn和相关性.txt",header = T,row.names = 1,sep="\t")
cor_pearson <- cor(dat, method = 'pearson')
cor_pearson

#install.packages("psych")
library(psych)
cor_pearson <- corr.test(dat, method = 'pearson')
cor_pearson

#全是0的去掉
dat2 = dat[!(rowSums(dat)==0),]
cor_pearson <- corr.test(dat2, method = 'pearson')
cor_pearson


#############################备用
res = cor_pearson
getwd()
setwd("C:/Users/19021/Desktop/data/")

cor_pearson$p[cor_pearson$p >= 0.05] <- 1
phylum_corr_final <- cor_pearson$r * cor_pearson$p
write.csv(phylum_corr_final, 'phylum_corr_final.csv', quote = FALSE)
cor_pearson <- corr.test((dat2), method = 'pearson')
cor_pearson
cor_pearson = res
cor_pearson$p[cor_pearson$p >= 0.05] <- 1
cor_pearson
View(cor_pearson)
write.csv(cor_pearson$p, 'p.csv', quote = FALSE)
write.csv(cor_pearson$r, 'p.csv', quote = FALSE)
write.csv(cor_pearson$p, 'p.csv', quote = FALSE)
write.csv(cor_pearson$r, 'r.csv', quote = FALSE)
write.csv(cor_pearson$p, 'p.csv', quote = FALSE)
cor_pearson <- corr.test((dat2), method = 'pearson')
cor_pearson
cor_pearson$p
?corr.test
