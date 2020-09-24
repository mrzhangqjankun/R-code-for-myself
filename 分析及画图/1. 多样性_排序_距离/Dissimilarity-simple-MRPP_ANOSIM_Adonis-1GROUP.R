##li 2017.7.12 learn

setwd('E:/桌面')
rm(list=ls(all=TRUE))
setwd('D:/?ļ?????/galaxy pipeline/galaxy/?Լ?????????/anosim/')
#setwd('D:/Kai/Desktop/2')
library("vegan")

#rm(list=ls(all=TRUE))
#fg.dat = read.table("resample_OTU_table.txt", sep="\t", row.names=1, header=T) 
#list.dat = read.table("sample_list.txt", sep="\t", row.names=1)

fg.dat = read.table("Galaxy62-resample_UPARSE_otu.txt", sep="\t", row.names=1, header=T) 

fg.dat = read.table("resample_OTU.txt", sep="\t", row.names=1, header=T) 

#list.dat1 = read.table("group1.txt", sep="\t", row.names=1)  ;list.dat1  ##ע??û?б?ͷ
#list.dat2 = read.table("group2.txt", sep="\t", row.names=1)  ;list.dat2  ##ע??û?б?ͷ

#list.dat=list.dat1
list.dat = read.table("group.txt", sep="\t", row.names=1)  ;list.dat1  ##ע??û?б?ͷ

site.names = list.dat[, 1]  #;site.names
site = unique(site.names)

fg.dat2 = fg.dat[, 2:ncol(fg.dat)]  #????????????һ?У????????汨??anosim????һ??Դ????Ӧ????д????
#fg.dat3 = fg.dat[, 1:ncol(fg.dat)] ##?????ͺ???
colMean = colMeans(fg.dat2, na.rm=TRUE)
fg.dat2 = fg.dat2 / colMean        ##ÿ??ֵ/??ƽ??ֵ

fg.dat2[is.na(fg.dat2)] = 0
sample = colnames(fg.dat2)    ##????????

# for adonis 
dis.method = "jaccard"
if(dis.method=="jaccard"){
  bi = TRUE
}else{
  bi = FALSE
}
#dat.ado = adonis(t(fg.dat2) ~ V2, data=list.dat, method = dis.method, binary = bi)
dat.ado = adonis(t(fg.dat2) ~ V2, data=list.dat, method = dis.method, binary = bi)
##????Error in G * t(hat) : non-conformable arrays
#a=t(fg.dat2)
#nrow(a)  ##ֻ??23?У?????һ?С??????????ı??���
#dat.ado = adonis(t(fg.dat3) ~ V2, data=list.dat, method = dis.method, binary = bi) ##?????Ϳ??ԡ???fg.dat3
dat.ado


report = c()
# for(x in 1:length(site)){
  #site.col = which(site.names == site[x])
  #grp.list = CO2.levels[site.col]
  dat = fg.dat2     ## dat = fg.dat3  ??fg.dat3?????Ͳ??ᱨ??
  grp.list = site.names
  
  # delete empty rows
  rsum1 = rowSums(dat)
  tempCK1 = which(rsum1==0)
  if(length(tempCK1)!=0) {dat = dat[-tempCK1,]}
  
  # calculate dissimilarity  mrpp??anosim??adonis
  dat1 = t(dat)
  dat.dist = vegdist(dat1, method = dis.method, binary=bi)
  dat.mrpp = mrpp(dat.dist, grp.list, distance = dis.method)  #????Error in dmat[ind == x, ind == x] : (subscript) logical subscript too long
dat.mrpp
  dat.ano = anosim(dat.dist, grp.list)
dat.ano  
  grp.vector = list(V1 = grp.list)
  dat.ado = adonis(dat1 ~ V1, data=grp.vector, method = dis.method, binary=bi)
dat.ado  
  report = rbind(report, c(dat.mrpp$delta, dat.mrpp$Pvalue, dat.ano$statistic, dat.ano$signif, dat.ado$aov.tab[1,4], dat.ado$aov.tab[1,6]))
# }
rownames(report) = site
report