
setwd('V:/my work in OU/All eCO2 samples/adonis')
library("vegan")

rm(list=ls(all=TRUE))
fg.dat = read.table("110samples_comb_gcut25percent.txt", sep="\t", row.names=1, header=T)  #110samples_comb.txt

list.dat = read.table("110samples_list.txt", sep="\t", row.names=1)
site.names = list.dat[, 1]
site = unique(site.names)
CO2.levels = list.dat[, 2]
CO2.uni = unique(CO2.levels)

fg.dat2 = fg.dat[, 5:ncol(fg.dat)]
# scaling the data
colMean = colMeans(fg.dat2, na.rm=TRUE)
fg.dat2 = fg.dat2 / colMean

fg.dat2[is.na(fg.dat2)] = 0
sample = colnames(fg.dat2)

# group cutting
cut = 0 #0.25 # 25%
for(x in 1:length(site)){
	site.col = which(site.names == site[x])
	for(a in 1:length(CO2.uni)){
		CO2.col = which(CO2.levels == CO2.uni[a])
		valid.col = intersect(CO2.col,site.col)
		dat = fg.dat2[,valid.col]
		rowS = rowSums(dat>0)
		row.inv = which(rowS<=(floor(cut * length(valid.col))))
		dat[row.inv,] = 0
		fg.dat2[,valid.col] = dat
	}
}
rowS = rowSums(fg.dat2>0)
valid.row = which(rowS>0)
fg.dat2 = fg.dat2[valid.row, ]

# output the datasets
#fg.dat3 = cbind(fg.dat[valid.row, 1:4], fg.dat2)
#write.table(fg.dat3, "110samples_comb_nocut.txt", sep="\t")  #_gcut25percent

# for adonis 
dis.method = "jaccard"
if(dis.method=="jaccard"){
	bi = TRUE
}else{
	bi = FALSE
}
dat.ado = adonis(t(fg.dat2) ~ V2 * V3, data=list.dat, method = dis.method, binary = bi)
dat.ado


# do adonis for each gene category
dat.ado = adonis(t(fg.dat2) ~ V3, data=list.dat, method = dis.method, strata=list.dat[,1], binary = bi)
dat.ado

report = c()
list.name = c()
gene.cat = fg.dat[,1][valid.row]
length(gene.cat)
cat = sort(unique(gene.cat))
for(i in 1:length(cat)){
  fg.cat.row = which(gene.cat==as.character(cat[i]))
  fg.dat.cat = fg.dat2[fg.cat.row, ]
  sums = colSums(fg.dat.cat)
  zeros = which(sums == 0)
  if(length(zeros)<=50){
    nonzeros = which(sums > 0)
    fg.dat.cat2 = fg.dat.cat[,nonzeros] # remove the samples without any data
    list.dat2   = list.dat[nonzeros, ]
    ado.cat = adonis(t(fg.dat.cat2) ~ list.dat2[,2], method = dis.method, strata=list.dat2[,1], binary = bi)
    report = rbind(report, c(ado.cat$aov.tab$F.Model[1], ado.cat$aov.tab$`Pr(>F)`[1], ado.cat$aov.tab$R2[1]))
    list.name = c(list.name,i)
  }
}
rownames(report) = cat[list.name]
report




# for each site testing dissimilarity
report = c()
for(x in 1:length(site)){
	site.col = which(site.names == site[x])
	grp.list = CO2.levels[site.col]
	dat = fg.dat2[,site.col]
	
	# delete empty rows
	rsum1 = rowSums(dat)
	tempCK1 = which(rsum1==0)
	if(length(tempCK1)!=0) {dat = dat[-tempCK1,]}
	
	# calculate dissimilarity
	dat1 = t(dat)
	dat.dist = vegdist(dat1, method = dis.method, binary=bi)
	dat.mrpp = mrpp(dat.dist, grp.list, distance = dis.method)
		
	dat.ano = anosim(dat.dist, grp.list)
	
	grp.vector = list(V1 = grp.list)
	dat.ado = adonis(dat1 ~ V1, data=grp.vector, method = dis.method, binary=bi)
	
	report = rbind(report, c(dat.mrpp$delta, dat.mrpp$Pvalue, dat.ano$statistic, dat.ano$signif, dat.ado$aov.tab[1,4], dat.ado$aov.tab[1,6]))
}
rownames(report) = site
report


# for Dissimilarity test for each group

dis.method = "bray"

mrpp.re = matrix(0, nrow=8, ncol=8)
ado.re = matrix(0, nrow=8, ncol=8)
ano.re = matrix(0, nrow=8, ncol=8)
report = c()
for(x in c(1:7)){
	for(y in c((x+1) : 8)){
		#list1 = paste("X", grp[[x]], sep="")
		#list2 = paste("X", grp[[y]], sep="")
		list1 = grp[[x]]
		list2 = grp[[y]]
		col1 = pmatch(list1, samp)
		col2 = pmatch(list2, samp)
		grp.list = c(rep("C",length(list1)), rep("T", length(list2)))
		dat = fg.dat2[, c(col1, col2)]
		#====cut empty row====
		sum1 = rowSums(dat, na.rm=T) 
		valid.row = which(sum1 > 0)
		#=====================
		dat = dat[valid.row,]
		dat[is.na(dat)] = 0
		#dat1 = decostand(t(dat), "norm")
		dat1 = t(dat)

		#dat.mrpp = mrpp(dat1, grp.list, distance = dis.method)
		#mrpp.re[x, y] = dat.mrpp$Pvalue  #paste(signif(dat.mrpp$delta,digits=3), "(", dat.mrpp$Pvalue, ")", sep="")
		
		#dat.dist = vegdist(dat1, method = dis.method)
		#dat.ano = anosim(dat.dist, grp.list)
		#ano.re[x, y] = dat.ano$signif  #paste(signif(dat.ano$statistic,digits=3), "(", dat.ano$signif, ")", sep="")
		
		grp.vector = list(V1 = grp.list)
		dat.ado = adonis(dat1 ~ V1, data=grp.vector, method = dis.method)
		ado.re[x, y] = dat.ado$aov.tab[1,6]  #paste(signif(dat.ado$aov.tab[1,5],digits=3), "(", dat.ado$aov.tab[1,6], ")", sep="")
	}
}
mrpp.re
ado.re
ano.re

# for Dissimilarity test for each meta-group
dis.method = "jaccard"
mrpp.re = matrix(0, nrow=4, ncol=4)
ado.re = matrix(0, nrow=4, ncol=4)
ano.re = matrix(0, nrow=4, ncol=4)
rep = matrix(0, nrow=4, ncol=4)

for(x in c(1:3)){
	for(y in c((x+1) : 4)){
		#list1 = paste("X", metagrp[[x]], sep="")
		#list2 = paste("X", metagrp[[y]], sep="")
		list1 = metagrp[[x]]
		list2 = metagrp[[y]]
		col1 = pmatch(list1, samp)
		col2 = pmatch(list2, samp)
		metagrp.list = c(rep("C",length(list1)), rep("T", length(list2)))
		dat = fg.dat2[, c(col1, col2)]
		#====cut empty row====
		sum1 = rowSums(dat, na.rm=T) 
		valid.row = which(sum1 > 0)
		#=====================
		dat = dat[valid.row,]
		dat[is.na(dat)] = 0
		if(dis.method == "jaccard"){
			dat1 = decostand(t(dat), "pa")
		}else{
			dat1 = t(dat)
		}

		dat.mrpp = mrpp(dat1, metagrp.list, distance = dis.method)
		mrpp.re[x, y] = dat.mrpp$Pvalue  #paste(signif(dat.mrpp$delta,digits=3), "(", dat.mrpp$Pvalue, ")", sep="")
		
		dat.dist = vegdist(dat1, method = dis.method)
		dat.ano = anosim(dat.dist, metagrp.list)
		ano.re[x, y] = dat.ano$signif  #paste(signif(dat.ano$statistic,digits=3), "(", dat.ano$signif, ")", sep="")
		
		metagrp.vector = list(V1 = metagrp.list)
		dat.ado = adonis(dat1 ~ V1, data=metagrp.vector, method = dis.method)
		ado.re[x, y] = dat.ado$aov.tab[1,6]  #paste(signif(dat.ado$aov.tab[1,5],digits=3), "(", dat.ado$aov.tab[1,6], ")", sep="")
		
		rep[x, y] = paste(mrpp.re[x, y], ano.re[x, y], ado.re[x, y], sep = ",")
	}
}
mrpp.re
ado.re
ano.re

# do adonis for whole dataset
dis.method = "bray"

fg.dat2[is.na(fg.dat2)] = 0

grp.list = c()
for(i in 1:length(grp)){
	grp.list = c(grp.list, rep(paste("grp",i,sep=""),length(grp[[i]])))
}
grp.vector = list(V1 = grp.list)
dat.ado = adonis(t(fg.dat2) ~ V1, data=grp.vector, method = dis.method)


