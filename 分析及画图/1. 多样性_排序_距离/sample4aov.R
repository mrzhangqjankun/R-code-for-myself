setwd('V:/my work in OU/All eCO2 samples/anova')
rm(list=ls(all=TRUE))

#dat = read.table("all110_soil.txt", sep= "\t", header=T, row.names=1)
fg.dat = read.table("110samples_comb_gcut25percent.txt", sep= "\t", header=T, row.names=1)
list.dat = read.table("110samples_list.txt", sep="\t", header=T, row.names=1)

# for each variable, do anova
rep = c()
for(i in 1:ncol(dat)){
	aov.ex = aov(dat[,i]~Site*CO2, data = list.dat)
	aov.rep = summary(aov.ex)
	rep = c(rep, aov.rep)
}
names(rep) = colnames(dat)

TukeyHSD(aov.ex)

# Dunn's test (Bonferroni t test)
attach(dat)
pairwise.t.test(Gene, Group, p.adj="bonf")

# Tukey's Honestly Signiccant Difference Test
TukeyHSD(aov.ex)

# Unadjusted T test
pairwise.t.test(Gene, Group, p.adj="none")


# for each fgene, get sum, do anova
fg = fg.dat[,1]
fg.list = sort(unique(fg))
fg.dat2 = fg.dat[,5:ncol(fg.dat)]
fg.dat2[is.na(fg.dat2)] = 0
rep = c()
for(x in 1:length(fg.list)){
	fgene = fg.list[x]
	row.fg = which(fg == fgene)
	dat2 = fg.dat2[row.fg,]
	dat.sum = colSums(dat2)
	aov.ex = aov(dat.sum~Site*CO2, data=list.dat)
	aov.rep = summary(aov.ex)
	rep = c(rep, aov.rep)
}
names(rep) = as.vector(fg.list)
