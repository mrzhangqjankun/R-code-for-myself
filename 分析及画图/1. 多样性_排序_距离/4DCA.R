
setwd('V:/my work in OU/All eCO2 samples/DCA')
library("vegan")
rm(list=ls(all=TRUE))

fg.dat = read.table("110samples_comb_gcut25percent_archaea.txt", sep="\t", header=T, row.names=1)
samp = colnames(fg.dat[,5:ncol(fg.dat)])
fg.dat2 = fg.dat[,5:ncol(fg.dat)]

#gene.name = fg.dat[,1]
#valid.row = which(gene.name == "gyrB")
#fg.dat2 = fg.dat2[valid.row,]

# for group

list.dat = read.table("110samples_list.txt", sep="\t", row.names=1)
site.names = list.dat[, 1]
site = unique(site.names)
CO2.levels = list.dat[, 2]


# for DCA
fg.dat2[is.na(fg.dat2)] = 0

x.dca = decorana(t(fg.dat2))
x.re = summary(x.dca)
x.re$site.scores
plot(x.dca, dis="site")

for(g in c(1:length(site))){
	#list.name = paste("X", grp[[g]], sep="")
	list.name = which(site.names == site[g])
	points(x.re$site.scores[list.name,1:2], col=g, pch=19)
}
legend(-1,1.3, site, col=c(1:length(site)), pch=19)

CO2 = unique(CO2.levels)
plot(x.dca, dis="site")
for(g in 1:length(CO2)){
	list.name = which(CO2.levels == CO2[g])
	points(x.re$site.scores[list.name,1:2], col=g, pch=19)
}
legend(-1,1.3, CO2, col=c(1:length(CO2)), pch=19)


# for NMDS
fg.nms = metaMDS(fg.dat2, autotransform=FALSE)
ordiplot(fg.nms, type="t", display="sites")
for(g in c(1:8)){
	list.name = grp[[g]]
	points(fg.nms$points[list.name,], col=g, pch=19)
}
