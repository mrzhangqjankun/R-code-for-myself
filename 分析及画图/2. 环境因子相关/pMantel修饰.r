library(vegan)
library(Imap)

setwd('C:/Users/Administrator/Desktop/all.R')
rm(list=ls(all=TRUE))

env_dat = read.csv("env240.all.csv", header=T, row.names=1)

dist.matrix = read.csv("weighted_Unifrac.csv", header=T,row.names=1)

env.dat = env_dat[colnames(dist.matrix),]

env.st = decostand(env.dat, "standardize", 2)

report = c()
for(i in 1:ncol(env.st)){
  env.dist = dist(env.st[,i])
  mantel.rep = mantel(env.dist, as.dist(dist.matrix))
  report = rbind(report,c(mantel.rep$statistic, mantel.rep$signif))
}
rownames(report) = colnames(env.st)
report

mnds = metaMDS(as.dist(dist.matrix))
mnds$points
plot(mnds, type="t")











#calculate geo distances among all wells
well = read.table("Long_lat.txt", header = T, row.names=1, sep="\t")
# longtitude	latitude
well = well[1:110,]

fg_dat = read.table(file="110samples_comb_gcut25percent.txt", sep = "\t", header = TRUE, row.names=1) 
gene.cat = fg_dat[,2]
gene.name = fg_dat[,1]
fg_dat = fg_dat[order(gene.cat, gene.name),]
gene.cat = fg_dat[,1]
gene.list = unique(gene.cat) 
fg.dat = fg_dat[,5:114]

env_dat = read.table("all110_soil.txt",sep="\t",header=T,row.names=1)
env_dat[is.na(env_dat)] = 0
env.dat = env_dat[1:110,]
env.st = decostand(env.dat, method="standardize", MARGIN=2)

# groups
grp0 = seq(1,110)
grp1 = seq(1,55)
grp2 = seq(56,110)
grp = list(grp0,grp1,grp2)

# calculate beta-distance 
fg.dat[is.na(fg.dat)] = 0
dist.method = "euclidean"

p.table = matrix(0, nrow=(length(gene.list)+1)*2, ncol = 2 * 3)

for(x in 1:3){
	samp = grp[[x]]
	longlat = well[samp,]
	# calculate geo distance
	n = nrow(longlat)
	geodis = matrix(0, n, n)
	for(i in c(1:(n-1))){
		for(j in c(i:n)){
			geodis[i, j] = gdist(longlat[i,1], longlat[i,2], longlat[j, 1], longlat[j,2], units = "km")
		}
	}
	geodis = as.dist(t(geodis))
	geodis[geodis==0] = 0.1
	
	fg.dat1 = fg.dat[,samp]
	env1 = env.st[samp,]
	beta.dist = vegdist(t(fg.dat1),method = dist.method)
	env.dist = vegdist(env1, method=dist.method)
	
	man1 = mantel.partial(beta.dist, env.dist, geodis)
	man2 = mantel.partial(beta.dist, geodis, env.dist)
	
	p.table[1,(x*2-1)] = man1$statistic
	p.table[2,(x*2-1)] = man1$signif
	p.table[1,(x*2)] = man2$statistic
	p.table[2,(x*2)] = man2$signif
	
	for(i in 1:length(gene.list)){
		valid.row = which(gene.cat == gene.list[i])
		dat1 = fg.dat[valid.row, samp]
		dat1.dist = vegdist(t(dat1), method=dist.method)
			
		man1 = mantel.partial(dat1.dist, env.dist, geodis)
		man2 = mantel.partial(dat1.dist, geodis, env.dist)
			
		p.table[(i*2+1),(x*2-1)] = man1$statistic
		p.table[(i*2+2),(x*2-1)] = man1$signif
		p.table[(i*2+1),(x*2)] = man2$statistic
		p.table[(i*2+2),(x*2)] = man2$signif
	}
}

rownames(p.table) = rep(c("whole", as.vector(gene.list)), each=2)


