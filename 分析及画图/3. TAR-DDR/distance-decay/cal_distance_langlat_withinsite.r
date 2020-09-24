# li 2017.7.12 take a glance
setwd('D:/文件存放/galaxy pipeline/galaxy/自己整理代码/R code/distance-decay')


library(vegan)
library(Imap)

#setwd('V:/my work in OU/All eCO2 samples/distance_decay/within_sites')
rm(list=ls(all=TRUE))

#calculate geo distances among all wells
well = read.table("Long_lat_wells.txt", header = T, row.names=1, sep="\t") ##文件夹没有这个文件
# longtitude	latitude
well = well[1:110,] #1:55,]  #56:110,]  #

fg_dat = read.table(file="../110samples_comb_nocut.txt", sep = "\t", header = TRUE, row.names=1) 
fg.dat = fg_dat[,5:ncol(fg_dat)]  #60:114] #,5:59]  #

# match env and fg datasets
samp.dist = rownames(well)
samp.fg = colnames(fg.dat)
my.samp = match(samp.fg, samp.dist)
my.samp 
well = well[my.samp, ] 
samp.dist = rownames(well)
longlat = well

# calculate geo distance
n = nrow(well)
geodis = matrix(0, n, n)
for(i in c(1:(n-1))){
	for(j in c(i:n)){
		geodis[i, j] = gdist(longlat[i,1], longlat[i,2], longlat[j, 1], longlat[j,2], units = "km")
	}
}
geodis = as.dist(t(geodis))
geodis[geodis==0] = 0.001

# calculate beta-distance 
fg.dat[is.na(fg.dat)] = 0
dist.method = "jaccard"

if(dist.method == "jaccard" ){
	beta.dist = vegdist(t(fg.dat),method = dist.method, binary=TRUE)
}else if(dist.method=="sorensen"){
  beta.dist = vegdist(t(fg.dat),method = "bray", binary=TRUE)
}else{
	beta.dist = vegdist(t(fg.dat),method = dist.method)
}

beta.sim = 1-beta.dist

#----for plots----------
cor = cor.test(geodis, beta.sim)
cor
cor$p.value

ppi = 500
png("eCO2_DDR_sorensen.png", width=6*ppi, height=6*ppi, res=ppi)
plot(geodis, beta.sim, xlab="Geo distance (km)", ylab="Similarity", cex.axis=1.5,cex.lab = 1.5)
text(700, 0.75, pos=4, labels = paste("r =", signif(cor$estimate,digits=3), ", p < 0.001"), cex = 1.5)
#title(paste("r =", signif(cor$estimate,digits=3), ", p =", signif(cor$p.value,digits=3)))
lm = lm(beta.sim~geodis)
abline(lm, col="red")
dev.off()
intercept = lm$coefficients[1]
slope = lm$coefficients[2]
slope

#--- for z value calculation----

log.beta.sim = log10(beta.sim)
log.geodis = log10(geodis)
lm1 = lm(log.beta.sim ~ log.geodis)
z1  = - lm1$coefficients[2]/2

# for aCO2/eCO2
aCO2.beta.sim = as.matrix(log.beta.sim)[1:55, 1:55]
aCO2.geodis   = as.matrix(log.geodis)[1:55, 1:55]
eCO2.beta.sim = as.matrix(log.beta.sim)[56:110, 56:110]
eCO2.geodis   = as.matrix(log.geodis)[56:110, 56:110]

lm2 = lm(as.dist(aCO2.beta.sim) ~ as.dist(aCO2.geodis))
z2 = - lm2$coefficients[2]/2

lm3 = lm(as.dist(eCO2.beta.sim) ~ as.dist(eCO2.geodis))
z3 = - lm3$coefficients[2]/2

c(z1, z2, z3)

# for permutation test on z value (randomize the pair of distances)
r = 1000  # number of random times
perm.z = c()
for(i in 1:r){
	perm.geodis = sample(geodis, replace=T)
	perm.beta.sim = sample(beta.sim, replace=T)
	#perm.cor = cor.test(perm.geodis, perm.beta.sim)
	perm.lm = lm(perm.beta.sim~perm.geodis)
	perm.z = c(perm.z, - perm.lm$coefficients[2]/2)
}
t.test(perm.z, mu = z, alternative = "two.sided")

# for within site distance decay
grplist = well[,3]
grp.name = unique(grplist)
par(mfrow=c(2, 3))
z.rep = c()

for(i in 1:length(grp.name)){
  site.num = which(grplist == grp.name[i])
  aCO2.num = site.num[site.num<56]
  eCO2.num = site.num[site.num>55]
  beta.sim.site = as.dist(as.matrix(beta.sim)[site.num, site.num])
  geodis.site   = as.dist(as.matrix(geodis)[site.num, site.num])
  beta.sim.aCO2 = as.dist(as.matrix(beta.sim)[aCO2.num, aCO2.num])
  geodis.aCO2   = as.dist(as.matrix(geodis)[aCO2.num, aCO2.num])
  beta.sim.eCO2 = as.dist(as.matrix(beta.sim)[eCO2.num, eCO2.num])
  geodis.eCO2   = as.dist(as.matrix(geodis)[eCO2.num, eCO2.num])
  
  # for plot
  plot(geodis.site, beta.sim.site, xlab="Geo distance (km)", ylab="Similarity", cex.axis=1.5,cex.lab = 1.5)
  cor = cor.test(geodis.site, beta.sim.site)
  title(paste(grp.name[i],"(r =", signif(cor$estimate,digits=3), ", p =", signif(cor$p.value,digits=3), ")"))
  lm = lm(beta.sim.site~geodis.site)
  abline(lm, col="red")
  
  # for z value
  lm1 = lm(log10(beta.sim.site) ~ log10(geodis.site))
  z1  = - lm1$coefficients[2]/2
  lm2 = lm(log10(beta.sim.aCO2) ~ log10(geodis.aCO2))
  z2 = - lm2$coefficients[2]/2
  lm3 = lm(log10(beta.sim.eCO2) ~ log10(geodis.eCO2))
  z3 = - lm3$coefficients[2]/2
  
  z.rep = rbind(z.rep, c(z1, z2, z3))
}
par()
rownames(z.rep) = grp.name
z.rep















# for multiple regression on matrices (MRM)
library(ecodist)
env_dat = read.table("all110_soil.txt",sep="\t",header=T,row.names=1)
env_dat[is.na(env_dat)] = 0

env.st = decostand(env_dat, method="standardize", MARGIN=2)

# match env and fg datasets
fg.dat= t(fg.dat)
samp.fg = rownames(fg.dat)
samp.env= rownames(env.st)
my.env = match(samp.fg, samp.env)
env.st2 = na.omit(env.st[my.env, ])  # omit the NA rows if without fg data
samp.env= rownames(env.st2)
my.fg = match(samp.env, samp.fg)
fg.dat2 = fg.dat[my.fg, ] 
fg.dat2[is.na(fg.dat2)] = 0

dist.method = "euclidean"
dist.fg = vegdist(fg.dat2,method = dist.method)
#dist.fg = vegdist(t(fg.dat),method = dist.method, binary=TRUE) # for jaccard
dist.NO3 = vegdist(env.st2[,1],method = dist.method)
dist.NH4 = vegdist(env.st2[,2],method = dist.method)
dist.N0.10 = vegdist(env.st2[,3],method = dist.method)
dist.C0.10 = vegdist(env.st2[,4],method = dist.method)
dist.SCN0.10 = vegdist(env.st2[,5],method = dist.method)
mrm1 = MRM(dist.fg ~ geodis + dist.NO3 + dist.NH4 + dist.N0.10 + dist.C0.10 + dist.SCN0.10)

mrm1

