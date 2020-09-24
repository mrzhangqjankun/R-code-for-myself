##li 2017.7.12 take a glance
rm(list=ls(all=TRUE))
#setwd('D:/?ļ?????/galaxy pipeline/galaxy/?Լ?????????/CCA/')
setwd('E:/桌面/Go-PCR实验/数据分析/9.25-重新分析/Resample-8000/CCA-VPA, mental test/')
library(vegan)

#setwd('D:/all.R-3.16')
#library("vegan")

#rm(list=ls(all=TRUE))

fg.dat = read.table("CCA-(resample_UPARSE_otu_table.txt).txt", sep="\t", row.names=1, header=T)


#fg.dat2 = fg.dat[, 5:ncol(fg.dat)]
fg.dat2 = fg.dat[, 1:ncol(fg.dat)]
fg.dat2[is.na(fg.dat2)] = 0
sample = colnames(fg.dat2)

env_dat = read.table("Env-li.txt",header=T,row.names=1)  #all110_soil.txt
env_dat[is.na(env_dat)] = 0
env.dat = env_dat[,1:ncol(env_dat)]
env.st = decostand(env.dat, method="standardize", MARGIN=2)

long_lat = read.csv("env-ave-long.csv", header=T, row.names=1) #Long_lat.txt

# match env and fg datasets
samp.fg = colnames(fg.dat2)
samp.env= rownames(env.st)
my.env = match(samp.fg, samp.env)
env.st2 = na.omit(env.st[my.env, ])  # omit the NA rows if without fg data
samp.env= rownames(env.st2)
my.fg = match(samp.env, samp.fg)
fg.dat2 = fg.dat2[, my.fg] 
long_lat = long_lat[my.env, ]


# for CCA calculation
fg.dat2 = t(fg.dat2)
C.whole = cca(fg.dat2, env.st2)
#C.whole

# for env selection by CCA inflation factors
inf_factor = vif.cca(C.whole)
# delete varable with max inflation factor
max_env = which(inf_factor == max(inf_factor))
env.st3 = env.st2
while ( inf_factor[max_env] > 20){
	env.st3 = env.st3[,-max_env]
	C.reduced = cca(fg.dat2, env.st3)
	inf_factor = vif.cca(C.reduced)
	max_env = which(inf_factor == max(inf_factor))
}
inf_factor
colnames(env.st3)

#env.st4 = env.st2[,-5] # removing C:N ratio
#C.whole = (fg.dat2, env.st4)

# for F and p values

ind.p = array(0,dim=c(1,ncol(env.st2)))
ind.F = array(0,dim=c(1,ncol(env.st2)))
for(j in 1:ncol(env.st2)){
	ind.cca = cca(fg.dat2, env.st2[,j]) #ind.cca = cca(fg.dat, env.st[,j], env.st[,-j])  #
	ind.sig = anova(ind.cca,step=1000)
	ind.p[1,j] = ind.sig$Pr[1]
	ind.F[1,j] = ind.sig$F[1]
}
colnames(ind.p) = colnames(env.st2)
t(rbind(ind.F,ind.p))




# for CCA figure
x.sig = anova(C.whole)
x.p = x.sig$Pr[1] #
x.p

plot(C.whole,dis=c('wa','cn'),main = paste( "(p=", x.p,")"))

for(g in 1:length(site)){
	list.name = which(site.names == site[g])
	points(C.whole$CCA$wa[list.name,1:2], col=g, pch=19)
}
legend(2,-1.7, site, col=c(1:length(site)), pch=19) #adjust according to your plot

CO2 = unique(CO2.levels)
plot(C.whole,dis=c('wa','cn'),main = paste( "(p=", x.p,")"))
for(g in 1:length(CO2)){
	list.name = which(CO2.levels == CO2[g])
	points(C.whole$CCA$wa[list.name,1:2], col=g, pch=19)
}
legend(2,-2, CO2, col=c(1:length(CO2)), pch=19)




#
library(Imap)
# calculate by latitude and longitude
n = nrow(env.st2)
geodis = matrix(0, n, n)
for(i in c(1:n)){
	for(j in c(1:n)){
		geodis[i, j] = gdist(long_lat[i,1], long_lat[i,2], long_lat[j, 1], long_lat[j,2], units = "m") # Long1, Lat1, Long2, Lat2)
	}
}
geodis[geodis==0] = 1
geodis = as.dist(t(geodis))

pcnm1 = pcnm(geodis)

ind.p = array(0,dim=c(1,ncol(pcnm1$vectors)))
ind.F = array(0,dim=c(1,ncol(pcnm1$vectors)))
for(j in 1:ncol(pcnm1$vectors)){ 
	ind.cca = cca(fg.dat2, pcnm1$vectors[,j]) #ind.cca = cca(fg.dat, env.st[,j], env.st[,-j])  #
	ind.sig = anova(ind.cca,step=200)
	ind.p[1,j] = ind.sig$Pr[1]
	ind.F[1,j] = ind.sig$F[1]
}
colnames(ind.p) = colnames(pcnm1$vectors)
t(rbind(ind.F,ind.p))

#manually picked up pcnm
sig.pcnm = c(2) #adjust according to above results
picked.pcnm = pcnm1$vectors[ , sig.pcnm]

# for pCCA
env.grp1 = c("W1","T","TN")
env.grp2 = colnames(picked.pcnm)
env.grp3 = c("Year")
env.selected = cbind(env.st2[,env.grp1], picked.pcnm, env.st2[,env.grp3])
colnames(env.selected)[5] = "Year"

C.whole = cca(fg.dat2, env.selected)

C.grp1.par  = cca(fg.dat2, env.selected[,env.grp1], env.selected[,c(env.grp2,env.grp3)])
C.grp2.par  = cca(fg.dat2, env.selected[,env.grp2], env.selected[,c(env.grp1,env.grp3)])
C.grp3.par  = cca(fg.dat2, env.selected[,env.grp3], env.selected[,c(env.grp1,env.grp2)])
C.grp1_2.par = cca(fg.dat2, env.selected[,c(env.grp1,env.grp2)], env.selected[,env.grp3])
C.grp1_3.par = cca(fg.dat2, env.selected[,c(env.grp1,env.grp3)], env.selected[,env.grp2])
C.grp2_3.par = cca(fg.dat2, env.selected[,c(env.grp2,env.grp3)], env.selected[,env.grp1])

total.chi = C.whole$tot.chi
total.constrained = C.whole$CCA$tot.chi
grp1.par.chi  = C.grp1.par$CCA$tot.chi
grp2.par.chi  = C.grp2.par$CCA$tot.chi
grp3.par.chi  = C.grp3.par$CCA$tot.chi
grp1_2.chi = C.grp1_2.par$CCA$tot.chi
grp1_3.chi = C.grp1_3.par$CCA$tot.chi
grp2_3.chi = C.grp2_3.par$CCA$tot.chi

overlap.grp1_2.chi = grp1_2.chi - (grp1.par.chi + grp2.par.chi)
overlap.grp1_3.chi = grp1_3.chi - (grp1.par.chi + grp3.par.chi)
overlap.grp2_3.chi = grp2_3.chi - (grp2.par.chi + grp3.par.chi)
overlap.all = total.constrained-(grp1.par.chi+grp2.par.chi+grp3.par.chi+overlap.grp1_2.chi+overlap.grp1_3.chi+overlap.grp2_3.chi)

grp1.percent = grp1.par.chi / total.chi
grp2.percent = grp2.par.chi / total.chi
grp3.percent = grp3.par.chi / total.chi
grp1_2.percent = overlap.grp1_2.chi/total.chi
grp1_3.percent = overlap.grp1_3.chi/total.chi
grp2_3.percent = overlap.grp2_3.chi/total.chi
overlap.percent = overlap.all/total.chi

unexplained.percent = (total.chi - total.constrained) / total.chi

grp1.percent
grp2.percent
grp3.percent
grp1_2.percent
grp1_3.percent
grp2_3.percent
overlap.percent
unexplained.percent














#C.grp1  = cca(fg.dat2, env.selected[,env.grp1])
#C.grp2  = cca(fg.dat2, env.selected[,env.grp2])
#C.grp3  = cca(fg.dat2, env.selected[,env.grp3])
C.grp1.par  = cca(fg.dat2, env.selected[,env.grp1], env.selected[,c(env.grp2)])
C.grp2.par  = cca(fg.dat2, env.selected[,env.grp2], env.selected[,c(env.grp1)])
C.grp1_2.par = C.whole

total.chi = C.whole$tot.chi
total.constrained = C.whole$CCA$tot.chi
grp1.par.chi  = C.grp1.par$CCA$tot.chi
grp2.par.chi  = C.grp2.par$CCA$tot.chi
grp1_2.chi = C.grp1_2.par$CCA$tot.chi

overlap.grp1_2.chi = grp1_2.chi - (grp1.par.chi + grp2.par.chi)

grp1.percent = grp1.par.chi / total.chi
grp2.percent = grp2.par.chi / total.chi
grp1_2.percent = overlap.grp1_2.chi/total.chi

unexplained.percent = (total.chi - total.constrained) / total.chi

grp1.percent
grp2.percent
grp1_2.percent
unexplained.percent






# for individual propotions
ind.p = array(0,dim=c(1,ncol(env.selected)))
for(j in 1:ncol(env.selected)){
	ind.par = cca(fg.dat2, env.selected[,j], env.selected[,-j])
	ind.chi = ind.par$CCA$tot.chi
	ind.per = ind.chi/total.chi
	ind.p[j] = ind.per
}
ind.p


# for each metagrp pCCA

metagrp1 = c(grp1, grp7, grp8)
metagrp2 = c(grp2, grp3, grp4)
metagrp3 = c(grp4, grp5, grp6)
metagrp = list(metagrp1, metagrp2, metagrp3)


env.grp1 = c("Specific", "pH")
env.grp2 = c("Acetate","U", "NO3", "SO4","Fe")
env.grp3 = c("Ag", "Al", "Ba", "Cr", "Ga") ##, "Ca","Mn", "Mg")  # )  #

state.Fp = c()
whole.p = c()
report = c()
for(x in c(1:length(metagrp))){
	fg.dat3 = fg.dat2[metagrp[[x]],]
	env.st3 = env.st2[metagrp[[x]],]
	
	# for pick up variables
#	ind.p = array(0,dim=c(1,ncol(env.st3)))
#	ind.F = array(0,dim=c(1,ncol(env.st3)))
#	for(j in 1:ncol(env.st3)){
#		ind.cca = cca(fg.dat3, env.st3[,j])
#		ind.sig = anova(ind.cca,step=1000)
#		ind.p[1,j] = ind.sig$Pr[1]
#		ind.F[1,j] = ind.sig$F[1]
#	}
#	colnames(ind.p) = colnames(env.st3)
#	Fp = t(rbind(ind.F,ind.p))
#	state.Fp = cbind(state.Fp, Fp)
#	
#	env.grp1 = c("Acetate")
#	env.grp2 = c("U", "NO3", "SO4")
#	my.pick = c()
#	for(j in 1:ncol(env.st3)){
#		if(ind.p[1,j] <= 0.1 && (colnames(ind.p)[j] %in% c(env.grp1, env.grp2) == FALSE) ){
#			my.pick = c(my.pick, j)
#		}
#	}
#	my.pick.env = colnames(env.st3)[my.pick]
#	env.grp3 = my.pick.env

	env.selected2 = env.st3[,c(env.grp1, env.grp2, env.grp3)]
	
	C.whole = cca(fg.dat3, env.selected2)
	whole.sig = anova(C.whole,step=1000)
	whole.p = c(whole.p, whole.sig$Pr[1])
	
	#C.grp1  = cca(fg.dat3, env.selected2[,env.grp1])
	#C.grp2  = cca(fg.dat3, env.selected2[,env.grp2])
	#C.grp3  = cca(fg.dat3, env.selected2[,env.grp3])
	C.grp1.par  = cca(fg.dat3, env.selected2[,env.grp1], env.selected2[,c(env.grp2,env.grp3)])
	C.grp2.par  = cca(fg.dat3, env.selected2[,env.grp2], env.selected2[,c(env.grp1,env.grp3)])
	C.grp3.par  = cca(fg.dat3, env.selected2[,env.grp3], env.selected2[,c(env.grp1,env.grp2)])
	C.grp1_2.par = cca(fg.dat3, env.selected2[,c(env.grp1,env.grp2)], env.selected2[,env.grp3])
	C.grp1_3.par = cca(fg.dat3, env.selected2[,c(env.grp1,env.grp3)], env.selected2[,env.grp2])
	C.grp2_3.par = cca(fg.dat3, env.selected2[,c(env.grp2,env.grp3)], env.selected2[,env.grp1])
	
	total.chi = C.whole$tot.chi
	total.constrained = C.whole$CCA$tot.chi
	grp1.par.chi  = C.grp1.par$CCA$tot.chi
	grp2.par.chi  = C.grp2.par$CCA$tot.chi
	grp3.par.chi  = C.grp3.par$CCA$tot.chi
	grp1_2.chi = C.grp1_2.par$CCA$tot.chi
	grp1_3.chi = C.grp1_3.par$CCA$tot.chi
	grp2_3.chi = C.grp2_3.par$CCA$tot.chi
	
	overlap.grp1_2.chi = grp1_2.chi - (grp1.par.chi + grp2.par.chi)
	overlap.grp1_3.chi = grp1_3.chi - (grp1.par.chi + grp3.par.chi)
	overlap.grp2_3.chi = grp2_3.chi - (grp2.par.chi + grp3.par.chi)
	overlap.all = total.constrained-(grp1.par.chi+grp2.par.chi+grp3.par.chi+overlap.grp1_2.chi+overlap.grp1_3.chi+overlap.grp2_3.chi)
	
	grp1.percent = grp1.par.chi / total.chi
	grp2.percent = grp2.par.chi / total.chi
	grp3.percent = grp3.par.chi / total.chi
	grp1_2.percent = overlap.grp1_2.chi/total.chi
	grp1_3.percent = overlap.grp1_3.chi/total.chi
	grp2_3.percent = overlap.grp2_3.chi/total.chi
	overlap.percent = overlap.all/total.chi
	
	unexplained.percent = (total.chi - total.constrained) / total.chi
	
	print(paste("metagroup",x, "\n"))
	result = c(grp1.percent,grp2.percent,grp3.percent,grp1_2.percent,grp1_3.percent,grp2_3.percent,overlap.percent,unexplained.percent)
	report = rbind(report, result)
}
#state.Fp
#whole.p
report
