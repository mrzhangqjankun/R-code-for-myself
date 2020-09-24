##li 2017.7.12 take a glance
rm(list=ls(all=TRUE))
#setwd('D:/?ļ?????/galaxy pipeline/galaxy/?Լ?????????/CCA/')
setwd('E:/桌面/Go-PCR实验/数据分析/9.25-重新分析/Resample-8000/CCA-VPA, mental test/')
library(vegan)

#setwd('V:/my work in OU/Yuting/VPA-4 D')

rm(list=ls(all=TRUE))
fg.dat = read.table("CCA-(resample_UPARSE_otu_table.txt).txt", sep="\t", row.names=1, header=T)

#fg_dat = read.csv("gene.csv",header=T,row.names=1)
fg.dat = t(fg_dat)
fg.dat[is.na(fg.dat)] = 0

env_dat = read.table("Env-li.txt",header=T,row.names=1)  #all110_soil.txt

#env_dat = read.csv("envall3.csv",header=T,row.names=1)
env_dat = env_dat[rownames(fg.dat),]
long_lat = env_dat[,8:9]
env.dat = env_dat[,1:6]
env.st = decostand(env.dat, method="standardize", MARGIN=2)
plant.div = env_dat[,7]


# for CCA calculation
C.whole = cca(fg.dat, env.st)

total.chi = C.whole$tot.chi
total.constrained = C.whole$CCA$tot.chi

# Option 1: for env selection by CCA inflation factors
inf_factor = vif.cca(C.whole)
inf_factor
# delete varable with max inflation factor
max_env = which(inf_factor == max(inf_factor))
env.st3 = env.st
while ( inf_factor[max_env] > 20){
	env.st3 = env.st3[,-max_env]
	C.reduced = cca(fg.dat, env.st3)
	inf_factor = vif.cca(C.reduced)
	max_env = which(inf_factor == max(inf_factor))
}
inf_factor
colnames(env.st3)

# Option 2: for F and p values, then according to p values to remove some non-significant variables
ind.p = array(0,dim=c(1,ncol(env.st)))
ind.F = array(0,dim=c(1,ncol(env.st)))
for(j in 1:ncol(env.st)){
	ind.cca = cca(fg.dat, env.st[,j]) #ind.cca = cca(fg.dat, env.st[,j], env.st[,-j])  #
	ind.sig = anova(ind.cca,step=1000)
	ind.p[1,j] = ind.sig$Pr[1]
	ind.F[1,j] = ind.sig$F[1]
}
colnames(ind.p) = colnames(env.st)
t(rbind(ind.F,ind.p))
#manually deleted some non-significant variables


# Option 3: Forward/Reverse selections for variable picking up
C.4forward = cca(fg.dat ~ 1, env.st)
ordistep(C.4forward, reformulate(names(env.st)), perm.max=200) #this step is pretty slow, it will be hard to carry out in GeoCHip data
step(C.4forward, reformulate(names(env.st3)), perm.max=200) # ordistep is based on p-value, step is based on AIC
# final selected variables could be picked up

final_soil_variables = c("moisture", "pH", "TN", "NH4")



# for partial spacial distance out from the environmental variables
# according to Ramette_2007_PNAS paper

library(Imap)

# calculate by latitude and longitude
n = nrow(env.st2)
geodis = matrix(0, n, n)
for(i in c(1:n)){
	for(j in c(1:n)){
		geodis[i, j] = gdist(long_lat[i,1], long_lat[i,2], long_lat[j, 1], long_lat[j,2], units = "m")
	}
}
geodis = as.dist(t(geodis))

# for xy 
geodis = dist(geodis)

geodis = dist(long_lat)

# transfer data to pcnm
pcnm1 = pcnm(geodis)

ind.p = array(0,dim=c(1,ncol(pcnm1$vectors)))
ind.F = array(0,dim=c(1,ncol(pcnm1$vectors)))
for(j in 1:12){
	ind.cca = cca(fg.dat, pcnm1$vectors[,j]) #ind.cca = cca(fg.dat, env.st[,j], env.st[,-j])  #
	ind.sig = anova(ind.cca,step=200)
	ind.p[1,j] = ind.sig$Pr[1]
	ind.F[1,j] = ind.sig$F[1]
}
colnames(ind.p) = colnames(pcnm1$vectors)
t(rbind(ind.F,ind.p))

#manually picked up pcnm
sig.pcnm = c(1,2)
picked.pcnm = pcnm1$vectors[ , sig.pcnm]

#========================
# Final CCA and VPA analysis
#========================

plant.alpha = plant.div

env.grp1 = c("TN", "NH4")
env.grp2 = "plant.alpha"
env.grp3 = colnames(picked.pcnm)
env.grp4 = c("pH", "moisture")
env.selected = cbind(env.st[,env.grp1], plant.alpha, picked.pcnm, env.st[, env.grp4])

fg.dat[is.na(fg.dat)] = 0


# for VPA based on RDA
mod = varpart(fg.dat, env.st[,env.grp1], plant.alpha, picked.pcnm, env.st[, env.grp4])
mod
plot(mod)

# for VPA based on CCA
C.whole = cca(fg.dat, env.selected)

# for CCA figure
x.sig = anova(C.whole)
x.p = x.sig$Pr[1]
plot(C.whole,dis=c('wa','cn'),main = paste( "(p=", x.p,")"), type="p")
C.whole$CCA$wa
C.whole$CCA$biplot

# for VPA partialling based on CCA
C.grp1.par  = cca(fg.dat, env.selected[,env.grp1], env.selected[,c(env.grp2,env.grp3,env.grp4)])
C.grp2.par  = cca(fg.dat, env.selected[,env.grp2], env.selected[,c(env.grp1,env.grp3,env.grp4)])
C.grp3.par  = cca(fg.dat, env.selected[,env.grp3], env.selected[,c(env.grp1,env.grp2,env.grp4)])
C.grp4.par  = cca(fg.dat, env.selected[,env.grp4], env.selected[,c(env.grp1,env.grp2,env.grp3)])

C.grp1_2.par = cca(fg.dat, env.selected[,c(env.grp1,env.grp2)], env.selected[,c(env.grp3,env.grp4)])
C.grp1_3.par = cca(fg.dat, env.selected[,c(env.grp1,env.grp3)], env.selected[,c(env.grp2,env.grp4)])
C.grp1_4.par = cca(fg.dat, env.selected[,c(env.grp1,env.grp4)], env.selected[,c(env.grp2,env.grp3)])
C.grp2_3.par = cca(fg.dat, env.selected[,c(env.grp2,env.grp3)], env.selected[,c(env.grp1,env.grp4)])
C.grp2_4.par = cca(fg.dat, env.selected[,c(env.grp2,env.grp4)], env.selected[,c(env.grp1,env.grp3)])
C.grp3_4.par = cca(fg.dat, env.selected[,c(env.grp3,env.grp4)], env.selected[,c(env.grp1,env.grp2)])

C.grp1_2_3.par = cca(fg.dat, env.selected[,c(env.grp1,env.grp2,env.grp3)], env.selected[,env.grp4])
C.grp1_2_4.par = cca(fg.dat, env.selected[,c(env.grp1,env.grp2,env.grp4)], env.selected[,env.grp3])
C.grp1_3_4.par = cca(fg.dat, env.selected[,c(env.grp1,env.grp3,env.grp4)], env.selected[,env.grp2])
C.grp2_3_4.par = cca(fg.dat, env.selected[,c(env.grp2,env.grp3,env.grp4)], env.selected[,env.grp1])

total.chi = C.whole$tot.chi
total.constrained = C.whole$CCA$tot.chi
grp1.par.chi  = C.grp1.par$CCA$tot.chi
grp2.par.chi  = C.grp2.par$CCA$tot.chi
grp3.par.chi  = C.grp3.par$CCA$tot.chi
grp4.par.chi  = C.grp4.par$CCA$tot.chi
grp1_2.chi = C.grp1_2.par$CCA$tot.chi
grp1_3.chi = C.grp1_3.par$CCA$tot.chi
grp1_4.chi = C.grp1_4.par$CCA$tot.chi
grp2_3.chi = C.grp2_3.par$CCA$tot.chi
grp2_4.chi = C.grp2_4.par$CCA$tot.chi
grp3_4.chi = C.grp3_4.par$CCA$tot.chi
grp1_2_3.chi = C.grp1_2_3.par$CCA$tot.chi
grp1_2_4.chi = C.grp1_2_4.par$CCA$tot.chi
grp1_3_4.chi = C.grp1_3_4.par$CCA$tot.chi
grp2_3_4.chi = C.grp2_3_4.par$CCA$tot.chi

#overlap.grp1_2.chi = grp1_2.chi - (grp1.par.chi + grp2.par.chi)
#overlap.grp1_3.chi = grp1_3.chi - (grp1.par.chi + grp3.par.chi)
#overlap.grp2_3.chi = grp2_3.chi - (grp2.par.chi + grp3.par.chi)
#overlap.all = total.constrained-(grp1.par.chi+grp2.par.chi+grp3.par.chi+overlap.grp1_2.chi+overlap.grp1_3.chi+overlap.grp2_3.chi)

grp1.percent = grp1.par.chi / total.chi
grp2.percent = grp2.par.chi / total.chi
grp3.percent = grp3.par.chi / total.chi
grp4.percent = grp4.par.chi / total.chi

grp1_2.percent = (grp1_2.chi - (grp1.par.chi + grp2.par.chi))/total.chi; if(grp1_2.percent < 0 ){grp1_2.percent = 0}
grp1_3.percent = (grp1_3.chi - (grp1.par.chi + grp3.par.chi))/total.chi; if(grp1_3.percent < 0 ){grp1_3.percent = 0}
grp1_4.percent = (grp1_4.chi - (grp1.par.chi + grp4.par.chi))/total.chi; if(grp1_4.percent < 0 ){grp1_4.percent = 0}
grp2_3.percent = (grp2_3.chi - (grp2.par.chi + grp3.par.chi))/total.chi; if(grp2_3.percent < 0 ){grp2_3.percent = 0}
grp2_4.percent = (grp2_4.chi - (grp2.par.chi + grp4.par.chi))/total.chi; if(grp2_4.percent < 0 ){grp2_4.percent = 0}
grp3_4.percent = (grp3_4.chi - (grp3.par.chi + grp4.par.chi))/total.chi; if(grp3_4.percent < 0 ){grp3_4.percent = 0}

grp1_2_3.percent = (grp1_2_3.chi - (grp1.par.chi + grp2.par.chi + grp3.par.chi))/total.chi; if(grp1_2_3.percent < 0 ){grp1_2_3.percent = 0}
grp1_2_4.percent = (grp1_2_4.chi - (grp1.par.chi + grp2.par.chi + grp4.par.chi))/total.chi; if(grp1_2_4.percent < 0 ){grp1_2_4.percent = 0}
grp1_3_4.percent = (grp1_3_4.chi - (grp1.par.chi + grp3.par.chi + grp4.par.chi))/total.chi; if(grp1_3_4.percent < 0 ){grp1_3_4.percent = 0}
grp2_3_4.percent = (grp2_3_4.chi - (grp2.par.chi + grp3.par.chi + grp4.par.chi))/total.chi; if(grp2_3_4.percent < 0 ){grp2_3_4.percent = 0}

overlap.percent = total.constrained/total.chi -(grp1.percent+grp2.percent+grp3.percent+grp4.percent+grp1_2.percent+grp1_3.percent+grp1_4.percent+grp2_3.percent+grp2_4.percent+grp3_4.percent+grp1_2_3.percent+grp1_2_4.percent+grp1_3_4.percent+grp2_3_4.percent)

unexplained.percent = (total.chi - total.constrained) / total.chi

grp1.percent
grp2.percent
grp3.percent
grp4.percent
grp1_2.percent
grp1_3.percent
grp1_4.percent
grp2_3.percent
grp2_4.percent
grp3_4.percent 
grp1_2_3.percent
grp1_2_4.percent
grp1_3_4.percent 
grp2_3_4.percent
overlap.percent
unexplained.percent

