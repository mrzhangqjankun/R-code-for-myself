##li 2017.7.12 learn


library(vegan)

rm(list=ls(all=TRUE))
setwd('D:/文件存放/galaxy pipeline/galaxy/自己整理代码/CCA/')
#setwd('D:/Kai/Desktop/CCA')
library("vegan")

#rm(list=ls(all=TRUE))
#fg.dat = read.table("rh.txt", sep="\t", row.names=1, header=T)
fg.dat = read.table("OTU.txt", sep="\t", row.names=1, header=T)

fg.dat2 = fg.dat[, 2:ncol(fg.dat)]   ##第一列数据去掉，为什么
fg.dat2[is.na(fg.dat2)] = 0
sample = colnames(fg.dat2)

env_dat = read.table("Env factor2017.6.28.txt",sep="\t", header=T,row.names=1)
#env_dat = read.table("S-factor-1.txt",sep="\t", header=T,row.names=1)  #all110_soil.txt
env_dat[is.na(env_dat)] = 0
env.dat = env_dat[,1:ncol(env_dat)]
env.st = decostand(env.dat, method="standardize", MARGIN=2)

# match env and fg datasets
samp.fg = colnames(fg.dat2)
samp.env= rownames(env.st)
my.env = match(samp.fg, samp.env)
env.st2 = na.omit(env.st[my.env, ])  # omit the NA rows if without fg data
samp.env= rownames(env.st2)
my.fg = match(samp.env, samp.fg)
fg.dat2 = fg.dat2[, my.fg] 

# for CCA calculation
fg.dat2 = t(fg.dat2)
C.whole = cca(fg.dat2, env.st2)
C.whole

# for env selection by CCA inflation factors
inf_factor = vif.cca(C.whole)
# delete varable with max inflation factor
max_env = which(inf_factor == max(inf_factor))
env.st3 = env.st2
while ( inf_factor[max_env] > 20){    ###膨胀系数>20的因子去掉，重新计算CCA得到新的膨胀系数。
  env.st3 = env.st3[,-max_env]        
  C.reduced = cca(fg.dat2, env.st3)
  inf_factor = vif.cca(C.reduced)
  max_env = which(inf_factor == max(inf_factor))
}
inf_factor
colnames(env.st3)

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

plot(C.whole,dis=c('cn','wa'),main = paste( "(p=", x.p,")"))


# for pCCA
env.grp1 = c("TK","pH")      ###自己选因子
env.grp2 = c("CL","RootL")   ###自己选因子

env.selected = cbind( env.st2[ ,env.grp1], env.st2[,env.grp2])
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
