# li 2017.7.12 take a glance
setwd('D:/文件存放/galaxy pipeline/galaxy/自己整理代码/R code/distance-decay')


library(vegan)
library(Imap)

#setwd('V:/my work in OU/All eCO2 samples/distance_decay/updated_7-15-14')
rm(list=ls(all=TRUE))

#calculate geo distances among all wells
well = read.table("Long_lat_wells.txt", header = T, row.names=1, sep="\t") ##文件夹没有这个文件
# longtitude	latitude
well = well[1:110,] #1:55,]  #56:110,]  #

fg_dat = read.table(file="../110samples_comb_nocut.txt", sep = "\t", header = TRUE, row.names=1) 
fg.dat = fg_dat[,5:ncol(fg_dat)]  #60:114] #5:59]  #

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
dist.method = "sorensen"

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

geodis.lg = log10(geodis)
beta.sim.lg = log10(beta.sim)
cor1 = cor.test(geodis.lg, beta.sim.lg)
cor1
cor1$p.value


#ppi = 500
#png("eCO2_DDR_sorensen.png", width=6*ppi, height=6*ppi, res=ppi)
plot(geodis.lg, beta.sim.lg, xlab="log(Geo distance (km))", ylab="log(Similarity)", cex.axis=1.5,cex.lab = 1.5)
text(700, 0.75, pos=4, labels = paste("r =", signif(cor1$estimate,digits=3), ", p < 0.001"), cex = 1.5)
#title(paste("r =", signif(cor$estimate,digits=3), ", p =", signif(cor$p.value,digits=3)))
lm = lm(beta.sim.lg~geodis.lg)
abline(lm, col="red")
#dev.off()
intercept = lm$coefficients[1]
slope = lm$coefficients[2]
slope

#----replot by ggplot2-----
dat.4plot = as.data.frame(cbind(as.vector(geodis.lg), as.vector(beta.sim.lg)))
colnames(dat.4plot) = c("lg.geodis", "lg.sim")
library("ggplot2")
theme_set(theme_gray(base_size = 18))
g1 = ggplot(dat.4plot, aes(x=lg.geodis, y=lg.sim))+
  geom_point()+
  geom_smooth(method="lm", size=1,fill=NA)+ 
  xlab("log(Geo distance (km))") +
  ylab("log(Similarity)")

ppi = 300
#png("eCO2_DDR_sorensen.png", width=6*ppi, height=6*ppi, res=ppi)
print(g1+ggtitle(bquote(r~"="~.(signif(cor1$estimate,digits=3))~", slope="~.(signif(slope,digits=3))))+
        theme(panel.background=element_rect(fill="white"),
              axis.title=element_text(size=24),
              axis.text=element_text(size=20),
              legend.position="none",
              panel.border = element_blank(), 
              axis.line = element_line()))
#dev.off()


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

# for different scale #
aCO2.geo.dist = as.dist(aCO2.geodis)
small = which(aCO2.geo.dist <= 0)
large = which(aCO2.geo.dist > 0)

lm2.small = lm(as.dist(aCO2.beta.sim)[small] ~ aCO2.geo.dist[small])
lm2.large = lm(as.dist(aCO2.beta.sim)[large] ~ aCO2.geo.dist[large])

eCO2.geo.dist = as.dist(eCO2.geodis)

lm3.small = lm(as.dist(eCO2.beta.sim)[small] ~ eCO2.geo.dist[small])
lm3.large = lm(as.dist(eCO2.beta.sim)[large] ~ eCO2.geo.dist[large])

c(lm2$coefficients[2], lm2.small$coefficients[2], lm2.large$coefficients[2])

c(lm3$coefficients[2], lm3.small$coefficients[2], lm3.large$coefficients[2])



#--- for plot both eCO2 and aCO2 together-----

dat.4plot = as.data.frame(cbind(as.dist(aCO2.geodis), as.dist(aCO2.beta.sim)))
dat.4p    = as.data.frame(cbind(as.dist(eCO2.geodis), as.dist(eCO2.beta.sim)))
dat.4plot = rbind(dat.4plot, dat.4p)
grp1 = rep(c("a", "e"), each= 1485)
grp2 = grp1
grp2[grp2=="a"] = "grey"
grp2[grp2=="e"] = "black"

colnames(dat.4plot) = c("lg.geodis", "lg.sim")

library("ggplot2")
#library("easyGgplot2")

theme_set(theme_gray(base_size = 18))
g1 = ggplot(dat.4plot, aes(x=lg.geodis, y=lg.sim, color=grp1))+
  geom_point(color=grp2)+
  geom_smooth(method="lm", size=1, se=FALSE)+ #,fill=NA
  xlab("log(Geo distance (km))") +
  ylab("log(Similarity)")

slope1 = lm2$coefficients[2]
slope2 = lm3$coefficients[2]

ppi = 300
png("DDR_sorensen_comb2.png", width=7*ppi, height=6*ppi, res=ppi)
print(g1+ggtitle(bquote(aCO2~slope~"="~.(signif(slope1,digits=3))~", eCO2 slope="~.(signif(slope2,digits=3))))+
        theme(panel.background=element_rect(fill="white"),
              axis.title=element_text(size=24),
              axis.text=element_text(size=20),
              #legend.position="none",
              panel.border = element_blank(), 
              axis.line = element_line()))
dev.off()


# for different scale #

grp3 = grp1
grp3[grp3=="a"][large] = "large"
grp3[grp3=="e"][large] = "large"
dat.4plot2 = dat.4plot[which(grp3=="large"),  ]
grp4 = grp1[which(grp3=="large")]
grp5 = grp4
grp5[grp5=="a"] = "grey"
grp5[grp5=="e"] = "black"


inter1 = as.numeric(lm2.large$coefficients[1])
slope1 = as.numeric(lm2.large$coefficients[2])

inter2 = as.numeric(lm3.large$coefficients[1])
slope2 = as.numeric(lm3.large$coefficients[2])

theme_set(theme_gray(base_size = 18))
g1 = ggplot(dat.4plot2, aes(x=lg.geodis, y=lg.sim, color=grp4))+
  geom_point(color=grp5)+
  geom_smooth(method="lm", size=1, se=F)+ #,fill=NA
  #geom_abline(aes(intercept=inter1, slope=slope1, colour="red", size=1))+
  #geom_abline(aes(intercept=inter2, slope=slope2, colour="blue", size=1))+
  xlab("log(Geo distance (km))") +
  ylab("log(Similarity)")


ppi = 300
png("DDR_sorensen_comb_large.png", width=7*ppi, height=6*ppi, res=ppi)
print(g1+ggtitle(bquote(aCO2~slope~"="~.(signif(slope1,digits=3))~", eCO2 slope="~.(signif(slope2,digits=3))))+
        theme(panel.background=element_rect(fill="white"),
              axis.title=element_text(size=24),
              axis.text=element_text(size=20),
              #legend.position="none",
              panel.border = element_blank(), 
              axis.line = element_line()))

dev.off()

cor.test(as.dist(aCO2.beta.sim)[large], aCO2.geo.dist[large])

cor.test(as.dist(eCO2.beta.sim)[large], eCO2.geo.dist[large])





# for permutation test on slope value (randomize the pair of distances)----
r = 1000  # number of random times
perm.slope = c()
eCO2.perm.slope = c()
aCO2.perm.slope = c()
for(i in 1:r){
	perm.geodis = sample(geodis, replace=T)
	perm.beta.sim = sample(beta.sim, replace=T)
	#perm.cor = cor.test(perm.geodis, perm.beta.sim)
	perm.lm = lm(perm.beta.sim~perm.geodis)
	perm.slope = c(perm.slope, perm.lm$coefficients[2])
  
	aCO2.perm.geodis = sample(aCO2.geodis, replace=T)
	aCO2.perm.beta.sim = sample(aCO2.beta.sim, replace=T)
	aCO2.perm.lm = lm(aCO2.perm.beta.sim~aCO2.perm.geodis)
	aCO2.perm.slope = c(aCO2.perm.slope, aCO2.perm.lm$coefficients[2])
  
	eCO2.perm.geodis = sample(eCO2.geodis, replace=T)
	eCO2.perm.beta.sim = sample(eCO2.beta.sim, replace=T)
	eCO2.perm.lm = lm(eCO2.perm.beta.sim~eCO2.perm.geodis)
	eCO2.perm.slope = c(eCO2.perm.slope, eCO2.perm.lm$coefficients[2])
}

t.test(aCO2.perm.slope, mu=slope1, alternative="two.sided")
c(sd(eCO2.perm.slope), sd(aCO2.perm.slope))


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

#for Distance decay on each gene category and some bigger genes----
gene.cat = fg_dat[,2]
cat.list = unique(gene.cat)
log.geodis = log10(geodis)

perm.slope = function(x, y, r=500){
  perm_slope = c()
  for(i in 1:r){
    perm.x = sample(x, replace=T)
    perm.y = sample(y, replace=T)
    perm.lm = lm(perm.y~perm.x)
    perm_slope = c(perm_slope, perm.lm$coefficients[2])
  }
  return(perm_slope)
}

t.test2 <- function(m1,m2,s1,s2,n1,n2,m0=0,equal.variance=FALSE)
{
  if( equal.variance==FALSE ) 
  {
    se <- sqrt( (s1^2/n1) + (s2^2/n2) )
    # welch-satterthwaite df
    df <- ( (s1^2/n1 + s2^2/n2)^2 )/( (s1^2/n1)^2/(n1-1) + (s2^2/n2)^2/(n2-1) )
  } else
  {
    # pooled standard deviation, scaled by the sample sizes
    se <- sqrt( (1/n1 + 1/n2) * ((n1-1)*s1^2 + (n2-1)*s2^2)/(n1+n2-2) ) 
    df <- n1+n2-2
  }      
  t <- (m1-m2-m0)/se 
  dat <- c(t, 2*pt(-abs(t),df))    
  names(dat) <- c("t", "p.value")
  return(dat) 
}

rep = c()
list.cat = c()
for(i in 1:length(cat.list)){
  cat = as.character(cat.list[i])
  row.valid = which(gene.cat == cat)
  sub.dat = fg.dat[row.valid,]
  sub.dat.aCO2 = fg.dat[row.valid,1:55]
  sub.dat.eCO2 = fg.dat[row.valid,56:110]
  gene = fg_dat[row.valid,1]
  gene.table = table(gene)
  
  tot.int = colSums(sub.dat)
  if(length(which(tot.int==0))>0){
    print(paste(cat, "has some samples without any data"))
    next
  }
  
  beta.dist.aCO2 = vegdist(t(sub.dat.aCO2),method = "bray", binary=TRUE) #Sorensen
  beta.sim.aCO2 = 1-beta.dist.aCO2
  
  beta.dist.eCO2 = vegdist(t(sub.dat.eCO2),method = "bray", binary=TRUE) #Sorensen
  beta.sim.eCO2 = 1-beta.dist.eCO2
  
  aCO2.geodis   = as.dist(as.matrix(log.geodis)[1:55, 1:55])
  eCO2.geodis   = as.dist(as.matrix(log.geodis)[56:110, 56:110])
  
  r.num = 100
  lm1 = lm(log10(beta.sim.aCO2) ~ aCO2.geodis)
  perm.slope1 = perm.slope(aCO2.geodis, log10(beta.sim.aCO2), r=r.num) #permutation
  ttest1 = t.test(perm.slope1, mu = lm1$coefficients[2], alternative = "two.sided")
  cor1 = cor.test(log10(beta.sim.aCO2), aCO2.geodis)
  
  lm2 = lm(log10(beta.sim.eCO2) ~ eCO2.geodis)
  perm.slope2 = perm.slope(eCO2.geodis, log10(beta.sim.eCO2), r=r.num) #permutation
  ttest2 = t.test(perm.slope2, mu = lm2$coefficients[2], alternative = "two.sided")
  cor2 = cor.test(log10(beta.sim.eCO2), eCO2.geodis)
  
  #--test difference between aCO2 and eCO2
  slope.aCO2 = lm1$coefficients[2]
  slope.eCO2 = lm2$coefficients[2]
  sd1 = sd(perm.slope1)
  sd2 = sd(perm.slope2)
  ttest.between = t.test2(slope.aCO2, slope.eCO2, sd1, sd2, r.num, r.num)
  #---
  rep = rbind(rep, c(cor1$estimate, cor1$p.value, slope.aCO2, ttest1$p.value, cor2$estimate, cor2$p.value, slope.eCO2, ttest2$p.value, ttest.between[2]))
  list.cat = c(list.cat, cat)
  
  big.gene = which(gene.table>200)
  for(j in 1:length(big.gene)){
    row.valid = which(gene == as.character(names(big.gene)[j]))
    sub.dat = fg.dat[row.valid,]
    sub.dat.aCO2 = fg.dat[row.valid,1:55]
    sub.dat.eCO2 = fg.dat[row.valid,56:110]
    
    tot.int = colSums(sub.dat)
    if(length(which(tot.int==0))>0){
      print(paste(names(big.gene)[j], "has some samples without any data"))
      next
    }
    
    beta.dist.aCO2 = vegdist(t(sub.dat.aCO2),method = "bray", binary=TRUE) #Sorensen
    beta.sim.aCO2 = 1-beta.dist.aCO2
    
    beta.dist.eCO2 = vegdist(t(sub.dat.eCO2),method = "bray", binary=TRUE) #Sorensen
    beta.sim.eCO2 = 1-beta.dist.eCO2
    
    lm1 = lm(log10(beta.sim.aCO2) ~ aCO2.geodis)
    perm.slope1 = perm.slope(aCO2.geodis, log10(beta.sim.aCO2), r=r.num) #permutation
    ttest1 = t.test(perm.slope1, mu = lm1$coefficients[2], alternative = "two.sided")
    cor1 = cor.test(log10(beta.sim.aCO2), aCO2.geodis)
    
    lm2 = lm(log10(beta.sim.eCO2) ~ eCO2.geodis)
    perm.slope2 = perm.slope(eCO2.geodis, log10(beta.sim.eCO2), r=r.num) #permutation
    ttest2 = t.test(perm.slope2, mu = lm2$coefficients[2], alternative = "two.sided")
    cor2 = cor.test(log10(beta.sim.eCO2), eCO2.geodis)
    
    #--test difference between aCO2 and eCO2
    slope.aCO2 = lm1$coefficients[2]
    slope.eCO2 = lm2$coefficients[2]
    sd1 = sd(perm.slope1)
    sd2 = sd(perm.slope2)
    ttest.between = t.test2(slope.aCO2, slope.eCO2, sd1, sd2, r.num, r.num)
        
    rep = rbind(rep, c(cor1$estimate, cor1$p.value, slope.aCO2, ttest1$p.value, cor2$estimate, cor2$p.value, slope.eCO2, ttest2$p.value, ttest.between[2]))
    list.cat = c(list.cat, names(big.gene)[j])
    
  }
}
rownames(rep) = list.cat
#colnames(rep) = c("r", "p", "z")
rep




# for multiple regression on matrices (MRM)
library(ecodist)
env_dat = read.table("../all110_soil.txt",sep="\t",header=T,row.names=1)
env_dat[is.na(env_dat)] = 0

# match env and fg datasets
fg.dat= t(fg.dat)
samp.fg = rownames(fg.dat)
samp.env= rownames(env_dat)
my.env = match(samp.fg, samp.env)
env.st = na.omit(env_dat[my.env, ])  # omit the NA rows if without fg data
samp.env= rownames(env.st)
my.fg = match(samp.env, samp.fg)
fg.dat2 = fg.dat[my.fg, ] 
fg.dat2[is.na(fg.dat2)] = 0

#env.st2 = decostand(env.st, method="standardize", MARGIN=2)
env.st2 = env.st

#dist.fg = vegdist(fg.dat2,method = "bray", binary=TRUE) # Sorensen
dist.fg = vegdist(fg.dat2,method = "bray") 

dist.method = "euclidean"
dist.NO3 = vegdist(env.st2[,1],method = dist.method)
dist.NH4 = vegdist(env.st2[,2],method = dist.method)
dist.N0.10 = vegdist(env.st2[,3],method = dist.method)
dist.C0.10 = vegdist(env.st2[,4],method = dist.method)
dist.SCN0.10 = vegdist(env.st2[,5],method = dist.method)


mrm1 = MRM(dist.fg ~ geodis + dist.NO3 + dist.NH4 + dist.N0.10 + dist.C0.10 + dist.SCN0.10)
mrm1


# After taking log
dist.fg.lg = log10(dist.fg)  #take log by following Martiny's PNAS 2011

geodis.lg = log10(geodis)

dist.method = "euclidean"
dist.NO3 = vegdist(log10(env.st[,1]+1),method = dist.method)
dist.NH4 = vegdist(log10(env.st[,2]),method = dist.method)
dist.N0.10 = vegdist(log10(env.st[,3]),method = dist.method)
dist.C0.10 = vegdist(log10(env.st[,4]),method = dist.method)
dist.SCN0.10 = vegdist(log10(env.st[,5]),method = dist.method)

mrm1 = MRM(dist.fg.lg ~ geodis.lg + dist.NO3 + dist.NH4 + dist.N0.10 + dist.C0.10 + dist.SCN0.10)
mrm1


##==Distance decay on env variables=======

env.st2 = decostand(env.st, method="standardize", MARGIN=2)  #env.st #

# for overall variables
env.dist = vegdist(env.st2, method = "euclidean")
#env.dist = env.dist/max(env.dist)
#env.sim = 1 - env.dist
#env.dist.lg = log10(env.dist)

geodis.lg = log10(geodis)
cor1 = cor.test(geodis.lg, env.dist)
lm = lm(env.dist ~ geodis.lg)
slope = lm$coefficients[2]

dat.4plot = as.data.frame(cbind(as.vector(geodis.lg), as.vector(env.dist)))
colnames(dat.4plot) = c("lg.geodis", "env.dist")
library("ggplot2")
theme_set(theme_gray(base_size = 18))
g1 = ggplot(dat.4plot, aes(x=lg.geodis, y=env.dist))+
  geom_point()+
  geom_smooth(method="lm", size=1,fill=NA)+ 
  xlab("log(Geo distance (km))") +
  ylab("Euclidean distance")

ppi = 300
png("aCO2_env_DDR.png", width=6*ppi, height=6*ppi, res=ppi)
print(g1+ggtitle(bquote(r~"="~.(signif(cor1$estimate,digits=3))~", slope="~.(signif(slope,digits=3))))+
        theme(panel.background=element_rect(fill="white"),
              axis.title=element_text(size=24),
              axis.text=element_text(size=20),
              legend.position="none",
              panel.border = element_blank(), 
              axis.line = element_line()))
dev.off()

#for each variable

for(i in 1:ncol(env.st)){
  env.dist = vegdist(env.st[,i], method = "euclidean")
  cor1 = cor.test(geodis.lg, env.dist)
  lm = lm(env.dist ~ geodis.lg)
  slope = lm$coefficients[2]
  
  dat.4plot = as.data.frame(cbind(as.vector(geodis.lg), as.vector(env.dist)))
  colnames(dat.4plot) = c("lg.geodis", "env.dist")
  
  g1 = ggplot(dat.4plot, aes(x=lg.geodis, y=env.dist))+
    geom_point()+
    geom_smooth(method="lm", size=1,fill=NA)+ 
    xlab("log(Geo distance (km))") +
    ylab("Euclidean distance")
  
  ppi = 300
  png(paste("aCO2", colnames(env.st)[i],"DDR.png"), width=6*ppi, height=6*ppi, res=ppi)
  print(g1+ggtitle(bquote(r~"="~.(signif(cor1$estimate,digits=3))~", p="~.(signif(cor1$p.value,digit=2))~", slope="~.(signif(slope,digits=3))))+
          theme(panel.background=element_rect(fill="white"),
                axis.title=element_text(size=24),
                axis.text=element_text(size=20),
                legend.position="none",
                panel.border = element_blank(), 
                axis.line = element_line()))
  dev.off()
  
}



