##2019.10.9


library(Morpho)
?CVA


## all examples are kindly provided by Marta Rufino

if (require(shapes)) {
  # perform procrustes fit on raw data
  alldat<-procSym(abind(gorf.dat,gorm.dat))
  # create factors
  groups<-as.factor(c(rep("female",30),rep("male",29)))
  # perform CVA and test Mahalanobis distance
  # between groups with permutation test by 100 rounds)            
  cvall<-CVA(alldat$orpdata,groups,rounds=10000)     
  ## visualize a shape change from score -5 to 5:
  cvvis5 <- 5*matrix(cvall$CVvis[,1],nrow(cvall$Grandm),ncol(cvall$Grandm))+cvall$Grandm
  cvvisNeg5 <- -5*matrix(cvall$CVvis[,1],nrow(cvall$Grandm),ncol(cvall$Grandm))+cvall$Grandm
  plot(cvvis5,asp=1)
  points(cvvisNeg5,col=2)
  for (i in 1:nrow(cvvisNeg5))
    lines(rbind(cvvis5[i,],cvvisNeg5[i,]))
}
### Morpho CVA
data(iris)
vari <- iris[,1:4]
facto <- iris[,5]

cva.1=CVA(vari, groups=facto)
## get the typicality probabilities and resulting classifications - tagging
## all specimens with a probability of < 0.01 as outliers (assigned to no class)
typprobs <- typprobClass(cva.1$CVscores,groups=facto)
print(typprobs)
## visualize the CV scores by their groups estimated from (cross-validated)
## typicality probabilities:
if (require(car)) {
  scatterplot(cva.1$CVscores[,1],cva.1$CVscores[,2],groups=typprobs$groupaffinCV,
              smooth=FALSE,reg.line=FALSE)
}
# plot the CVA
plot(cva.1$CVscores, col=facto, pch=as.numeric(facto), typ="n",asp=1,
     xlab=paste("1st canonical axis", paste(round(cva.1$Var[1,2],1),"%")),
     ylab=paste("2nd canonical axis", paste(round(cva.1$Var[2,2],1),"%")))

text(cva.1$CVscores, as.character(facto), col=as.numeric(facto), cex=.7)

# add chull (merge groups)
for(jj in 1:length(levels(facto))){
  ii=levels(facto)[jj]
  kk=chull(cva.1$CVscores[facto==ii,1:2])
  lines(cva.1$CVscores[facto==ii,1][c(kk, kk[1])],
        cva.1$CVscores[facto==ii,2][c(kk, kk[1])], col=jj)
}

# add 80% ellipses
if (require(car)) {
  for(ii in 1:length(levels(facto))){
    dataEllipse(cva.1$CVscores[facto==levels(facto)[ii],1],
                cva.1$CVscores[facto==levels(facto)[ii],2], 
                add=TRUE,levels=.80, col=c(1:7)[ii])}
}
# histogram per group
if (require(lattice)) {
  histogram(~cva.1$CVscores[,1]|facto,
            layout=c(1,length(levels(facto))),
            xlab=paste("1st canonical axis", paste(round(cva.1$Var[1,2],1),"%")))
  histogram(~cva.1$CVscores[,2]|facto, layout=c(1,length(levels(facto))),
            xlab=paste("2nd canonical axis", paste(round(cva.1$Var[2,2],1),"%")))
} 
# plot Mahalahobis
dendroS=hclust(cva.1$Dist$GroupdistMaha)
dendroS$labels=levels(facto)
par(mar=c(4,4.5,1,1))
dendroS=as.dendrogram(dendroS)
plot(dendroS, main='',sub='', xlab="Geographic areas",
     ylab='Mahalahobis distance')


# Variance explained by the canonical roots:
cva.1$Var
# or plot it:
barplot(cva.1$Var[,2])

# another landmark based example in 3D: 
data(boneData)
groups <- name2factor(boneLM,which=3:4)
proc <- procSym(boneLM)
cvall<-CVA(proc$orpdata,groups)    
#' ## visualize a shape change from score -5 to 5:
cvvis5 <- 5*matrix(cvall$CVvis[,1],nrow(cvall$Grandm),ncol(cvall$Grandm))+cvall$Grandm
cvvisNeg5 <- -5*matrix(cvall$CVvis[,1],nrow(cvall$Grandm),ncol(cvall$Grandm))+cvall$Grandm
## Not run: 
#visualize it
deformGrid3d(cvvis5,cvvisNeg5,ngrid = 0)

## End(Not run)

#for using (e.g. the first 5) PCscores, one will do:
cvall <- CVA(proc$PCscores[,1:5],groups)    
#' ## visualize a shape change from score -5 to 5:
cvvis5 <- 5*cvall$CVvis[,1]+cvall$Grandm
cvvisNeg5 <- -5*cvall$CVvis[,1]+cvall$Grandm
cvvis5 <- showPC(cvvis5,proc$PCs[,1:5],proc$mshape)
cvvisNeg5 <- showPC(cvvisNeg5,proc$PCs[,1:5],proc$mshape)
## Not run: 
#visualize it
deformGrid3d(cvvis5,cvvisNeg5,ngrid = 0)

## End(Not run)
dev.off()
X11()
##############################groupPCA 

data(iris)
vari <- iris[,1:4]
facto <- iris[,5]
pca.1 <-groupPCA(vari,groups=facto,rounds=100,mc.cores=1)
str(pca.1)

### plot scores
if (require(car)) {
  scatterplotMatrix(pca.1$Scores,groups=facto, ellipse=TRUE,
                    by.groups=TRUE,var.labels=c("PC1","PC2","PC3"))
}

##普通PCA
library(vegan)
r = summary(rda(t(vari)))
str(r)
a = r$species ;a
b = r$cont$importance ;b
class(facto)
data.plot = cbind(a,as.vector(facto))
colnames(data.plot) = c("PC1","PC2","PC3","group")
rownames(data.plot) = NULL
data.plot = as.data.frame(data.plot)

plot_theme = theme(panel.background=element_blank(),
                   panel.grid=element_blank(),
                   axis.line.x=element_line(size=.5, colour="black"),
                   axis.line.y=element_line(size=.5, colour="black"),
                   axis.ticks=element_line(color="black"),
                   axis.text=element_text(color="black", size=7),
                   legend.position="right",
                   legend.background=element_blank(),
                   legend.key=element_blank(),
                   legend.text= element_text(size=7),
                   text=element_text(family="sans", size=7)
) + theme_bw()
p = ggplot(data.plot,aes(PC1,PC2))
p = p + geom_point(aes(colour = group,shape = group),alpha=0.5,size = 3);p
p = p +  plot_theme + theme(plot.title=element_text(hjust=0.5));p
p = p + theme_bw();p
p=p+theme(axis.text= element_text(size=16, color="black", family  = "serif", face= "bold", vjust=0.5, hjust=0.5))+
  theme(axis.title = element_text(size=16, color="black", family  = "serif",face= "bold", vjust=0.5, hjust=0.5))+
  theme(legend.text = element_text(colour = 'black', size = 16,  family  = "serif",face = 'bold'))+
  theme(legend.title=element_blank());p
p=p+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())
p = p + scale_x_discrete(labels = NULL)+scale_y_discrete(labels = NULL)
p = p +  stat_ellipse(colour = as.factor(facto),level=0.95,linetype = 2);p  



## example with shape data
data(boneData)
proc <- procSym(boneLM)
pop_sex <- name2factor(boneLM, which=3:4)
gpca <- groupPCA(proc$orpdata, groups=pop_sex, rounds=0, mc.cores=2)
## Not run: 
## visualize shape associated with first between group PC
dims <- dim(proc$mshape)
## calculate matrix containing landmarks of grandmean
grandmean <-gpca$Grandmean
## calculate landmarks from first between-group PC
#                   (+2 and -2 standard deviations)
gpcavis2sd<- showPC(2*sd(gpca$Scores[,1]), gpca$groupPCs[,1], grandmean)
gpcavis2sd.neg<- showPC(-2*sd(gpca$Scores[,1]), gpca$groupPCs[,1], grandmean)
deformGrid3d(gpcavis2sd, gpcavis2sd.neg, ngrid = 0)
require(rgl)
## visualize grandmean mesh

grandm.mesh <- tps3d(skull_0144_ch_fe.mesh, boneLM[,,1],grandmean,threads=1)
wire3d(grandm.mesh, col="white")
spheres3d(grandmean, radius=0.005)

## End(Not run)