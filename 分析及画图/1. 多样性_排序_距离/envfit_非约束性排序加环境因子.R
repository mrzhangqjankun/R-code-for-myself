##2019.3.6
#http://blog.sciencenet.cn/blog-267448-1025741.html

library(vegan)

data(varechem)   ##环境因子

data(varespec)   ##OTU

vare.mds<- metaMDS(varespec)

ef <- envfit(vare.mds, varechem, permu = 999)

ef

plot(vare.mds, display = "sites")

plot(ef, p.max = 0.1) #Maximum estimated P value for displayed variables. 
?envfit
##
pca = rda(varespec)

ef <- envfit(pca, varechem, permu = 999)
ef
plot(pca,display = "sites")
plot(ef, p.max = 0.1)

#
ef <- envfit(vare.mds ~ Al + Ca, varechem)

plot(vare.mds, display = "sites")

plot(ef)

tmp <- with(varechem, ordisurf(vare.mds, Al, add = TRUE))  ##ordisurf 添加趋势面

with(varechem, ordisurf(vare.mds, Ca, add = TRUE, col = "green4"))
