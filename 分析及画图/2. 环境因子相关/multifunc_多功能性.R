#2019.11.28

#生态系统多功能性

#multifunc
#https://github.com/jebyrnes/multifunc

library(devtools)
install_github("jebyrnes/multifunc")

library(multifunc)


data(all_biodepth)
#qw添加引号
allVars<-qw(biomassY3, root3, N.g.m2,  light3, N.Soil, wood3, cotton3)

germany<-subset(all_biodepth, all_biodepth$location=="Germany")

vars<-whichVars(germany, allVars)

#re-normalize N.Soil so that everything is on the same 
#sign-scale (e.g. the maximum level of a function is 
#the "best" function)
germany$N.Soil<- -1*germany$N.Soil +max(germany$N.Soil, na.rm=TRUE)

germanyThresh<-getFuncsMaxed(germany, vars, threshmin=0.05, 
                             threshmax=0.99, prepend=c("plot","Diversity"), maxN=7)

germanyLinearSlopes<-getCoefTab(funcMaxed ~ Diversity, 
                                data=germanyThresh, coefVar="Diversity", family=quasipoisson(link="identity"))
