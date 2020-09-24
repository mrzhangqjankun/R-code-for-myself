##li 2017.7.12 look a time


# MRT
library("mvpart")                  ##¶àÔª»Ø¹éÊ÷

setwd('D:/all.R-3.16/MRT')

rm(list=ls(all=TRUE))

fg_dat = read.table("SNR3-ave.txt",sep="\t",header=T,row.names=1)

group = read.table("env-ave.txt",sep="\t",header=T,row.names=1)
geochip = fg_dat
geochip[is.na(geochip)] = 0  
geochip = t(geochip)
dim(geochip)

colnames(group)
mrt_re1 = mvpart(data.matrix(geochip)~W1+pH+Temp+Year,group, xv="1se", xval=50, xvmult=10)
summary(mrt_re1)


mrt_re = mvpart(gdist(geochip,meth="bray",full=TRUE,sq=TRUE)~W1+pH+Temp+Year,group, xv="1se", xval=50, xvmult=10)
#bray,gower,euclidean,manhattan#