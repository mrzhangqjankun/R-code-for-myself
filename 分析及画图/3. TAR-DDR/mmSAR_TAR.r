##2018.12.25
##http://mmsar.r-forge.r-project.org/documentation_usage.php

rm(list=ls())
#install.packages("mmSAR", repos="http://R-Forge.R-project.org")

library(mmSAR)

######################################Use case 1 : the simple case. 一个数据一个模型
#Basic non linear SAR model fits are obtained with the rssoptim function,
#this function takes for arguments a model object and a data set object. 
#loading the exponential model
data(expo)
expo
#loading the Galapagos Islands plants data set (Preston, 1962)
data(data.galap)
data.galap
#fitting the exponential model to the Galapagos dataset
res <- rssoptim(expo,data.galap)

data.galap$name

######################################Use case 2 : multimodel SARs. 一个数据多个模型
#Multimodel SAR fits are obtained with the multiSAR function,
#this function takes for arguments a vector of character strings for model names and a data set object.
#loading all available models
data(power)  ##幂律
data(expo)  ##指数
data(negexpo) ##负指数
data(monod)
data(ratio)  ##有理函数
data(logist)  ##logic
data(lomolino)
data(weibull)

#loading the Galapagos Islands plants data set (Preston, 1962)
data(data.galap)

#creating a vector of model names
mods <- c("power","expo","negexpo","monod","logist","ratio","lomolino","weibull")

#fitting all the models to the Galapagos dataset and perform multimodel averaging
#这一步比较慢
resAverage <- multiSAR(modelList=mods,data.galap)
str(resAverage)
resAverage$optimRes
###########################################Data sets
##示例datasets
# data.arr
# data.atl
# data.galap
# data.glea
# data.gleas
##格式
#mmSAR handles SAR data sets as list-objects. A data set is a list of 2 elements : 
  #$name : a character string specifying the name of the data set

  #$data : a R data.frame object with 2 columns :
  
  #$a : a numeric vector of areas

  #$s : a numeric vector of species richness
str(data.galap)

###########################################Models
#一共有上面八种模型


###########################################Functions
###model fitting
#rssoptim : function (model, data, norTest = "lillie", verb = T)
###multimodel averaging & confidence intervals
#multiSAR : function (modelList = c("power","expo","weibull"), data = data.galap, nBoot = 1000, verb = T, crit = "Bayes")

?mmSAR

##晔晔2018年的EM用的结果是AIC和R2
#Spatial scaling of forest soil microbial communities across a temperature gradient