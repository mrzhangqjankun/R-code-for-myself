##2019.4.25
##mvabund: Statistical Methods for Analysing Multivariate Abundance Data

#install.packages("mvabund")
library(mvabund);?mvabund
#多元广义线性模型


data(solberg) 

## Create an mvabund object:
solbergdat <- mvabund(solberg$abund)

## Turn solberg$abund into an mvabund object and store as solbergdat:
solbergdat <- as.mvabund(solberg$abund)

## Check if solbergdat  is an mvabund object:
is.mvabund(solbergdat)
