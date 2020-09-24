##2018.12.23
#Hmisc
#来源于2017ISME-Habitat-specific patterns and drivers of bacterial β-diversity in China’s drylands
#在multiple regression on matrices (MRM)之前先去掉共线性的环境因子。Spearman’s ρ2>0.7的环境因子都被丢弃


#包帮助文档
##https://cran.r-project.org/web/packages/Hmisc/Hmisc.pdf
#392页，varclus


#install.packages("Hmisc")
library(Hmisc)

set.seed(1) 
x1 <- rnorm(200) 
x2 <- rnorm(200) 
x3 <- x1 + x2 + rnorm(200) 
x4 <- x2 + rnorm(200) 
x <- cbind(x1,x2,x3,x4) ;head(x)
v <- varclus(x, similarity="spear")
# spearman is the default 
v # invokes print.varclus 
str(v)
print(round(v$sim,2)) 
plot(v)

?varclus
