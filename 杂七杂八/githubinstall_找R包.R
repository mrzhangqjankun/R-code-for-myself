##2019.9.25

##'githubinstall'
##'http://wap.sciencenet.cn/blog-267448-1052661.html
##'如果CRAN上找不到包，那怎么办？ 
##
#如果CRAN包找不到的包，可以使用githubinstall函数来找包的资源
install.packages('githubinstall') #已发布至CRAN
library(githubinstall)
githubinstall('AnomalyDetection')#输入任何包的名称即可在web上找到这些包的位置

githubinstall('microbiome')
?githubinstall
